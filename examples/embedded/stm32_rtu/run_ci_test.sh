#!/bin/bash
# run_ci_test.sh - Run RTU master/slave test in QEMU for CI
# Copyright (c) 2026 Florian Fischer
# SPDX-License-Identifier: MIT
#
# This script starts a slave QEMU instance in the background,
# then runs the master which performs tests and exits with status code.
#
# NOTE: We use lm3s6965evb instead of netduinoplus2/STM32F4 because
# QEMU's netduinoplus2 emulation does not support ARM semihosting.
# The lm3s6965evb is a well-supported reference platform in QEMU
# with full semihosting support for debug output.

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
BIN_DIR="$SCRIPT_DIR/bin"
SLAVE_ELF="$BIN_DIR/main_slave.elf"
MASTER_ELF="$BIN_DIR/main_master.elf"

PORT=5555
TIMEOUT_SECONDS=60

# Check that binaries exist
if [ ! -f "$SLAVE_ELF" ]; then
    echo "ERROR: Slave binary not found: $SLAVE_ELF"
    exit 1
fi

if [ ! -f "$MASTER_ELF" ]; then
    echo "ERROR: Master binary not found: $MASTER_ELF"
    exit 1
fi

echo "=== Starting Modbus RTU CI Test ==="
echo "Slave: $SLAVE_ELF"
echo "Master: $MASTER_ELF"
echo ""

# Create temp files for output
SLAVE_LOG=$(mktemp)
MASTER_LOG=$(mktemp)
trap "rm -f $SLAVE_LOG $MASTER_LOG; kill %1 2>/dev/null || true" EXIT

# Start slave QEMU instance in background
# Uses QEMU 8.x chardev syntax for socket
echo "Starting slave QEMU (port $PORT)..."
qemu-system-arm -M lm3s6965evb -nographic -semihosting \
    -kernel "$SLAVE_ELF" \
    -chardev socket,id=serial0,host=localhost,port=$PORT,server=on,wait=off \
    -serial chardev:serial0 \
    > "$SLAVE_LOG" 2>&1 &

SLAVE_PID=$!
echo "Slave PID: $SLAVE_PID"

# Wait for slave to start up and open the socket
sleep 2

# Check if slave is still running
if ! kill -0 $SLAVE_PID 2>/dev/null; then
    echo "ERROR: Slave QEMU exited unexpectedly"
    echo "Slave output:"
    cat "$SLAVE_LOG"
    exit 1
fi

# Run master QEMU instance
# The master will connect to the slave via socket and run tests
echo "Starting master QEMU..."
echo ""

# Run master and capture output
timeout $TIMEOUT_SECONDS qemu-system-arm -M lm3s6965evb -nographic -semihosting \
    -kernel "$MASTER_ELF" \
    -chardev socket,id=serial0,host=localhost,port=$PORT \
    -serial chardev:serial0 \
    2>&1 | tee "$MASTER_LOG" || true

# Kill the slave
kill $SLAVE_PID 2>/dev/null || true
wait $SLAVE_PID 2>/dev/null || true

echo ""
echo "=== Slave output ==="
cat "$SLAVE_LOG"
echo ""

# Check if tests passed by looking for the success message
if grep -q "ALL TESTS PASSED" "$MASTER_LOG"; then
    echo "=== CI Test Completed Successfully ==="
    exit 0
elif grep -q "TESTS FAILED" "$MASTER_LOG"; then
    echo "ERROR: Tests failed"
    exit 1
else
    echo "ERROR: Could not determine test result (timeout or other error)"
    exit 1
fi
