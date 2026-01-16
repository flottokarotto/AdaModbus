#!/bin/bash
# QEMU Launch Script for STM32 RTU Examples
# Copyright (c) 2026 Florian Fischer
# SPDX-License-Identifier: MIT
#
# Usage:
#   ./run_qemu.sh slave              # Start slave only
#   ./run_qemu.sh master             # Start master only
#   ./run_qemu.sh both               # Start both connected via socket
#
# Prerequisites:
#   - qemu-system-arm installed
#   - Built ELF files in bin/ directory

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BIN_DIR="$SCRIPT_DIR/bin"
QEMU=qemu-system-arm
MACHINE=lm3s6965evb
SOCKET_PORT=5555

# Check QEMU
if ! command -v $QEMU &> /dev/null; then
    echo "Error: $QEMU not found. Please install QEMU."
    exit 1
fi

start_slave() {
    local SERIAL_OPT="$1"
    echo "Starting RTU Slave..."
    $QEMU \
        -M $MACHINE \
        -nographic \
        -semihosting \
        -kernel "$BIN_DIR/main_slave.elf" \
        $SERIAL_OPT
}

start_master() {
    local SERIAL_OPT="$1"
    echo "Starting RTU Master..."
    $QEMU \
        -M $MACHINE \
        -nographic \
        -semihosting \
        -kernel "$BIN_DIR/main_master.elf" \
        $SERIAL_OPT
}

case "${1:-help}" in
    slave)
        # Standalone slave with stdio
        start_slave "-serial stdio"
        ;;
    master)
        # Standalone master with stdio
        start_master "-serial stdio"
        ;;
    both)
        echo "Starting Master-Slave test via socket on port $SOCKET_PORT"
        echo "Press Ctrl+A X to exit QEMU"
        echo ""

        # Start slave as server in background
        echo "Starting Slave (server)..."
        $QEMU \
            -M $MACHINE \
            -nographic \
            -semihosting \
            -kernel "$BIN_DIR/main_slave.elf" \
            -serial socket:localhost:$SOCKET_PORT,server,nowait &
        SLAVE_PID=$!

        # Wait a bit for slave to start
        sleep 1

        # Start master as client
        echo "Starting Master (client)..."
        $QEMU \
            -M $MACHINE \
            -nographic \
            -semihosting \
            -kernel "$BIN_DIR/main_master.elf" \
            -serial socket:localhost:$SOCKET_PORT

        # Cleanup
        kill $SLAVE_PID 2>/dev/null
        ;;
    slave-server)
        # Slave as socket server (for connecting external master)
        echo "Starting Slave as socket server on port $SOCKET_PORT"
        echo "Connect with: socat - TCP:localhost:$SOCKET_PORT"
        start_slave "-serial socket:localhost:$SOCKET_PORT,server"
        ;;
    master-client)
        # Master as socket client (for connecting to external slave)
        start_master "-serial socket:localhost:$SOCKET_PORT"
        ;;
    help|*)
        echo "QEMU Launch Script for STM32 RTU Examples"
        echo ""
        echo "Usage: $0 <command>"
        echo ""
        echo "Commands:"
        echo "  slave         Start slave with UART on stdio"
        echo "  master        Start master with UART on stdio"
        echo "  both          Start both connected via socket (automatic test)"
        echo "  slave-server  Start slave as socket server (port $SOCKET_PORT)"
        echo "  master-client Start master connecting to socket server"
        echo ""
        echo "Build first:"
        echo "  gprbuild -P stm32_rtu.gpr -XMAIN=slave"
        echo "  gprbuild -P stm32_rtu.gpr -XMAIN=master"
        ;;
esac
