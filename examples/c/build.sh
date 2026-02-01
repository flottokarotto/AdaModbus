#!/bin/bash
# Build script for C examples
# This script builds the Ada library and links C examples against it.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

echo "=== Building AdaModbus C Examples ==="
echo

# Build Ada library first
echo "Building Ada library..."
cd "$PROJECT_ROOT"
gprbuild -p -P adamodbus.gpr

# Get library path
LIB_DIR="$PROJECT_ROOT/lib"
OBJ_DIR="$PROJECT_ROOT/obj"
BIN_DIR="$PROJECT_ROOT/bin"

mkdir -p "$BIN_DIR"
mkdir -p "$PROJECT_ROOT/obj/c_examples"

# Find Ada runtime library path
GNAT_LIB=$(dirname "$(gnatls -v 2>/dev/null | grep adalib | head -1 | sed 's/.*: //')" 2>/dev/null || echo "")

# Compile C examples
echo
echo "Compiling C examples..."

# Include paths
INCLUDES="-I$SCRIPT_DIR/../c_api"

# Compile c_tcp_master.c
echo "  Building c_tcp_master..."
gcc -Wall -Wextra -g -Og $INCLUDES \
    -c "$SCRIPT_DIR/c_tcp_master.c" \
    -o "$PROJECT_ROOT/obj/c_examples/c_tcp_master.o"

# Compile c_tcp_slave.c
echo "  Building c_tcp_slave..."
gcc -Wall -Wextra -g -Og $INCLUDES \
    -c "$SCRIPT_DIR/c_tcp_slave.c" \
    -o "$PROJECT_ROOT/obj/c_examples/c_tcp_slave.o"

# Link with Ada library
echo
echo "Linking executables..."

# Link c_tcp_master
echo "  Linking c_tcp_master..."
gnatbind -n -L adamodbus "$OBJ_DIR/simple_modbus-c_api.ali" 2>/dev/null || true
gnatlink -o "$BIN_DIR/c_tcp_master" \
    "$PROJECT_ROOT/obj/c_examples/c_tcp_master.o" \
    -L"$LIB_DIR" -ladamodbus \
    --LINK=gcc 2>/dev/null || \
gcc -o "$BIN_DIR/c_tcp_master" \
    "$PROJECT_ROOT/obj/c_examples/c_tcp_master.o" \
    -L"$LIB_DIR" -ladamodbus \
    -L"$GNAT_LIB" -lgnat -lws2_32

# Link c_tcp_slave
echo "  Linking c_tcp_slave..."
gcc -o "$BIN_DIR/c_tcp_slave" \
    "$PROJECT_ROOT/obj/c_examples/c_tcp_slave.o" \
    -L"$LIB_DIR" -ladamodbus \
    -L"$GNAT_LIB" -lgnat -lws2_32

echo
echo "Build complete!"
echo "Executables in: $BIN_DIR"
echo "  - c_tcp_master"
echo "  - c_tcp_slave"
