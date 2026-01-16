# Makefile for C examples
# Builds C programs that link against the libSimpleModbus Ada library
#
# Usage:
#   make          - Build all C examples
#   make clean    - Remove built files
#
# Note: Must be run from within Alire environment:
#   alr exec -- make

# Paths
PROJECT_ROOT := ../..
LIB_DIR := $(PROJECT_ROOT)/lib
OBJ_DIR := $(PROJECT_ROOT)/obj/c_examples
BIN_DIR := $(PROJECT_ROOT)/bin
C_API_DIR := $(PROJECT_ROOT)/src/c_api

# Compiler settings
CC := gcc
CFLAGS := -Wall -Wextra -std=c11 -g -O0
CPPFLAGS := -I$(C_API_DIR)

# Find GNAT adalib directory (contains libgnat.a)
# This assumes we're running within Alire environment
GNAT_PREFIX := $(shell dirname $(shell dirname $(shell which gnatmake 2>/dev/null)))
ADALIB_DIR := $(GNAT_PREFIX)/lib/gcc/x86_64-w64-mingw32/15.2.0/adalib

# Linker settings
LDFLAGS := -L$(LIB_DIR) -L$(ADALIB_DIR)
LDLIBS := -ladamodbus -lgnat -lws2_32

# Targets
TARGETS := $(BIN_DIR)/c_tcp_master.exe $(BIN_DIR)/c_tcp_slave.exe

.PHONY: all clean dirs

all: dirs $(TARGETS)
	@echo "Build complete!"
	@echo "  $(BIN_DIR)/c_tcp_master.exe"
	@echo "  $(BIN_DIR)/c_tcp_slave.exe"

dirs:
	@mkdir -p $(OBJ_DIR)
	@mkdir -p $(BIN_DIR)

$(OBJ_DIR)/%.o: %.c $(C_API_DIR)/ada_modbus.h
	@echo "Compiling $<..."
	$(CC) $(CFLAGS) $(CPPFLAGS) -c $< -o $@

$(BIN_DIR)/c_tcp_master.exe: $(OBJ_DIR)/c_tcp_master.o
	@echo "Linking $@..."
	$(CC) $< $(LDFLAGS) $(LDLIBS) -o $@

$(BIN_DIR)/c_tcp_slave.exe: $(OBJ_DIR)/c_tcp_slave.o
	@echo "Linking $@..."
	$(CC) $< $(LDFLAGS) $(LDLIBS) -o $@

clean:
	rm -f $(OBJ_DIR)/*.o
	rm -f $(BIN_DIR)/c_tcp_master.exe
	rm -f $(BIN_DIR)/c_tcp_slave.exe

help:
	@echo "Modbus C Examples Build"
	@echo ""
	@echo "Targets:"
	@echo "  all      - Build all C examples (default)"
	@echo "  clean    - Remove built files"
	@echo ""
	@echo "Usage: alr exec -- make"
