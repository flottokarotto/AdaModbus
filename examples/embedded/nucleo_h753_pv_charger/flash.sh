#!/bin/bash
# Flash script for NUCLEO-H753ZI
# Usage: ./flash.sh

set -e

ELF_FILE="bin/nucleo_h753_pv_charger.elf"
BIN_FILE="bin/nucleo_h753_pv_charger.bin"

# Check if ELF exists
if [ ! -f "$ELF_FILE" ]; then
    echo "Error: $ELF_FILE not found!"
    echo "Run 'gprbuild -P nucleo_h753_pv_charger.gpr' first."
    exit 1
fi

# Generate BIN from ELF
echo "Converting ELF to BIN..."
arm-none-eabi-objcopy -O binary "$ELF_FILE" "$BIN_FILE"

# Try different flash methods
if command -v st-flash &> /dev/null; then
    echo "Flashing with st-flash..."
    st-flash write "$BIN_FILE" 0x08000000
    echo "Done! Reset the board to run."

elif command -v openocd &> /dev/null; then
    echo "Flashing with OpenOCD..."
    openocd -f board/st_nucleo_h7.cfg \
        -c "program $ELF_FILE verify reset exit"
    echo "Done!"

elif command -v STM32_Programmer_CLI &> /dev/null; then
    echo "Flashing with STM32CubeProgrammer..."
    STM32_Programmer_CLI -c port=SWD -w "$BIN_FILE" 0x08000000 -v -rst
    echo "Done!"

else
    echo "Error: No flash tool found!"
    echo "Please install one of:"
    echo "  - stlink-tools (st-flash)"
    echo "  - openocd"
    echo "  - STM32CubeProgrammer"
    exit 1
fi
