#!/bin/bash
# Build, convert and flash firmware for NUCLEO-H753ZI
# Usage: ./flash.sh

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ELF_FILE="$SCRIPT_DIR/bin/main.elf"
BIN_FILE="$SCRIPT_DIR/bin/main.bin"
CUBEPROG="/c/Program Files/STMicroelectronics/STM32Cube/STM32CubeProgrammer/bin/STM32_Programmer_CLI.exe"

# Build
echo "Building..."
(cd "$SCRIPT_DIR" && alr build)

# Find objcopy from Alire ARM toolchain
OBJCOPY=""
for f in /c/Users/*/AppData/Local/alire/cache/toolchains/gnat_arm_elf_*/bin/arm-eabi-objcopy.exe; do
    [[ -x "$f" ]] && OBJCOPY="$f" && break
done
if [[ -z "$OBJCOPY" ]]; then
    echo "Error: arm-eabi-objcopy not found in Alire toolchain cache"
    exit 1
fi

# Convert ELF to BIN
echo "Converting ELF to BIN..."
"$OBJCOPY" -O binary "$ELF_FILE" "$BIN_FILE"

# Flash (try tools in order)
if [[ -x "$CUBEPROG" ]]; then
    echo "Flashing with STM32CubeProgrammer..."
    "$CUBEPROG" -c port=SWD -w "$BIN_FILE" 0x08000000 -v -rst

elif command -v st-flash &> /dev/null; then
    echo "Flashing with st-flash..."
    st-flash write "$BIN_FILE" 0x08000000

elif command -v openocd &> /dev/null; then
    echo "Flashing with OpenOCD..."
    openocd -f board/st_nucleo_h7.cfg \
        -c "program $ELF_FILE verify reset exit"

else
    echo "Error: No flash tool found!"
    echo "Please install one of:"
    echo "  - STM32CubeProgrammer (recommended)"
    echo "  - stlink-tools (st-flash)"
    echo "  - openocd"
    exit 1
fi

echo "Done! Open serial console at 115200 8N1."
