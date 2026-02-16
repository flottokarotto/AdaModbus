#!/bin/bash
# Setup submodules for NUCLEO-H753ZI KSEM Test
#
# Usage: ./setup.sh
#
# This initializes the STM32CubeH7 and LwIP submodules.
# STM32CubeH7 uses nested submodules for individual driver components.

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"

echo "Initializing submodules..."

# Initialize STM32CubeH7 (top-level)
cd "$REPO_ROOT"
git submodule update --init examples/embedded/nucleo_h753_ksem_test/STM32CubeH7

# Initialize only the needed nested submodules within STM32CubeH7
# (the full repo has ~50 BSP components we don't need)
cd "$SCRIPT_DIR/STM32CubeH7"
git submodule update --init \
    Drivers/STM32H7xx_HAL_Driver \
    Drivers/CMSIS/Device/ST/STM32H7xx \
    Drivers/BSP/Components/lan8742

# Initialize LwIP
cd "$REPO_ROOT"
git submodule update --init examples/embedded/nucleo_h753_ksem_test/lwip

echo ""
echo "Done. You can now build:"
echo "  cd $SCRIPT_DIR"
echo "  alr build"
