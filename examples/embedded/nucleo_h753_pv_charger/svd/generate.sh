#!/usr/bin/env bash
#
# Generate SVD Ada bindings for STM32H753.
#
# SVD source (CMSIS-Pack by STMicroelectronics):
#   https://github.com/Open-CMSIS-Pack/STM32H7xx_DFP/blob/main/CMSIS/SVD/STM32H753.svd
#
# Usage:
#   cd examples/embedded/nucleo_h753_pv_charger/svd
#   bash generate.sh
#
# Prerequisites: Alire (alr) on PATH.
# The script clones and builds svd2ada from GitHub automatically on first run.
# We use the Git version (https://github.com/AdaCore/svd2ada) because the Alire
# crate (0.1.0) lacks --no-elaboration-code-all which is required for ZFP/Light
# runtimes.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
EXAMPLE_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
OUTPUT_DIR="$EXAMPLE_DIR/src/svd"
SVD2ADA_DIR="$SCRIPT_DIR/svd2ada"
SVD2ADA_COMMIT="7a726b49a57b6f61944f03f7173163fb91c9e17f"
TEMP_DIR="$(mktemp -d)"
trap 'rm -rf "$TEMP_DIR"' EXIT

# Clone svd2ada at pinned commit if needed
if [[ ! -d "$SVD2ADA_DIR" ]]; then
    echo "Cloning svd2ada (${SVD2ADA_COMMIT:0:8}) ..."
    git clone https://github.com/AdaCore/svd2ada.git "$SVD2ADA_DIR"
    (cd "$SVD2ADA_DIR" && git checkout "$SVD2ADA_COMMIT")
fi

SVD2ADA_BIN="$SVD2ADA_DIR/bin/svd2ada"
[[ -x "$SVD2ADA_BIN" ]] || [[ -x "$SVD2ADA_BIN.exe" ]] || {
    echo "Building svd2ada ..."
    (cd "$SVD2ADA_DIR" && alr build)
}
[[ -x "$SVD2ADA_BIN.exe" ]] && SVD2ADA_BIN="$SVD2ADA_BIN.exe"

# Generate all peripherals
#   --boolean                  single-bit fields as Boolean instead of UInt1
#   --no-elaboration-code-all  emit pragma No_Elaboration_Code_All (required for ZFP/Light runtime)
"$SVD2ADA_BIN" "$SCRIPT_DIR/STM32H753.svd" \
    -o "$TEMP_DIR" \
    --boolean \
    --no-elaboration-code-all

# Copy all generated specs to output directory
rm -f "$OUTPUT_DIR"/stm32h753*.ads
cp "$TEMP_DIR"/stm32h753*.ads "$OUTPUT_DIR/"
ls -1 "$OUTPUT_DIR"/stm32h753*.ads | while read -r f; do
    echo "  $(basename "$f")"
done

echo "Done."
