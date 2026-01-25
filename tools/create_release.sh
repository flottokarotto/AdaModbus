#!/bin/bash
# Create release artifacts for AdaModbus
# Usage: ./tools/create_release.sh <version>
# Example: ./tools/create_release.sh 1.1.1

set -e

VERSION=${1:?Usage: $0 <version>}
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
BUILD_DIR="$ROOT_DIR/release-build"

echo "=== Creating AdaModbus v$VERSION release artifacts ==="

# Clean build directory
rm -rf "$BUILD_DIR"
mkdir -p "$BUILD_DIR"

cd "$ROOT_DIR"

# Build library and examples in release mode
echo "Building library..."
alr build -- -XADAMODBUS_BUILD_MODE=release

echo "Building examples..."
alr exec -- gprbuild -P examples/examples.gpr -XADAMODBUS_BUILD_MODE=release

# Create Windows x64 package (examples + C API)
echo "Creating Windows x64 package..."
WIN_DIR="$BUILD_DIR/windows-x64"
mkdir -p "$WIN_DIR/bin" "$WIN_DIR/c-api/include" "$WIN_DIR/c-api/lib"
cp bin/*.exe "$WIN_DIR/bin/" 2>/dev/null || true
cp src/c_api/*.h "$WIN_DIR/c-api/include/"
cp lib/libadamodbus.a "$WIN_DIR/c-api/lib/"
(cd "$WIN_DIR" && zip -r "$BUILD_DIR/adamodbus-$VERSION-windows-x64.zip" bin c-api)

# Create Ada library package
echo "Creating Ada library package..."
ADA_DIR="$BUILD_DIR/ada-lib"
mkdir -p "$ADA_DIR/lib" "$ADA_DIR/include"
cp lib/libadamodbus.a lib/*.ali "$ADA_DIR/lib/"
cp src/core/*.ads src/transport/*.ads src/energy/*.ads "$ADA_DIR/include/" 2>/dev/null || true
cp src/c_api/*.ads "$ADA_DIR/include/" 2>/dev/null || true
(cd "$ADA_DIR" && zip -r "$BUILD_DIR/adamodbus-$VERSION-ada-windows-x64.zip" lib include)

# Create ARM Cortex-M4 package (if binaries exist)
if [ -d "examples/embedded" ]; then
    echo "Creating ARM package..."
    ARM_DIR="$BUILD_DIR/arm"
    mkdir -p "$ARM_DIR"
    find examples/embedded -name "*.elf" -exec cp {} "$ARM_DIR/" \; 2>/dev/null || true
    if [ "$(ls -A "$ARM_DIR" 2>/dev/null)" ]; then
        (cd "$ARM_DIR" && zip -r "$BUILD_DIR/adamodbus-$VERSION-arm-cortexm4.zip" *.elf)
    fi
fi

# Summary
echo ""
echo "=== Release artifacts created in $BUILD_DIR ==="
ls -lh "$BUILD_DIR"/*.zip 2>/dev/null || echo "No zip files created"

echo ""
echo "To create GitHub release:"
echo "  git tag v$VERSION"
echo "  git push origin v$VERSION"
echo "  gh release create v$VERSION --title \"v$VERSION\" --notes-file CHANGELOG.md $BUILD_DIR/*.zip"
