#!/bin/bash
# Setup script for LwIP
# Downloads and configures LwIP for the PV charger project

set -e

echo "========================================"
echo " LwIP Setup for NUCLEO-H753ZI"
echo "========================================"
echo

# Check if lwip directory exists
if [ -d "lwip" ]; then
    echo "LwIP directory already exists."
    echo "Updating..."
    cd lwip
    git pull
    cd ..
else
    echo "Cloning LwIP repository..."
    git clone --depth 1 https://github.com/lwip-tcpip/lwip.git
fi

echo
echo "========================================"
echo " Setup Complete!"
echo "========================================"
echo
echo "Build with LwIP enabled:"
echo "  gprbuild -P nucleo_h753_pv_charger.gpr -XLWIP=enabled"
echo
echo "Build without LwIP (TCP disabled):"
echo "  gprbuild -P nucleo_h753_pv_charger.gpr"
echo
