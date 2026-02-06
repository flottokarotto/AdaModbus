@echo off
REM Setup script for LwIP on Windows
REM Downloads and configures LwIP for the PV charger project

echo ========================================
echo  LwIP Setup for NUCLEO-H753ZI
echo ========================================
echo.

REM Check if git is available
where git >nul 2>nul
if %ERRORLEVEL% NEQ 0 (
    echo Error: Git is not installed or not in PATH
    echo Please install Git from https://git-scm.com/
    exit /b 1
)

REM Check if lwip directory exists
if exist lwip (
    echo LwIP directory already exists.
    echo Updating...
    cd lwip
    git pull
    cd ..
) else (
    echo Cloning LwIP repository...
    git clone --depth 1 https://github.com/lwip-tcpip/lwip.git
)

echo.
echo ========================================
echo  Setup Complete!
echo ========================================
echo.
echo Build with LwIP enabled:
echo   gprbuild -P nucleo_h753_pv_charger.gpr -XLWIP=enabled
echo.
echo Build without LwIP (TCP disabled):
echo   gprbuild -P nucleo_h753_pv_charger.gpr
echo.
