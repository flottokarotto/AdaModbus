@echo off
REM Debug script for NUCLEO-H753ZI (Windows)
REM Starts OpenOCD and GDB

set ELF_FILE=bin\nucleo_h753_pv_charger.elf

REM Check if ELF exists
if not exist "%ELF_FILE%" (
    echo Error: %ELF_FILE% not found!
    echo Run 'gprbuild -P nucleo_h753_pv_charger.gpr' first.
    exit /b 1
)

echo Starting OpenOCD in background...
start "OpenOCD" openocd -f board/st_nucleo_h7.cfg

REM Wait for OpenOCD to start
timeout /t 2 /nobreak >nul

echo Starting GDB...
echo.
echo GDB Commands:
echo   target remote :3333   - Connect to OpenOCD
echo   monitor reset halt    - Reset and halt
echo   load                  - Flash program
echo   continue              - Run program
echo   break main            - Set breakpoint
echo   info registers        - Show registers
echo   quit                  - Exit GDB
echo.

arm-none-eabi-gdb "%ELF_FILE%" -ex "target remote :3333"

REM Kill OpenOCD when GDB exits
taskkill /FI "WINDOWTITLE eq OpenOCD*" /F >nul 2>nul
