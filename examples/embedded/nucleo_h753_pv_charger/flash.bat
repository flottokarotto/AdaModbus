@echo off
REM Flash script for NUCLEO-H753ZI (Windows)
REM Usage: flash.bat

set ELF_FILE=bin\nucleo_h753_pv_charger.elf
set BIN_FILE=bin\nucleo_h753_pv_charger.bin

REM Check if ELF exists
if not exist "%ELF_FILE%" (
    echo Error: %ELF_FILE% not found!
    echo Run 'gprbuild -P nucleo_h753_pv_charger.gpr' first.
    exit /b 1
)

REM Generate BIN from ELF
echo Converting ELF to BIN...
arm-none-eabi-objcopy -O binary "%ELF_FILE%" "%BIN_FILE%"

REM Try STM32CubeProgrammer first (most common on Windows)
where STM32_Programmer_CLI >nul 2>nul
if %ERRORLEVEL% EQU 0 (
    echo Flashing with STM32CubeProgrammer...
    STM32_Programmer_CLI -c port=SWD -w "%BIN_FILE%" 0x08000000 -v -rst
    echo Done!
    exit /b 0
)

REM Try st-flash
where st-flash >nul 2>nul
if %ERRORLEVEL% EQU 0 (
    echo Flashing with st-flash...
    st-flash write "%BIN_FILE%" 0x08000000
    echo Done! Reset the board to run.
    exit /b 0
)

REM Try OpenOCD
where openocd >nul 2>nul
if %ERRORLEVEL% EQU 0 (
    echo Flashing with OpenOCD...
    openocd -f board/st_nucleo_h7.cfg -c "program %ELF_FILE% verify reset exit"
    echo Done!
    exit /b 0
)

echo Error: No flash tool found!
echo Please install one of:
echo   - STM32CubeProgrammer (recommended for Windows)
echo   - stlink-tools (st-flash)
echo   - openocd
exit /b 1
