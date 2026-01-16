@echo off
REM Build script for C examples (Windows)
REM This script builds the Ada library and links C examples against it.

setlocal enabledelayedexpansion

set SCRIPT_DIR=%~dp0
set PROJECT_ROOT=%SCRIPT_DIR%..\..

echo === Building AdaModbus C Examples ===
echo.

REM Build Ada library first
echo Building Ada library...
pushd %PROJECT_ROOT%
gprbuild -p -P adamodbus.gpr
if errorlevel 1 (
    echo Failed to build Ada library
    popd
    exit /b 1
)
popd

REM Create directories
if not exist "%PROJECT_ROOT%\bin" mkdir "%PROJECT_ROOT%\bin"
if not exist "%PROJECT_ROOT%\obj\c_examples" mkdir "%PROJECT_ROOT%\obj\c_examples"

set LIB_DIR=%PROJECT_ROOT%\lib
set OBJ_DIR=%PROJECT_ROOT%\obj
set BIN_DIR=%PROJECT_ROOT%\bin

echo.
echo Compiling C examples...

REM Compile c_tcp_master.c
echo   Building c_tcp_master...
gcc -Wall -Wextra -g -O0 -I"%SCRIPT_DIR%../c_api" ^
    -c "%SCRIPT_DIR%c_tcp_master.c" ^
    -o "%OBJ_DIR%\c_examples\c_tcp_master.o"
if errorlevel 1 (
    echo Failed to compile c_tcp_master.c
    exit /b 1
)

REM Compile c_tcp_slave.c
echo   Building c_tcp_slave...
gcc -Wall -Wextra -g -O0 -I"%SCRIPT_DIR%../c_api" ^
    -c "%SCRIPT_DIR%c_tcp_slave.c" ^
    -o "%OBJ_DIR%\c_examples\c_tcp_slave.o"
if errorlevel 1 (
    echo Failed to compile c_tcp_slave.c
    exit /b 1
)

echo.
echo Linking executables...

REM Link c_tcp_master
echo   Linking c_tcp_master...
gcc -o "%BIN_DIR%\c_tcp_master.exe" ^
    "%OBJ_DIR%\c_examples\c_tcp_master.o" ^
    -L"%LIB_DIR%" -ladamodbus ^
    -lgnat -lws2_32
if errorlevel 1 (
    echo Failed to link c_tcp_master
    exit /b 1
)

REM Link c_tcp_slave
echo   Linking c_tcp_slave...
gcc -o "%BIN_DIR%\c_tcp_slave.exe" ^
    "%OBJ_DIR%\c_examples\c_tcp_slave.o" ^
    -L"%LIB_DIR%" -ladamodbus ^
    -lgnat -lws2_32
if errorlevel 1 (
    echo Failed to link c_tcp_slave
    exit /b 1
)

echo.
echo Build complete!
echo Executables in: %BIN_DIR%
echo   - c_tcp_master.exe
echo   - c_tcp_slave.exe
