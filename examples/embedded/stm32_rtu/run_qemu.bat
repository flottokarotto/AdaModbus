@echo off
REM QEMU Launch Script for STM32 RTU Examples
REM Copyright (c) 2026 Florian Fischer
REM SPDX-License-Identifier: MIT
REM
REM Usage:
REM   run_qemu.bat slave              Start slave only
REM   run_qemu.bat master             Start master only
REM   run_qemu.bat slave-server       Start slave as socket server

setlocal
set SCRIPT_DIR=%~dp0
set BIN_DIR=%SCRIPT_DIR%bin
set QEMU=qemu-system-arm
set MACHINE=lm3s6965evb
set SOCKET_PORT=5555

if "%1"=="slave" goto slave
if "%1"=="master" goto master
if "%1"=="slave-server" goto slave_server
if "%1"=="master-client" goto master_client
goto help

:slave
echo Starting RTU Slave with stdio...
%QEMU% -M %MACHINE% -nographic -semihosting -kernel "%BIN_DIR%\main_slave.elf" -serial stdio
goto end

:master
echo Starting RTU Master with stdio...
%QEMU% -M %MACHINE% -nographic -semihosting -kernel "%BIN_DIR%\main_master.elf" -serial stdio
goto end

:slave_server
echo Starting Slave as socket server on port %SOCKET_PORT%
echo Connect master in another terminal: run_qemu.bat master-client
%QEMU% -M %MACHINE% -nographic -semihosting -kernel "%BIN_DIR%\main_slave.elf" -serial socket:localhost:%SOCKET_PORT%,server
goto end

:master_client
echo Starting Master connecting to socket server on port %SOCKET_PORT%
%QEMU% -M %MACHINE% -nographic -semihosting -kernel "%BIN_DIR%\main_master.elf" -serial socket:localhost:%SOCKET_PORT%
goto end

:help
echo QEMU Launch Script for STM32 RTU Examples
echo.
echo Usage: %0 ^<command^>
echo.
echo Commands:
echo   slave         Start slave with UART on stdio
echo   master        Start master with UART on stdio
echo   slave-server  Start slave as socket server (port %SOCKET_PORT%)
echo   master-client Start master connecting to socket server
echo.
echo Build first:
echo   gprbuild -P stm32_rtu.gpr -XMAIN=slave
echo   gprbuild -P stm32_rtu.gpr -XMAIN=master
echo.
echo To test both together:
echo   Terminal 1: run_qemu.bat slave-server
echo   Terminal 2: run_qemu.bat master-client

:end
endlocal
