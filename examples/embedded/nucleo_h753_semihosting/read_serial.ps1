# read_serial.ps1 - Reset NUCLEO board and read serial output
# Usage: powershell -ExecutionPolicy Bypass -File read_serial.ps1
#
# Resets the board via STM32CubeProgrammer, then reads USART3 output
# from ST-Link VCP (COM3, 115200 8N1) for 30 seconds.
# Press Ctrl+C to stop.

param(
    [string]$ComPort = "COM3",
    [int]$BaudRate = 115200,
    [int]$DurationSec = 30
)

$CubeProg = "C:\Program Files\STMicroelectronics\STM32Cube\STM32CubeProgrammer\bin\STM32_Programmer_CLI.exe"

# Open serial port first so we don't miss early output
$port = New-Object System.IO.Ports.SerialPort $ComPort, $BaudRate, 'None', 8, 'One'
$port.ReadTimeout = 2000
$port.DtrEnable = $true
$port.Open()
$port.DiscardInBuffer()

Write-Host "Serial port $ComPort opened at $BaudRate baud."

# Reset board
if (Test-Path $CubeProg) {
    Write-Host "Resetting board via STM32CubeProgrammer..."
    $proc = Start-Process -NoNewWindow -Wait -PassThru -FilePath $CubeProg -ArgumentList "-c port=SWD -rst"
    Write-Host "Board reset done."
} else {
    Write-Host "WARNING: STM32CubeProgrammer not found, reading without reset."
}

Write-Host "--- serial output (${DurationSec}s) ---"

$sw = [System.Diagnostics.Stopwatch]::StartNew()
try {
    while ($sw.ElapsedMilliseconds -lt ($DurationSec * 1000)) {
        try {
            $line = $port.ReadLine()
            Write-Host $line
        } catch [System.TimeoutException] {
            # No data available, keep waiting
        }
    }
} finally {
    $port.Close()
    Write-Host "--- end ---"
}
