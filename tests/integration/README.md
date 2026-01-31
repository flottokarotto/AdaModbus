# Integration Tests

Tests AdaModbus against a pymodbus-based Modbus TCP simulator.

## Prerequisites

```bash
pip install -r requirements.txt
```

## Running

```bash
# Terminal 1: Start the simulator
python modbus_simulator.py --port 5020

# Terminal 2: Build and run tests
alr exec -- gprbuild -P integration_tests.gpr
./bin/integration_test_runner
```

## What is Tested

- Read Coils (FC 01)
- Read Discrete Inputs (FC 02)
- Read Holding Registers (FC 03)
- Read Input Registers (FC 04)
- Write Single Coil (FC 05)
- Write Single Register (FC 06)
- Write Multiple Coils (FC 15)
- Write Multiple Registers (FC 16)
- Mask Write Register (FC 22)
- Read/Write Multiple Registers (FC 23)
- Exception responses for invalid addresses
- Maximum register read (125 registers)

## Configuration

Edit `integration_tests.adb` to change host, port, or timeout:

```ada
Simulator_Host : constant String := "localhost";
Simulator_Port : constant Natural := 5020;
Test_Timeout   : constant Natural := 3000;
```
