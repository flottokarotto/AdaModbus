# Integration Tests

Tests AdaModbus against a pymodbus simulator.

## Prerequisites

```bash
pip install -r requirements.txt
```

## Run

```bash
# Terminal 1: Start simulator
python modbus_simulator.py --port 5020

# Terminal 2: Run tests
alr exec -- gprbuild -P integration_tests.gpr
./bin/integration_test_runner
```

## Test Coverage

- FC 01-06: Read/Write Coils and Registers
- FC 15-16: Write Multiple
- FC 22: Mask Write Register
- FC 23: Read/Write Multiple Registers
- Exception handling
- Maximum register read (125)

## Simulator Test Data

| Type | Pattern |
|------|---------|
| Coils | Even=ON, Odd=OFF |
| Discrete Inputs | Divisible by 3=ON |
| Holding Registers | 0-9: Sequential, 10-19: Test values |
| Input Registers | Value = Address + 1000 |

## Configuration

Edit `integration_tests.adb`:

```ada
Simulator_Host : constant String := "localhost";
Simulator_Port : constant Natural := 5020;
```
