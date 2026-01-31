# Testing

## Running Tests

```bash
alr exec -- gprbuild -P tests/aunit_tests.gpr
./bin/test_runner
```

## Unit Tests

83 unit tests covering:
- CRC-16 and LRC checksums
- PDU encoding and decoding for all function codes
- RTU, ASCII, and TCP framing
- Master and Slave request/response handling
- Async API

## Integration Tests

Tests the library against a Python-based Modbus simulator:

```bash
# Start the simulator
python tests/integration/modbus_simulator.py --port 5020 &

# Run the tests
alr exec -- gprbuild -P tests/integration/integration_tests.gpr
./bin/integration_test_runner
```

Covers all standard function codes (FC 01-06, 15, 16, 22, 23) and exception handling.

## Code Coverage

The protocol core has 88-100% coverage. Master and Slave modules have lower coverage because some code paths require specific hardware or network conditions.
