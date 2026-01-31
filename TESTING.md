# Testing

## Run Tests

```bash
alr exec -- gprbuild -P tests/aunit_tests.gpr
./bin/test_runner
```

## Test Summary

| Suite | Tests |
|-------|-------|
| CRC-16 | 8 |
| LRC | 4 |
| Utilities | 6 |
| PDU Encoding/Decoding | 19 |
| RTU Framing | 5 |
| ASCII Framing | 6 |
| TCP Framing | 6 |
| Slave Processing | 13 |
| Master | 8 |
| Async Master | 8 |
| **Total** | **83** |

## Coverage

| Module | Coverage |
|--------|----------|
| Protocol Core | 88-100% |
| Master | 54% |
| Slave | 44% |
| Transport | Integration only |

## Integration Tests

```bash
# Start simulator
python tests/integration/modbus_simulator.py --port 5020 &

# Run tests
alr exec -- gprbuild -P tests/integration/integration_tests.gpr
./bin/integration_test_runner
```

See `tests/integration/README.md` for details.
