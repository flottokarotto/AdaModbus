# Testing

## Test Framework

Unit tests use [AUnit](https://github.com/AdaCore/aunit), the Ada unit testing framework.

## Running Tests

```bash
# Build and run tests
alr exec -- gprbuild -P tests/aunit_tests.gpr
./bin/test_runner

# With coverage (gcov)
alr exec -- gprbuild -P tests/aunit_tests.gpr -XCOVERAGE=enabled
./bin/test_runner
gcov obj/tests/*.gcda
```

## Test Summary

| Suite | Tests | Status |
|-------|-------|--------|
| CRC-16 | 8 | OK |
| LRC | 4 | OK |
| Utilities | 6 | OK |
| PDU Encoding/Decoding | 19 | OK |
| RTU Framing | 5 | OK |
| ASCII Framing | 6 | OK |
| TCP Framing | 6 | OK |
| Slave Processing | 13 | OK |
| Master (Client) | 8 | OK |
| Async Master | 8 | OK |
| **Total** | **83** | **OK** |

## Test Coverage

### CRC-16 (`test_crc.adb`)

| Test | Description | Module |
|------|-------------|--------|
| Known CRC value | Verifies CRC calculation against Modbus spec example | `Ada_Modbus.CRC16` |
| Verify valid CRC | Tests CRC verification with correct checksum | `Ada_Modbus.CRC16` |
| Verify invalid CRC | Tests CRC verification rejects wrong checksum | `Ada_Modbus.CRC16` |
| Empty data CRC | Tests CRC of empty array returns 0xFFFF | `Ada_Modbus.CRC16` |
| Extract CRC | Tests extracting CRC from frame (LSB first) | `Ada_Modbus.CRC16` |
| Append CRC | Tests appending CRC to buffer | `Ada_Modbus.CRC16` |
| Single byte CRC | Tests CRC of single byte | `Ada_Modbus.CRC16` |
| CRC consistency | Tests CRC calculation is deterministic | `Ada_Modbus.CRC16` |

### LRC (`test_lrc.adb`)

| Test | Description | Module |
|------|-------------|--------|
| Known LRC value | Verifies LRC calculation (sum + two's complement) | `Ada_Modbus.LRC` |
| Verify valid LRC | Tests LRC verification with correct checksum | `Ada_Modbus.LRC` |
| Verify invalid LRC | Tests LRC verification rejects wrong checksum | `Ada_Modbus.LRC` |
| Single byte LRC | Tests LRC calculation for single byte input | `Ada_Modbus.LRC` |

### Utilities (`test_utilities.adb`)

| Test | Description | Module |
|------|-------------|--------|
| To_Big_Endian | Tests 16-bit to big-endian byte array conversion | `Ada_Modbus.Utilities` |
| From_Big_Endian (bytes) | Tests two bytes to 16-bit conversion | `Ada_Modbus.Utilities` |
| From_Big_Endian (array) | Tests byte array to 16-bit conversion | `Ada_Modbus.Utilities` |
| High_Byte | Tests extraction of high byte from 16-bit value | `Ada_Modbus.Utilities` |
| Low_Byte | Tests extraction of low byte from 16-bit value | `Ada_Modbus.Utilities` |
| Round-trip | Tests encode/decode preserves value | `Ada_Modbus.Utilities` |

### PDU Encoding/Decoding (`test_pdu.adb`)

| Test | Description | Module |
|------|-------------|--------|
| Read Registers Request | Verifies FC03 request encoding | `Ada_Modbus.Protocol` |
| Read Bits Request | Verifies FC01 request encoding | `Ada_Modbus.Protocol` |
| Write Single Register | Verifies FC06 request encoding | `Ada_Modbus.Protocol` |
| Write Single Coil | Verifies FC05 request encoding (ON/OFF values) | `Ada_Modbus.Protocol` |
| Write Multiple Coils | Verifies FC15 request encoding (bit packing) | `Ada_Modbus.Protocol` |
| Write Multiple Registers | Verifies FC16 request encoding | `Ada_Modbus.Protocol` |
| Exception Codes (all) | Tests all exception code conversions | `Ada_Modbus.Protocol` |
| Decode Read Registers Response | Tests FC03 response decoding | `Ada_Modbus.Protocol` |
| Decode Read Bits Response | Tests FC01 response decoding | `Ada_Modbus.Protocol` |
| Decode Write Single Response | Tests FC06 echo response decoding | `Ada_Modbus.Protocol` |
| Decode Write Multiple Response | Tests FC16 response decoding | `Ada_Modbus.Protocol` |
| Decode Exception Response | Tests exception response decoding | `Ada_Modbus.Protocol` |
| Decode Detects Exception | Tests that decode detects exception FC | `Ada_Modbus.Protocol` |
| Decode Frame Error | Tests frame error detection | `Ada_Modbus.Protocol` |

### RTU Framing (`test_rtu.adb`)

| Test | Description | Module |
|------|-------------|--------|
| Build RTU frame | Tests frame construction with CRC | `Ada_Modbus.Protocol.RTU` |
| Parse valid frame | Tests frame parsing with valid CRC | `Ada_Modbus.Protocol.RTU` |
| Parse invalid CRC | Tests rejection of frames with bad CRC | `Ada_Modbus.Protocol.RTU` |
| Frame too short | Tests rejection of frames < 4 bytes | `Ada_Modbus.Protocol.RTU` |
| Round-trip | Tests build/parse preserves data | `Ada_Modbus.Protocol.RTU` |

### ASCII Framing (`test_ascii.adb`)

| Test | Description | Module |
|------|-------------|--------|
| Byte to Hex | Tests binary to hex ASCII conversion | `Ada_Modbus.Protocol.ASCII` |
| Hex to Byte | Tests hex ASCII to binary conversion | `Ada_Modbus.Protocol.ASCII` |
| Is_Hex_Digit | Tests hex character validation | `Ada_Modbus.Protocol.ASCII` |
| Build ASCII frame | Tests frame construction with LRC | `Ada_Modbus.Protocol.ASCII` |
| Parse valid frame | Tests frame parsing with valid LRC | `Ada_Modbus.Protocol.ASCII` |
| Round-trip | Tests build/parse preserves data | `Ada_Modbus.Protocol.ASCII` |

### TCP/MBAP Framing (`test_tcp.adb`)

| Test | Description | Module |
|------|-------------|--------|
| Build TCP frame | Tests MBAP header + PDU construction | `Ada_Modbus.Protocol.TCP` |
| Parse valid frame | Tests MBAP parsing and PDU extraction | `Ada_Modbus.Protocol.TCP` |
| Invalid protocol ID | Tests rejection of non-Modbus protocol | `Ada_Modbus.Protocol.TCP` |
| Get_Transaction_Id | Tests transaction ID extraction | `Ada_Modbus.Protocol.TCP` |
| Get_Expected_Length | Tests length field extraction | `Ada_Modbus.Protocol.TCP` |
| Round-trip | Tests build/parse preserves data | `Ada_Modbus.Protocol.TCP` |

### Slave Processing (`test_slave.adb`)

| Test | Description | Module |
|------|-------------|--------|
| Read Holding Registers RTU | Tests FC03 processing with mock callbacks | `Ada_Modbus.Slave` |
| Write Single Register RTU | Tests FC06 processing | `Ada_Modbus.Slave` |
| Illegal Function Exception | Tests exception when callback not registered | `Ada_Modbus.Slave` |
| Broadcast No Response | Tests that broadcast requests get no response | `Ada_Modbus.Slave` |
| Wrong Slave ID | Tests that requests for other slaves are ignored | `Ada_Modbus.Slave` |
| TCP Processing | Tests slave with TCP framing | `Ada_Modbus.Slave` |
| Read Coils | Tests FC01 processing | `Ada_Modbus.Slave` |
| Write Multiple Registers | Tests FC16 processing | `Ada_Modbus.Slave` |

### Master/Client (`test_master.adb`)

| Test | Description | Module |
|------|-------------|--------|
| Initialize Master | Tests context initialization | `Ada_Modbus.Master` |
| Read Holding Registers RTU | Tests FC03 with mock transport (RTU) | `Ada_Modbus.Master` |
| Read Holding Registers TCP | Tests FC03 with mock transport (TCP) | `Ada_Modbus.Master` |
| Write Single Register | Tests FC06 request/response | `Ada_Modbus.Master` |
| Timeout Handling | Tests timeout detection | `Ada_Modbus.Master` |
| Exception Response | Tests exception response handling | `Ada_Modbus.Master` |
| Write Single Coil | Tests FC05 request/response | `Ada_Modbus.Master` |
| Read Coils | Tests FC01 with coil unpacking | `Ada_Modbus.Master` |

## Module Coverage (gnatcov)

| Module | Coverage | Statements |
|--------|----------|------------|
| `Ada_Modbus.Utilities` | **100%** | 5/5 |
| `Ada_Modbus.CRC16` | **100%** | 13/13 |
| `Ada_Modbus.LRC` | **100%** | 8/8 |
| `Ada_Modbus.Protocol` | **88%** | 142/161 |
| `Ada_Modbus.Protocol.RTU` | **100%** | 22/22 |
| `Ada_Modbus.Protocol.ASCII` | **83%** | 65/78 |
| `Ada_Modbus.Protocol.TCP` | **90%** | 36/40 |
| `Ada_Modbus.Master` | **54%** | 117/215 |
| `Ada_Modbus.Slave` | **44%** | 133/305 |
| `Ada_Modbus.Transport.TCP` | 0% | 0/116 (integration only) |
| **Total** | **56%** | **541/963** |

Note: `Transport.TCP` uses GNAT.Sockets directly and requires real network I/O, so it's tested via integration (example programs) rather than unit tests.

## CI

GitHub Actions runs tests on:
- Ubuntu (latest)
- Windows (latest)

See `.github/workflows/ci.yml` for configuration.

## Adding Tests

1. Create test file in `tests/unit/`
2. Create spec (`.ads`) and body (`.adb`)
3. Add suite to `test_runner.adb`

Example:
```ada
--  tests/unit/test_new.ads
with AUnit.Test_Suites;
package Test_New is
   function Suite return AUnit.Test_Suites.Access_Test_Suite;
end Test_New;
```
