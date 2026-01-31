<p align="center">
  <img src="logo.svg" alt="AdaModbus" width="450">
</p>

# AdaModbus

[![CI](https://github.com/flottokarotto/AdaModbus/actions/workflows/ci.yml/badge.svg)](https://github.com/flottokarotto/AdaModbus/actions/workflows/ci.yml)
[![codecov](https://codecov.io/gh/flottokarotto/AdaModbus/graph/badge.svg)](https://codecov.io/gh/flottokarotto/AdaModbus)
[![SPARK](https://img.shields.io/badge/SPARK-verified-blue)](https://www.adacore.com/about-spark)

Ada 2022 Modbus library for embedded and desktop systems.

## Features

- Modbus RTU, ASCII, and TCP
- Master (client) and Slave (server)
- SPARK verified protocol core (no buffer overflows, formally proven)
- ZFP-compatible (no tasking, exceptions, or dynamic allocation)
- Energy management: SunSpec profiles, SG-Ready, §14a grid control
- C API for integration into existing projects

## Status

**Tested with real hardware:**
- Modbus TCP with Kostal PLENTICORE inverter
- Modbus TCP with Kostal Smart Energy Meter (KSEM)
- SunSpec inverter and meter models

**Unit tests only:**
- Modbus RTU and ASCII framing
- Serial transport on Windows

**Experimental:**
- TLS transport (compiles, not tested with real Modbus/TCP Security devices)
- Embedded ARM (runs in QEMU, not tested on real hardware)

## Quick Start

```bash
alr build
alr exec -- gprbuild -P tests/aunit_tests.gpr
./bin/test_runner
```

## Usage Example

```ada
with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.Transport.TCP; use Ada_Modbus.Transport.TCP;

--  Connect to Modbus TCP server
Connect (Connection, "192.168.1.100", 502, 5.0, Result);

--  Read 10 holding registers starting at address 0
Result := My_Master.Read_Holding_Registers
  (Ctx, Slave => 1, Start_Address => 0, Quantity => 10, Values => Values);

Disconnect (Connection);
```

## Packages

| Package | Description |
|---------|-------------|
| `Ada_Modbus.Protocol` | PDU encoding/decoding (SPARK verified) |
| `Ada_Modbus.Protocol.RTU/ASCII/TCP` | Framing layers (SPARK verified) |
| `Ada_Modbus.Master` | Client with synchronous and asynchronous API |
| `Ada_Modbus.Slave` | Server with callback-based request handling |
| `Ada_Modbus.Transport.TCP` | TCP transport using GNAT.Sockets |
| `Ada_Modbus.Energy.SunSpec` | SunSpec profiles for inverters, meters, batteries |
| `Ada_Modbus.C_API` | C bindings for use without Ada |

## TLS Support

| Crate | Backend | Platform |
|-------|---------|----------|
| `adamodbus_tls` | AWS / OpenSSL | Desktop (Windows, Linux) |
| `adamodbus_tls_mbed` | mbedTLS | Embedded (Cortex-M) |

## Example Programs

```bash
./bin/tcp_master localhost 1502      # TCP client demo
./bin/tcp_slave 1502                 # TCP server demo
./bin/kostal_dashboard 192.168.1.10  # Live dashboard for Kostal inverters
./bin/go_e_dashboard 192.168.1.50    # Live dashboard for go-e wallbox
```

## Documentation

- [TESTING.md](TESTING.md) - Test coverage
- [CHANGELOG.md](CHANGELOG.md) - Release notes

## License

[MIT](LICENSE)
