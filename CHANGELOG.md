# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.1.0] - 2026-03-01

### Added

#### Register Mapping (Three-Level Architecture)
- `Ada_Modbus.Scaling` — SPARK-verified scaling toolkit (SF, Factor, Affine)
- `Ada_Modbus.Scaled_IO` — Declarative register-to-Float record mapping (generic)
- `Ada_Modbus.Record_IO` — Binary record↔register mapping with SPARK_Mode
  - 32-bit word order support for Float32/U32 fields
  - Field_Sizes API for querying field sizes at runtime

#### Energy Devices
- `Ada_Modbus.Energy.Delta_Charger` — Delta AC Max Basic wallbox support
- `Ada_Modbus.Gateway` — TCP↔RTU protocol translation
- `Ada_Modbus.Energy.Kostal` / `Ada_Modbus.Energy.KSEM` as library packages

#### Transport
- `Ada_Modbus.Transport.TLS_Mbed` — mbedTLS transport for embedded (Cortex-M)
- IEEE Float32 support in Utilities (endian-portable record overlays)

#### Embedded (NUCLEO-H753ZI)
- ST HAL Ethernet replacing bare-metal (svd2ada instead of a0b SVD)
- Shared components: semihosting, last chance handler, LwIP bindings, TCP client
- NUCLEO-H743ZI2 support with improved crash diagnostics
- STM32CubeH7 submodule pinned to v1.12.1

#### Examples
- `ksem_record_io` — KSEM reading via Record_IO (binary mapping)
- `ksem_scaled_io` — KSEM reading via Scaled_IO (declarative mapping)

### Fixed
- ETH RX HardFault and KSEM scale factor handling
- Off-by-one in KSEM SunSpec base address (40069→40070)
- LwIP TCP stability: PCB leak, keepalive, critical sections
- TCP reconnect: tear down connection on send/receive failure
- gnatprove overflow warnings in Gateway, Delta_Charger, Protocol

### Changed
- SunSpec now delegates scaling to `Ada_Modbus.Scaling` (5 justified checks eliminated)
- Scaling functions proven via preconditions instead of Annotate pragmas
- SPARK verification: 1180 checks, 0 unproved, 2 justified (was 1177/7)
- 257 unit tests (was 214), test coverage 96%+
- CI optimized with parallel jobs
- Documentation rewritten

## [1.0.1] - 2026-01-27

### Fixed
- Security issues identified in code review
- Alire index review feedback

### Changed
- Improved test coverage for protocol framing to 100%
- Improved test coverage for ada_modbus-protocol.adb to 99%
- sunspec_codegen: Use specific exception types instead of generic catch-all

### Added
- Unit tests for all word order conversions (ABCD, CDAB, BADC, DCBA)

## [1.0.0] - 2026-01-25

### Added

#### Core Protocol
- Modbus RTU, ASCII, and TCP protocol support
- Master (Client) with synchronous and asynchronous APIs
- Slave (Server) with callback-based request handling
- All standard function codes (FC 01-06, 07, 08, 15, 16, 17, 22, 23)
- 100% SPARK-verified protocol core

#### Transport Backends
- TCP socket transport (Windows/Linux)
- Serial port transport (COM/TTY) for RTU/ASCII
- TLS transport (separate `adamodbus_tls` crate)

#### Energy Management
- **SunSpec Alliance profiles**:
  - Model 1: Common (device information)
  - Models 101-103: Inverters (single/split/three-phase)
  - Model 120: Nameplate ratings
  - Model 121: Basic settings
  - Model 122: Measurements
  - Model 123: Immediate controls
  - Model 124: Basic storage
  - Model 160: Multiple MPPT
  - Models 201-204: Meters (1P/SP/3P Wye/Delta)
  - Models 701/704: DER AC measurements and controls
  - Model 802: Battery (extended)
- **go-e Charger**: `Ada_Modbus.Energy.Go_E` package for go-e wallbox
- SG-Ready heat pump control
- §14a EnWG grid power limitation

#### C API
- Base TCP master/slave API (`ada_modbus.h`)
- SunSpec high-level API (`ada_modbus_sunspec.h`)
- Serial/RTU API (`ada_modbus_serial.h`)

#### Examples
- TCP master/slave demos
- RTU master/slave demos
- ASCII master/slave demos
- Async master demo
- Kostal inverter reader + dashboard
- KSEM energy meter reader + dashboard
- go-e charger dashboard + simulator
- C language examples
- Embedded examples (Cortex-M4 with LwIP, STM32 RTU)

#### Build & Test
- Alire package manager support
- GitHub Actions CI (build, test, SPARK, embedded)
- Automated release workflow with binary artifacts
- 214 unit tests with AUnit
- Integration tests with Python Modbus simulator
- Code coverage with Codecov

### Technical Details

- Ada 2022 standard
- ZFP-compatible core (no tasking, exceptions, or dynamic allocation)
- Generic transport abstraction for custom backends
- 32-bit word order support (ABCD, CDAB, BADC, DCBA)
- Signed scale factor support for SunSpec

[1.1.0]: https://github.com/flottokarotto/AdaModbus/releases/tag/v1.1.0
[1.0.1]: https://github.com/flottokarotto/AdaModbus/releases/tag/v1.0.1
[1.0.0]: https://github.com/flottokarotto/AdaModbus/releases/tag/v1.0.0
