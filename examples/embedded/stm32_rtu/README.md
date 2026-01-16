# STM32 Modbus RTU Examples

Modbus RTU Master and Slave examples for STM32, testable with QEMU.

## Features

- Pure Ada UART driver (no C code)
- SysTick-based timing
- Compatible with QEMU's LM3S6965EVB emulation
- Master-Slave communication via QEMU socket

## Files

```
stm32_rtu/
├── lm3s_uart.ads/adb       # LM3S6965 UART driver (for QEMU)
├── stm32_systick.ads/adb   # SysTick timer driver
├── semihosting.ads/adb     # ARM semihosting for debug output
├── rtu_slave.ads/adb       # Modbus RTU slave
├── rtu_master.ads/adb      # Modbus RTU master
├── main_slave.adb          # Slave entry point
├── main_master.adb         # Master entry point
├── last_chance_handler.*   # Exception handler for light runtime
├── stm32_rtu.gpr           # GPR build project
├── run_qemu.sh             # Linux/MSYS2 launch script
├── run_qemu.bat            # Windows launch script
└── run_ci_test.sh          # CI test script
```

## Prerequisites

1. **GNAT ARM ELF Toolchain**:
   ```bash
   alr toolchain --select gnat_arm_elf
   ```

2. **QEMU with ARM support**:
   ```bash
   # Linux
   sudo apt install qemu-system-arm

   # Windows (MSYS2)
   pacman -S mingw-w64-x86_64-qemu

   # Or download from https://www.qemu.org/
   ```

## Building

```bash
cd examples/embedded/stm32_rtu

# Build slave
alr exec -- gprbuild -P stm32_rtu.gpr -XMAIN=slave

# Build master
alr exec -- gprbuild -P stm32_rtu.gpr -XMAIN=master
```

## Running with QEMU

### Single Instance (for debugging)

```bash
# Linux/MSYS2
./run_qemu.sh slave
./run_qemu.sh master

# Windows CMD
run_qemu.bat slave
run_qemu.bat master
```

### Master-Slave Communication

Start two terminals:

**Terminal 1 (Slave)**:
```bash
./run_qemu.sh slave-server
# or
run_qemu.bat slave-server
```

**Terminal 2 (Master)**:
```bash
./run_qemu.sh master-client
# or
run_qemu.bat master-client
```

The UART is connected via TCP socket (port 5555).

### Linux: Automatic Test

```bash
./run_qemu.sh both
```

This starts both instances connected via socket.

## Architecture

```
┌─────────────────────┐     Socket:5555     ┌─────────────────────┐
│  QEMU Instance 1    │◄───────────────────►│  QEMU Instance 2    │
│  (LM3S6965EVB)      │      (UART)         │  (LM3S6965EVB)      │
│                     │                     │                     │
│  ┌───────────────┐  │                     │  ┌───────────────┐  │
│  │  RTU Slave    │  │                     │  │  RTU Master   │  │
│  │  Unit ID: 1   │  │                     │  │               │  │
│  └───────────────┘  │                     │  └───────────────┘  │
│         ▲           │                     │         │           │
│         │           │                     │         ▼           │
│  ┌──────┴────────┐  │                     │  ┌──────┴────────┐  │
│  │  LM3S UART    │  │                     │  │  LM3S UART    │  │
│  │  (UART0)      │  │                     │  │  (UART0)      │  │
│  └───────────────┘  │                     │  └───────────────┘  │
└─────────────────────┘                     └─────────────────────┘
```

## Slave Register Map

| Address | Type    | Description           |
|---------|---------|----------------------|
| 0-31    | Holding | Read/Write registers |
| 0-31    | Input   | Read-only registers  |
| 0       | Input   | Request count        |
| 1       | Input   | Uptime (seconds)     |
| 0-63    | Coils   | Read/Write bits      |

## Master Test Sequence

The master performs these operations every 500ms:

1. Read holding registers 0-4
2. Write incremented counter to register 0
3. Read coils 0-7
4. Toggle a coil based on counter
5. Read input registers 0-1 (request count, uptime)

## Exiting QEMU

- Press `Ctrl+A` then `X`
- Or close the terminal

## Notes

- The LM3S6965EVB target is used because it has full ARM semihosting support in QEMU
- QEMU's netduinoplus2 (STM32F4) does NOT support semihosting, so we use lm3s6965evb
- The UART driver uses LM3S6965-specific memory-mapped registers
- For real STM32 hardware, you'll need a different UART driver with STM32 register addresses
