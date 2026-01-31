# STM32 Modbus RTU Example

RTU Master and Slave for STM32, testable with QEMU.

## Build

```bash
cd examples/embedded/stm32_rtu
alr exec -- gprbuild -P stm32_rtu.gpr -XMAIN=slave
alr exec -- gprbuild -P stm32_rtu.gpr -XMAIN=master
```

## Run with QEMU

```bash
# Single instance
./run_qemu.sh slave
./run_qemu.sh master

# Master-Slave via socket
./run_qemu.sh slave-server  # Terminal 1
./run_qemu.sh master-client # Terminal 2

# CI test
./run_ci_test.sh
```

Exit QEMU: `Ctrl+A`, then `X`

## Architecture

```
QEMU (Slave)  <-- Socket:5555 -->  QEMU (Master)
   UART0                              UART0
```

## Slave Register Map

| Address | Type | Description |
|---------|------|-------------|
| 0-31 | Holding | R/W registers |
| 0 | Input | Request count |
| 1 | Input | Uptime (seconds) |
| 0-63 | Coils | R/W bits |
