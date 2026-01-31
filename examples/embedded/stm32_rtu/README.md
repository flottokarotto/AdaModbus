# STM32 Modbus RTU

Modbus RTU master and slave examples, testable with QEMU.

## Building

```bash
cd examples/embedded/stm32_rtu
alr exec -- gprbuild -P stm32_rtu.gpr -XMAIN=slave
alr exec -- gprbuild -P stm32_rtu.gpr -XMAIN=master
```

## Running with QEMU

Single instance:
```bash
./run_qemu.sh slave
./run_qemu.sh master
```

Master-slave communication via socket:
```bash
# Terminal 1
./run_qemu.sh slave-server

# Terminal 2
./run_qemu.sh master-client
```

Exit QEMU with `Ctrl+A`, then `X`.

## Slave Register Map

| Address | Type | Description |
|---------|------|-------------|
| 0-31 | Holding Registers | Read/write |
| 0-31 | Input Registers | Read-only (0=request count, 1=uptime) |
| 0-63 | Coils | Read/write bits |
