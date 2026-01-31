# Cortex-M4 with LwIP

Modbus TCP example for ARM Cortex-M microcontrollers using the LwIP TCP/IP stack.

## Quick Start (QEMU, no network)

```bash
alr toolchain --select gnat_arm_elf
cd examples/embedded/cortex_m4_lwip
alr build
qemu-system-arm -M lm3s6965evb -nographic -semihosting -kernel bin/main_loopback.elf
```

Exit QEMU with `Ctrl+A`, then `X`.

## Programs

| Program | Description |
|---------|-------------|
| `main_loopback` | Loopback self-test, runs without network |
| `main_slave` | Modbus TCP server using LwIP |
| `main_master` | Modbus TCP client using LwIP |

## Building with LwIP

```bash
git submodule update --init
alr exec -- gprbuild -P cortex_m4_lwip.gpr -XLWIP=enabled
```

## Memory Usage

- Loopback test: ~11 KB Flash, ~9 KB RAM
- With LwIP: ~45 KB Flash, ~20 KB RAM

## Stack Size

The default 2 KB stack is too small for Modbus. This project uses 8 KB:

```gpr
for Default_Switches ("Ada") use ("-Wl,--defsym=__stack_size=8192");
```
