# Cortex-M4 LwIP Example

Modbus TCP for ARM Cortex-M with LwIP.

## Quick Start (QEMU)

```bash
alr toolchain --select gnat_arm_elf
cd examples/embedded/cortex_m4_lwip
alr build
qemu-system-arm -M lm3s6965evb -nographic -semihosting \
  -kernel bin/main_loopback.elf
```

Exit QEMU: `Ctrl+A`, then `X`

## Programs

| Program | Network | Description |
|---------|---------|-------------|
| `main_loopback` | No | Self-test (QEMU) |
| `main_slave` | Yes | TCP server |
| `main_master` | Yes | TCP client |

## Build with LwIP

```bash
git submodule update --init
alr exec -- gprbuild -P cortex_m4_lwip.gpr -XLWIP=enabled
```

## Memory

| Config | Flash | RAM |
|--------|-------|-----|
| Loopback | ~11 KB | ~9 KB |
| With LwIP | ~45 KB | ~20 KB |

## Stack Size

Default 2 KB is too small. Project uses 8 KB:

```gpr
for Default_Switches ("Ada") use ("-Wl,--defsym=__stack_size=8192");
```
