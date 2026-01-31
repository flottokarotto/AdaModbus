# Cortex-M4 TLS Example

Modbus over TLS for ARM Cortex-M with mbedTLS.

## Quick Start (QEMU)

```bash
alr toolchain --select gnat_arm_elf
cd examples/embedded/cortex_m4_tls
alr build
qemu-system-arm -M lm3s6965evb -nographic -semihosting \
  -kernel bin/main_loopback_tls.elf
```

## Programs

| Program | Description |
|---------|-------------|
| `main_loopback_tls` | TLS loopback test |
| `main_tls_client` | TLS client example |
| `main_tls_server` | TLS server example |

## Build Modes

```bash
# Loopback (CI)
alr exec -- gprbuild -P cortex_m4_tls.gpr -XMBEDTLS_MODE=loopback

# Full mbedTLS
alr exec -- gprbuild -P cortex_m4_tls.gpr -XMBEDTLS_MODE=full
```

## Memory

| Config | Flash | RAM |
|--------|-------|-----|
| Loopback (stubs) | ~15 KB | ~12 KB |
| Full mbedTLS | ~80 KB | ~25 KB |
