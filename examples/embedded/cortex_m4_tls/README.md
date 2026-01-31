# Cortex-M4 with TLS

Modbus over TLS example for ARM Cortex-M using mbedTLS.

## Quick Start (QEMU)

```bash
alr toolchain --select gnat_arm_elf
cd examples/embedded/cortex_m4_tls
alr build
qemu-system-arm -M lm3s6965evb -nographic -semihosting -kernel bin/main_loopback_tls.elf
```

Exit QEMU with `Ctrl+A`, then `X`.

## Programs

| Program | Description |
|---------|-------------|
| `main_loopback_tls` | TLS loopback self-test |
| `main_tls_client` | TLS client example |
| `main_tls_server` | TLS server example |

## Build Modes

```bash
# Loopback with stubs (for CI)
alr exec -- gprbuild -P cortex_m4_tls.gpr -XMBEDTLS_MODE=loopback

# Full mbedTLS
alr exec -- gprbuild -P cortex_m4_tls.gpr -XMBEDTLS_MODE=full
```

## Memory Usage

- Loopback with stubs: ~15 KB Flash, ~12 KB RAM
- Full mbedTLS: ~80 KB Flash, ~25 KB RAM
