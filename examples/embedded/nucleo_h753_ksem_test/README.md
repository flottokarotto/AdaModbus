# NUCLEO-H753ZI KSEM Test

Minimal Modbus TCP test application for NUCLEO-H753ZI.
Reads power values from a Kostal Smart Energy Meter (KSEM) and outputs
them via USART3 serial console (ST-Link VCP, 115200 8N1).

## Hardware

- NUCLEO-H753ZI board (STM32H753ZI, Cortex-M7, 480 MHz)
- Ethernet cable to local network with KSEM

## Setup

```bash
# Initialize submodules (STM32CubeH7 is ~1 GB, uses shallow clone)
./setup.sh

# Or manually:
git submodule update --init --depth 1 examples/embedded/nucleo_h753_ksem_test/STM32CubeH7
git submodule update --init examples/embedded/nucleo_h753_ksem_test/lwip
```

## Build

```bash
alr build
```

## Flash

```bash
./flash.sh
```

Or manually:
```bash
arm-eabi-objcopy -O binary bin/main.elf bin/main.bin
st-flash write bin/main.bin 0x08000000
```

## LED Status (MB1364 Rev E)

| LED | Pin | Meaning |
|-----|-----|---------|
| Green (LD1) | PB0 | Heartbeat, blinks every 500 ms |
| Yellow (LD2) | PE1 | Activity, short pulse on successful KSEM read |
| Red (LD3) | PB14 | Error (on = last read failed) |

Red + no heartbeat: fatal error (no Ethernet link or KSEM connect failed).

## Serial Output

Connect to ST-Link VCP at **115200 8N1** (e.g. PuTTY, minicom, or `screen /dev/ttyACMx 115200`).

Example output:
```
NUCLEO-H753ZI KSEM Test
Initializing Ethernet...
Ethernet OK, waiting for link...
Link up!
Local IP: 192.168.1.100
KSEM IP:  192.168.1.50:502
Waiting for network settle...
Connecting to KSEM...
KSEM connected!
Grid: 1234W  L1:400  L2:412  L3:422W
```

## Configuration

Edit `src/config.ads` to set:
- Board IP address
- KSEM IP address and port
- Modbus timeout

## File Provenance

### Own Code (MIT License)

| File | Description |
|------|-------------|
| `src/main.adb` | KSEM test application with UART output |
| `src/uart_console.ads/adb` | Serial console via USART3 (ST-Link VCP) |
| `src/startup.S` | Vector table and Reset_Handler |
| `src/config.ads` | Network configuration |
| `src/hal_stubs.ads/adb` | HAL abstraction layer |
| `src/tcp_client.ads/adb` | Blocking TCP client over LwIP |
| `src/ksem_client.ads/adb` | KSEM Modbus TCP reader |
| `src/lwip_bindings.ads/adb` | Ada bindings for LwIP raw API |
| `src/stm32h7_hal.ads/adb` | STM32H7 peripheral drivers (SVD-based) |
| `src/time_exports.ads/adb` | Ada→C tick export for LwIP |
| `src/svd/*.ads` | SVD-generated register definitions (svd2ada) |
| `lwip_port/hal_shim.c` | Minimal HAL glue (GetTick, ETH MspInit) |
| `lwip_port/ada_lwip_wrapper.c` | C wrapper for Ada netif_add |
| `lwip_port/sys_arch.c` | LwIP sys_now + C library stubs |
| `lwip_port/arch/cc.h` | LwIP compiler abstraction |
| `lwip_port/arch/lwipopts.h` | LwIP configuration |
| `lwip_port/stm32h7xx_hal_conf.h` | HAL module selection (ETH only) |

### ST ETH Driver Adaptation (BSD-3-Clause)

| File | Source |
|------|--------|
| `lwip_port/ethernetif.c` | Based on STM32CubeH7 LwIP example (`Middlewares/Third_Party/LwIP/system/OS/ethernetif.c`), adapted for NUCLEO-H753ZI pin mapping |
| `lwip_port/ethernetif.h` | Header for above |

### From STM32CubeH7 Submodule (BSD-3-Clause)

Compiled via `#include` wrappers in `lwip_port/`:

| Wrapper | Compiled Source | License |
|---------|----------------|---------|
| `stm32_hal_eth.c` | `Drivers/STM32H7xx_HAL_Driver/Src/stm32h7xx_hal_eth.c` | BSD-3-Clause (ST) |
| `stm32_lan8742.c` | `Drivers/BSP/Components/lan8742/lan8742.c` | BSD-3-Clause (ST) |

Headers used via `-I` flags:
- `Drivers/CMSIS/Core/Include/` — ARM CMSIS Core (Apache-2.0)
- `Drivers/CMSIS/Device/ST/STM32H7xx/Include/` — CMSIS Device (BSD-3-Clause)
- `Drivers/STM32H7xx_HAL_Driver/Inc/` — HAL headers (BSD-3-Clause)
- `Drivers/BSP/Components/lan8742/` — LAN8742 PHY driver (BSD-3-Clause)

### From LwIP Submodule (BSD-3-Clause)

- `lwip/src/core/` — TCP/IP stack core
- `lwip/src/netif/` — Network interface layer
- `lwip/src/api/` — API layer (minimal, NO_SYS mode)
- `lwip/src/include/` — Public headers

### From AdaModbus (MIT License)

Referenced via relative source dirs in GPR:
- `../../../src/core/` — Modbus protocol core (SPARK verified)
- `../../../src/energy/` — KSEM energy profile
