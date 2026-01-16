# Minimal RTU Embedded Example

This is a minimal Modbus RTU slave example for embedded systems.

## Features

- No TCP/IP stack required (RTU over UART only)
- ZFP/Light runtime compatible
- Minimal RAM footprint (~2KB)
- Simple callback-based design

## Hardware Requirements

- Any ARM Cortex-M microcontroller
- UART peripheral for RS-485/RS-232

## Building

```bash
# With Alire (using arm-eabi toolchain)
alr toolchain --select gnat_arm_elf
alr exec -- gprbuild -P minimal_rtu.gpr --target=arm-eabi --RTS=light-cortex-m4f
```

## Integration

1. Implement `UART_Send` and `UART_Receive` for your hardware
2. Call `Modbus_Poll` from your main loop
3. Implement callbacks for your register map

## Memory Usage

Approximate RAM usage:
- Request buffer: 256 bytes
- Response buffer: 256 bytes
- Register data: configurable
- Stack: ~1KB per call

Total: ~2-3KB minimum
