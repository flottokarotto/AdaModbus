# Minimal RTU Example

Minimal Modbus RTU slave for embedded systems.

## Features

- No TCP/IP stack required
- ZFP/Light runtime compatible
- ~2 KB RAM footprint

## Building

```bash
alr toolchain --select gnat_arm_elf
alr exec -- gprbuild -P minimal_rtu.gpr --target=arm-eabi --RTS=light-cortex-m4f
```

## Integration

1. Implement `UART_Send` and `UART_Receive` for your hardware
2. Call `Modbus_Poll` from your main loop
3. Implement register/coil callbacks
