/*
 * UART Stub for testing - Replace with actual hardware implementation
 * Copyright (c) 2026 Florian Fischer
 * SPDX-License-Identifier: MIT
 */

#include <stdint.h>
#include <stddef.h>

/*
 * Receive data from UART
 * Returns number of bytes received, 0 on timeout
 *
 * For real hardware, implement this using:
 * - HAL_UART_Receive (STM32)
 * - uart_read (Zephyr)
 * - UARTCharGetNonBlocking (TI)
 * etc.
 */
size_t uart_receive(uint8_t *buffer, size_t max_length, unsigned int timeout_ms)
{
    (void)buffer;
    (void)max_length;
    (void)timeout_ms;

    /* Stub: no data available */
    return 0;
}

/*
 * Send data via UART
 * Returns number of bytes sent
 *
 * For real hardware, implement this using:
 * - HAL_UART_Transmit (STM32)
 * - uart_write (Zephyr)
 * - UARTCharPut (TI)
 * etc.
 */
size_t uart_send(const uint8_t *data, size_t length)
{
    (void)data;

    /* Stub: pretend all bytes were sent */
    return length;
}
