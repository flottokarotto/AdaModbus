/**
 * uart_debug.c - C wrappers for UART debug output
 *
 * Provides uart_put(), uart_put_line(), uart_put_int(), uart_put_hex()
 * for debug messages from C code (ethernetif.c init, link status, etc.).
 *
 * Calls Ada's USART3 send routine directly using the GNAT mangled name
 * stm32h7_hal__usart3_send_byte (defined in stm32h7_hal.adb).  This
 * avoids a separate C UART driver -- the Ada HAL already configures
 * USART3 on PD8/PD9 (ST-Link VCP) at 115200 8N1.
 */
#include <stdint.h>
#include <stddef.h>

/* Ada's USART3 send byte function */
extern void stm32h7_hal__usart3_send_byte(uint8_t byte);

void uart_put(const char *msg)
{
    while (*msg) {
        stm32h7_hal__usart3_send_byte((uint8_t)*msg++);
    }
}

void uart_put_line(const char *msg)
{
    uart_put(msg);
    stm32h7_hal__usart3_send_byte(0x0D);
    stm32h7_hal__usart3_send_byte(0x0A);
}

void uart_put_int(int val)
{
    char buf[12];
    int pos = 11;
    int neg = 0;
    unsigned int v;

    buf[pos--] = '\0';

    if (val < 0) {
        neg = 1;
        v = (unsigned int)(-(long long)val);
    } else {
        v = (unsigned int)val;
    }

    if (v == 0) {
        buf[pos--] = '0';
    } else {
        while (v > 0) {
            buf[pos--] = '0' + (v % 10);
            v /= 10;
        }
    }

    if (neg) {
        buf[pos--] = '-';
    }

    uart_put(&buf[pos + 1]);
}

void uart_put_hex(uint8_t val)
{
    static const char hex[] = "0123456789ABCDEF";
    char buf[3];
    buf[0] = hex[(val >> 4) & 0xF];
    buf[1] = hex[val & 0xF];
    buf[2] = '\0';
    uart_put(buf);
}
