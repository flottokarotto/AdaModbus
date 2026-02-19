/**
 * hardfault_diagnostic.c - HardFault register dump via USART3
 *
 * Called from HardFault_Handler (startup.S) with the stacked frame pointer.
 * Prints fault cause and stacked PC/LR to the serial console.
 */

#include <stdint.h>

/* USART3 registers (APB1, 0x40004800) */
#define USART3_ISR  (*(volatile uint32_t *)0x4000481C)
#define USART3_TDR  (*(volatile uint32_t *)0x40004828)
#define USART3_TXE  (1 << 7)

/* Cortex-M7 fault status registers */
#define SCB_CFSR    (*(volatile uint32_t *)0xE000ED28)
#define SCB_HFSR    (*(volatile uint32_t *)0xE000ED2C)
#define SCB_MMFAR   (*(volatile uint32_t *)0xE000ED34)
#define SCB_BFAR    (*(volatile uint32_t *)0xE000ED38)

static void uart_putc(char c)
{
    while (!(USART3_ISR & USART3_TXE)) {}
    USART3_TDR = (uint32_t)c;
}

static void uart_puts(const char *s)
{
    while (*s) uart_putc(*s++);
}

static void uart_hex32(uint32_t val)
{
    const char hex[] = "0123456789ABCDEF";
    uart_puts("0x");
    for (int i = 28; i >= 0; i -= 4)
        uart_putc(hex[(val >> i) & 0xF]);
}

/* Stacked register frame (pushed by Cortex-M on exception entry) */
typedef struct {
    uint32_t r0, r1, r2, r3, r12, lr, pc, xpsr;
} stacked_regs_t;

void hardfault_diagnostic(stacked_regs_t *regs)
{
    uint32_t cfsr = SCB_CFSR;

    uart_puts("\r\n\r\n*** HARDFAULT ***\r\n");

    uart_puts("PC=");  uart_hex32(regs->pc);
    uart_puts(" LR="); uart_hex32(regs->lr);
    uart_puts("\r\n");

    uart_puts("CFSR="); uart_hex32(cfsr);
    uart_puts(" HFSR="); uart_hex32(SCB_HFSR);
    uart_puts("\r\n");

    /* Memory Management Fault */
    if (cfsr & 0xFF) {
        uart_puts("MemManage: ");
        if (cfsr & (1 << 7)) { uart_puts("MMARVALID MMFAR="); uart_hex32(SCB_MMFAR); }
        if (cfsr & (1 << 0)) uart_puts(" IACCVIOL");
        if (cfsr & (1 << 1)) uart_puts(" DACCVIOL");
        if (cfsr & (1 << 3)) uart_puts(" MUNSTKERR");
        if (cfsr & (1 << 4)) uart_puts(" MSTKERR");
        if (cfsr & (1 << 5)) uart_puts(" MLSPERR");
        uart_puts("\r\n");
    }

    /* Bus Fault */
    if (cfsr & 0xFF00) {
        uart_puts("BusFault: ");
        if (cfsr & (1 << 15)) { uart_puts("BFARVALID BFAR="); uart_hex32(SCB_BFAR); }
        if (cfsr & (1 << 8))  uart_puts(" IBUSERR");
        if (cfsr & (1 << 9))  uart_puts(" PRECISERR");
        if (cfsr & (1 << 10)) uart_puts(" IMPRECISERR");
        if (cfsr & (1 << 11)) uart_puts(" UNSTKERR");
        if (cfsr & (1 << 12)) uart_puts(" STKERR");
        if (cfsr & (1 << 13)) uart_puts(" LSPERR");
        uart_puts("\r\n");
    }

    /* Usage Fault */
    if (cfsr & 0xFFFF0000) {
        uart_puts("UsageFault:");
        if (cfsr & (1 << 16)) uart_puts(" UNDEFINSTR");
        if (cfsr & (1 << 17)) uart_puts(" INVSTATE");
        if (cfsr & (1 << 18)) uart_puts(" INVPC");
        if (cfsr & (1 << 19)) uart_puts(" NOCP");
        if (cfsr & (1 << 24)) uart_puts(" UNALIGNED");
        if (cfsr & (1 << 25)) uart_puts(" DIVBYZERO");
        uart_puts("\r\n");
    }

    uart_puts("R0=");  uart_hex32(regs->r0);
    uart_puts(" R1="); uart_hex32(regs->r1);
    uart_puts(" R2="); uart_hex32(regs->r2);
    uart_puts(" R3="); uart_hex32(regs->r3);
    uart_puts("\r\n");
    uart_puts("R12="); uart_hex32(regs->r12);
    uart_puts(" SP="); uart_hex32((uint32_t)regs + sizeof(stacked_regs_t));
    uart_puts("\r\n");

    /* Halt */
    while (1) {}
}
