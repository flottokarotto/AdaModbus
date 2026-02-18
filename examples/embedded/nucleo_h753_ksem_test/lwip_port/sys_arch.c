/**
 * sys_arch.c - LwIP System Architecture for NO_SYS bare-metal mode
 *
 * Self-written (not from STM32CubeH7).
 * Reason: LwIP's NO_SYS=1 mode requires a sys_arch implementation, but
 *         the ST examples only provide RTOS-based versions (FreeRTOS).
 *         For bare-metal, we need minimal C library stubs that newlib's
 *         nano libc doesn't provide in our link configuration.
 *
 * Provides:
 *   - sys_arch_protect/unprotect -- PRIMASK-based critical sections
 *     (SYS_LIGHTWEIGHT_PROT=1) to guard LwIP memory pools against
 *     concurrent access from ISR (HAL ETH callbacks) and mainloop.
 *   - rand()/srand() -- needed by LwIP for TCP ISN generation
 *   - strlen()       -- needed by LwIP internally
 *   - printf()/puts() -- stubs (LwIP LWIP_PLATFORM_DIAG references these)
 *
 * Note: sys_now() is provided by ethernetif.c (returns HAL_GetTick()),
 * keeping all time-related functions close to the ETH driver.
 */

#include <stdarg.h>
#include <stddef.h>
#include "lwip/opt.h"
#include "arch/cc.h"

/*---------------------------------------------------------------------------*/
/* Critical Sections (SYS_LIGHTWEIGHT_PROT=1)                               */
/*---------------------------------------------------------------------------*/

/* Save PRIMASK and disable all interrupts.
 * Returns previous PRIMASK so the caller can restore it exactly,
 * rather than unconditionally re-enabling IRQs on unprotect. */
sys_prot_t sys_arch_protect(void)
{
    uint32_t primask;
    __asm volatile ("mrs %0, primask" : "=r" (primask) :: "memory");
    __asm volatile ("cpsid i" ::: "memory");  /* disable IRQ */
    return primask;
}

/* Restore PRIMASK to the value saved by sys_arch_protect.
 * Only re-enables IRQs if they were enabled before the protect call. */
void sys_arch_unprotect(sys_prot_t pval)
{
    __asm volatile ("msr primask, %0" :: "r" (pval) : "memory");
}

/*---------------------------------------------------------------------------*/
/* C Library Stubs for Bare Metal */
/*---------------------------------------------------------------------------*/

/* Simple random number generator (linear congruential) */
static unsigned int rand_seed = 1;

int rand(void)
{
    rand_seed = rand_seed * 1103515245 + 12345;
    return (int)((rand_seed >> 16) & 0x7FFF);
}

void srand(unsigned int seed)
{
    rand_seed = seed;
}

/* String length */
size_t strlen(const char *s)
{
    const char *p = s;
    while (*p) p++;
    return p - s;
}

/* Printf stub - does nothing in bare metal */
int printf(const char *format, ...)
{
    (void)format;
    return 0;
}

/* Puts stub */
int puts(const char *s)
{
    (void)s;
    return 0;
}
