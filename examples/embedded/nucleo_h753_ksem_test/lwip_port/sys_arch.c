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
 *   - rand()/srand() -- needed by LwIP for TCP ISN generation
 *   - strlen()       -- needed by LwIP internally
 *   - printf()/puts() -- stubs (LwIP LWIP_PLATFORM_DIAG references these)
 *
 * Note: sys_now() is provided by ethernetif.c (returns HAL_GetTick()),
 * keeping all time-related functions close to the ETH driver.
 */

#include <stdarg.h>
#include <stddef.h>

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
