/**
 * sys_arch.c - LwIP System Architecture for NO_SYS mode
 *
 * Minimal sys_arch for bare-metal (NO_SYS=1) operation.
 * Note: sys_now() is provided by ethernetif.c (calls HAL_GetTick).
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
