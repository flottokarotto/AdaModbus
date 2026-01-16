/*
 * sys_arch.c - LwIP System Architecture for NO_SYS mode
 * Copyright (c) 2026 Florian Fischer
 * SPDX-License-Identifier: MIT
 *
 * Minimal stubs required when LwIP is configured with NO_SYS=1.
 * No threading support needed.
 */

#include "lwip/sys.h"
#include "lwip/opt.h"

#if NO_SYS

/* Initialize the system architecture layer */
void sys_init(void)
{
    /* Nothing to do for bare-metal NO_SYS */
}

/* Returns the current time in milliseconds */
/* This should be implemented with a hardware timer */
static volatile u32_t sys_tick_counter = 0;

u32_t sys_now(void)
{
    return sys_tick_counter;
}

/* Call this from SysTick or Timer interrupt (every 1ms) */
void sys_tick_increment(void)
{
    sys_tick_counter++;
}

#if LWIP_SOCKET

/*
 * errno support for sockets
 * Each "thread" has its own errno, but with NO_SYS there's only one
 */
static int lwip_errno_val = 0;

int *__errno(void)
{
    return &lwip_errno_val;
}

/* Alternative name used by some toolchains */
int *__errno_location(void)
{
    return &lwip_errno_val;
}

#endif /* LWIP_SOCKET */

#endif /* NO_SYS */

/*
 * Random number generation (weak default)
 * Override with hardware RNG if available
 */
__attribute__((weak))
unsigned int lwip_port_rand(void)
{
    static unsigned int seed = 123456789;
    seed = seed * 1103515245 + 12345;
    return seed;
}
