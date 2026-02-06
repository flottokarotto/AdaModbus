/**
 * ada_lwip_wrapper.c - C Wrapper Functions for Ada LwIP Bindings
 * Copyright (c) 2026 Florian Fischer
 * SPDX-License-Identifier: MIT
 *
 * Provides simplified C functions that can be called from Ada.
 */

#include "lwip/init.h"
#include "lwip/netif.h"
#include "lwip/ip4_addr.h"
#include "lwip/etharp.h"
#include "ethernetif.h"

/* Global network interface */
static struct netif g_netif;

/**
 * Simplified netif_add for Ada
 * Sets up the network interface with the Ethernet driver
 */
void ada_netif_add(struct netif *netif_struct,
                   ip4_addr_t ip_addr,
                   ip4_addr_t netmask,
                   ip4_addr_t gateway)
{
    ip4_addr_t ip, nm, gw;

    ip.addr = ip_addr.addr;
    nm.addr = netmask.addr;
    gw.addr = gateway.addr;

    /* Use provided netif struct or global */
    struct netif *nif = netif_struct ? netif_struct : &g_netif;

    netif_add(nif, &ip, &nm, &gw, NULL, ethernetif_init, netif_input);
}

/**
 * Ada tick callback (weak implementation, overridden by Ada)
 */
uint32_t __attribute__((weak)) ada_get_tick_ms(void)
{
    /* Weak implementation - should be overridden by Ada */
    static uint32_t tick = 0;
    return tick++;
}
