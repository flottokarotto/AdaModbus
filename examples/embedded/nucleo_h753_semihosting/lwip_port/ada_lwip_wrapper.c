/**
 * ada_lwip_wrapper.c - C Wrapper Functions for Ada LwIP Bindings
 *
 * Provides simplified C functions that can be called from Ada.
 * Uses uint32_t for IP addresses to avoid struct-by-value ABI issues
 * between Ada and C on ARM.
 */

#include "lwip/init.h"
#include "lwip/netif.h"
#include "lwip/ip4_addr.h"
#include "lwip/etharp.h"
#include "ethernetif.h"

/* Global network interface */
static struct netif g_netif;

/**
 * Simplified netif_add for Ada.
 * Takes raw uint32_t IP addresses (in LwIP byte order) instead of ip4_addr_t
 * structs to avoid ARM ABI struct-by-value mismatch between GNAT and GCC.
 */
void ada_netif_add(struct netif *netif_struct,
                   uint32_t ip_addr,
                   uint32_t netmask,
                   uint32_t gateway)
{
    ip4_addr_t ip, nm, gw;

    ip.addr = ip_addr;
    nm.addr = netmask;
    gw.addr = gateway;

    /* Use provided netif struct or global */
    struct netif *nif = netif_struct ? netif_struct : &g_netif;

    netif_add(nif, &ip, &nm, &gw, NULL, ethernetif_init, netif_input);

    /* Store netif pointer for link status checks */
    ethernetif_set_netif(nif);
}
