/**
 * ethernetif.h - Ethernet Interface Driver Header
 * Copyright (c) 2026 Florian Fischer
 * SPDX-License-Identifier: MIT
 */

#ifndef ETHERNETIF_H
#define ETHERNETIF_H

#include "lwip/netif.h"
#include "lwip/err.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Initialize the Ethernet network interface
 * @param netif  Network interface structure to initialize
 * @return       ERR_OK on success
 */
err_t ethernetif_init(struct netif *netif);

/**
 * Poll for received Ethernet frames
 * Call this from the main loop
 * @param netif  Network interface
 */
void ethernetif_input(struct netif *netif);

/**
 * Check Ethernet link status
 * @return  1 if link is up, 0 if down
 */
int ethernetif_link_status(void);

#ifdef __cplusplus
}
#endif

#endif /* ETHERNETIF_H */
