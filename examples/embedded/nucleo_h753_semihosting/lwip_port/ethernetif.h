/**
 * ethernetif.h - Ethernet network interface driver header
 *
 * Based on STM32CubeH7 LwIP examples.
 * Copyright (c) 2017 STMicroelectronics. All rights reserved.
 * License: BSD-3-Clause
 */

#ifndef __ETHERNETIF_H__
#define __ETHERNETIF_H__

#include "lwip/err.h"
#include "lwip/netif.h"

/* Exported functions */
err_t ethernetif_init(struct netif *netif);
void ethernetif_input(struct netif *netif);
void ethernet_link_check_state(struct netif *netif);

/* Ada-compatible helpers */
void ethernetif_set_netif(struct netif *netif);
int ethernetif_link_status(void);

#endif /* __ETHERNETIF_H__ */
