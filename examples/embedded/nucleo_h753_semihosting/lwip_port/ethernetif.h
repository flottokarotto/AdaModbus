/**
 * ethernetif.h - Ethernet network interface driver header
 *
 * Based on: STM32CubeH7/Projects/STM32H743I-EVAL/Applications/LwIP/
 *           LwIP_TCP_Echo_Server/Inc/ethernetif.h
 * Copyright (c) 2017 STMicroelectronics. All rights reserved.
 * License: BSD-3-Clause
 *
 * Extended with Ada-callable helper functions (ethernetif_set_netif,
 * ethernetif_link_status) that are not in ST's original header.
 * These allow the Ada main loop to poll link state without importing
 * the full netif struct layout.
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
