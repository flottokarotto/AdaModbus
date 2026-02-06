/**
 * lwipopts.h - LwIP Configuration for NUCLEO-H753ZI
 * Copyright (c) 2026 Florian Fischer
 * SPDX-License-Identifier: MIT
 *
 * Optimized for Modbus TCP client with minimal footprint.
 */

#ifndef LWIPOPTS_H
#define LWIPOPTS_H

/*---------------------------------------------------------------------------*/
/* Platform / Architecture */
/*---------------------------------------------------------------------------*/
#define NO_SYS                          1       /* No OS, bare metal */
#define LWIP_SOCKET                     0       /* No socket API */
#define LWIP_NETCONN                    0       /* No netconn API */
#define LWIP_NETIF_API                  0

#define SYS_LIGHTWEIGHT_PROT            0       /* No protection needed (no OS) */
#define LWIP_PROVIDE_ERRNO              1

/*---------------------------------------------------------------------------*/
/* Memory Configuration - Optimized for minimal RAM usage */
/*---------------------------------------------------------------------------*/
#define MEM_ALIGNMENT                   4       /* 32-bit alignment */
#define MEM_SIZE                        (4 * 1024)   /* 4 KB heap (was 16 KB) */

#define MEMP_NUM_PBUF                   8       /* Reduced from 16 */
#define MEMP_NUM_UDP_PCB                2       /* Reduced from 4 */
#define MEMP_NUM_TCP_PCB                2       /* Only need 1-2 connections */
#define MEMP_NUM_TCP_PCB_LISTEN         0       /* Client only, no listen */
#define MEMP_NUM_TCP_SEG                8       /* Reduced from 16 */
#define MEMP_NUM_NETBUF                 0       /* Not using netconn API */
#define MEMP_NUM_NETCONN                0       /* Not using netconn API */
#define MEMP_NUM_SYS_TIMEOUT            6       /* Reduced from 8 */

#define PBUF_POOL_SIZE                  8       /* Reduced from 16 */
#define PBUF_POOL_BUFSIZE               512     /* Modbus max ~260 bytes */

/*---------------------------------------------------------------------------*/
/* TCP Configuration - Optimized for Modbus (small packets) */
/*---------------------------------------------------------------------------*/
#define LWIP_TCP                        1
#define TCP_TTL                         255
#define TCP_QUEUE_OOSEQ                 0       /* No out-of-order segments */
#define TCP_MSS                         512     /* Modbus needs ~260 bytes max */
#define TCP_SND_BUF                     (2 * TCP_MSS)  /* Reduced send buffer */
#define TCP_SND_QUEUELEN                4       /* Reduced queue */
#define TCP_SNDLOWAT                    (TCP_SND_BUF / 4)
#define TCP_SNDQUEUELOWAT               2       /* Must be < TCP_SND_QUEUELEN */
#define TCP_WND                         (2 * TCP_MSS)  /* Receive window */
#define TCP_MAXRTX                      8       /* Reduced retries */
#define TCP_SYNMAXRTX                   3       /* Reduced SYN retries */

#define LWIP_TCP_KEEPALIVE              0       /* Disable keepalive */
#define LWIP_TCP_TIMESTAMPS             0

/*---------------------------------------------------------------------------*/
/* UDP Configuration */
/*---------------------------------------------------------------------------*/
#define LWIP_UDP                        1
#define UDP_TTL                         255

/*---------------------------------------------------------------------------*/
/* ICMP Configuration */
/*---------------------------------------------------------------------------*/
#define LWIP_ICMP                       1

/*---------------------------------------------------------------------------*/
/* DHCP Configuration */
/*---------------------------------------------------------------------------*/
#define LWIP_DHCP                       0       /* Static IP, no DHCP */
#define DHCP_DOES_ARP_CHECK             0

/*---------------------------------------------------------------------------*/
/* ARP Configuration */
/*---------------------------------------------------------------------------*/
#define LWIP_ARP                        1
#define ARP_TABLE_SIZE                  4       /* Only need gateway + KSEM */
#define ARP_QUEUEING                    0       /* Disable queuing */
#define ETHARP_SUPPORT_STATIC_ENTRIES   0       /* Disable static entries */

/*---------------------------------------------------------------------------*/
/* IP Configuration */
/*---------------------------------------------------------------------------*/
#define IP_FORWARD                      0
#define IP_OPTIONS_ALLOWED              0
#define IP_REASSEMBLY                   0
#define IP_FRAG                         0
#define IP_DEFAULT_TTL                  255

#define LWIP_BROADCAST_PING             1
#define LWIP_MULTICAST_PING             0

/*---------------------------------------------------------------------------*/
/* Network Interface */
/*---------------------------------------------------------------------------*/
#define LWIP_NETIF_STATUS_CALLBACK      1
#define LWIP_NETIF_LINK_CALLBACK        1
#define LWIP_NETIF_HOSTNAME             1

#define LWIP_SINGLE_NETIF               1       /* Only one interface */

/*---------------------------------------------------------------------------*/
/* Checksum */
/*---------------------------------------------------------------------------*/
/* Use hardware checksums where available */
#define CHECKSUM_GEN_IP                 0
#define CHECKSUM_GEN_UDP                0
#define CHECKSUM_GEN_TCP                0
#define CHECKSUM_GEN_ICMP               0
#define CHECKSUM_CHECK_IP               0
#define CHECKSUM_CHECK_UDP              0
#define CHECKSUM_CHECK_TCP              0
#define CHECKSUM_CHECK_ICMP             0

/*---------------------------------------------------------------------------*/
/* Statistics */
/*---------------------------------------------------------------------------*/
#define LWIP_STATS                      0
#define LWIP_STATS_DISPLAY              0

/*---------------------------------------------------------------------------*/
/* Debugging */
/*---------------------------------------------------------------------------*/
#define LWIP_DEBUG                      0
#define LWIP_DBG_MIN_LEVEL              LWIP_DBG_LEVEL_OFF
#define LWIP_DBG_TYPES_ON               LWIP_DBG_OFF

/*---------------------------------------------------------------------------*/
/* Callbacks */
/*---------------------------------------------------------------------------*/
#define LWIP_CALLBACK_API               1

/*---------------------------------------------------------------------------*/
/* Raw API (used for Modbus TCP) */
/*---------------------------------------------------------------------------*/
#define LWIP_RAW                        0
#define MEMP_NUM_RAW_PCB                0

#endif /* LWIPOPTS_H */
