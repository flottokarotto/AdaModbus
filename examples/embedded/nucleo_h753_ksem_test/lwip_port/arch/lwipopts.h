/**
 * lwipopts.h - LwIP Configuration for NUCLEO-H753ZI
 * Copyright (c) 2026 Florian Fischer
 * SPDX-License-Identifier: MIT
 *
 * Self-written, guided by:
 *   STM32CubeH7/Projects/STM32H743I-EVAL/Applications/LwIP/
 *   LwIP_TCP_Echo_Server/Inc/lwipopts.h
 *
 * Optimized for a single Modbus TCP client connection (~260 byte frames):
 *   - NO_SYS=1 (bare-metal, no RTOS)
 *   - Static IP (no DHCP)
 *   - Callback API only (no socket/netconn)
 *   - Small TCP MSS/window (512 bytes, sufficient for Modbus)
 *   - LwIP heap relocated to SRAM2 (0x30004000) for ETH DMA access
 *   - Software checksums (hardware offload disabled for debugging)
 *   - Custom pbuf support for zero-copy RX (LWIP_SUPPORT_CUSTOM_PBUF)
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

#define SYS_LIGHTWEIGHT_PROT            1       /* Defensive: guard memory pools for possible future IRQ-based ETH */
#define LWIP_PROVIDE_ERRNO              1

/*---------------------------------------------------------------------------*/
/* Memory Configuration                                                      */
/*---------------------------------------------------------------------------*/
#define MEM_ALIGNMENT                   4       /* 32-bit alignment */
#define MEM_SIZE                        (14 * 1024)  /* 14 KB heap (matches ST example) */

/* Relocate LwIP RAM heap to SRAM2 (DMA-accessible, 0x30000000 region) */
#define LWIP_RAM_HEAP_POINTER           (0x30004000)

#define MEMP_NUM_PBUF                   16
#define MEMP_NUM_UDP_PCB                2
#define MEMP_NUM_TCP_PCB                2       /* Only need 1-2 connections */
#define MEMP_NUM_TCP_PCB_LISTEN         0       /* Client only, no listen */
#define MEMP_NUM_TCP_SEG                16
#define MEMP_NUM_NETBUF                 0       /* Not using netconn API */
#define MEMP_NUM_NETCONN                0       /* Not using netconn API */
#define MEMP_NUM_SYS_TIMEOUT            6

#define PBUF_POOL_SIZE                  16
#define PBUF_POOL_BUFSIZE               1536    /* Full Ethernet frame */

/* Custom pbuf support for zero-copy RX (required by ST HAL ETH driver) */
#define LWIP_SUPPORT_CUSTOM_PBUF        1

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

#define LWIP_TCP_KEEPALIVE              1       /* Detect dead connections */
#define TCP_KEEPIDLE_DEFAULT            10000   /* 10s before first probe */
#define TCP_KEEPINTVL_DEFAULT           5000    /* 5s between probes */
#define TCP_KEEPCNT_DEFAULT             3       /* 3 probes before drop */
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
#define ARP_QUEUEING                    1       /* Queue packets during ARP resolution */
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
/* Checksum - all software for now (debug)                                   */
/*---------------------------------------------------------------------------*/
#define CHECKSUM_GEN_IP                 1
#define CHECKSUM_GEN_UDP                1
#define CHECKSUM_GEN_TCP                1
#define CHECKSUM_GEN_ICMP               1
#define CHECKSUM_CHECK_IP               1
#define CHECKSUM_CHECK_UDP              1
#define CHECKSUM_CHECK_TCP              1
#define CHECKSUM_CHECK_ICMP             1

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
