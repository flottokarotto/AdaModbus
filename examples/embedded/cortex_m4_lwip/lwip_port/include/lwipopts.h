/*
 * LwIP Options for Modbus TCP on Cortex-M
 * Copyright (c) 2026 Florian Fischer
 * SPDX-License-Identifier: MIT
 *
 * Memory-optimierte Konfiguration für Modbus TCP.
 * Modbus ADU max 260 Bytes, daher kleine Puffer ausreichend.
 *
 * Geschätzte RAM-Nutzung: ~6-8 KB
 * - Heap: 4KB
 * - PBUF Pool: 8 * 512 = 4KB
 * - TCP PCBs, ARP etc: ~1KB
 */

#ifndef LWIPOPTS_H
#define LWIPOPTS_H

/* ========== Platform ========== */
#define NO_SYS                      1       /* Kein OS */
#define LWIP_SOCKET                 1       /* BSD Socket API */
#define LWIP_COMPAT_SOCKETS         0       /* Keine POSIX-Kompatibilität */
#define LWIP_NETCONN                0       /* Netconn deaktiviert (spart RAM) */

/* ========== Memory (optimiert für Modbus) ========== */
#define MEM_ALIGNMENT               4
#define MEM_SIZE                    (4 * 1024)    /* 4KB Heap (reduziert) */
#define MEM_LIBC_MALLOC             0             /* Eigener Allocator */
#define MEMP_MEM_MALLOC             0             /* Pools statt malloc */

/* ========== Packet Buffers ========== */
/* Modbus ADU max 260 Bytes, 512 reicht mit Header-Overhead */
#define MEMP_NUM_PBUF               8             /* Reduziert von 16 */
#define PBUF_POOL_SIZE              8             /* Reduziert von 16 */
#define PBUF_POOL_BUFSIZE           512           /* Reduziert von 1536 */

/* ========== TCP (optimiert für Modbus) ========== */
#define LWIP_TCP                    1
#define TCP_MSS                     512           /* Reduziert für Modbus */
#define TCP_SND_BUF                 (2 * TCP_MSS) /* 1KB Send Buffer */
#define TCP_WND                     (2 * TCP_MSS) /* 1KB Window */
#define TCP_SND_QUEUELEN            4             /* Kleine Queue */
#define MEMP_NUM_TCP_PCB            2             /* Max 2 Verbindungen */
#define MEMP_NUM_TCP_PCB_LISTEN     1             /* 1 Listener reicht */
#define MEMP_NUM_TCP_SEG            8             /* Reduziert von 16 */
#define TCP_QUEUE_OOSEQ             0             /* Out-of-order deaktiviert */
#define LWIP_TCP_SACK_OUT           0             /* SACK deaktiviert */
#define LWIP_TCP_TIMESTAMPS         0             /* Timestamps deaktiviert */
#define LWIP_WND_SCALE              0             /* Window Scaling aus */

/* ========== IP ========== */
#define LWIP_IPV4                   1
#define LWIP_IPV6                   0             /* IPv6 deaktiviert */
#define IP_REASSEMBLY               0             /* Keine Reassembly */
#define IP_FRAG                     0             /* Keine Fragmentierung */
#define IP_OPTIONS_ALLOWED          0             /* IP Options ignorieren */

/* ========== UDP (für DHCP) ========== */
#define LWIP_UDP                    1
#define MEMP_NUM_UDP_PCB            2             /* Reduziert */

/* ========== DHCP ========== */
#define LWIP_DHCP                   1
/* Für statische IP: LWIP_DHCP auf 0 setzen spart ~1KB */

/* ========== Ethernet/ARP ========== */
#define LWIP_ARP                    1
#define ARP_TABLE_SIZE              4             /* Reduziert von 10 */
#define ARP_QUEUEING                0             /* Kein ARP Queueing */
#define ETHARP_SUPPORT_STATIC_ENTRIES 0

/* ========== ICMP (Ping) ========== */
#define LWIP_ICMP                   1
/* Für Produktion ohne Ping: LWIP_ICMP auf 0 setzen */

/* ========== Deaktivierte Features ========== */
#define LWIP_DNS                    0             /* DNS nicht benötigt */
#define LWIP_IGMP                   0             /* Multicast aus */
#define LWIP_RAW                    0             /* Raw sockets aus */
#define LWIP_SNMP                   0             /* SNMP aus */
#define LWIP_AUTOIP                 0             /* AutoIP aus */

/* ========== Statistics (Debug) ========== */
#define LWIP_STATS                  0
#define LWIP_STATS_DISPLAY          0

/* ========== Checksum ========== */
/* Software-Checksummen (für HW-Checksum: auf 0 setzen) */
#define CHECKSUM_GEN_IP             1
#define CHECKSUM_GEN_TCP            1
#define CHECKSUM_GEN_UDP            1
#define CHECKSUM_CHECK_IP           1
#define CHECKSUM_CHECK_TCP          1
#define CHECKSUM_CHECK_UDP          1

/* ========== Debug ========== */
#define LWIP_DEBUG                  0
#define LWIP_DBG_MIN_LEVEL          LWIP_DBG_LEVEL_ALL
#define LWIP_DBG_TYPES_ON           LWIP_DBG_ON

/* ========== Threading (NO_SYS=1) ========== */
#define LWIP_TCPIP_CORE_LOCKING     0
#define SYS_LIGHTWEIGHT_PROT        0

#endif /* LWIPOPTS_H */
