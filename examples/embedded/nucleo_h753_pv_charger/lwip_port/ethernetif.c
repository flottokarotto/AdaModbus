/**
 * ethernetif.c - Ethernet Interface Driver for STM32H7 + LAN8742A
 * Copyright (c) 2026 Florian Fischer
 * SPDX-License-Identifier: MIT
 *
 * Low-level Ethernet driver for NUCLEO-H753ZI with LAN8742A PHY.
 * Implements the lwIP netif interface.
 */

#include "lwip/opt.h"
#include "lwip/def.h"
#include "lwip/mem.h"
#include "lwip/pbuf.h"
#include "lwip/timeouts.h"
#include "lwip/etharp.h"
#include "netif/ethernet.h"
#include "ethernetif.h"
#include <string.h>

/*---------------------------------------------------------------------------*/
/* STM32H7 Ethernet Registers */
/*---------------------------------------------------------------------------*/
#define ETH_BASE            0x40028000UL

/* MAC registers */
#define ETH_MACCR           (*(volatile uint32_t *)(ETH_BASE + 0x0000))
#define ETH_MACECR          (*(volatile uint32_t *)(ETH_BASE + 0x0004))
#define ETH_MACPFR          (*(volatile uint32_t *)(ETH_BASE + 0x0008))
#define ETH_MACWTR          (*(volatile uint32_t *)(ETH_BASE + 0x000C))
#define ETH_MACA0HR         (*(volatile uint32_t *)(ETH_BASE + 0x0300))
#define ETH_MACA0LR         (*(volatile uint32_t *)(ETH_BASE + 0x0304))
#define ETH_MACMDIOAR       (*(volatile uint32_t *)(ETH_BASE + 0x0200))
#define ETH_MACMDIODR       (*(volatile uint32_t *)(ETH_BASE + 0x0204))

/* MTL registers */
#define ETH_MTLTQOMR        (*(volatile uint32_t *)(ETH_BASE + 0x0D00))
#define ETH_MTLRQOMR        (*(volatile uint32_t *)(ETH_BASE + 0x0D30))

/* DMA registers */
#define ETH_DMAMR           (*(volatile uint32_t *)(ETH_BASE + 0x1000))
#define ETH_DMASBMR         (*(volatile uint32_t *)(ETH_BASE + 0x1004))
#define ETH_DMAISR          (*(volatile uint32_t *)(ETH_BASE + 0x1008))
#define ETH_DMAC0CR         (*(volatile uint32_t *)(ETH_BASE + 0x1100))
#define ETH_DMAC0TXCR       (*(volatile uint32_t *)(ETH_BASE + 0x1104))
#define ETH_DMAC0RXCR       (*(volatile uint32_t *)(ETH_BASE + 0x1108))
#define ETH_DMAC0TXDLAR     (*(volatile uint32_t *)(ETH_BASE + 0x1114))
#define ETH_DMAC0RXDLAR     (*(volatile uint32_t *)(ETH_BASE + 0x111C))
#define ETH_DMAC0TXDTPR     (*(volatile uint32_t *)(ETH_BASE + 0x1120))
#define ETH_DMAC0RXDTPR     (*(volatile uint32_t *)(ETH_BASE + 0x1128))
#define ETH_DMAC0TXDRLR     (*(volatile uint32_t *)(ETH_BASE + 0x112C))
#define ETH_DMAC0RXDRLR     (*(volatile uint32_t *)(ETH_BASE + 0x1130))
#define ETH_DMAC0IER        (*(volatile uint32_t *)(ETH_BASE + 0x1134))
#define ETH_DMAC0SR         (*(volatile uint32_t *)(ETH_BASE + 0x1160))

/* RCC registers */
#define RCC_BASE            0x58024400UL
#define RCC_AHB1ENR         (*(volatile uint32_t *)(RCC_BASE + 0x00D8))
#define RCC_AHB1ENR_ETH1MACEN   (1 << 15)
#define RCC_AHB1ENR_ETH1TXEN    (1 << 16)
#define RCC_AHB1ENR_ETH1RXEN    (1 << 17)

/* SYSCFG for RMII */
#define SYSCFG_BASE         0x58000400UL
#define SYSCFG_PMCR         (*(volatile uint32_t *)(SYSCFG_BASE + 0x0004))
#define SYSCFG_PMCR_EPIS_RMII   (4 << 21)

/*---------------------------------------------------------------------------*/
/* LAN8742A PHY Registers */
/*---------------------------------------------------------------------------*/
#define PHY_ADDRESS         0x00    /* Default LAN8742A address */
#define PHY_BCR             0x00    /* Basic Control Register */
#define PHY_BSR             0x01    /* Basic Status Register */
#define PHY_PHYI1R          0x02    /* PHY Identifier 1 */
#define PHY_PHYI2R          0x03    /* PHY Identifier 2 */
#define PHY_ANAR            0x04    /* Auto-Negotiation Advertisement */
#define PHY_ANLPAR          0x05    /* Auto-Negotiation Link Partner Ability */
#define PHY_ANER            0x06    /* Auto-Negotiation Expansion */
#define PHY_ANNPTR          0x07    /* Auto-Negotiation Next Page TX */
#define PHY_SPECIAL_CTRL    0x1F    /* Special Control/Status */

#define PHY_BCR_RESET       (1 << 15)
#define PHY_BCR_AUTONEG_EN  (1 << 12)
#define PHY_BCR_RESTART_AN  (1 << 9)

#define PHY_BSR_LINK_UP     (1 << 2)
#define PHY_BSR_AUTONEG_DONE (1 << 5)

/*---------------------------------------------------------------------------*/
/* DMA Descriptors */
/*---------------------------------------------------------------------------*/
#define ETH_RXBUFNB         4
#define ETH_TXBUFNB         4
#define ETH_RX_BUF_SIZE     1524

/* DMA Descriptor structure (STM32H7 enhanced descriptors) */
typedef struct {
    volatile uint32_t DESC0;
    volatile uint32_t DESC1;
    volatile uint32_t DESC2;
    volatile uint32_t DESC3;
    uint32_t BackupAddr0;
    uint32_t BackupAddr1;
    uint32_t Reserved[2];
} ETH_DMADescTypeDef;

/* Descriptor bits */
#define ETH_DMARXDESC_OWN   0x80000000U
#define ETH_DMARXDESC_IOC   0x40000000U
#define ETH_DMARXDESC_BUF1V 0x01000000U
#define ETH_DMATXDESC_OWN   0x80000000U
#define ETH_DMATXDESC_IOC   0x40000000U
#define ETH_DMATXDESC_LS    0x20000000U
#define ETH_DMATXDESC_FS    0x10000000U
#define ETH_DMATXDESC_FD    0x00000001U
#define ETH_DMATXDESC_LD    0x00000002U

/*---------------------------------------------------------------------------*/
/* Static Data (must be in SRAM1/SRAM2 for DMA access) */
/*---------------------------------------------------------------------------*/
__attribute__((section(".eth_buffers")))
static ETH_DMADescTypeDef DMARxDscrTab[ETH_RXBUFNB];

__attribute__((section(".eth_buffers")))
static ETH_DMADescTypeDef DMATxDscrTab[ETH_TXBUFNB];

__attribute__((section(".eth_buffers")))
static uint8_t Rx_Buff[ETH_RXBUFNB][ETH_RX_BUF_SIZE];

__attribute__((section(".eth_buffers")))
static uint8_t Tx_Buff[ETH_TXBUFNB][ETH_RX_BUF_SIZE];

static uint32_t RxDescIdx = 0;
static uint32_t TxDescIdx = 0;

/* Network interface */
static struct netif *g_netif = NULL;

/*---------------------------------------------------------------------------*/
/* PHY Functions */
/*---------------------------------------------------------------------------*/
static uint32_t phy_read(uint32_t reg)
{
    uint32_t tmpreg;

    /* Set PHY address and register */
    tmpreg = ETH_MACMDIOAR;
    tmpreg &= ~0x03FF0000U;  /* Clear address bits */
    tmpreg |= ((PHY_ADDRESS << 21) | (reg << 16));
    tmpreg &= ~0x00000003U;  /* Clear CR bits */
    tmpreg |= (0x02 << 2);   /* CSR clock: HCLK/102 */
    tmpreg |= 0x0000000CU;   /* Read operation */
    ETH_MACMDIOAR = tmpreg;

    /* Wait for completion */
    while (ETH_MACMDIOAR & 0x00000001U);

    return ETH_MACMDIODR;
}

static void phy_write(uint32_t reg, uint32_t value)
{
    uint32_t tmpreg;

    ETH_MACMDIODR = value;

    tmpreg = ETH_MACMDIOAR;
    tmpreg &= ~0x03FF0000U;
    tmpreg |= ((PHY_ADDRESS << 21) | (reg << 16));
    tmpreg &= ~0x00000003U;
    tmpreg |= (0x02 << 2);
    tmpreg |= 0x00000004U;   /* Write operation */
    ETH_MACMDIOAR = tmpreg;

    while (ETH_MACMDIOAR & 0x00000001U);
}

/*---------------------------------------------------------------------------*/
/* Ethernet Low-Level Init */
/*---------------------------------------------------------------------------*/
static void low_level_init(struct netif *netif)
{
    uint32_t i;

    /* Set MAC address */
    netif->hwaddr_len = 6;
    netif->hwaddr[0] = 0x00;
    netif->hwaddr[1] = 0x80;
    netif->hwaddr[2] = 0xE1;
    netif->hwaddr[3] = 0x00;
    netif->hwaddr[4] = 0x00;
    netif->hwaddr[5] = 0x01;

    /* Set maximum transfer unit */
    netif->mtu = 1500;

    /* Device capabilities */
    netif->flags = NETIF_FLAG_BROADCAST | NETIF_FLAG_ETHARP | NETIF_FLAG_LINK_UP;

    /* Enable Ethernet clocks */
    RCC_AHB1ENR |= RCC_AHB1ENR_ETH1MACEN | RCC_AHB1ENR_ETH1TXEN | RCC_AHB1ENR_ETH1RXEN;

    /* Configure SYSCFG for RMII mode */
    SYSCFG_PMCR |= SYSCFG_PMCR_EPIS_RMII;

    /* Reset DMA */
    ETH_DMAMR |= 0x00000001U;  /* Software reset */
    while (ETH_DMAMR & 0x00000001U);

    /* PHY reset */
    phy_write(PHY_BCR, PHY_BCR_RESET);
    for (i = 0; i < 100000; i++) __asm__("nop");

    /* Enable auto-negotiation */
    phy_write(PHY_BCR, PHY_BCR_AUTONEG_EN | PHY_BCR_RESTART_AN);

    /* Initialize DMA descriptors */
    for (i = 0; i < ETH_RXBUFNB; i++) {
        DMARxDscrTab[i].DESC0 = (uint32_t)&Rx_Buff[i][0];
        DMARxDscrTab[i].DESC3 = ETH_DMARXDESC_OWN | ETH_DMARXDESC_IOC | ETH_DMARXDESC_BUF1V;
        DMARxDscrTab[i].BackupAddr0 = DMARxDscrTab[i].DESC0;
    }

    for (i = 0; i < ETH_TXBUFNB; i++) {
        DMATxDscrTab[i].DESC0 = (uint32_t)&Tx_Buff[i][0];
        DMATxDscrTab[i].DESC3 = 0;
    }

    /* Set descriptor addresses */
    ETH_DMAC0RXDLAR = (uint32_t)&DMARxDscrTab[0];
    ETH_DMAC0TXDLAR = (uint32_t)&DMATxDscrTab[0];
    ETH_DMAC0RXDRLR = ETH_RXBUFNB - 1;
    ETH_DMAC0TXDRLR = ETH_TXBUFNB - 1;

    /* Configure MAC */
    ETH_MACCR = 0x00000000U;    /* Default config */
    ETH_MACCR |= (1 << 14);     /* Full duplex */
    ETH_MACCR |= (1 << 13);     /* 100 Mbps */
    ETH_MACCR |= (1 << 0);      /* Receiver enable */
    ETH_MACCR |= (1 << 1);      /* Transmitter enable */

    /* Set MAC address in hardware */
    ETH_MACA0HR = ((uint32_t)netif->hwaddr[5] << 8) | netif->hwaddr[4];
    ETH_MACA0LR = ((uint32_t)netif->hwaddr[3] << 24) |
                  ((uint32_t)netif->hwaddr[2] << 16) |
                  ((uint32_t)netif->hwaddr[1] << 8) |
                  netif->hwaddr[0];

    /* Start DMA */
    ETH_DMAC0TXCR |= (1 << 0);  /* Start TX */
    ETH_DMAC0RXCR |= (1 << 0);  /* Start RX */

    /* Set tail pointer */
    ETH_DMAC0RXDTPR = (uint32_t)&DMARxDscrTab[ETH_RXBUFNB - 1];

    g_netif = netif;
}

/*---------------------------------------------------------------------------*/
/* Low-Level Output */
/*---------------------------------------------------------------------------*/
static err_t low_level_output(struct netif *netif, struct pbuf *p)
{
    struct pbuf *q;
    uint8_t *buffer;
    uint32_t framelength = 0;
    uint32_t i;
    ETH_DMADescTypeDef *DmaTxDesc;

    (void)netif;

    DmaTxDesc = &DMATxDscrTab[TxDescIdx];

    /* Wait for descriptor to be available */
    for (i = 0; i < 100000; i++) {
        if ((DmaTxDesc->DESC3 & ETH_DMATXDESC_OWN) == 0) break;
    }
    if (DmaTxDesc->DESC3 & ETH_DMATXDESC_OWN) {
        return ERR_USE;
    }

    buffer = (uint8_t *)DmaTxDesc->DESC0;

    /* Copy pbuf chain to TX buffer */
    for (q = p; q != NULL; q = q->next) {
        memcpy(&buffer[framelength], q->payload, q->len);
        framelength += q->len;
    }

    /* Set frame length and flags */
    DmaTxDesc->DESC2 = framelength;
    DmaTxDesc->DESC3 = ETH_DMATXDESC_OWN | ETH_DMATXDESC_FS | ETH_DMATXDESC_LS | framelength;

    /* Update tail pointer */
    TxDescIdx = (TxDescIdx + 1) % ETH_TXBUFNB;
    ETH_DMAC0TXDTPR = (uint32_t)&DMATxDscrTab[TxDescIdx];

    return ERR_OK;
}

/*---------------------------------------------------------------------------*/
/* Low-Level Input */
/*---------------------------------------------------------------------------*/
static struct pbuf *low_level_input(struct netif *netif)
{
    struct pbuf *p = NULL;
    uint32_t len;
    uint8_t *buffer;
    ETH_DMADescTypeDef *DmaRxDesc;

    (void)netif;

    DmaRxDesc = &DMARxDscrTab[RxDescIdx];

    /* Check if descriptor owned by DMA */
    if (DmaRxDesc->DESC3 & ETH_DMARXDESC_OWN) {
        return NULL;
    }

    /* Get frame length */
    len = (DmaRxDesc->DESC3 & 0x00007FFFU);
    buffer = (uint8_t *)DmaRxDesc->DESC0;

    if (len > 0) {
        /* Allocate pbuf */
        p = pbuf_alloc(PBUF_RAW, len, PBUF_POOL);

        if (p != NULL) {
            memcpy(p->payload, buffer, len);
        }
    }

    /* Give descriptor back to DMA */
    DmaRxDesc->DESC0 = DmaRxDesc->BackupAddr0;
    DmaRxDesc->DESC3 = ETH_DMARXDESC_OWN | ETH_DMARXDESC_IOC | ETH_DMARXDESC_BUF1V;

    /* Update index and tail pointer */
    RxDescIdx = (RxDescIdx + 1) % ETH_RXBUFNB;
    ETH_DMAC0RXDTPR = (uint32_t)&DMARxDscrTab[RxDescIdx];

    return p;
}

/*---------------------------------------------------------------------------*/
/* Network Interface Initialization */
/*---------------------------------------------------------------------------*/
err_t ethernetif_init(struct netif *netif)
{
    netif->name[0] = 'e';
    netif->name[1] = 'n';
    netif->output = etharp_output;
    netif->linkoutput = low_level_output;

    low_level_init(netif);

    return ERR_OK;
}

/*---------------------------------------------------------------------------*/
/* Poll for Received Frames (call from main loop) */
/*---------------------------------------------------------------------------*/
void ethernetif_input(struct netif *netif)
{
    struct pbuf *p;

    while (1) {
        p = low_level_input(netif);
        if (p == NULL) break;

        if (netif->input(p, netif) != ERR_OK) {
            pbuf_free(p);
        }
    }
}

/*---------------------------------------------------------------------------*/
/* Check Link Status */
/*---------------------------------------------------------------------------*/
int ethernetif_link_status(void)
{
    uint32_t bsr = phy_read(PHY_BSR);
    return (bsr & PHY_BSR_LINK_UP) ? 1 : 0;
}
