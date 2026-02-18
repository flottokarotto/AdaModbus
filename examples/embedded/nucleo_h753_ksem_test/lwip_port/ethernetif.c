/**
 * ethernetif.c - Ethernet network interface driver for lwIP
 * Copyright (c) 2017 STMicroelectronics. All rights reserved.
 * License: BSD-3-Clause
 *
 * Origin:
 *   Copied from STM32CubeH7/Projects/STM32H743I-EVAL/Applications/LwIP/
 *   LwIP_TCP_Echo_Server/Src/ethernetif.c (the only H7 LwIP example with
 *   zero-copy RX using LWIP_SUPPORT_CUSTOM_PBUF).  The H743I-EVAL was
 *   chosen because it uses the same LAN8742A PHY as NUCLEO-H753ZI.
 *   Ada_Drivers_Library was evaluated but has no STM32H7 Ethernet support.
 *
 * Adaptations for NUCLEO-H753ZI:
 *   - HAL_ETH_MspInit GPIO: PB13 for ETH_TXD1 (EVAL uses PG12)
 *   - LAN8742 #include path adjusted for project layout
 *   - Added Ada-callable helpers: ethernetif_set_netif(),
 *     ethernetif_link_status(), frame counter getters
 *   - uart_put/uart_put_int debug output during ETH/PHY init
 *   - Software checksums (ETH_CHECKSUM_DISABLE in TxConfig) since we
 *     run at HSI 64 MHz without full HAL_Init clock setup
 *
 * All other code (DMA descriptors, zero-copy RX pool, HAL_ETH callbacks,
 * link negotiation) is unchanged from the ST reference.
 */

#include "stm32h7xx_hal.h"
#include "lwip/opt.h"
#include "lwip/timeouts.h"
#include "lwip/netif.h"
#include "netif/etharp.h"
#include "ethernetif.h"
#include "lan8742.h"
#include <string.h>

/* Debug output via Ada UART console */
extern void uart_put_line(const char *msg);
extern void uart_put(const char *msg);
extern void uart_put_int(int val);
extern void uart_put_hex(uint8_t val);

static void dbg(const char *msg) { uart_put_line(msg); }
static void dbg_val(const char *label, int val) {
    uart_put(label);
    uart_put_int(val);
    uart_put_line("");
}

/* Frame counters for debugging */
static volatile uint32_t tx_count = 0;
static volatile uint32_t rx_count = 0;
uint32_t ethernetif_get_tx_count(void) { return tx_count; }
uint32_t ethernetif_get_rx_count(void) { return rx_count; }

/* Private define ------------------------------------------------------------*/
#define IFNAME0 's'
#define IFNAME1 't'

#define ETH_DMA_TRANSMIT_TIMEOUT                (20U)

#define ETH_RX_BUFFER_SIZE            1000U
#define ETH_RX_BUFFER_CNT             12U
#define ETH_TX_BUFFER_MAX             ((ETH_TX_DESC_CNT) * 2U)

/* Private typedef -----------------------------------------------------------*/
typedef enum
{
  RX_ALLOC_OK       = 0x00,
  RX_ALLOC_ERROR    = 0x01
} RxAllocStatusTypeDef;

typedef struct
{
  struct pbuf_custom pbuf_custom;
  uint8_t buff[(ETH_RX_BUFFER_SIZE + 31) & ~31] __ALIGNED(32);
} RxBuff_t;

/* Private variables ---------------------------------------------------------*/
/* DMA descriptors in SRAM2 (DMA-accessible, 0x30000000) */
ETH_DMADescTypeDef DMARxDscrTab[ETH_RX_DESC_CNT] __attribute__((section(".RxDescripSection")));
ETH_DMADescTypeDef DMATxDscrTab[ETH_TX_DESC_CNT] __attribute__((section(".TxDescripSection")));

/* Memory Pool Declaration */
LWIP_MEMPOOL_DECLARE(RX_POOL, ETH_RX_BUFFER_CNT, sizeof(RxBuff_t), "Zero-copy RX PBUF pool");

__attribute__((section(".Rx_PoolSection"))) extern u8_t memp_memory_RX_POOL_base[];

/* Variable Definitions */
static volatile RxAllocStatusTypeDef RxAllocStatus;

/* Global Ethernet handle */
ETH_HandleTypeDef EthHandle;
ETH_TxPacketConfig TxConfig;

/* Private function prototypes -----------------------------------------------*/
u32_t sys_now(void);

int32_t ETH_PHY_IO_Init(void);
int32_t ETH_PHY_IO_DeInit(void);
int32_t ETH_PHY_IO_ReadReg(uint32_t DevAddr, uint32_t RegAddr, uint32_t *pRegVal);
int32_t ETH_PHY_IO_WriteReg(uint32_t DevAddr, uint32_t RegAddr, uint32_t RegVal);
int32_t ETH_PHY_IO_GetTick(void);

lan8742_Object_t LAN8742;
lan8742_IOCtx_t  LAN8742_IOCtx = {ETH_PHY_IO_Init,
                                   ETH_PHY_IO_DeInit,
                                   ETH_PHY_IO_WriteReg,
                                   ETH_PHY_IO_ReadReg,
                                   ETH_PHY_IO_GetTick};

/* Private functions ---------------------------------------------------------*/
void pbuf_free_custom(struct pbuf *p);

/*******************************************************************************
                       LL Driver Interface ( LwIP stack --> ETH)
*******************************************************************************/
static void low_level_init(struct netif *netif)
{
  uint8_t macaddress[6] = {ETH_MAC_ADDR0, ETH_MAC_ADDR1, ETH_MAC_ADDR2,
                           ETH_MAC_ADDR3, ETH_MAC_ADDR4, ETH_MAC_ADDR5};

  EthHandle.Instance = ETH;
  EthHandle.Init.MACAddr = macaddress;
  EthHandle.Init.MediaInterface = HAL_ETH_RMII_MODE;
  EthHandle.Init.RxDesc = DMARxDscrTab;
  EthHandle.Init.TxDesc = DMATxDscrTab;
  EthHandle.Init.RxBuffLen = ETH_RX_BUFFER_SIZE;

  /* Configure ethernet peripheral (GPIOs, clocks, MAC, DMA) */
  dbg("[ETH] HAL_ETH_Init...");
  {
    HAL_StatusTypeDef rc = HAL_ETH_Init(&EthHandle);
    dbg_val("[ETH] HAL_ETH_Init rc=", (int)rc);
  }

  /* Set MAC hardware address length */
  netif->hwaddr_len = ETH_HWADDR_LEN;

  /* Set MAC hardware address */
  netif->hwaddr[0] = ETH_MAC_ADDR0;
  netif->hwaddr[1] = ETH_MAC_ADDR1;
  netif->hwaddr[2] = ETH_MAC_ADDR2;
  netif->hwaddr[3] = ETH_MAC_ADDR3;
  netif->hwaddr[4] = ETH_MAC_ADDR4;
  netif->hwaddr[5] = ETH_MAC_ADDR5;

  /* Maximum transfer unit */
  netif->mtu = ETH_MAX_PAYLOAD;

  /* Device capabilities */
  netif->flags |= NETIF_FLAG_BROADCAST | NETIF_FLAG_ETHARP;

  /* Initialize the RX POOL */
  LWIP_MEMPOOL_INIT(RX_POOL);

  /* Set Tx packet config common parameters */
  memset(&TxConfig, 0, sizeof(ETH_TxPacketConfig));
  TxConfig.Attributes = ETH_TX_PACKETS_FEATURES_CRCPAD;
  TxConfig.ChecksumCtrl = ETH_CHECKSUM_DISABLE;
  TxConfig.CRCPadCtrl = ETH_CRC_PAD_INSERT;

  /* Set PHY IO functions */
  LAN8742_RegisterBusIO(&LAN8742, &LAN8742_IOCtx);

  /* Initialize the LAN8742 ETH PHY */
  dbg("[ETH] LAN8742_Init...");
  {
    int32_t phy_rc = LAN8742_Init(&LAN8742);
    dbg_val("[ETH] LAN8742_Init rc=", (int)phy_rc);
    if (phy_rc != LAN8742_STATUS_OK)
    {
      dbg("[ETH] PHY init FAILED!");
      netif_set_link_down(netif);
      netif_set_down(netif);
      return;
    }
  }

  dbg("[ETH] ethernet_link_check_state...");
  ethernet_link_check_state(netif);
  dbg_val("[ETH] link_up=", netif_is_link_up(netif) ? 1 : 0);
}

static err_t low_level_output(struct netif *netif, struct pbuf *p)
{
  uint32_t i = 0U;
  struct pbuf *q = NULL;
  err_t errval = ERR_OK;
  ETH_BufferTypeDef Txbuffer[ETH_TX_DESC_CNT] = {0};

  memset(Txbuffer, 0, ETH_TX_DESC_CNT * sizeof(ETH_BufferTypeDef));

  for (q = p; q != NULL; q = q->next)
  {
    if (i >= ETH_TX_DESC_CNT)
      return ERR_IF;

    Txbuffer[i].buffer = q->payload;
    Txbuffer[i].len = q->len;

    if (i > 0)
    {
      Txbuffer[i - 1].next = &Txbuffer[i];
    }

    if (q->next == NULL)
    {
      Txbuffer[i].next = NULL;
    }

    i++;
  }

  TxConfig.Length = p->tot_len;
  TxConfig.TxBuffer = Txbuffer;
  TxConfig.pData = p;

  {
    HAL_StatusTypeDef rc = HAL_ETH_Transmit(&EthHandle, &TxConfig, ETH_DMA_TRANSMIT_TIMEOUT);
    if (rc == HAL_OK) tx_count++;
  }

  return errval;
}

static struct pbuf *low_level_input(struct netif *netif)
{
  struct pbuf *p = NULL;

  if (RxAllocStatus == RX_ALLOC_OK)
  {
    HAL_ETH_ReadData(&EthHandle, (void **)&p);
  }
  return p;
}

void ethernetif_input(struct netif *netif)
{
  struct pbuf *p = NULL;

  do
  {
    p = low_level_input(netif);
    if (p != NULL)
    {
      rx_count++;
      if (netif->input(p, netif) != ERR_OK)
      {
        pbuf_free(p);
      }
    }
  } while (p != NULL);
}

err_t ethernetif_init(struct netif *netif)
{
  LWIP_ASSERT("netif != NULL", (netif != NULL));

#if LWIP_NETIF_HOSTNAME
  netif->hostname = "lwip";
#endif

  netif->name[0] = IFNAME0;
  netif->name[1] = IFNAME1;
  netif->output = etharp_output;
  netif->linkoutput = low_level_output;

  low_level_init(netif);

  return ERR_OK;
}

void pbuf_free_custom(struct pbuf *p)
{
  struct pbuf_custom *custom_pbuf = (struct pbuf_custom *)p;
  LWIP_MEMPOOL_FREE(RX_POOL, custom_pbuf);

  if (RxAllocStatus == RX_ALLOC_ERROR)
  {
    RxAllocStatus = RX_ALLOC_OK;
  }
}

u32_t sys_now(void)
{
  return HAL_GetTick();
}

/*******************************************************************************
                       Ethernet MSP Routines
*******************************************************************************/
/**
 * Initializes the ETH MSP.
 *
 * NUCLEO-H753ZI RMII pin assignment:
 *   PA1  = ETH_REF_CLK  (AF11)
 *   PA2  = ETH_MDIO     (AF11)
 *   PA7  = ETH_CRS_DV   (AF11)
 *   PB13 = ETH_TXD1     (AF11)  <-- NUCLEO specific (EVAL uses PG12)
 *   PC1  = ETH_MDC       (AF11)
 *   PC4  = ETH_RXD0      (AF11)
 *   PC5  = ETH_RXD1      (AF11)
 *   PG11 = ETH_TX_EN     (AF11)
 *   PG13 = ETH_TXD0      (AF11)
 */
void HAL_ETH_MspInit(ETH_HandleTypeDef *heth)
{
  GPIO_InitTypeDef GPIO_InitStructure = {0};

  /* Enable SYSCFG clock (needed for RMII interface selection).
   * Normally done by HAL_Init(), but we use custom startup. */
  __HAL_RCC_SYSCFG_CLK_ENABLE();

  /* Enable GPIOs clocks */
  __HAL_RCC_GPIOA_CLK_ENABLE();
  __HAL_RCC_GPIOB_CLK_ENABLE();
  __HAL_RCC_GPIOC_CLK_ENABLE();
  __HAL_RCC_GPIOG_CLK_ENABLE();

  /* Configure PA1, PA2, PA7 */
  GPIO_InitStructure.Pin = GPIO_PIN_1 | GPIO_PIN_2 | GPIO_PIN_7;
  GPIO_InitStructure.Speed = GPIO_SPEED_FREQ_HIGH;
  GPIO_InitStructure.Mode = GPIO_MODE_AF_PP;
  GPIO_InitStructure.Pull = GPIO_NOPULL;
  GPIO_InitStructure.Alternate = GPIO_AF11_ETH;
  HAL_GPIO_Init(GPIOA, &GPIO_InitStructure);

  /* Configure PB13 (TXD1 - NUCLEO specific) */
  GPIO_InitStructure.Pin = GPIO_PIN_13;
  HAL_GPIO_Init(GPIOB, &GPIO_InitStructure);

  /* Configure PC1, PC4, PC5 */
  GPIO_InitStructure.Pin = GPIO_PIN_1 | GPIO_PIN_4 | GPIO_PIN_5;
  HAL_GPIO_Init(GPIOC, &GPIO_InitStructure);

  /* Configure PG11, PG13 */
  GPIO_InitStructure.Pin = GPIO_PIN_11 | GPIO_PIN_13;
  HAL_GPIO_Init(GPIOG, &GPIO_InitStructure);

  /* Enable Ethernet clocks */
  __HAL_RCC_ETH1MAC_CLK_ENABLE();
  __HAL_RCC_ETH1TX_CLK_ENABLE();
  __HAL_RCC_ETH1RX_CLK_ENABLE();
}

/*******************************************************************************
                       PHY IO Functions
*******************************************************************************/
int32_t ETH_PHY_IO_Init(void)
{
  HAL_ETH_SetMDIOClockRange(&EthHandle);
  return 0;
}

int32_t ETH_PHY_IO_DeInit(void)
{
  return 0;
}

int32_t ETH_PHY_IO_ReadReg(uint32_t DevAddr, uint32_t RegAddr, uint32_t *pRegVal)
{
  if (HAL_ETH_ReadPHYRegister(&EthHandle, DevAddr, RegAddr, pRegVal) != HAL_OK)
  {
    return -1;
  }
  return 0;
}

int32_t ETH_PHY_IO_WriteReg(uint32_t DevAddr, uint32_t RegAddr, uint32_t RegVal)
{
  if (HAL_ETH_WritePHYRegister(&EthHandle, DevAddr, RegAddr, RegVal) != HAL_OK)
  {
    return -1;
  }
  return 0;
}

int32_t ETH_PHY_IO_GetTick(void)
{
  return HAL_GetTick();
}

/*******************************************************************************
                       Link Check
*******************************************************************************/
void ethernet_link_check_state(struct netif *netif)
{
  ETH_MACConfigTypeDef MACConf = {0};
  int32_t PHYLinkState = 0;
  uint32_t linkchanged = 0U, speed = 0U, duplex = 0U;

  PHYLinkState = LAN8742_GetLinkState(&LAN8742);

  if (netif_is_link_up(netif) && (PHYLinkState <= LAN8742_STATUS_LINK_DOWN))
  {
    HAL_ETH_Stop(&EthHandle);
    netif_set_down(netif);
    netif_set_link_down(netif);
  }
  else if (!netif_is_link_up(netif) && (PHYLinkState > LAN8742_STATUS_LINK_DOWN))
  {
    switch (PHYLinkState)
    {
    case LAN8742_STATUS_100MBITS_FULLDUPLEX:
      duplex = ETH_FULLDUPLEX_MODE;
      speed = ETH_SPEED_100M;
      linkchanged = 1;
      break;
    case LAN8742_STATUS_100MBITS_HALFDUPLEX:
      duplex = ETH_HALFDUPLEX_MODE;
      speed = ETH_SPEED_100M;
      linkchanged = 1;
      break;
    case LAN8742_STATUS_10MBITS_FULLDUPLEX:
      duplex = ETH_FULLDUPLEX_MODE;
      speed = ETH_SPEED_10M;
      linkchanged = 1;
      break;
    case LAN8742_STATUS_10MBITS_HALFDUPLEX:
      duplex = ETH_HALFDUPLEX_MODE;
      speed = ETH_SPEED_10M;
      linkchanged = 1;
      break;
    default:
      break;
    }

    if (linkchanged)
    {
      dbg_val("[ETH] PHY link state=", PHYLinkState);
      dbg_val("[ETH] speed=", (int)speed);
      dbg_val("[ETH] duplex=", (int)duplex);
      HAL_ETH_GetMACConfig(&EthHandle, &MACConf);
      MACConf.DuplexMode = duplex;
      MACConf.Speed = speed;
      HAL_ETH_SetMACConfig(&EthHandle, &MACConf);
      {
        HAL_StatusTypeDef rc = HAL_ETH_Start(&EthHandle);
        dbg_val("[ETH] HAL_ETH_Start rc=", (int)rc);
      }
      netif_set_up(netif);
      netif_set_link_up(netif);
      dbg("[ETH] Link UP, MAC started");
    }
  }
}

void HAL_ETH_RxAllocateCallback(uint8_t **buff)
{
  struct pbuf_custom *p = LWIP_MEMPOOL_ALLOC(RX_POOL);
  if (p)
  {
    *buff = (uint8_t *)p + offsetof(RxBuff_t, buff);
    p->custom_free_function = pbuf_free_custom;
    pbuf_alloced_custom(PBUF_RAW, 0, PBUF_REF, p, *buff, ETH_RX_BUFFER_SIZE);
  }
  else
  {
    RxAllocStatus = RX_ALLOC_ERROR;
    *buff = NULL;
  }
}

void HAL_ETH_RxLinkCallback(void **pStart, void **pEnd, uint8_t *buff, uint16_t Length)
{
  struct pbuf **ppStart = (struct pbuf **)pStart;
  struct pbuf **ppEnd = (struct pbuf **)pEnd;
  struct pbuf *p = NULL;

  p = (struct pbuf *)(buff - offsetof(RxBuff_t, buff));
  p->next = NULL;
  p->tot_len = 0;
  p->len = Length;

  if (!*ppStart)
  {
    *ppStart = p;
  }
  else
  {
    (*ppEnd)->next = p;
  }
  *ppEnd = p;

  for (p = *ppStart; p != NULL; p = p->next)
  {
    p->tot_len += Length;
  }

  SCB_InvalidateDCache_by_Addr((uint32_t *)buff, Length);
}

void HAL_ETH_TxFreeCallback(uint32_t *buff)
{
  pbuf_free((struct pbuf *)buff);
}

/**
 * Ada-compatible link status check (called from Ada code).
 * Returns 1 if link is up, 0 if down.
 */
static struct netif *g_netif_ptr = NULL;

/* Hook: remember netif pointer after init */
void ethernetif_set_netif(struct netif *netif)
{
  g_netif_ptr = netif;
}

int ethernetif_link_status(void)
{
  if (g_netif_ptr != NULL)
  {
    ethernet_link_check_state(g_netif_ptr);
    return netif_is_link_up(g_netif_ptr) ? 1 : 0;
  }
  return 0;
}
