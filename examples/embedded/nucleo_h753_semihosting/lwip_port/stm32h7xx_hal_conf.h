/**
 * stm32h7xx_hal_conf.h - HAL configuration for NUCLEO-H753ZI Ethernet
 *
 * Based on STM32CubeH7 LwIP examples.
 * Enables only the modules needed for Ethernet + LwIP.
 */
#ifndef STM32H7XX_HAL_CONF_H
#define STM32H7XX_HAL_CONF_H

#ifdef __cplusplus
extern "C" {
#endif

/* ########################## Module Selection ############################## */
#define HAL_MODULE_ENABLED
#define HAL_CORTEX_MODULE_ENABLED
#define HAL_ETH_MODULE_ENABLED
#define HAL_GPIO_MODULE_ENABLED
#define HAL_RCC_MODULE_ENABLED

/* ########################## Oscillator Values ############################ */
#if !defined(HSE_VALUE)
#define HSE_VALUE    25000000UL
#endif
#if !defined(HSE_STARTUP_TIMEOUT)
#define HSE_STARTUP_TIMEOUT    100U
#endif
#if !defined(CSI_VALUE)
#define CSI_VALUE    4000000UL
#endif
#if !defined(HSI_VALUE)
#define HSI_VALUE    64000000UL
#endif
#if !defined(LSE_VALUE)
#define LSE_VALUE    32768UL
#endif
#if !defined(LSE_STARTUP_TIMEOUT)
#define LSE_STARTUP_TIMEOUT    5000U
#endif
#if !defined(LSI_VALUE)
#define LSI_VALUE    32000UL
#endif
#if !defined(EXTERNAL_CLOCK_VALUE)
#define EXTERNAL_CLOCK_VALUE    12288000UL
#endif

/* ########################### System Configuration ######################## */
#define VDD_VALUE                    3300UL
#define TICK_INT_PRIORITY            0x0FU
#define USE_RTOS                     0

/* ########################### Ethernet Configuration ###################### */
#define ETH_TX_DESC_CNT         4U
#define ETH_RX_DESC_CNT         4U

#define ETH_MAC_ADDR0    ((uint8_t)0x00)
#define ETH_MAC_ADDR1    ((uint8_t)0x80)
#define ETH_MAC_ADDR2    ((uint8_t)0xE1)
#define ETH_MAC_ADDR3    ((uint8_t)0x00)
#define ETH_MAC_ADDR4    ((uint8_t)0x00)
#define ETH_MAC_ADDR5    ((uint8_t)0x01)

/* ########################## Assert Selection ############################## */
#define assert_param(expr) ((void)0U)

/* ################## Includes ############################################# */
#ifdef HAL_RCC_MODULE_ENABLED
#include "stm32h7xx_hal_rcc.h"
#endif

#ifdef HAL_GPIO_MODULE_ENABLED
#include "stm32h7xx_hal_gpio.h"
#endif

#ifdef HAL_ETH_MODULE_ENABLED
#include "stm32h7xx_hal_eth.h"
#endif

#ifdef HAL_CORTEX_MODULE_ENABLED
#include "stm32h7xx_hal_cortex.h"
#endif

#ifdef __cplusplus
}
#endif

#endif /* STM32H7XX_HAL_CONF_H */
