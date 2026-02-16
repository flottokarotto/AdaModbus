/**
 * hal_rcc_stubs.c - Minimal RCC stubs for ETH driver
 *
 * Provides: HAL_RCC_GetHCLKFreq()
 * Replaces: STM32CubeH7/Drivers/STM32H7xx_HAL_Driver/Src/stm32h7xx_hal_rcc.c
 * Reason: The full RCC driver is >2000 lines and pulls in PWR, PLL, and
 *         clock-tree dependencies that we don't need.  The only function
 *         required is HAL_RCC_GetHCLKFreq(), called by HAL_ETH_Init() to
 *         compute the MDIO clock divider (ETH_MACMDIOAR.CR field).
 *
 * This board runs at HSI default (64 MHz, no PLL configured), so we
 * simply return HSI_VALUE.  If the clock tree is reconfigured later,
 * this stub must be updated accordingly.
 */
#include "stm32h7xx_hal.h"

uint32_t HAL_RCC_GetHCLKFreq(void)
{
    return HSI_VALUE;  /* 64 MHz */
}

/* HAL_SYSCFG_ETHInterfaceSelect is already provided by stm32h7xx_hal.c */
