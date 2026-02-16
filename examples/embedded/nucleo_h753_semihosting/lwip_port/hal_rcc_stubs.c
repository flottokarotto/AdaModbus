/**
 * hal_rcc_stubs.c - Minimal RCC stubs for ETH driver
 *
 * The full stm32h7xx_hal_rcc.c is very large and pulls in PWR dependencies.
 * We only need HAL_RCC_GetHCLKFreq() for MDIO clock divider calculation.
 *
 * Running at HSI default (64 MHz, no PLL configured).
 */
#include "stm32h7xx_hal.h"

uint32_t HAL_RCC_GetHCLKFreq(void)
{
    return HSI_VALUE;  /* 64 MHz */
}

/**
 * SYSCFG ETH interface select - called by newer HAL ETH drivers.
 * RMII mode is selected via SYSCFG_PMCR in HAL_ETH_MspInit.
 * This stub satisfies the linker; actual config is done in MspInit.
 */
void HAL_SYSCFG_ETHInterfaceSelect(uint32_t ETH_Interface)
{
    (void)ETH_Interface;
}
