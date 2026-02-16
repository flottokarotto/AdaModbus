/**
 * stm32_hal_eth_ex.c - Compile STM32H7 HAL ETH Extended driver from submodule
 *
 * Provides: HAL_ETHEx_GetMACFilterConfig, HAL_ETHEx_SetMACFilterConfig,
 *           HAL_ETHEx_SetDMAArbitration, etc.
 * Source: STM32CubeH7/Drivers/STM32H7xx_HAL_Driver/Src/stm32h7xx_hal_eth_ex.c
 * License: BSD-3-Clause (STMicroelectronics)
 *
 * Extended ETH functions (MAC filtering, PTP timestamping, DMA config).
 * Required as a companion to stm32_hal_eth.c -- the base ETH driver
 * references symbols from this module.
 * See stm32_hal.c header for the #include-wrapper approach.
 */
#include "../STM32CubeH7/Drivers/STM32H7xx_HAL_Driver/Src/stm32h7xx_hal_eth_ex.c"
