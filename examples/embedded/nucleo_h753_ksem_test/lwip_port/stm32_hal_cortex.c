/**
 * stm32_hal_cortex.c - Compile STM32H7 HAL Cortex driver from submodule
 *
 * Provides: HAL_NVIC_SetPriority, HAL_SYSTICK_Config, SCB_InvalidateDCache_by_Addr
 * Source: STM32CubeH7/Drivers/STM32H7xx_HAL_Driver/Src/stm32h7xx_hal_cortex.c
 * License: BSD-3-Clause (STMicroelectronics)
 *
 * Needed by HAL_ETH_Init (configures ETH interrupt priority) and by
 * the zero-copy RX path (SCB_InvalidateDCache_by_Addr in ethernetif.c).
 * See stm32_hal.c header for the #include-wrapper approach.
 */
#include "../STM32CubeH7/Drivers/STM32H7xx_HAL_Driver/Src/stm32h7xx_hal_cortex.c"
