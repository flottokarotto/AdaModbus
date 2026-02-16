/**
 * stm32_hal.c - Compile STM32H7 HAL base driver from submodule
 *
 * Provides: HAL_GetTick, HAL_IncTick, HAL_Delay, HAL_InitTick,
 *           HAL_SYSCFG_ETHInterfaceSelect (selects RMII for Ethernet)
 * Source: STM32CubeH7/Drivers/STM32H7xx_HAL_Driver/Src/stm32h7xx_hal.c
 * License: BSD-3-Clause (STMicroelectronics)
 *
 * Approach: Instead of adding the entire HAL source tree to the GPR
 * project, each needed HAL module is compiled via a thin #include
 * wrapper.  This keeps the GPR simple and makes dependencies explicit.
 * The STM32CubeH7 submodule is at ../STM32CubeH7 (git submodule).
 *
 * HAL_IncTick is called from SysTick_Handler in startup.S alongside
 * the Ada SysTick handler, providing the tick base for HAL_Delay()
 * and HAL_ETH timeout calculations.
 */
#include "../STM32CubeH7/Drivers/STM32H7xx_HAL_Driver/Src/stm32h7xx_hal.c"
