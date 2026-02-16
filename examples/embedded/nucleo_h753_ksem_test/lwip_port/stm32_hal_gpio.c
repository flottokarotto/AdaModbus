/**
 * stm32_hal_gpio.c - Compile STM32H7 HAL GPIO driver from submodule
 *
 * Provides: HAL_GPIO_Init, HAL_GPIO_WritePin, etc.
 * Source: STM32CubeH7/Drivers/STM32H7xx_HAL_Driver/Src/stm32h7xx_hal_gpio.c
 * License: BSD-3-Clause (STMicroelectronics)
 *
 * Required by HAL_ETH_MspInit() in ethernetif.c to configure the
 * RMII GPIO pins (PA1, PA2, PA7, PB13, PC1, PC4, PC5).
 * See stm32_hal.c header for the #include-wrapper approach.
 */
#include "../STM32CubeH7/Drivers/STM32H7xx_HAL_Driver/Src/stm32h7xx_hal_gpio.c"
