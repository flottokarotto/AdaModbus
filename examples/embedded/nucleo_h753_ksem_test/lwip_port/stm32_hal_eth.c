/**
 * stm32_hal_eth.c - Compile STM32H7 HAL ETH driver from submodule
 *
 * Provides: HAL_ETH_Init, HAL_ETH_Start, HAL_ETH_Transmit_IT,
 *           HAL_ETH_ReadData, HAL_ETH_BuildRxDescriptors, etc.
 * Source: STM32CubeH7/Drivers/STM32H7xx_HAL_Driver/Src/stm32h7xx_hal_eth.c
 * License: BSD-3-Clause (STMicroelectronics)
 *
 * Core of the Ethernet stack -- manages DMA descriptors, frame TX/RX,
 * and the ETH peripheral registers.  Used by ethernetif.c for all
 * low-level Ethernet operations.
 * See stm32_hal.c header for the #include-wrapper approach.
 */
#include "../STM32CubeH7/Drivers/STM32H7xx_HAL_Driver/Src/stm32h7xx_hal_eth.c"
