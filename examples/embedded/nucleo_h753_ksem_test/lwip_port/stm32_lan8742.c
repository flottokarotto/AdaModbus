/**
 * stm32_lan8742.c - Compile LAN8742 PHY driver from submodule
 *
 * Provides: LAN8742_Init, LAN8742_GetLinkState, LAN8742_RegisterBusIO, etc.
 * Source: STM32CubeH7/Drivers/BSP/Components/lan8742/lan8742.c
 * License: BSD-3-Clause (STMicroelectronics)
 *
 * The NUCLEO-H753ZI uses a LAN8742A Ethernet PHY (on-board, directly
 * connected via RMII).  This driver communicates with the PHY over
 * the MDIO/MDC bus to negotiate link speed/duplex and read link status.
 * Called by ethernetif.c during init and in ethernet_link_check_state().
 * See stm32_hal.c header for the #include-wrapper approach.
 */
#include "../STM32CubeH7/Drivers/BSP/Components/lan8742/lan8742.c"
