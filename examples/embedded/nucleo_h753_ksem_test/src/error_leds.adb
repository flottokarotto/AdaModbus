--  Error_LEDs - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with STM32H7_HAL;

package body Error_LEDs is

   procedure On_Exception is
   begin
      STM32H7_HAL.GPIO_Write (STM32H7_HAL.Port_B, 14, True);   --  Red on
      STM32H7_HAL.GPIO_Write (STM32H7_HAL.Port_B, 0, False);   --  Green off
   end On_Exception;

end Error_LEDs;
