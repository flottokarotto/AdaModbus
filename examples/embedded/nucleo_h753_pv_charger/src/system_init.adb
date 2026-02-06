--  System_Init - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with STM32H7_HAL;

package body System_Init is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      --  Initialize system clock (HSI 64MHz)
      STM32H7_HAL.System_Init;

      --  Initialize SysTick for 1ms interrupts
      STM32H7_HAL.SysTick_Init;

      --  Initialize GPIO (LEDs, Button, RS485 DE)
      STM32H7_HAL.GPIO_Init;
   end Initialize;

begin
   --  Auto-initialize during elaboration
   Initialize;
end System_Init;
