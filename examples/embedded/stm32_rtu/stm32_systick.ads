--  STM32_SysTick - SysTick Timer Driver
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Provides millisecond tick counter using ARM Cortex-M SysTick timer.

with Interfaces; use Interfaces;

package STM32_SysTick is

   --  Initialize SysTick for 1ms ticks
   --  Clock_Hz should be the system clock frequency (e.g., 16_000_000)
   procedure Initialize (Clock_Hz : Unsigned_32 := 16_000_000);

   --  Get current tick count in milliseconds
   function Get_Tick_Ms return Unsigned_32;

   --  Delay for specified milliseconds
   procedure Delay_Ms (Ms : Natural);

   --  Check if specified duration has elapsed since start time
   function Has_Elapsed
     (Start_Ms   : Unsigned_32;
      Duration_Ms : Unsigned_32) return Boolean;

end STM32_SysTick;
