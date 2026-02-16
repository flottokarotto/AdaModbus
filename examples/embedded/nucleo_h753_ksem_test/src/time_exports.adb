--  Time_Exports - Export tick function to C
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Provides the sys_now() time source for LwIP from Ada's SysTick.

with Interfaces; use Interfaces;
with STM32H7_HAL;

package body Time_Exports is

   function Ada_Get_Tick_Ms return Unsigned_32
     with Export, Convention => C, External_Name => "ada_get_tick_ms";

   function Ada_Get_Tick_Ms return Unsigned_32 is
   begin
      return STM32H7_HAL.Get_Tick;
   end Ada_Get_Tick_Ms;

end Time_Exports;
