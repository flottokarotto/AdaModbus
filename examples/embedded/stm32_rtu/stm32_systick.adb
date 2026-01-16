--  STM32_SysTick - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with System;

package body STM32_SysTick is

   --  Cortex-M SysTick registers (same for all Cortex-M)
   type SysTick_Registers is record
      CTRL  : Unsigned_32;  --  Control and status
      LOAD  : Unsigned_32;  --  Reload value
      VAL   : Unsigned_32;  --  Current value
      CALIB : Unsigned_32;  --  Calibration
   end record
     with Volatile;

   for SysTick_Registers use record
      CTRL  at 16#00# range 0 .. 31;
      LOAD  at 16#04# range 0 .. 31;
      VAL   at 16#08# range 0 .. 31;
      CALIB at 16#0C# range 0 .. 31;
   end record;

   SysTick_Base : constant System.Address := System'To_Address (16#E000_E010#);

   SysTick : SysTick_Registers
     with Import, Volatile, Address => SysTick_Base;

   --  CTRL register bits
   CTRL_ENABLE    : constant := 16#0001#;  --  Enable counter
   CTRL_CLKSOURCE : constant := 16#0004#;  --  Use processor clock
   CTRL_COUNTFLAG : constant := 16#0001_0000#;  --  Count reached zero

   --  Global tick counter (incremented by polling or interrupt)
   Tick_Counter : Unsigned_32 := 0 with Volatile;

   --  Store reload value for polling
   Reload_Value : Unsigned_32 := 0;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Clock_Hz : Unsigned_32 := 16_000_000) is
   begin
      --  Calculate reload value for 1ms tick
      --  Reload = (Clock_Hz / 1000) - 1
      Reload_Value := (Clock_Hz / 1000) - 1;

      --  Disable SysTick first
      SysTick.CTRL := 0;

      --  Set reload value
      SysTick.LOAD := Reload_Value;

      --  Clear current value
      SysTick.VAL := 0;

      --  Enable SysTick with processor clock, no interrupt
      --  (we'll poll COUNTFLAG for simplicity)
      SysTick.CTRL := CTRL_ENABLE or CTRL_CLKSOURCE;

      Tick_Counter := 0;
   end Initialize;

   -----------------
   -- Get_Tick_Ms --
   -----------------

   function Get_Tick_Ms return Unsigned_32 is
   begin
      --  Check if counter wrapped (COUNTFLAG is set on wrap)
      --  This needs to be called frequently enough to not miss ticks
      if (SysTick.CTRL and CTRL_COUNTFLAG) /= 0 then
         Tick_Counter := Tick_Counter + 1;
      end if;

      return Tick_Counter;
   end Get_Tick_Ms;

   --------------
   -- Delay_Ms --
   --------------

   procedure Delay_Ms (Ms : Natural) is
      Start : constant Unsigned_32 := Get_Tick_Ms;
      Target : constant Unsigned_32 := Unsigned_32 (Ms);
   begin
      while not Has_Elapsed (Start, Target) loop
         --  Keep polling to update tick counter
         declare
            Dummy : constant Unsigned_32 := Get_Tick_Ms;
            pragma Unreferenced (Dummy);
         begin
            null;
         end;
      end loop;
   end Delay_Ms;

   -----------------
   -- Has_Elapsed --
   -----------------

   function Has_Elapsed
     (Start_Ms    : Unsigned_32;
      Duration_Ms : Unsigned_32) return Boolean
   is
      Current : constant Unsigned_32 := Get_Tick_Ms;
      Elapsed : Unsigned_32;
   begin
      --  Handle wraparound
      if Current >= Start_Ms then
         Elapsed := Current - Start_Ms;
      else
         Elapsed := (Unsigned_32'Last - Start_Ms) + Current + 1;
      end if;

      return Elapsed >= Duration_Ms;
   end Has_Elapsed;

end STM32_SysTick;
