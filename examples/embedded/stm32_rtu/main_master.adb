--  Main_Master - STM32 RTU Master Entry Point
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Minimal RTU master for STM32, designed to run in QEMU.
--  Performs test sequence and exits with status code for CI.

with RTU_Master;
with STM32_SysTick;
with Ada_Modbus; use Ada_Modbus;
with Semihosting;
with Interfaces;

procedure Main_Master is
   Values : Register_Array (0 .. 4);
   Coils  : Coil_Array (0 .. 7);
   Result : Status;
   Counter : Register_Value := 0;
   Poll_Interval : constant := 500;  --  500 ms between polls
   Last_Poll : Interfaces.Unsigned_32 := 0;

   --  CI test tracking
   Successful_Polls : Natural := 0;
   Required_Polls   : constant := 3;  --  Exit after 3 successful cycles
   Test_Passed      : Boolean := True;

begin
   Semihosting.Put_Line ("=== Modbus RTU Master Test ===");
   Semihosting.Put_Line ("Initializing...");

   --  Initialize the Modbus master
   RTU_Master.Initialize;

   --  Wait a bit for slave to initialize
   STM32_SysTick.Delay_Ms (200);

   Semihosting.Put_Line ("Starting test sequence...");

   --  Main loop
   loop
      --  Check if it's time to poll
      if STM32_SysTick.Has_Elapsed (Last_Poll, Poll_Interval) then
         Last_Poll := STM32_SysTick.Get_Tick_Ms;

         Semihosting.Put ("Poll #");
         Semihosting.Put (Interfaces.Unsigned_32 (Successful_Polls + 1));
         Semihosting.New_Line;

         --  Test 1: Read holding registers 0-4
         Result := RTU_Master.Read_Holding_Registers
           (Slave         => 1,
            Start_Address => 0,
            Quantity      => 5,
            Values        => Values);

         if Result = Success then
            Semihosting.Put ("  Read registers: OK (reg0=");
            Semihosting.Put (Interfaces.Unsigned_32 (Values (0)));
            Semihosting.Put_Line (")");
            Counter := Values (0);
         else
            Semihosting.Put_Line ("  Read registers: FAILED");
            Test_Passed := False;
         end if;

         --  Test 2: Write a register (increment counter)
         Counter := Counter + 1;
         Result := RTU_Master.Write_Single_Register
           (Slave   => 1,
            Address => 0,
            Value   => Counter);

         if Result = Success then
            Semihosting.Put ("  Write register: OK (value=");
            Semihosting.Put (Interfaces.Unsigned_32 (Counter));
            Semihosting.Put_Line (")");
         else
            Semihosting.Put_Line ("  Write register: FAILED");
            Test_Passed := False;
         end if;

         --  Test 3: Read coils
         Result := RTU_Master.Read_Coils
           (Slave         => 1,
            Start_Address => 0,
            Quantity      => 8,
            Values        => Coils);

         if Result = Success then
            Semihosting.Put_Line ("  Read coils: OK");
         else
            Semihosting.Put_Line ("  Read coils: FAILED");
            Test_Passed := False;
         end if;

         --  Test 4: Toggle a coil based on counter
         Result := RTU_Master.Write_Single_Coil
           (Slave   => 1,
            Address => Coil_Address (Counter mod 8),
            Value   => (Counter mod 2) = 0);

         if Result = Success then
            Semihosting.Put_Line ("  Write coil: OK");
         else
            Semihosting.Put_Line ("  Write coil: FAILED");
            Test_Passed := False;
         end if;

         --  Test 5: Read input registers (uptime from slave)
         Result := RTU_Master.Read_Input_Registers
           (Slave         => 1,
            Start_Address => 0,
            Quantity      => 2,
            Values        => Values (0 .. 1));

         if Result = Success then
            Semihosting.Put ("  Read input regs: OK (count=");
            Semihosting.Put (Interfaces.Unsigned_32 (Values (0)));
            Semihosting.Put (", uptime=");
            Semihosting.Put (Interfaces.Unsigned_32 (Values (1)));
            Semihosting.Put_Line ("s)");
         else
            Semihosting.Put_Line ("  Read input regs: FAILED");
            Test_Passed := False;
         end if;

         --  Count successful polls
         Successful_Polls := Successful_Polls + 1;

         --  Check if we're done
         if Successful_Polls >= Required_Polls then
            Semihosting.New_Line;
            if Test_Passed then
               Semihosting.Put_Line ("=== ALL TESTS PASSED ===");
               Semihosting.Exit_App (0);  --  Success
            else
               Semihosting.Put_Line ("=== TESTS FAILED ===");
               Semihosting.Exit_App (1);  --  Failure
            end if;
         end if;
      end if;
   end loop;
end Main_Master;
