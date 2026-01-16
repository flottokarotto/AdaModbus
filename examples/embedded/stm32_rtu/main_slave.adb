--  Main_Slave - STM32 RTU Slave Entry Point
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Minimal RTU slave for STM32, designed to run in QEMU.

with RTU_Slave;
with STM32_SysTick;
with Ada_Modbus;
with Semihosting;
with Interfaces;

procedure Main_Slave is
   use Ada_Modbus;
   use type Interfaces.Unsigned_32;

   Uptime_Seconds : Interfaces.Unsigned_32;
   Last_Count     : Interfaces.Unsigned_32 := 0;
begin
   Semihosting.Put_Line ("=== Modbus RTU Slave ===");
   Semihosting.Put_Line ("Initializing...");

   --  Initialize the Modbus slave
   RTU_Slave.Initialize;

   Semihosting.Put_Line ("Slave ready, waiting for requests...");

   --  Main loop
   loop
      --  Poll for Modbus requests
      if RTU_Slave.Poll then
         --  A request was processed
         --  Update input register 0 with request count
         RTU_Slave.Input_Registers (0) :=
           Register_Value (RTU_Slave.Request_Count mod 65536);

         --  Log when request count changes
         if Interfaces.Unsigned_32 (RTU_Slave.Request_Count) > Last_Count then
            Last_Count := Interfaces.Unsigned_32 (RTU_Slave.Request_Count);
            Semihosting.Put ("Request #");
            Semihosting.Put (Last_Count);
            Semihosting.Put_Line (" processed");
         end if;
      end if;

      --  Update input register 1 with uptime (seconds)
      Uptime_Seconds := STM32_SysTick.Get_Tick_Ms / 1000;
      RTU_Slave.Input_Registers (1) :=
        Register_Value (Uptime_Seconds mod 65536);
   end loop;
end Main_Slave;
