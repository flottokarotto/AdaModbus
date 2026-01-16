--  Main - Minimal RTU Slave Entry Point
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  This is the main entry point for the minimal RTU slave.
--  It initializes the slave and enters the main polling loop.

with Minimal_RTU_Slave;

procedure Main is
begin
   --  Initialize the Modbus slave
   Minimal_RTU_Slave.Initialize;

   --  Main loop
   loop
      --  Poll for Modbus requests
      if Minimal_RTU_Slave.Poll then
         --  A request was processed
         --  Application can check Holding_Registers for new values
         null;
      end if;

      --  Application logic here:
      --  - Update Input_Registers with sensor data
      --  - Check Coils for output commands
      --  - etc.

      --  Example: Update input register with request count
      Minimal_RTU_Slave.Input_Registers (0) :=
        Ada_Modbus.Register_Value (Minimal_RTU_Slave.Request_Count mod 65536);
   end loop;
end Main;
