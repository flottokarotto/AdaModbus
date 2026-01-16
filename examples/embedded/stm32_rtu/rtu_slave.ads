--  RTU_Slave - Modbus RTU Slave for LM3S6965 (QEMU)
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Minimal RTU slave implementation using LM3S UART.

with Ada_Modbus; use Ada_Modbus;
with LM3S_UART;

package RTU_Slave is

   --  Configuration
   Unit_Id_Value : constant Unit_Id := 1;
   UART_Port     : constant LM3S_UART.UART_Peripheral := LM3S_UART.UART0;

   --  Data stores (directly accessible for application code)
   Holding_Registers : Register_Array (0 .. 31) := [others => 0];
   Input_Registers   : Register_Array (0 .. 31) := [others => 0];
   Coils             : Coil_Array (0 .. 63) := [others => False];

   --  Statistics
   Request_Count : Natural := 0;
   Error_Count   : Natural := 0;

   --  Initialize slave (call once at startup)
   procedure Initialize;

   --  Poll for requests (call repeatedly in main loop)
   --  Returns True if a request was processed
   function Poll return Boolean;

end RTU_Slave;
