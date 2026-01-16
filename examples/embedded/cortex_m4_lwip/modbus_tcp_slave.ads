--  Modbus_TCP_Slave - Embedded Modbus TCP Slave for Cortex-M4
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Simple API for integrating Modbus TCP slave into embedded application.
--
--  Usage:
--    1. Call Modbus_Init(502, Result) after LwIP is initialized
--    2. Call Modbus_Poll() periodically from main loop
--    3. Use Set_Input_Register() to update sensor values
--    4. Read Get_Holding_Register() for values written by master
--    5. Read Get_Coil() for coil outputs set by master
--    6. Use Set_Discrete_Input() for digital input status

with Interfaces; use Interfaces;
with Ada_Modbus; use Ada_Modbus;

package Modbus_TCP_Slave is

   --  Initialize Modbus TCP slave on specified port
   --  Call once after LwIP initialization
   procedure Modbus_Init (Port : Unsigned_16; Result : out Status);

   --  Poll for Modbus requests
   --  Call this periodically from main loop (e.g., every 10ms)
   procedure Modbus_Poll;

   --  Access holding registers (read/write by master)
   function Get_Holding_Register (Index : Natural) return Register_Value;
   procedure Set_Holding_Register (Index : Natural; Value : Register_Value);

   --  Set input registers (read-only for master, updated by application)
   --  Typical use: ADC values, sensor readings
   procedure Set_Input_Register (Index : Natural; Value : Register_Value);

   --  Get coil status (read/write by master)
   --  Typical use: Digital outputs controlled by master
   function Get_Coil (Index : Natural) return Boolean;

   --  Set discrete inputs (read-only for master)
   --  Typical use: Button states, limit switches
   procedure Set_Discrete_Input (Index : Natural; Value : Boolean);

end Modbus_TCP_Slave;
