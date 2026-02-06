--  KSEM_Client - Kostal Smart Energy Meter Modbus TCP Client
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Reads power values from KSEM via Modbus TCP.
--  KSEM provides:
--    - Total active power (grid exchange)
--    - Per-phase power values
--    - Voltage and current
--
--  KSEM Modbus Register Map (SunSpec-compatible):
--    40000+  SunSpec models
--    See KSEM documentation for details

with Interfaces; use Interfaces;
with Ada_Modbus; use Ada_Modbus;

package KSEM_Client is

   --  Power measurement result
   type Power_Data is record
      Total_Power_W   : Integer_32;   --  Total active power (+export/-import)
      Phase_L1_W      : Integer_32;   --  Phase 1 power
      Phase_L2_W      : Integer_32;   --  Phase 2 power
      Phase_L3_W      : Integer_32;   --  Phase 3 power
      Valid           : Boolean;      --  Data validity flag
   end record;

   --  Initialize KSEM client
   procedure Initialize;

   --  Connect to KSEM via Modbus TCP
   procedure Connect (Result : out Status);

   --  Disconnect from KSEM
   procedure Disconnect;

   --  Check connection status
   function Is_Connected return Boolean;

   --  Read current power values
   --  Positive = export to grid, Negative = import from grid
   procedure Read_Power (Data : out Power_Data; Result : out Status);

   --  Get last known power values (cached)
   function Get_Last_Power return Power_Data;

end KSEM_Client;
