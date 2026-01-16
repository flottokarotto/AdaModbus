--  Modbus_TCP_Master - Embedded Modbus TCP Master for Cortex-M4
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Simple API for Modbus TCP master (client) on embedded systems.
--
--  Usage:
--    1. Call Modbus_Connect() to connect to a Modbus slave
--    2. Use Read/Write functions to communicate
--    3. Call Modbus_Disconnect() when done

with Interfaces; use Interfaces;
with Ada_Modbus; use Ada_Modbus;

package Modbus_TCP_Master is

   --  Connect to a Modbus TCP slave
   procedure Modbus_Connect
     (IP_A, IP_B, IP_C, IP_D : Unsigned_8;  --  IP address octets
      Port                   : Unsigned_16;
      Result                 : out Status);

   --  Disconnect from slave
   procedure Modbus_Disconnect;

   --  Check if connected
   function Is_Connected return Boolean;

   --  Read Holding Registers (FC03)
   procedure Read_Holding_Registers
     (Slave_ID      : Unit_Id;
      Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array;
      Result        : out Status);

   --  Read Input Registers (FC04)
   procedure Read_Input_Registers
     (Slave_ID      : Unit_Id;
      Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array;
      Result        : out Status);

   --  Write Single Register (FC06)
   procedure Write_Single_Register
     (Slave_ID : Unit_Id;
      Address  : Register_Address;
      Value    : Register_Value;
      Result   : out Status);

   --  Write Multiple Registers (FC16)
   procedure Write_Multiple_Registers
     (Slave_ID      : Unit_Id;
      Start_Address : Register_Address;
      Values        : Register_Array;
      Result        : out Status);

   --  Read Coils (FC01)
   procedure Read_Coils
     (Slave_ID      : Unit_Id;
      Start_Address : Coil_Address;
      Quantity      : Coil_Count;
      Values        : out Coil_Array;
      Result        : out Status);

   --  Write Single Coil (FC05)
   procedure Write_Single_Coil
     (Slave_ID : Unit_Id;
      Address  : Coil_Address;
      Value    : Coil_Value;
      Result   : out Status);

end Modbus_TCP_Master;
