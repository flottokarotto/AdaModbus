--  Modbus_TCP_Master - Modbus TCP master transactions over LwIP TCP_Client
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Board-independent Modbus TCP master using the shared TCP_Client.
--  Handles PDU encoding, MBAP framing, transceive, and response decoding.

with Interfaces; use Interfaces;
with Ada_Modbus; use Ada_Modbus;

package Modbus_TCP_Master is

   --  Read Holding Registers (FC 03)
   procedure Read_Holding_Registers
     (Unit          : Unit_Id;
      Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array;
      Count         : out Natural;
      Timeout_Ms    : Unsigned_32;
      Result        : out Status);

   --  Read Input Registers (FC 04)
   procedure Read_Input_Registers
     (Unit          : Unit_Id;
      Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array;
      Count         : out Natural;
      Timeout_Ms    : Unsigned_32;
      Result        : out Status);

   --  Write Single Register (FC 06)
   procedure Write_Single_Register
     (Unit          : Unit_Id;
      Address       : Register_Address;
      Value         : Register_Value;
      Timeout_Ms    : Unsigned_32;
      Result        : out Status);

   --  Write Multiple Registers (FC 16)
   procedure Write_Multiple_Registers
     (Unit          : Unit_Id;
      Start_Address : Register_Address;
      Values        : Register_Array;
      Timeout_Ms    : Unsigned_32;
      Result        : out Status);

end Modbus_TCP_Master;
