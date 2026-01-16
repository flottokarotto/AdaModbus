--  Loopback_Slave - Slave data and callbacks for loopback test
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.Slave; use Ada_Modbus.Slave;

package Loopback_Slave is

   --  Data stores (accessible from main for verification)
   Holding_Registers : Register_Array (0 .. 99) := [others => 0];
   Input_Registers   : Register_Array (0 .. 15) := [others => 0];
   Coils             : Coil_Array (0 .. 31) := [others => False];

   --  Slave callbacks (library-level for light runtime)
   function Read_Holding_Registers_CB
     (Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array) return Status;

   function Read_Input_Registers_CB
     (Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array) return Status;

   function Write_Single_Register_CB
     (Address : Register_Address;
      Value   : Register_Value) return Status;

   function Write_Multiple_Registers_CB
     (Start_Address : Register_Address;
      Values        : Register_Array) return Status;

   function Read_Coils_CB
     (Start_Address : Coil_Address;
      Quantity      : Coil_Count;
      Values        : out Coil_Array) return Status;

   function Write_Single_Coil_CB
     (Address : Coil_Address;
      Value   : Coil_Value) return Status;

   --  Slave configuration
   Slave_Cfg : constant Slave_Config :=
     (Mode      => RTU,
      Unit_Id   => 1,
      Callbacks =>
        (Read_Holding_Registers   => Read_Holding_Registers_CB'Access,
         Read_Input_Registers     => Read_Input_Registers_CB'Access,
         Write_Single_Register    => Write_Single_Register_CB'Access,
         Write_Multiple_Registers => Write_Multiple_Registers_CB'Access,
         Read_Coils               => Read_Coils_CB'Access,
         Read_Discrete_Inputs     => null,
         Write_Single_Coil        => Write_Single_Coil_CB'Access,
         Write_Multiple_Coils     => null,
         --  New FCs not implemented in this example
         Read_Exception_Status    => null,
         Diagnostics              => null,
         Report_Server_Id         => null,
         Mask_Write_Register      => null,
         Read_Write_Registers     => null));

end Loopback_Slave;
