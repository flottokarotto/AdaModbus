--  Ada_Modbus.Slave_Stubs - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

package body Ada_Modbus.Slave_Stubs
  with SPARK_Mode => On
is

   ---------------------
   -- Null_Read_Coils --
   ---------------------

   function Null_Read_Coils
     (Start_Address : Coil_Address;
      Quantity      : Coil_Count;
      Values        : out Coil_Array) return Status
   is
      pragma Unreferenced (Start_Address, Quantity);
   begin
      Values := [others => False];
      return Exception_Illegal_Function;
   end Null_Read_Coils;

   -------------------------------
   -- Null_Read_Discrete_Inputs --
   -------------------------------

   function Null_Read_Discrete_Inputs
     (Start_Address : Coil_Address;
      Quantity      : Coil_Count;
      Values        : out Coil_Array) return Status
   is
      pragma Unreferenced (Start_Address, Quantity);
   begin
      Values := [others => False];
      return Exception_Illegal_Function;
   end Null_Read_Discrete_Inputs;

   ---------------------------------
   -- Null_Read_Holding_Registers --
   ---------------------------------

   function Null_Read_Holding_Registers
     (Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array) return Status
   is
      pragma Unreferenced (Start_Address, Quantity);
   begin
      Values := [others => 0];
      return Exception_Illegal_Function;
   end Null_Read_Holding_Registers;

   -------------------------------
   -- Null_Read_Input_Registers --
   -------------------------------

   function Null_Read_Input_Registers
     (Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array) return Status
   is
      pragma Unreferenced (Start_Address, Quantity);
   begin
      Values := [others => 0];
      return Exception_Illegal_Function;
   end Null_Read_Input_Registers;

   ---------------------------
   -- Null_Write_Single_Coil --
   ---------------------------

   function Null_Write_Single_Coil
     (Address : Coil_Address;
      Value   : Coil_Value) return Status
   is
      pragma Unreferenced (Address, Value);
   begin
      return Exception_Illegal_Function;
   end Null_Write_Single_Coil;

   -------------------------------
   -- Null_Write_Single_Register --
   -------------------------------

   function Null_Write_Single_Register
     (Address : Register_Address;
      Value   : Register_Value) return Status
   is
      pragma Unreferenced (Address, Value);
   begin
      return Exception_Illegal_Function;
   end Null_Write_Single_Register;

   ------------------------------
   -- Null_Write_Multiple_Coils --
   ------------------------------

   function Null_Write_Multiple_Coils
     (Start_Address : Coil_Address;
      Values        : Coil_Array) return Status
   is
      pragma Unreferenced (Start_Address, Values);
   begin
      return Exception_Illegal_Function;
   end Null_Write_Multiple_Coils;

   ----------------------------------
   -- Null_Write_Multiple_Registers --
   ----------------------------------

   function Null_Write_Multiple_Registers
     (Start_Address : Register_Address;
      Values        : Register_Array) return Status
   is
      pragma Unreferenced (Start_Address, Values);
   begin
      return Exception_Illegal_Function;
   end Null_Write_Multiple_Registers;

   -------------------------------
   -- Null_Read_Exception_Status --
   -------------------------------

   function Null_Read_Exception_Status
     (Exception_Status : out Byte) return Status
   is
   begin
      Exception_Status := 0;
      return Exception_Illegal_Function;
   end Null_Read_Exception_Status;

   ----------------------
   -- Null_Diagnostics --
   ----------------------

   function Null_Diagnostics
     (Sub_Function : Interfaces.Unsigned_16;
      Data_In      : Interfaces.Unsigned_16;
      Data_Out     : out Interfaces.Unsigned_16) return Status
   is
      pragma Unreferenced (Sub_Function, Data_In);
   begin
      Data_Out := 0;
      return Exception_Illegal_Function;
   end Null_Diagnostics;

   ---------------------------
   -- Null_Report_Server_Id --
   ---------------------------

   function Null_Report_Server_Id
     (Server_Id     : out Byte;
      Run_Indicator : out Boolean;
      Add_Data      : out Byte_Array;
      Add_Data_Len  : out Natural) return Status
   is
   begin
      Server_Id := 0;
      Run_Indicator := False;
      Add_Data := [others => 0];
      Add_Data_Len := 0;
      return Exception_Illegal_Function;
   end Null_Report_Server_Id;

   -------------------------------
   -- Null_Mask_Write_Register --
   -------------------------------

   function Null_Mask_Write_Register
     (Address  : Register_Address;
      And_Mask : Register_Value;
      Or_Mask  : Register_Value) return Status
   is
      pragma Unreferenced (Address, And_Mask, Or_Mask);
   begin
      return Exception_Illegal_Function;
   end Null_Mask_Write_Register;

   -------------------------------
   -- Null_Read_Write_Registers --
   -------------------------------

   function Null_Read_Write_Registers
     (Read_Start    : Register_Address;
      Read_Quantity : Register_Count;
      Read_Values   : out Register_Array;
      Write_Start   : Register_Address;
      Write_Values  : Register_Array) return Status
   is
      pragma Unreferenced (Read_Start, Read_Quantity, Write_Start, Write_Values);
   begin
      Read_Values := [others => 0];
      return Exception_Illegal_Function;
   end Null_Read_Write_Registers;

end Ada_Modbus.Slave_Stubs;
