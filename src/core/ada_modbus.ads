--  Ada_Modbus - Ada 2022 Modbus Library
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with Interfaces;

package Ada_Modbus
  with SPARK_Mode => On
is

   pragma Pure;

   --  Basic byte type for protocol operations
   type Byte is new Interfaces.Unsigned_8;
   type Byte_Array is array (Natural range <>) of Byte;

   --  Modbus uses Big-Endian (Network Byte Order)

   --  Register types (16-bit)
   type Register_Value is new Interfaces.Unsigned_16;
   type Register_Address is new Interfaces.Unsigned_16;
   type Register_Count is range 1 .. 125;  --  Max per Modbus spec
   type Register_Array is array (Natural range <>) of Register_Value;

   --  Coil types (1-bit)
   subtype Coil_Value is Boolean;
   type Coil_Address is new Interfaces.Unsigned_16;
   type Coil_Count is range 1 .. 2000;  --  Max per Modbus spec
   type Coil_Array is array (Natural range <>) of Coil_Value
     with Pack;

   --  Slave/Unit address
   type Unit_Id is new Interfaces.Unsigned_8 range 0 .. 247;
   Broadcast_Address : constant Unit_Id := 0;

   --  Function codes
   type Function_Code is new Interfaces.Unsigned_8;

   FC_Read_Coils                    : constant Function_Code := 16#01#;
   FC_Read_Discrete_Inputs          : constant Function_Code := 16#02#;
   FC_Read_Holding_Registers        : constant Function_Code := 16#03#;
   FC_Read_Input_Registers          : constant Function_Code := 16#04#;
   FC_Write_Single_Coil             : constant Function_Code := 16#05#;
   FC_Write_Single_Register         : constant Function_Code := 16#06#;
   FC_Read_Exception_Status         : constant Function_Code := 16#07#;
   FC_Diagnostics                   : constant Function_Code := 16#08#;
   FC_Write_Multiple_Coils          : constant Function_Code := 16#0F#;
   FC_Write_Multiple_Registers      : constant Function_Code := 16#10#;
   FC_Report_Server_Id              : constant Function_Code := 16#11#;
   FC_Mask_Write_Register           : constant Function_Code := 16#16#;
   FC_Read_Write_Multiple_Registers : constant Function_Code := 16#17#;

   --  Status codes for all operations
   type Status is
     (Success,
      Timeout,
      CRC_Error,
      LRC_Error,
      Frame_Error,
      Invalid_Response,
      Invalid_Request,
      Buffer_Too_Small,
      Not_Implemented,
      --  Modbus Exception Codes (from slave)
      Exception_Illegal_Function,       --  01
      Exception_Illegal_Address,        --  02
      Exception_Illegal_Value,          --  03
      Exception_Slave_Failure,          --  04
      Exception_Acknowledge,            --  05
      Exception_Slave_Busy,             --  06
      Exception_Gateway_Path,           --  10
      Exception_Gateway_Target);        --  11

end Ada_Modbus;
