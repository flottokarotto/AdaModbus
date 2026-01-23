--  Ada_Modbus.Energy.SunSpec.Common - SunSpec Common Model (Model 1)
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Model 1 contains device identification:
--  - Manufacturer, Model, Serial Number
--  - Software Version
--  - Device Address
--
--  This model is always present in SunSpec-compliant devices.

with Ada_Modbus.Protocol;

package Ada_Modbus.Energy.SunSpec.Common
  with SPARK_Mode => On
is

   --  Model 1 register offsets (from model start, after 2-reg header)
   --  Note: Add 2 to account for model header (ID + Length)
   Reg_Manufacturer : constant := 2;   --  16 registers (32 chars)
   Reg_Model        : constant := 18;  --  16 registers (32 chars)
   Reg_Options      : constant := 34;  --  8 registers (16 chars)
   Reg_Version      : constant := 42;  --  8 registers (16 chars)
   Reg_Serial       : constant := 50;  --  16 registers (32 chars)
   Reg_Device_Addr  : constant := 66;  --  1 register

   --  Model 1 fixed length
   Model_1_Length : constant := 66;

   --  Common model data
   type Common_Info is record
      Manufacturer : SunSpec_String;
      Model        : SunSpec_String;
      Options      : String (1 .. 16);
      Version      : String (1 .. 16);
      Serial       : SunSpec_String;
      Device_Addr  : Register_Value;
   end record;

   --  Read manufacturer name (registers 2-17)
   procedure Encode_Read_Manufacturer_Request
     (Base_Address  : Register_Address;
      Model_Offset  : Register_Address;
      Buffer        : out Protocol.PDU_Buffer;
      Length        : out Natural)
     with Post => Length <= Protocol.Max_PDU_Size;

   --  Read model name (registers 18-33)
   procedure Encode_Read_Model_Request
     (Base_Address  : Register_Address;
      Model_Offset  : Register_Address;
      Buffer        : out Protocol.PDU_Buffer;
      Length        : out Natural)
     with Post => Length <= Protocol.Max_PDU_Size;

   --  Read serial number (registers 50-65)
   procedure Encode_Read_Serial_Request
     (Base_Address  : Register_Address;
      Model_Offset  : Register_Address;
      Buffer        : out Protocol.PDU_Buffer;
      Length        : out Natural)
     with Post => Length <= Protocol.Max_PDU_Size;

   --  Read version (registers 42-49)
   procedure Encode_Read_Version_Request
     (Base_Address  : Register_Address;
      Model_Offset  : Register_Address;
      Buffer        : out Protocol.PDU_Buffer;
      Length        : out Natural)
     with Post => Length <= Protocol.Max_PDU_Size;

   --  Decode string response (16 registers max)
   procedure Decode_String_Response
     (Buffer : Protocol.PDU_Buffer;
      Length : Natural;
      Value  : out SunSpec_String;
      Len    : out Natural;
      Result : out Status)
     with Pre => Length <= Protocol.Max_PDU_Size;

   --  Decode short string response (8 registers max)
   procedure Decode_Short_String_Response
     (Buffer : Protocol.PDU_Buffer;
      Length : Natural;
      Value  : out String;
      Len    : out Natural;
      Result : out Status)
     with Pre => Length <= Protocol.Max_PDU_Size
                 and then Value'Length = 16
                 and then Value'First <= Integer'Last - 16;

end Ada_Modbus.Energy.SunSpec.Common;
