--  Ada_Modbus.Energy.SunSpec.Common - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

package body Ada_Modbus.Energy.SunSpec.Common
  with SPARK_Mode => On
is

   ------------------------------------
   -- Encode_Read_Manufacturer_Request --
   ------------------------------------

   procedure Encode_Read_Manufacturer_Request
     (Base_Address  : Register_Address;
      Model_Offset  : Register_Address;
      Buffer        : out Protocol.PDU_Buffer;
      Length        : out Natural)
   is
   begin
      Protocol.Encode_Read_Registers_Request
        (FC            => FC_Read_Holding_Registers,
         Start_Address => Base_Address + Model_Offset + Reg_Manufacturer,
         Quantity      => 16,
         Buffer        => Buffer,
         Length        => Length);
   end Encode_Read_Manufacturer_Request;

   ------------------------------
   -- Encode_Read_Model_Request --
   ------------------------------

   procedure Encode_Read_Model_Request
     (Base_Address  : Register_Address;
      Model_Offset  : Register_Address;
      Buffer        : out Protocol.PDU_Buffer;
      Length        : out Natural)
   is
   begin
      Protocol.Encode_Read_Registers_Request
        (FC            => FC_Read_Holding_Registers,
         Start_Address => Base_Address + Model_Offset + Reg_Model,
         Quantity      => 16,
         Buffer        => Buffer,
         Length        => Length);
   end Encode_Read_Model_Request;

   --------------------------------
   -- Encode_Read_Serial_Request --
   --------------------------------

   procedure Encode_Read_Serial_Request
     (Base_Address  : Register_Address;
      Model_Offset  : Register_Address;
      Buffer        : out Protocol.PDU_Buffer;
      Length        : out Natural)
   is
   begin
      Protocol.Encode_Read_Registers_Request
        (FC            => FC_Read_Holding_Registers,
         Start_Address => Base_Address + Model_Offset + Reg_Serial,
         Quantity      => 16,
         Buffer        => Buffer,
         Length        => Length);
   end Encode_Read_Serial_Request;

   ---------------------------------
   -- Encode_Read_Version_Request --
   ---------------------------------

   procedure Encode_Read_Version_Request
     (Base_Address  : Register_Address;
      Model_Offset  : Register_Address;
      Buffer        : out Protocol.PDU_Buffer;
      Length        : out Natural)
   is
   begin
      Protocol.Encode_Read_Registers_Request
        (FC            => FC_Read_Holding_Registers,
         Start_Address => Base_Address + Model_Offset + Reg_Version,
         Quantity      => 8,
         Buffer        => Buffer,
         Length        => Length);
   end Encode_Read_Version_Request;

   ----------------------------
   -- Decode_String_Response --
   ----------------------------

   procedure Decode_String_Response
     (Buffer : Protocol.PDU_Buffer;
      Length : Natural;
      Value  : out SunSpec_String;
      Len    : out Natural;
      Result : out Status)
   is
      Values    : Register_Array (0 .. 15);
      Reg_Count : Natural;
   begin
      Value := [others => ' '];
      Len   := 0;

      Protocol.Decode_Read_Registers_Response
        (Buffer   => Buffer,
         Length   => Length,
         Values   => Values,
         Count    => Reg_Count,
         Response => Result);

      if Result = Success and then Reg_Count <= 16 then
         Decode_String (Values (0 .. Reg_Count - 1), Value, Len);
      end if;
   end Decode_String_Response;

   ----------------------------------
   -- Decode_Short_String_Response --
   ----------------------------------

   procedure Decode_Short_String_Response
     (Buffer : Protocol.PDU_Buffer;
      Length : Natural;
      Value  : out String;
      Len    : out Natural;
      Result : out Status)
   is
      Values     : Register_Array (0 .. 7);
      Reg_Count  : Natural;
      Temp       : SunSpec_String;
      Temp_Len   : Natural;
   begin
      Value := [others => ' '];
      Len   := 0;

      Protocol.Decode_Read_Registers_Response
        (Buffer   => Buffer,
         Length   => Length,
         Values   => Values,
         Count    => Reg_Count,
         Response => Result);

      if Result = Success and then Reg_Count >= 1 and then Reg_Count <= 8 then
         Decode_String (Values (0 .. Reg_Count - 1), Temp, Temp_Len);
         if Temp_Len = 0 then
            Len := 0;
         elsif Temp_Len <= 16 then
            Len := Temp_Len;
            Value (Value'First .. Value'First + Len - 1) := Temp (1 .. Len);
         else
            Len := 16;
            Value := Temp (1 .. 16);
         end if;
      end if;
   end Decode_Short_String_Response;

end Ada_Modbus.Energy.SunSpec.Common;
