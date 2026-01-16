--  Ada_Modbus.Utilities - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

package body Ada_Modbus.Utilities
  with SPARK_Mode => On
is

   -------------------
   -- To_Big_Endian --
   -------------------

   function To_Big_Endian (Value : Register_Value) return Byte_Array is
   begin
      return [High_Byte (Value), Low_Byte (Value)];
   end To_Big_Endian;

   --------------------
   -- From_Big_Endian --
   --------------------

   function From_Big_Endian (High, Low : Byte) return Register_Value is
   begin
      return Register_Value (High) * 256 + Register_Value (Low);
   end From_Big_Endian;

   function From_Big_Endian (Data : Byte_Array) return Register_Value is
   begin
      return From_Big_Endian (Data (Data'First), Data (Data'First + 1));
   end From_Big_Endian;

   ---------------
   -- High_Byte --
   ---------------

   function High_Byte (Value : Register_Value) return Byte is
   begin
      return Byte (Value / 256);
   end High_Byte;

   --------------
   -- Low_Byte --
   --------------

   function Low_Byte (Value : Register_Value) return Byte is
   begin
      return Byte (Value mod 256);
   end Low_Byte;

end Ada_Modbus.Utilities;
