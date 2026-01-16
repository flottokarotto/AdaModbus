--  Ada_Modbus.Utilities - Byte order and helper functions
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

package Ada_Modbus.Utilities
  with SPARK_Mode => On
is

   pragma Pure;

   --  Byte order conversion (Modbus uses Big-Endian / Network Byte Order)

   function To_Big_Endian (Value : Register_Value) return Byte_Array
     with Post => To_Big_Endian'Result'First = 0
                  and then To_Big_Endian'Result'Last = 1;

   function From_Big_Endian (High, Low : Byte) return Register_Value;

   function From_Big_Endian (Data : Byte_Array) return Register_Value
     with Pre => Data'Length = 2;

   --  Byte extraction
   function High_Byte (Value : Register_Value) return Byte;
   function Low_Byte (Value : Register_Value) return Byte;

end Ada_Modbus.Utilities;
