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

   --  32-bit word order for combining two 16-bit registers
   --  Different devices use different conventions:
   type Word_Order is
     (Big_Endian,      --  ABCD: High word first (SunSpec, Modbus standard)
      Little_Endian,   --  DCBA: Low word first
      Mid_Big_Endian,  --  BADC: High word first, bytes swapped
      Mid_Little_Endian); --  CDAB: Low word first, bytes swapped (common)

   --  Combine two registers into 32-bit value
   function To_Unsigned_32
     (High_Word : Register_Value;
      Low_Word  : Register_Value;
      Order     : Word_Order := Big_Endian) return Interfaces.Unsigned_32;

   --  Split 32-bit value into two registers
   procedure From_Unsigned_32
     (Value     : Interfaces.Unsigned_32;
      High_Word : out Register_Value;
      Low_Word  : out Register_Value;
      Order     : Word_Order := Big_Endian);

   --  Convenience: combine register array (2 elements) to 32-bit
   function Registers_To_Unsigned_32
     (Regs  : Register_Array;
      Order : Word_Order := Big_Endian) return Interfaces.Unsigned_32
     with Pre => Regs'Length >= 2;

   --  Human-readable status string (for logging/display)
   subtype Status_String is String (1 .. 24);
   function Status_Image (S : Status) return Status_String;

end Ada_Modbus.Utilities;
