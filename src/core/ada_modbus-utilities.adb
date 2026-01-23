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

   -------------------
   -- To_Unsigned_32 --
   -------------------

   function To_Unsigned_32
     (High_Word : Register_Value;
      Low_Word  : Register_Value;
      Order     : Word_Order := Big_Endian) return Interfaces.Unsigned_32
   is
      use type Interfaces.Unsigned_32;
      H : constant Interfaces.Unsigned_32 := Interfaces.Unsigned_32 (High_Word);
      L : constant Interfaces.Unsigned_32 := Interfaces.Unsigned_32 (Low_Word);
      HH, HL, LH, LL : Interfaces.Unsigned_32;
   begin
      case Order is
         when Big_Endian =>
            --  ABCD: High word first, standard Modbus/SunSpec
            return H * 65536 + L;

         when Little_Endian =>
            --  DCBA: Low word first, bytes swapped
            HH := Interfaces.Unsigned_32 (High_Byte (High_Word));
            HL := Interfaces.Unsigned_32 (Low_Byte (High_Word));
            LH := Interfaces.Unsigned_32 (High_Byte (Low_Word));
            LL := Interfaces.Unsigned_32 (Low_Byte (Low_Word));
            return LL * 16777216 + LH * 65536 + HL * 256 + HH;

         when Mid_Big_Endian =>
            --  BADC: High word first, bytes swapped within words
            HH := Interfaces.Unsigned_32 (Low_Byte (High_Word));
            HL := Interfaces.Unsigned_32 (High_Byte (High_Word));
            LH := Interfaces.Unsigned_32 (Low_Byte (Low_Word));
            LL := Interfaces.Unsigned_32 (High_Byte (Low_Word));
            return HH * 16777216 + HL * 65536 + LH * 256 + LL;

         when Mid_Little_Endian =>
            --  CDAB: Low word first (word-swapped big-endian)
            return L * 65536 + H;
      end case;
   end To_Unsigned_32;

   ---------------------
   -- From_Unsigned_32 --
   ---------------------

   procedure From_Unsigned_32
     (Value     : Interfaces.Unsigned_32;
      High_Word : out Register_Value;
      Low_Word  : out Register_Value;
      Order     : Word_Order := Big_Endian)
   is
      use type Interfaces.Unsigned_32;
      A, B, C, D : Byte;
   begin
      --  Extract bytes from value (big-endian: A is MSB, D is LSB)
      A := Byte ((Value / 16777216) mod 256);
      B := Byte ((Value / 65536) mod 256);
      C := Byte ((Value / 256) mod 256);
      D := Byte (Value mod 256);

      case Order is
         when Big_Endian =>
            --  ABCD
            High_Word := Register_Value (A) * 256 + Register_Value (B);
            Low_Word  := Register_Value (C) * 256 + Register_Value (D);

         when Little_Endian =>
            --  DCBA
            High_Word := Register_Value (C) * 256 + Register_Value (D);
            Low_Word  := Register_Value (A) * 256 + Register_Value (B);

         when Mid_Big_Endian =>
            --  BADC
            High_Word := Register_Value (B) * 256 + Register_Value (A);
            Low_Word  := Register_Value (D) * 256 + Register_Value (C);

         when Mid_Little_Endian =>
            --  CDAB
            High_Word := Register_Value (C) * 256 + Register_Value (D);
            Low_Word  := Register_Value (A) * 256 + Register_Value (B);
      end case;
   end From_Unsigned_32;

   ------------------------------
   -- Registers_To_Unsigned_32 --
   ------------------------------

   function Registers_To_Unsigned_32
     (Regs  : Register_Array;
      Order : Word_Order := Big_Endian) return Interfaces.Unsigned_32
   is
   begin
      return To_Unsigned_32 (Regs (Regs'First), Regs (Regs'First + 1), Order);
   end Registers_To_Unsigned_32;

end Ada_Modbus.Utilities;
