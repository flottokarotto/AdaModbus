--  Ada_Modbus.Utilities - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with Ada.Unchecked_Conversion;

package body Ada_Modbus.Utilities
  with SPARK_Mode => On
is

   --  Record overlays for combining/splitting words and bytes.
   --  Layout assumes little-endian storage (x86, ARM Cortex-M).

   type Byte_Pair is record
      Lo : Byte;
      Hi : Byte;
   end record with Size => 16;

   for Byte_Pair use record
      Lo at 0 range 0 .. 7;
      Hi at 1 range 0 .. 7;
   end record;

   type Word_Pair is record
      Lo : Register_Value;
      Hi : Register_Value;
   end record with Size => 32;

   for Word_Pair use record
      Lo at 0 range 0 .. 15;
      Hi at 2 range 0 .. 15;
   end record;

   function To_Reg is new Ada.Unchecked_Conversion (Byte_Pair, Register_Value);
   function To_Bytes is new Ada.Unchecked_Conversion (Register_Value, Byte_Pair);
   function To_U32 is new Ada.Unchecked_Conversion (Word_Pair, Interfaces.Unsigned_32);
   function From_U32 is new Ada.Unchecked_Conversion (Interfaces.Unsigned_32, Word_Pair);
   function To_F32 is new Ada.Unchecked_Conversion (Word_Pair, IEEE_Float_32);
   function From_F32 is new Ada.Unchecked_Conversion (IEEE_Float_32, Word_Pair);

   function Swap_Bytes (V : Register_Value) return Register_Value is
      B : constant Byte_Pair := To_Bytes (V);
   begin
      return To_Reg ((Lo => B.Hi, Hi => B.Lo));
   end Swap_Bytes;

   -------------------
   -- To_Big_Endian --
   -------------------

   function To_Big_Endian (Value : Register_Value) return Byte_Array is
      B : constant Byte_Pair := To_Bytes (Value);
   begin
      return [B.Hi, B.Lo];
   end To_Big_Endian;

   --------------------
   -- From_Big_Endian --
   --------------------

   function From_Big_Endian (High, Low : Byte) return Register_Value is
   begin
      return To_Reg ((Lo => Low, Hi => High));
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
      return To_Bytes (Value).Hi;
   end High_Byte;

   --------------
   -- Low_Byte --
   --------------

   function Low_Byte (Value : Register_Value) return Byte is
   begin
      return To_Bytes (Value).Lo;
   end Low_Byte;

   -------------------
   -- To_Unsigned_32 --
   -------------------

   function To_Unsigned_32
     (High_Word : Register_Value;
      Low_Word  : Register_Value;
      Order     : Word_Order := Big_Endian) return Interfaces.Unsigned_32
   is
   begin
      case Order is
         when Big_Endian =>
            --  ABCD: High word first, standard Modbus/SunSpec
            return To_U32 ((Lo => Low_Word, Hi => High_Word));

         when Little_Endian =>
            --  DCBA: bytes fully reversed
            return To_U32 ((Lo => Swap_Bytes (High_Word),
                            Hi => Swap_Bytes (Low_Word)));

         when Mid_Big_Endian =>
            --  BADC: bytes swapped within each word
            return To_U32 ((Lo => Swap_Bytes (Low_Word),
                            Hi => Swap_Bytes (High_Word)));

         when Mid_Little_Endian =>
            --  CDAB: words swapped (low word first)
            return To_U32 ((Lo => High_Word, Hi => Low_Word));
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
      W : constant Word_Pair := From_U32 (Value);
   begin
      case Order is
         when Big_Endian =>
            --  ABCD
            High_Word := W.Hi;
            Low_Word  := W.Lo;

         when Little_Endian =>
            --  DCBA: bytes fully reversed
            High_Word := Swap_Bytes (W.Lo);
            Low_Word  := Swap_Bytes (W.Hi);

         when Mid_Big_Endian =>
            --  BADC: bytes swapped within each word
            High_Word := Swap_Bytes (W.Hi);
            Low_Word  := Swap_Bytes (W.Lo);

         when Mid_Little_Endian =>
            --  CDAB: words swapped (low word first)
            High_Word := W.Lo;
            Low_Word  := W.Hi;
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

   -----------------
   -- To_Float_32 --
   -----------------

   function To_Float_32
     (High_Word : Register_Value;
      Low_Word  : Register_Value;
      Order     : Word_Order := Big_Endian) return IEEE_Float_32
   is
   begin
      case Order is
         when Big_Endian =>
            return To_F32 ((Lo => Low_Word, Hi => High_Word));

         when Little_Endian =>
            return To_F32 ((Lo => Swap_Bytes (High_Word),
                            Hi => Swap_Bytes (Low_Word)));

         when Mid_Big_Endian =>
            return To_F32 ((Lo => Swap_Bytes (Low_Word),
                            Hi => Swap_Bytes (High_Word)));

         when Mid_Little_Endian =>
            return To_F32 ((Lo => High_Word, Hi => Low_Word));
      end case;
   end To_Float_32;

   -------------------
   -- From_Float_32 --
   -------------------

   procedure From_Float_32
     (Value     : IEEE_Float_32;
      High_Word : out Register_Value;
      Low_Word  : out Register_Value;
      Order     : Word_Order := Big_Endian)
   is
      W : constant Word_Pair := From_F32 (Value);
   begin
      case Order is
         when Big_Endian =>
            High_Word := W.Hi;
            Low_Word  := W.Lo;

         when Little_Endian =>
            High_Word := Swap_Bytes (W.Lo);
            Low_Word  := Swap_Bytes (W.Hi);

         when Mid_Big_Endian =>
            High_Word := Swap_Bytes (W.Hi);
            Low_Word  := Swap_Bytes (W.Lo);

         when Mid_Little_Endian =>
            High_Word := W.Lo;
            Low_Word  := W.Hi;
      end case;
   end From_Float_32;

   ---------------------------
   -- Registers_To_Float_32 --
   ---------------------------

   function Registers_To_Float_32
     (Regs  : Register_Array;
      Order : Word_Order := Big_Endian) return IEEE_Float_32
   is
   begin
      return To_Float_32 (Regs (Regs'First), Regs (Regs'First + 1), Order);
   end Registers_To_Float_32;

   ------------------
   -- Status_Image --
   ------------------

   function Status_Image (S : Status) return Status_String is
   begin
      return (case S is
         when Success                    => "Success                 ",
         when Timeout                    => "Timeout                 ",
         when CRC_Error                  => "CRC Error               ",
         when LRC_Error                  => "LRC Error               ",
         when Frame_Error                => "Frame Error             ",
         when Invalid_Response           => "Invalid Response        ",
         when Invalid_Request            => "Invalid Request         ",
         when Buffer_Too_Small           => "Buffer Too Small        ",
         when Not_Implemented            => "Not Implemented         ",
         when Exception_Illegal_Function => "Illegal Function (01)   ",
         when Exception_Illegal_Address  => "Illegal Address (02)    ",
         when Exception_Illegal_Value    => "Illegal Value (03)      ",
         when Exception_Slave_Failure    => "Slave Failure (04)      ",
         when Exception_Acknowledge      => "Acknowledge (05)        ",
         when Exception_Slave_Busy       => "Slave Busy (06)         ",
         when Exception_Gateway_Path     => "Gateway Path (10)       ",
         when Exception_Gateway_Target   => "Gateway Target (11)     ");
   end Status_Image;

end Ada_Modbus.Utilities;
