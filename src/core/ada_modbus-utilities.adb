--  Ada_Modbus.Utilities - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with Ada.Unchecked_Conversion;
with System;

package body Ada_Modbus.Utilities
  with SPARK_Mode => On
is

   use type System.Bit_Order;

   --  Record overlays for combining/splitting words and bytes.
   --  Field names reflect memory position (B0/W0 = lower address).
   --  Endian-aware accessors below map position to logical Hi/Lo.

   type Byte_Pair is record
      B0 : Byte;
      B1 : Byte;
   end record with Size => 16;

   for Byte_Pair use record
      B0 at 0 range 0 .. 7;
      B1 at 1 range 0 .. 7;
   end record;

   type Word_Pair is record
      W0 : Register_Value;
      W1 : Register_Value;
   end record with Size => 32;

   for Word_Pair use record
      W0 at 0 range 0 .. 15;
      W1 at 2 range 0 .. 15;
   end record;

   function To_Reg is new Ada.Unchecked_Conversion (Byte_Pair, Register_Value);
   function To_Bytes is new Ada.Unchecked_Conversion (Register_Value, Byte_Pair);
   function To_U32 is new Ada.Unchecked_Conversion (Word_Pair, Interfaces.Unsigned_32);
   function From_U32 is new Ada.Unchecked_Conversion (Interfaces.Unsigned_32, Word_Pair);
   function To_F32 is new Ada.Unchecked_Conversion (Word_Pair, IEEE_Float_32);
   function From_F32 is new Ada.Unchecked_Conversion (IEEE_Float_32, Word_Pair);

   --  Endian-aware accessors: on LE B0/W0 is low, on BE B0/W0 is high.
   --  System.Default_Bit_Order is static, so the compiler eliminates
   --  the dead branch.

   function Lo_Byte (B : Byte_Pair) return Byte is
     (if System.Default_Bit_Order = System.Low_Order_First then B.B0 else B.B1);

   function Hi_Byte (B : Byte_Pair) return Byte is
     (if System.Default_Bit_Order = System.Low_Order_First then B.B1 else B.B0);

   function Make_Reg (Lo, Hi : Byte) return Register_Value is
     (if System.Default_Bit_Order = System.Low_Order_First
      then To_Reg ((B0 => Lo, B1 => Hi))
      else To_Reg ((B0 => Hi, B1 => Lo)));

   function Lo_Word (W : Word_Pair) return Register_Value is
     (if System.Default_Bit_Order = System.Low_Order_First then W.W0 else W.W1);

   function Hi_Word (W : Word_Pair) return Register_Value is
     (if System.Default_Bit_Order = System.Low_Order_First then W.W1 else W.W0);

   function Make_Words (Lo, Hi : Register_Value) return Word_Pair is
     (if System.Default_Bit_Order = System.Low_Order_First
      then (W0 => Lo, W1 => Hi)
      else (W0 => Hi, W1 => Lo));

   function Swap_Bytes (V : Register_Value) return Register_Value is
      B : constant Byte_Pair := To_Bytes (V);
   begin
      return Make_Reg (Lo => Hi_Byte (B), Hi => Lo_Byte (B));
   end Swap_Bytes;

   -------------------
   -- To_Big_Endian --
   -------------------

   function To_Big_Endian (Value : Register_Value) return Byte_Array is
      B : constant Byte_Pair := To_Bytes (Value);
   begin
      return [Hi_Byte (B), Lo_Byte (B)];
   end To_Big_Endian;

   --------------------
   -- From_Big_Endian --
   --------------------

   function From_Big_Endian (High, Low : Byte) return Register_Value is
   begin
      return Make_Reg (Lo => Low, Hi => High);
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
      return Hi_Byte (To_Bytes (Value));
   end High_Byte;

   --------------
   -- Low_Byte --
   --------------

   function Low_Byte (Value : Register_Value) return Byte is
   begin
      return Lo_Byte (To_Bytes (Value));
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
            return To_U32 (Make_Words (Lo => Low_Word, Hi => High_Word));

         when Little_Endian =>
            --  DCBA: bytes fully reversed
            return To_U32 (Make_Words (Lo => Swap_Bytes (High_Word),
                                       Hi => Swap_Bytes (Low_Word)));

         when Mid_Big_Endian =>
            --  BADC: bytes swapped within each word
            return To_U32 (Make_Words (Lo => Swap_Bytes (Low_Word),
                                       Hi => Swap_Bytes (High_Word)));

         when Mid_Little_Endian =>
            --  CDAB: words swapped (low word first)
            return To_U32 (Make_Words (Lo => High_Word, Hi => Low_Word));
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
            High_Word := Hi_Word (W);
            Low_Word  := Lo_Word (W);

         when Little_Endian =>
            --  DCBA: bytes fully reversed
            High_Word := Swap_Bytes (Lo_Word (W));
            Low_Word  := Swap_Bytes (Hi_Word (W));

         when Mid_Big_Endian =>
            --  BADC: bytes swapped within each word
            High_Word := Swap_Bytes (Hi_Word (W));
            Low_Word  := Swap_Bytes (Lo_Word (W));

         when Mid_Little_Endian =>
            --  CDAB: words swapped (low word first)
            High_Word := Lo_Word (W);
            Low_Word  := Hi_Word (W);
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
            return To_F32 (Make_Words (Lo => Low_Word, Hi => High_Word));

         when Little_Endian =>
            return To_F32 (Make_Words (Lo => Swap_Bytes (High_Word),
                                       Hi => Swap_Bytes (Low_Word)));

         when Mid_Big_Endian =>
            return To_F32 (Make_Words (Lo => Swap_Bytes (Low_Word),
                                       Hi => Swap_Bytes (High_Word)));

         when Mid_Little_Endian =>
            return To_F32 (Make_Words (Lo => High_Word, Hi => Low_Word));
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
            High_Word := Hi_Word (W);
            Low_Word  := Lo_Word (W);

         when Little_Endian =>
            High_Word := Swap_Bytes (Lo_Word (W));
            Low_Word  := Swap_Bytes (Hi_Word (W));

         when Mid_Big_Endian =>
            High_Word := Swap_Bytes (Hi_Word (W));
            Low_Word  := Swap_Bytes (Lo_Word (W));

         when Mid_Little_Endian =>
            High_Word := Lo_Word (W);
            Low_Word  := Hi_Word (W);
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
