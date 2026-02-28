--  Ada_Modbus.Record_IO - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with Ada.Unchecked_Conversion;
with Interfaces;

package body Ada_Modbus.Record_IO
  with SPARK_Mode => On
is

   --  Unchecked_Conversion instantiations at package level for SPARK

   function To_Map is new Ada.Unchecked_Conversion
     (Source => Map_Registers, Target => Register_Map);
   function To_Regs is new Ada.Unchecked_Conversion
     (Source => Register_Map, Target => Map_Registers);

   --  Local types for splitting native 32-bit values into register pairs.
   --  Unchecked_Conversion preserves the native memory layout, so the
   --  two Register_Values match what the record overlay expects.

   type Reg_Pair is array (0 .. 1) of Register_Value
     with Size => 32, Pack;

   function To_Pair is new Ada.Unchecked_Conversion
     (Interfaces.Unsigned_32, Reg_Pair);
   function From_Pair is new Ada.Unchecked_Conversion
     (Reg_Pair, Interfaces.Unsigned_32);

   ------------------
   -- To_Registers --
   ------------------

   function To_Registers (Map : Register_Map) return Map_Registers is
   begin
      return To_Regs (Map);
   end To_Registers;

   --------------------
   -- From_Registers --
   --------------------

   function From_Registers (Regs : Map_Registers) return Register_Map is
   begin
      return To_Map (Regs);
   end From_Registers;

   -----------------------------------------
   -- From_Registers (with word order)    --
   -----------------------------------------

   function From_Registers
     (Regs  : Map_Registers;
      Pairs : Word_Pair_Indices;
      Order : Utilities.Word_Order :=
        Utilities.Big_Endian) return Register_Map
   is
      Tmp : Map_Registers := Regs;
   begin
      --  Adjust each 32-bit field from wire word order to native layout
      for Idx of Pairs loop
         declare
            --  Decode wire registers according to device word order
            Native : constant Interfaces.Unsigned_32 :=
              Utilities.To_Unsigned_32
                (High_Word => Tmp (Idx),
                 Low_Word  => Tmp (Idx + 1),
                 Order     => Order);
            --  Split native U32 into register pair matching memory layout
            Pair : constant Reg_Pair := To_Pair (Native);
         begin
            Tmp (Idx)     := Pair (0);
            Tmp (Idx + 1) := Pair (1);
         end;
      end loop;
      return To_Map (Tmp);
   end From_Registers;

   --------------------------------------
   -- To_Registers (with word order)   --
   --------------------------------------

   function To_Registers
     (Map   : Register_Map;
      Pairs : Word_Pair_Indices;
      Order : Utilities.Word_Order :=
        Utilities.Big_Endian) return Map_Registers
   is
      Tmp : Map_Registers := To_Regs (Map);
   begin
      --  Adjust each 32-bit field from native layout to wire word order
      for Idx of Pairs loop
         declare
            --  Reconstruct native U32 from register pair
            Native : constant Interfaces.Unsigned_32 :=
              From_Pair ([Tmp (Idx), Tmp (Idx + 1)]);
            High_Word, Low_Word : Register_Value;
         begin
            --  Encode to wire registers according to device word order
            Utilities.From_Unsigned_32
              (Value     => Native,
               High_Word => High_Word,
               Low_Word  => Low_Word,
               Order     => Order);
            Tmp (Idx)     := High_Word;
            Tmp (Idx + 1) := Low_Word;
         end;
      end loop;
      return Tmp;
   end To_Registers;

   ------------------------
   -- Register_Count_Of --
   ------------------------

   function Register_Count_Of (Fields : Field_Sizes) return Natural is
      Count : Natural := 0;
   begin
      for F of Fields loop
         case F is
            when Bits_16 => Count := Count + 1;
            when Bits_32 => Count := Count + 2;
         end case;
      end loop;
      return Count;
   end Register_Count_Of;

   -------------------------------------------
   -- From_Registers (with field sizes)     --
   -------------------------------------------

   function From_Registers
     (Regs   : Map_Registers;
      Fields : Field_Sizes;
      Order  : Utilities.Word_Order :=
        Utilities.Big_Endian) return Register_Map
   is
      Tmp : Map_Registers := Regs;
      Idx : Natural := 0;
   begin
      for F of Fields loop
         case F is
            when Bits_16 =>
               Idx := Idx + 1;
            when Bits_32 =>
               declare
                  Native : constant Interfaces.Unsigned_32 :=
                    Utilities.To_Unsigned_32
                      (High_Word => Tmp (Idx),
                       Low_Word  => Tmp (Idx + 1),
                       Order     => Order);
                  Pair : constant Reg_Pair := To_Pair (Native);
               begin
                  Tmp (Idx)     := Pair (0);
                  Tmp (Idx + 1) := Pair (1);
               end;
               Idx := Idx + 2;
         end case;
      end loop;
      return To_Map (Tmp);
   end From_Registers;

   ----------------------------------------
   -- To_Registers (with field sizes)    --
   ----------------------------------------

   function To_Registers
     (Map    : Register_Map;
      Fields : Field_Sizes;
      Order  : Utilities.Word_Order :=
        Utilities.Big_Endian) return Map_Registers
   is
      Tmp : Map_Registers := To_Regs (Map);
      Idx : Natural := 0;
   begin
      for F of Fields loop
         case F is
            when Bits_16 =>
               Idx := Idx + 1;
            when Bits_32 =>
               declare
                  Native : constant Interfaces.Unsigned_32 :=
                    From_Pair ([Tmp (Idx), Tmp (Idx + 1)]);
                  High_Word, Low_Word : Register_Value;
               begin
                  Utilities.From_Unsigned_32
                    (Value     => Native,
                     High_Word => High_Word,
                     Low_Word  => Low_Word,
                     Order     => Order);
                  Tmp (Idx)     := High_Word;
                  Tmp (Idx + 1) := Low_Word;
               end;
               Idx := Idx + 2;
         end case;
      end loop;
      return Tmp;
   end To_Registers;

end Ada_Modbus.Record_IO;
