--  Ada_Modbus.Scaling - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

package body Ada_Modbus.Scaling
  with SPARK_Mode => On
is

   ---------------
   -- To_Signed --
   ---------------

   function To_Signed (Value : Register_Value) return Integer is
   begin
      if Value > 32767 then
         return Integer (Value) - 65536;
      else
         return Integer (Value);
      end if;
   end To_Signed;

   -----------
   -- To_SF --
   -----------

   function To_SF (Value : Register_Value) return Scale_Factor is
      Raw : constant Integer := To_Signed (Value);
   begin
      if Raw in -10 .. 10 then
         return Scale_Factor (Raw);
      else
         return 0;
      end if;
   end To_SF;

   -----------
   -- Apply --
   -----------

   function Apply (Value : Register_Value; SF : Scale_Factor) return Float is
   begin
      return Float (Value) * Scale_Multipliers (SF);
   end Apply;

   ------------------
   -- Apply_Signed --
   ------------------

   function Apply_Signed
     (Value : Register_Value; SF : Scale_Factor) return Float
   is
      Signed_Val : constant Integer := To_Signed (Value);
      Float_Val  : constant Float := Float (Signed_Val);
   begin
      --  Signed_Val in -32768..32767 (postcondition of To_Signed)
      --  Scale_Multipliers in 1E-10..1E10
      --  Max: 32768 * 1E10 = 3.2768E14 < Float'Last (3.4E38)
      pragma Assert (Float_Val >= -32768.0 and then Float_Val <= 32767.0);
      return Float_Val * Scale_Multipliers (SF);
   end Apply_Signed;

   ---------------
   -- Apply_U32 --
   ---------------

   function Apply_U32
     (High_Word : Register_Value;
      Low_Word  : Register_Value;
      SF        : Scale_Factor;
      Order     : Utilities.Word_Order :=
        Utilities.Big_Endian) return Float
   is
      Val : constant Interfaces.Unsigned_32 :=
        Utilities.To_Unsigned_32 (High_Word, Low_Word, Order);
      Result : Float;
   begin
      --  Unsigned_32'Last * 1E10 = 4.29E19 < Float'Last (3.4E38)
      Result := Float (Val) * Scale_Multipliers (SF);
      return Result;
   end Apply_U32;

   -----------
   -- Scale --
   -----------

   function Scale (Value : Register_Value; Factor : Float) return Float is
   begin
      --  Pre: abs(Factor) <= 1E20, Value <= 65535
      --  Max: 65535 * 1E20 = 6.55E24 < Float'Last (3.4E38)
      return Float (Value) * Factor;
   end Scale;

   ------------------
   -- Scale_Signed --
   ------------------

   function Scale_Signed (Value : Register_Value; Factor : Float) return Float
   is
      Signed_Val : constant Integer := To_Signed (Value);
   begin
      --  Pre: abs(Factor) <= 1E20, Signed_Val in -32768..32767
      --  Max: 32768 * 1E20 = 3.28E24 < Float'Last (3.4E38)
      pragma Assert (Signed_Val in -32768 .. 32767);
      return Float (Signed_Val) * Factor;
   end Scale_Signed;

   ---------------
   -- Scale_U32 --
   ---------------

   function Scale_U32
     (High_Word : Register_Value;
      Low_Word  : Register_Value;
      Factor    : Float;
      Order     : Utilities.Word_Order :=
        Utilities.Big_Endian) return Float
   is
      Val : constant Interfaces.Unsigned_32 :=
        Utilities.To_Unsigned_32 (High_Word, Low_Word, Order);
   begin
      --  Pre: abs(Factor) <= 1E20, Val <= Unsigned_32'Last (4.29E9)
      --  Max: 4.29E9 * 1E20 = 4.29E29 < Float'Last (3.4E38)
      return Float (Val) * Factor;
   end Scale_U32;

   ------------
   -- Affine --
   ------------

   function Affine
     (Value : Register_Value; Factor, Offset : Float) return Float
   is
   begin
      --  Pre: abs(Factor) <= 1E20, abs(Offset) <= 1E30
      --  Max: 65535 * 1E20 + 1E30 ~ 1E30 < Float'Last (3.4E38)
      return Float (Value) * Factor + Offset;
   end Affine;

   -------------------
   -- Affine_Signed --
   -------------------

   function Affine_Signed
     (Value : Register_Value; Factor, Offset : Float) return Float
   is
      Signed_Val : constant Integer := To_Signed (Value);
   begin
      --  Pre: abs(Factor) <= 1E20, abs(Offset) <= 1E30
      --  Max: 32768 * 1E20 + 1E30 ~ 1E30 < Float'Last (3.4E38)
      pragma Assert (Signed_Val in -32768 .. 32767);
      return Float (Signed_Val) * Factor + Offset;
   end Affine_Signed;

   ---------------
   -- Valid_U16 --
   ---------------

   function Valid_U16 (V : Register_Value) return Boolean is
   begin
      return V /= 16#FFFF# and then V /= 16#7FFF#;
   end Valid_U16;

   ---------------
   -- Valid_S16 --
   ---------------

   function Valid_S16 (V : Register_Value) return Boolean is
   begin
      return V /= 16#8000# and then V /= 16#FFFF#;
   end Valid_S16;

end Ada_Modbus.Scaling;
