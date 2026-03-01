--  Ada_Modbus.Scaling - Register value scaling utilities
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Standalone scaling toolkit for converting raw Modbus register values
--  to physical Float values. Common patterns:
--
--  - Scale Factor:  value * 10^SF  (SunSpec, many industrial devices)
--  - Fixed-Point:   value * Factor  (0.1, 0.01, Pn/10000, ...)
--  - Affine:        value * Factor + Offset  (temperature sensors, ...)
--
--  All functions are pure and SPARK-compatible. Can be used standalone
--  or as the basis for Scaled_IO declarative register mapping.

with Ada_Modbus.Utilities;

package Ada_Modbus.Scaling
  with SPARK_Mode => On
is

   pragma Pure;

   --  Scale factor type (SunSpec convention: -10 .. +10)
   type Scale_Factor is range -10 .. 10;

   --  Pre-computed lookup table for 10^SF (avoids runtime exponentiation)
   Scale_Multipliers : constant array (Scale_Factor) of Float :=
     [-10 => 1.0E-10,
      -9  => 1.0E-9,
      -8  => 1.0E-8,
      -7  => 1.0E-7,
      -6  => 1.0E-6,
      -5  => 1.0E-5,
      -4  => 1.0E-4,
      -3  => 1.0E-3,
      -2  => 1.0E-2,
      -1  => 1.0E-1,
       0  => 1.0,
       1  => 1.0E1,
       2  => 1.0E2,
       3  => 1.0E3,
       4  => 1.0E4,
       5  => 1.0E5,
       6  => 1.0E6,
       7  => 1.0E7,
       8  => 1.0E8,
       9  => 1.0E9,
       10 => 1.0E10];

   -------------------------
   --  Type Conversion    --
   -------------------------

   --  Convert unsigned register to signed 16-bit integer
   function To_Signed (Value : Register_Value) return Integer
     with Inline,
          Post => To_Signed'Result in -32768 .. 32767;

   --  Convert unsigned register to scale factor (clamps invalid to 0)
   function To_SF (Value : Register_Value) return Scale_Factor
     with Inline;

   ----------------------------
   --  Scale Factor Scaling  --
   ----------------------------

   --  Unsigned value * 10^SF
   function Apply (Value : Register_Value; SF : Scale_Factor) return Float
     with Inline;

   --  Signed value * 10^SF
   function Apply_Signed (Value : Register_Value; SF : Scale_Factor) return Float
     with Inline;

   --  Unsigned 32-bit value * 10^SF
   function Apply_U32
     (High_Word : Register_Value;
      Low_Word  : Register_Value;
      SF        : Scale_Factor;
      Order     : Utilities.Word_Order :=
        Utilities.Big_Endian) return Float
     with Inline;

   ----------------------------
   --  Fixed-Point Scaling   --
   ----------------------------

   --  Unsigned value * Factor
   function Scale (Value : Register_Value; Factor : Float) return Float
     with Inline,
          Pre => Factor >= -1.0E20 and then Factor <= 1.0E20;

   --  Signed value * Factor
   function Scale_Signed (Value : Register_Value; Factor : Float) return Float
     with Inline,
          Pre => Factor >= -1.0E20 and then Factor <= 1.0E20;

   --  Unsigned 32-bit value * Factor
   function Scale_U32
     (High_Word : Register_Value;
      Low_Word  : Register_Value;
      Factor    : Float;
      Order     : Utilities.Word_Order :=
        Utilities.Big_Endian) return Float
     with Inline,
          Pre => Factor >= -1.0E20 and then Factor <= 1.0E20;

   -----------------------
   --  Affine Scaling   --
   -----------------------

   --  Unsigned value * Factor + Offset
   function Affine (Value : Register_Value; Factor, Offset : Float) return Float
     with Inline,
          Pre => Factor >= -1.0E20 and then Factor <= 1.0E20
                 and then Offset >= -1.0E30 and then Offset <= 1.0E30;

   --  Signed value * Factor + Offset
   function Affine_Signed
     (Value : Register_Value; Factor, Offset : Float) return Float
     with Inline,
          Pre => Factor >= -1.0E20 and then Factor <= 1.0E20
                 and then Offset >= -1.0E30 and then Offset <= 1.0E30;

   -------------------------
   --  Validity Checks   --
   -------------------------

   --  Check for SunSpec "not implemented" markers
   function Valid_U16 (V : Register_Value) return Boolean
     with Inline;
   --  True when V /= 16#FFFF# and V /= 16#7FFF#

   function Valid_S16 (V : Register_Value) return Boolean
     with Inline;
   --  True when V /= 16#8000# and V /= 16#FFFF#

end Ada_Modbus.Scaling;
