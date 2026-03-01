--  Ada_Modbus.Scaled_IO - Declarative register-to-Float mapping
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Two-level package for mapping Modbus registers to an Ada record with
--  Float fields. The outer package defines descriptor types, the inner
--  generic Map binds a record type and its field descriptors at
--  instantiation time.
--
--  Usage:
--    with Ada_Modbus.Scaled_IO; use Ada_Modbus.Scaled_IO;
--
--    type Sensor_Data is record
--       Temperature : Float;  --  degC, register in 0.01 degC
--       Humidity    : Float;  --  %RH, register in 0.1%
--    end record;
--
--    Sensor_Fields : constant Field_Descriptor_Array :=
--      [(Reg => 0, Kind => Factor_S16, Factor => 0.01, others => <>),
--       (Reg => 1, Kind => Factor_U16, Factor => 0.1,  others => <>)];
--
--    package Sensor_IO is new Ada_Modbus.Scaled_IO.Map
--      (Sensor_Data, Sensor_Fields);
--
--    Data := Sensor_IO.From_Registers (Regs);

with Ada_Modbus.Utilities;

package Ada_Modbus.Scaled_IO
  with SPARK_Mode => On
is

   ---------------------
   --  Scaling Kinds  --
   ---------------------

   type Scale_Kind is (
      --  Raw conversion (no scaling)
      Raw_U16,               --  Float (unsigned_16)
      Raw_S16,               --  Float (signed_16)
      Raw_U32,               --  Float (unsigned_32), consumes 2 registers

      --  Scale factor: value * 10^SF, SF read from SF_Reg
      SF_U16,                --  unsigned * 10^SF
      SF_S16,                --  signed * 10^SF
      SF_U32,                --  unsigned_32 * 10^SF, consumes 2 registers

      --  Fixed multiplier: value * Factor
      Factor_U16,            --  unsigned * Factor
      Factor_S16,            --  signed * Factor
      Factor_U32,            --  unsigned_32 * Factor, consumes 2 registers

      --  Affine: value * Factor + Offset
      Affine_U16,            --  unsigned * Factor + Offset
      Affine_S16);           --  signed * Factor + Offset

   -------------------------
   --  Field Descriptor   --
   -------------------------

   type Field_Descriptor is record
      Reg    : Natural    := 0;        --  Source register index in input array
      Kind   : Scale_Kind := Raw_U16;  --  Scaling function to apply
      SF_Reg : Natural    := 0;        --  Scale factor register index (SF_* kinds)
      Factor : Float      := 1.0;      --  Multiplier (Factor_*/Affine_* kinds)
      Offset : Float      := 0.0;      --  Offset (Affine_* kinds)
   end record;

   type Field_Descriptor_Array is array (Positive range <>) of Field_Descriptor;

   -------------------------
   --  Generic Map        --
   -------------------------

   --  Bind a record type and its field descriptors at instantiation time.
   --  Fields'Length must equal Scaled_Record'Size / 32 (one descriptor per
   --  Float field in the record).
   generic
      type Scaled_Record is private;
      Fields : Field_Descriptor_Array;
   package Map
     with SPARK_Mode => On
   is

      --  Record must consist entirely of 32-bit Float fields
      pragma Compile_Time_Error
        (Scaled_Record'Size mod 32 /= 0,
         "Scaled_Record size must be a multiple of 32 bits (Float)");

      --  Number of Float fields in the record
      Field_Count : constant Positive := Scaled_Record'Size / 32;

      ---------------
      --  Decode   --
      ---------------

      --  Convert register array to scaled record using the bound descriptors.
      --  Each descriptor maps one output Float to source register(s).
      --  U32 kinds read Regs(Reg) as high word and Regs(Reg+1) as low word.
      --  SF kinds read the scale factor from Regs(SF_Reg).
      function From_Registers
        (Regs  : Register_Array;
         Order : Utilities.Word_Order :=
           Utilities.Big_Endian) return Scaled_Record;

   end Map;

end Ada_Modbus.Scaled_IO;
