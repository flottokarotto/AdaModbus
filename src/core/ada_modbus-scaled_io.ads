--  Ada_Modbus.Scaled_IO - Declarative register-to-Float mapping
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Generic package for mapping Modbus registers to an Ada record with
--  Float fields. The user declares a record type and a descriptor array
--  that specifies source register index and scaling kind per field.
--
--  Three abstraction levels (user chooses):
--    Record_IO  - Binary mapper (Unchecked_Conversion, raw integer fields)
--    Scaling    - Standalone scaling functions (manual decode)
--    Scaled_IO  - Declarative mapping (this package, Float fields)
--
--  Usage:
--    type Sensor_Data is record
--       Temperature : Float;  --  degC, register in 0.01 degC
--       Humidity    : Float;  --  %RH, register in 0.1%
--    end record;
--
--    package Sensor_IO is new Ada_Modbus.Scaled_IO (Sensor_Data);
--
--    Fields : constant Sensor_IO.Field_Descriptors :=
--      ((Reg => 0, Kind => Factor_S16, Factor => 0.01, others => <>),
--       (Reg => 1, Kind => Factor_U16, Factor => 0.1,  others => <>));
--
--    Data := Sensor_IO.From_Registers (Regs, Fields);

with Ada_Modbus.Utilities;

generic
   type Scaled_Record is private;
package Ada_Modbus.Scaled_IO
  with SPARK_Mode => On
is

   --  Record must consist entirely of 32-bit Float fields
   pragma Compile_Time_Error
     (Scaled_Record'Size mod 32 /= 0,
      "Scaled_Record size must be a multiple of 32 bits (Float)");

   --  Number of Float fields in the record
   Field_Count : constant Positive := Scaled_Record'Size / 32;

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

   type Field_Descriptors is array (1 .. Field_Count) of Field_Descriptor;

   ---------------
   --  Decode   --
   ---------------

   --  Convert register array to scaled record using field descriptors.
   --  Each field descriptor maps one output Float to source register(s).
   --  U32 kinds read Regs(Reg) as high word and Regs(Reg+1) as low word.
   --  SF kinds read the scale factor from Regs(SF_Reg).
   function From_Registers
     (Regs   : Register_Array;
      Fields : Field_Descriptors;
      Order  : Utilities.Word_Order :=
        Utilities.Big_Endian) return Scaled_Record;

end Ada_Modbus.Scaled_IO;
