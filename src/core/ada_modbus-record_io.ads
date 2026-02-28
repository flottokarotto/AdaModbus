--  Ada_Modbus.Record_IO - Record-based register mapping
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Generic package for mapping Ada records with representation clauses
--  to/from Modbus register arrays. The record type must have a size
--  that is a multiple of 16 bits (register width).
--
--  Usage:
--    type Sensor_Data is record
--       Temperature : Register_Value;
--       Humidity    : Register_Value;
--    end record with Size => 2 * 16;
--
--    for Sensor_Data use record
--       Temperature at 0 range 0 .. 15;
--       Humidity    at 2 range 0 .. 15;
--    end record;
--
--    package Sensor_IO is new Ada_Modbus.Record_IO (Sensor_Data);

with Ada_Modbus.Utilities;

generic
   type Register_Map is private;
package Ada_Modbus.Record_IO
  with SPARK_Mode => On
is

   pragma Compile_Time_Error
     (Register_Map'Size mod 16 /= 0,
      "Register_Map size must be a multiple of 16 bits");

   Register_Size : constant Register_Count :=
     Register_Count (Register_Map'Size / 16);

   subtype Map_Registers is Register_Array (0 .. Natural (Register_Size) - 1);

   --  Simple conversion (16-bit fields only)
   function To_Registers (Map : Register_Map) return Map_Registers;
   function From_Registers (Regs : Map_Registers) return Register_Map;

   --  Register indices of 32-bit fields (each index marks the first
   --  of two consecutive registers forming a 32-bit value)
   type Word_Pair_Indices is array (Positive range <>) of Natural;

   --  Convert registers with 32-bit field word order adjustment.
   --  Pairs specifies register indices of 32-bit fields (Float32/U32).
   function From_Registers
     (Regs  : Map_Registers;
      Pairs : Word_Pair_Indices;
      Order : Utilities.Word_Order :=
        Utilities.Big_Endian) return Register_Map
     with Pre => (for all I of Pairs => I + 1 <= Natural (Register_Size) - 1);

   function To_Registers
     (Map   : Register_Map;
      Pairs : Word_Pair_Indices;
      Order : Utilities.Word_Order :=
        Utilities.Big_Endian) return Map_Registers
     with Pre => (for all I of Pairs => I + 1 <= Natural (Register_Size) - 1);

end Ada_Modbus.Record_IO;
