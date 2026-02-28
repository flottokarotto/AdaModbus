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

generic
   type Register_Map is private;
package Ada_Modbus.Record_IO is

   pragma Compile_Time_Error
     (Register_Map'Size mod 16 /= 0,
      "Register_Map size must be a multiple of 16 bits");

   Register_Size : constant Register_Count :=
     Register_Count (Register_Map'Size / 16);

   subtype Map_Registers is Register_Array (0 .. Natural (Register_Size) - 1);

   function To_Registers (Map : Register_Map) return Map_Registers;
   function From_Registers (Regs : Map_Registers) return Register_Map;

end Ada_Modbus.Record_IO;
