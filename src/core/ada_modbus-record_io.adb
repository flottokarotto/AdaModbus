--  Ada_Modbus.Record_IO - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with Ada.Unchecked_Conversion;

package body Ada_Modbus.Record_IO is

   function To_Registers (Map : Register_Map) return Map_Registers is
      function Convert is new Ada.Unchecked_Conversion
        (Source => Register_Map, Target => Map_Registers);
   begin
      return Convert (Map);
   end To_Registers;

   function From_Registers (Regs : Map_Registers) return Register_Map is
      function Convert is new Ada.Unchecked_Conversion
        (Source => Map_Registers, Target => Register_Map);
   begin
      return Convert (Regs);
   end From_Registers;

end Ada_Modbus.Record_IO;
