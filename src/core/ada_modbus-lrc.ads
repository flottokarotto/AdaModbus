--  Ada_Modbus.LRC - LRC calculation for Modbus ASCII
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  LRC (Longitudinal Redundancy Check) is calculated as:
--  1. Sum all bytes
--  2. Take two's complement (negate)
--  The result is transmitted as two ASCII hex characters

package Ada_Modbus.LRC
  with SPARK_Mode => On
is

   pragma Pure;

   subtype LRC_Value is Byte;

   --  Calculate LRC for a byte array
   function Calculate (Data : Byte_Array) return LRC_Value;

   --  Verify LRC (data includes LRC byte at the end)
   --  Returns True if LRC is valid (sum of all bytes including LRC = 0)
   function Verify (Data : Byte_Array) return Boolean
     with Pre => Data'Length >= 1;

end Ada_Modbus.LRC;
