--  Ada_Modbus.LRC - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

package body Ada_Modbus.LRC
  with SPARK_Mode => On
is

   ---------------
   -- Calculate --
   ---------------

   function Calculate (Data : Byte_Array) return LRC_Value is
      Sum : Byte := 0;
   begin
      for B of Data loop
         Sum := Sum + B;
      end loop;
      --  Two's complement (negate)
      return (not Sum) + 1;
   end Calculate;

   ------------
   -- Verify --
   ------------

   function Verify (Data : Byte_Array) return Boolean is
      Sum : Byte := 0;
   begin
      for B of Data loop
         Sum := Sum + B;
      end loop;
      --  If LRC is correct, sum of all bytes including LRC equals 0
      return Sum = 0;
   end Verify;

end Ada_Modbus.LRC;
