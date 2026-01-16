--  Test_Utilities - Utilities unit tests implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.Utilities; use Ada_Modbus.Utilities;

package body Test_Utilities is

   type Utilities_Test_Case is new Test_Case with null record;

   overriding function Name (T : Utilities_Test_Case) return AUnit.Message_String is
     (AUnit.Format ("Utilities Tests"));

   overriding procedure Register_Tests (T : in out Utilities_Test_Case);

   --  Test: To_Big_Endian conversion
   procedure Test_To_Big_Endian (T : in Out Test_Case'Class);
   procedure Test_To_Big_Endian (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Result : Byte_Array (0 .. 1);
   begin
      Result := To_Big_Endian (16#1234#);
      Assert (Result (0) = 16#12#, "High byte should be 0x12");
      Assert (Result (1) = 16#34#, "Low byte should be 0x34");

      Result := To_Big_Endian (16#0000#);
      Assert (Result (0) = 16#00# and Result (1) = 16#00#, "Zero value");

      Result := To_Big_Endian (16#FFFF#);
      Assert (Result (0) = 16#FF# and Result (1) = 16#FF#, "Max value");
   end Test_To_Big_Endian;

   --  Test: From_Big_Endian (two bytes)
   procedure Test_From_Big_Endian_Bytes (T : in Out Test_Case'Class);
   procedure Test_From_Big_Endian_Bytes (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Result : Register_Value;
   begin
      Result := From_Big_Endian (16#12#, 16#34#);
      Assert (Result = 16#1234#, "Should combine to 0x1234");

      Result := From_Big_Endian (16#00#, 16#00#);
      Assert (Result = 16#0000#, "Zero value");

      Result := From_Big_Endian (16#FF#, 16#FF#);
      Assert (Result = 16#FFFF#, "Max value");
   end Test_From_Big_Endian_Bytes;

   --  Test: From_Big_Endian (array)
   procedure Test_From_Big_Endian_Array (T : in Out Test_Case'Class);
   procedure Test_From_Big_Endian_Array (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Data   : constant Byte_Array := [16#AB#, 16#CD#];
      Result : Register_Value;
   begin
      Result := From_Big_Endian (Data);
      Assert (Result = 16#ABCD#, "Should combine to 0xABCD");
   end Test_From_Big_Endian_Array;

   --  Test: High_Byte extraction
   procedure Test_High_Byte (T : in Out Test_Case'Class);
   procedure Test_High_Byte (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert (High_Byte (16#1234#) = 16#12#, "High byte of 0x1234");
      Assert (High_Byte (16#FF00#) = 16#FF#, "High byte of 0xFF00");
      Assert (High_Byte (16#00FF#) = 16#00#, "High byte of 0x00FF");
   end Test_High_Byte;

   --  Test: Low_Byte extraction
   procedure Test_Low_Byte (T : in Out Test_Case'Class);
   procedure Test_Low_Byte (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert (Low_Byte (16#1234#) = 16#34#, "Low byte of 0x1234");
      Assert (Low_Byte (16#FF00#) = 16#00#, "Low byte of 0xFF00");
      Assert (Low_Byte (16#00FF#) = 16#FF#, "Low byte of 0x00FF");
   end Test_Low_Byte;

   --  Test: Round-trip conversion
   procedure Test_Round_Trip (T : in Out Test_Case'Class);
   procedure Test_Round_Trip (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Original : constant Register_Value := 16#BEEF#;
      Encoded  : Byte_Array (0 .. 1);
      Decoded  : Register_Value;
   begin
      Encoded := To_Big_Endian (Original);
      Decoded := From_Big_Endian (Encoded);
      Assert (Decoded = Original, "Round-trip should preserve value");
   end Test_Round_Trip;

   overriding procedure Register_Tests (T : in Out Utilities_Test_Case) is
   begin
      Registration.Register_Routine (T, Test_To_Big_Endian'Access, "To_Big_Endian");
      Registration.Register_Routine (T, Test_From_Big_Endian_Bytes'Access, "From_Big_Endian (bytes)");
      Registration.Register_Routine (T, Test_From_Big_Endian_Array'Access, "From_Big_Endian (array)");
      Registration.Register_Routine (T, Test_High_Byte'Access, "High_Byte");
      Registration.Register_Routine (T, Test_Low_Byte'Access, "Low_Byte");
      Registration.Register_Routine (T, Test_Round_Trip'Access, "Round-trip conversion");
   end Register_Tests;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      S : constant AUnit.Test_Suites.Access_Test_Suite := new AUnit.Test_Suites.Test_Suite;
   begin
      S.Add_Test (new Utilities_Test_Case);
      return S;
   end Suite;

end Test_Utilities;
