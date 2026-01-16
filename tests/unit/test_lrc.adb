--  Test_LRC - LRC unit tests implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.LRC;

package body Test_LRC is

   type LRC_Test_Case is new Test_Case with null record;

   overriding function Name (T : LRC_Test_Case) return AUnit.Message_String is
     (AUnit.Format ("LRC Tests"));

   overriding procedure Register_Tests (T : in Out LRC_Test_Case);

   --  Test: Known LRC value
   --  Example: Address=01, FC=03, StartAddr=0000, Qty=000A
   --  Sum = 01+03+00+00+00+0A = 0E, LRC = -0E = F2
   procedure Test_Known_LRC (T : in Out Test_Case'Class);
   procedure Test_Known_LRC (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Data : constant Byte_Array := [16#01#, 16#03#, 16#00#, 16#00#, 16#00#, 16#0A#];
      LRC_Val : LRC.LRC_Value;
   begin
      LRC_Val := LRC.Calculate (Data);
      Assert (LRC_Val = 16#F2#, "LRC mismatch, expected 0xF2, got " & LRC_Val'Image);
   end Test_Known_LRC;

   --  Test: Verify function with valid LRC
   procedure Test_Verify_Valid (T : in Out Test_Case'Class);
   procedure Test_Verify_Valid (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      --  Data with appended LRC
      Data : constant Byte_Array :=
        [16#01#, 16#03#, 16#00#, 16#00#, 16#00#, 16#0A#, 16#F2#];
   begin
      Assert (LRC.Verify (Data), "LRC verification should pass");
   end Test_Verify_Valid;

   --  Test: Verify function with invalid LRC
   procedure Test_Verify_Invalid (T : in Out Test_Case'Class);
   procedure Test_Verify_Invalid (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      --  Data with wrong LRC
      Data : constant Byte_Array :=
        [16#01#, 16#03#, 16#00#, 16#00#, 16#00#, 16#0A#, 16#00#];
   begin
      Assert (not LRC.Verify (Data), "LRC verification should fail");
   end Test_Verify_Invalid;

   --  Test: Single byte
   procedure Test_Single_Byte (T : in Out Test_Case'Class);
   procedure Test_Single_Byte (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Data : constant Byte_Array := [16#05#];
      LRC_Val : LRC.LRC_Value;
   begin
      LRC_Val := LRC.Calculate (Data);
      --  LRC of 0x05 = -0x05 = 0xFB
      Assert (LRC_Val = 16#FB#, "LRC of 0x05 should be 0xFB");
   end Test_Single_Byte;

   overriding procedure Register_Tests (T : in Out LRC_Test_Case) is
   begin
      Registration.Register_Routine (T, Test_Known_LRC'Access, "Known LRC value");
      Registration.Register_Routine (T, Test_Verify_Valid'Access, "Verify valid LRC");
      Registration.Register_Routine (T, Test_Verify_Invalid'Access, "Verify invalid LRC");
      Registration.Register_Routine (T, Test_Single_Byte'Access, "Single byte LRC");
   end Register_Tests;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      S : constant AUnit.Test_Suites.Access_Test_Suite := new AUnit.Test_Suites.Test_Suite;
   begin
      S.Add_Test (new LRC_Test_Case);
      return S;
   end Suite;

end Test_LRC;
