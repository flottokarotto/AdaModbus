--  Test_CRC - CRC-16 unit tests implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Interfaces; use Interfaces;
with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.CRC16;

package body Test_CRC is

   type CRC_Test_Case is new Test_Case with null record;

   overriding function Name (T : CRC_Test_Case) return AUnit.Message_String is
     (AUnit.Format ("CRC-16 Tests"));

   overriding procedure Register_Tests (T : in out CRC_Test_Case);

   --  Test: Known CRC value
   --  Example from Modbus spec: Address=01, FC=03, StartAddr=0000, Qty=000A
   --  Data: 01 03 00 00 00 0A -> CRC: C5 CD
   procedure Test_Known_CRC (T : in out Test_Case'Class);
   procedure Test_Known_CRC (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Data : constant Byte_Array := [16#01#, 16#03#, 16#00#, 16#00#, 16#00#, 16#0A#];
      CRC  : CRC16.CRC_Value;
   begin
      CRC := CRC16.Calculate (Data);
      --  CRC should be 0xCDC5 (stored as C5 CD in LSB first order)
      Assert (CRC = 16#CDC5#, "CRC mismatch, expected 0xCDC5, got " & CRC'Image);
   end Test_Known_CRC;

   --  Test: Verify function with valid CRC
   procedure Test_Verify_Valid (T : in Out Test_Case'Class);
   procedure Test_Verify_Valid (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      --  Data with appended CRC (LSB first)
      Data : constant Byte_Array :=
        [16#01#, 16#03#, 16#00#, 16#00#, 16#00#, 16#0A#, 16#C5#, 16#CD#];
   begin
      Assert (CRC16.Verify (Data), "CRC verification should pass");
   end Test_Verify_Valid;

   --  Test: Verify function with invalid CRC
   procedure Test_Verify_Invalid (T : in Out Test_Case'Class);
   procedure Test_Verify_Invalid (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      --  Data with wrong CRC
      Data : constant Byte_Array :=
        [16#01#, 16#03#, 16#00#, 16#00#, 16#00#, 16#0A#, 16#00#, 16#00#];
   begin
      Assert (not CRC16.Verify (Data), "CRC verification should fail");
   end Test_Verify_Invalid;

   --  Test: Empty data
   procedure Test_Empty_Data (T : in Out Test_Case'Class);
   procedure Test_Empty_Data (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Data : constant Byte_Array (1 .. 0) := [];
      CRC  : CRC16.CRC_Value;
   begin
      CRC := CRC16.Calculate (Data);
      --  CRC of empty data with initial 0xFFFF should be 0xFFFF
      Assert (CRC = 16#FFFF#, "CRC of empty data should be 0xFFFF");
   end Test_Empty_Data;

   --  Test: Extract CRC from frame
   procedure Test_Extract_CRC (T : in Out Test_Case'Class);
   procedure Test_Extract_CRC (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      --  Data with CRC (LSB first): C5 CD = 0xCDC5
      Data : constant Byte_Array :=
        [16#01#, 16#03#, 16#00#, 16#00#, 16#00#, 16#0A#, 16#C5#, 16#CD#];
      CRC  : CRC16.CRC_Value;
   begin
      CRC := CRC16.Extract (Data);
      Assert (CRC = 16#CDC5#, "Extracted CRC should be 0xCDC5");
   end Test_Extract_CRC;

   --  Test: Append CRC to buffer
   procedure Test_Append_CRC (T : in Out Test_Case'Class);
   procedure Test_Append_CRC (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer : Byte_Array (0 .. 9) := [others => 0];
      Length : Natural := 6;
      CRC    : constant CRC16.CRC_Value := 16#CDC5#;
   begin
      --  Setup data
      Buffer (0) := 16#01#;
      Buffer (1) := 16#03#;
      Buffer (2) := 16#00#;
      Buffer (3) := 16#00#;
      Buffer (4) := 16#00#;
      Buffer (5) := 16#0A#;

      CRC16.Append (Buffer, Length, CRC);

      Assert (Length = 8, "Length should be 8 after append");
      Assert (Buffer (6) = 16#C5#, "CRC low byte should be 0xC5");
      Assert (Buffer (7) = 16#CD#, "CRC high byte should be 0xCD");
   end Test_Append_CRC;

   --  Test: Single byte CRC
   procedure Test_Single_Byte_CRC (T : in Out Test_Case'Class);
   procedure Test_Single_Byte_CRC (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Data : constant Byte_Array := [16#00#];
      CRC  : CRC16.CRC_Value;
   begin
      CRC := CRC16.Calculate (Data);
      --  CRC of single byte 0x00 starting from 0xFFFF
      Assert (CRC /= 16#FFFF#, "CRC of 0x00 should not be 0xFFFF");
   end Test_Single_Byte_CRC;

   --  Test: CRC calculation is consistent
   procedure Test_CRC_Consistency (T : in Out Test_Case'Class);
   procedure Test_CRC_Consistency (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Data : constant Byte_Array := [16#01#, 16#03#, 16#00#, 16#00#, 16#00#, 16#0A#];
      CRC1, CRC2 : CRC16.CRC_Value;
   begin
      CRC1 := CRC16.Calculate (Data);
      CRC2 := CRC16.Calculate (Data);
      Assert (CRC1 = CRC2, "CRC calculation should be consistent");
   end Test_CRC_Consistency;

   overriding procedure Register_Tests (T : in out CRC_Test_Case) is
   begin
      Registration.Register_Routine (T, Test_Known_CRC'Access, "Known CRC value");
      Registration.Register_Routine (T, Test_Verify_Valid'Access, "Verify valid CRC");
      Registration.Register_Routine (T, Test_Verify_Invalid'Access, "Verify invalid CRC");
      Registration.Register_Routine (T, Test_Empty_Data'Access, "Empty data CRC");
      Registration.Register_Routine (T, Test_Extract_CRC'Access, "Extract CRC");
      Registration.Register_Routine (T, Test_Append_CRC'Access, "Append CRC");
      Registration.Register_Routine (T, Test_Single_Byte_CRC'Access, "Single byte CRC");
      Registration.Register_Routine (T, Test_CRC_Consistency'Access, "CRC consistency");
   end Register_Tests;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      S : constant AUnit.Test_Suites.Access_Test_Suite := new AUnit.Test_Suites.Test_Suite;
   begin
      S.Add_Test (new CRC_Test_Case);
      return S;
   end Suite;

end Test_CRC;
