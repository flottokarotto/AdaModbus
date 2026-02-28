--  Test_Record_IO - Record_IO unit tests implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.Record_IO;

package body Test_Record_IO is

   --  Simple 2-register record
   type Two_Reg is record
      A : Register_Value;
      B : Register_Value;
   end record with Size => 2 * 16;

   for Two_Reg use record
      A at 0 range 0 .. 15;
      B at 2 range 0 .. 15;
   end record;

   package Two_IO is new Ada_Modbus.Record_IO (Two_Reg);

   --  4-register record (simulates 32-bit fields split across registers)
   type Four_Reg is record
      W0 : Register_Value;
      W1 : Register_Value;
      W2 : Register_Value;
      W3 : Register_Value;
   end record with Size => 4 * 16;

   for Four_Reg use record
      W0 at 0 range 0 .. 15;
      W1 at 2 range 0 .. 15;
      W2 at 4 range 0 .. 15;
      W3 at 6 range 0 .. 15;
   end record;

   package Four_IO is new Ada_Modbus.Record_IO (Four_Reg);

   type Record_IO_Test_Case is new Test_Case with null record;

   overriding function Name (T : Record_IO_Test_Case)
     return AUnit.Message_String is
     (AUnit.Format ("Record_IO Tests"));

   overriding procedure Register_Tests (T : in out Record_IO_Test_Case);

   --  Test: Register_Size and Map_Registers subtype
   procedure Test_Register_Size (T : in out Test_Case'Class);
   procedure Test_Register_Size (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Two_Size  : constant Natural := Natural (Two_IO.Register_Size);
      Four_Size : constant Natural := Natural (Four_IO.Register_Size);
      Two_Regs  : Two_IO.Map_Registers := [others => 0];
      Four_Regs : Four_IO.Map_Registers := [others => 0];
   begin
      Assert (Two_Size = 2, "Two_Reg should be 2 registers");
      Assert (Four_Size = 4, "Four_Reg should be 4 registers");
      Assert (Two_Regs'Length = 2, "Map_Registers length should be 2");
      Assert (Four_Regs'Length = 4, "Map_Registers length should be 4");
   end Test_Register_Size;

   --  Test: Round-trip conversion preserves data
   procedure Test_Round_Trip (T : in out Test_Case'Class);
   procedure Test_Round_Trip (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Original : constant Two_Reg := (A => 16#1234#, B => 16#5678#);
      Result   : Two_Reg;
   begin
      Result := Two_IO.From_Registers (Two_IO.To_Registers (Original));
      Assert (Result.A = Original.A,
              "Round-trip: A mismatch");
      Assert (Result.B = Original.B,
              "Round-trip: B mismatch");
   end Test_Round_Trip;

   --  Test: Known register values map to expected fields
   procedure Test_Known_Values (T : in out Test_Case'Class);
   procedure Test_Known_Values (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Regs : constant Two_IO.Map_Registers :=
        [0 => 16#AABB#, 1 => 16#CCDD#];
      Data : Two_Reg;
   begin
      Data := Two_IO.From_Registers (Regs);
      Assert (Data.A = 16#AABB#,
              "A should be 16#AABB#, got " & Register_Value'Image (Data.A));
      Assert (Data.B = 16#CCDD#,
              "B should be 16#CCDD#, got " & Register_Value'Image (Data.B));
   end Test_Known_Values;

   --  Test: To_Registers produces expected register values
   procedure Test_To_Registers (T : in out Test_Case'Class);
   procedure Test_To_Registers (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Data : constant Two_Reg := (A => 16#1111#, B => 16#2222#);
      Regs : Two_IO.Map_Registers;
   begin
      Regs := Two_IO.To_Registers (Data);
      Assert (Regs (0) = 16#1111#,
              "Reg 0 should be 16#1111#");
      Assert (Regs (1) = 16#2222#,
              "Reg 1 should be 16#2222#");
   end Test_To_Registers;

   --  Test: 4-register round-trip
   procedure Test_Multi_Register (T : in out Test_Case'Class);
   procedure Test_Multi_Register (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Original : constant Four_Reg :=
        (W0 => 16#0001#, W1 => 16#0002#,
         W2 => 16#FFFE#, W3 => 16#FFFF#);
      Regs   : Four_IO.Map_Registers;
      Result : Four_Reg;
   begin
      Regs := Four_IO.To_Registers (Original);
      Assert (Regs (0) = 16#0001#, "Reg 0 mismatch");
      Assert (Regs (1) = 16#0002#, "Reg 1 mismatch");
      Assert (Regs (2) = 16#FFFE#, "Reg 2 mismatch");
      Assert (Regs (3) = 16#FFFF#, "Reg 3 mismatch");

      Result := Four_IO.From_Registers (Regs);
      Assert (Result.W0 = Original.W0, "W0 mismatch");
      Assert (Result.W1 = Original.W1, "W1 mismatch");
      Assert (Result.W2 = Original.W2, "W2 mismatch");
      Assert (Result.W3 = Original.W3, "W3 mismatch");
   end Test_Multi_Register;

   --  Test: Zero values
   procedure Test_Zero_Values (T : in out Test_Case'Class);
   procedure Test_Zero_Values (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Regs : constant Two_IO.Map_Registers := [0 => 0, 1 => 0];
      Data : Two_Reg;
   begin
      Data := Two_IO.From_Registers (Regs);
      Assert (Data.A = 0, "A should be 0");
      Assert (Data.B = 0, "B should be 0");
   end Test_Zero_Values;

   --  Test: Max values
   procedure Test_Max_Values (T : in out Test_Case'Class);
   procedure Test_Max_Values (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Regs : constant Two_IO.Map_Registers :=
        [0 => 16#FFFF#, 1 => 16#FFFF#];
      Data : Two_Reg;
   begin
      Data := Two_IO.From_Registers (Regs);
      Assert (Data.A = 16#FFFF#, "A should be 16#FFFF#");
      Assert (Data.B = 16#FFFF#, "B should be 16#FFFF#");
   end Test_Max_Values;

   overriding procedure Register_Tests (T : in out Record_IO_Test_Case) is
   begin
      Registration.Register_Routine (T, Test_Register_Size'Access,
                                     "Register_Size");
      Registration.Register_Routine (T, Test_Round_Trip'Access,
                                     "Round-trip conversion");
      Registration.Register_Routine (T, Test_Known_Values'Access,
                                     "Known register values");
      Registration.Register_Routine (T, Test_To_Registers'Access,
                                     "To_Registers");
      Registration.Register_Routine (T, Test_Multi_Register'Access,
                                     "Multi-register round-trip");
      Registration.Register_Routine (T, Test_Zero_Values'Access,
                                     "Zero values");
      Registration.Register_Routine (T, Test_Max_Values'Access,
                                     "Max values");
   end Register_Tests;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      S : constant AUnit.Test_Suites.Access_Test_Suite :=
        new AUnit.Test_Suites.Test_Suite;
   begin
      S.Add_Test (new Record_IO_Test_Case);
      return S;
   end Suite;

end Test_Record_IO;
