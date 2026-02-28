--  Test_Record_IO - Record_IO unit tests implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.Record_IO;
with Ada_Modbus.Utilities; use Ada_Modbus.Utilities;
with Interfaces; use type Interfaces.IEEE_Float_32;

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

   --  ===== Float32 / 32-bit word order tests =====

   --  Record with a single Float32 field (2 registers)
   type Float_Reg is record
      Value : IEEE_Float_32;
   end record with Size => 2 * 16;

   for Float_Reg use record
      Value at 0 range 0 .. 31;
   end record;

   package Float_IO is new Ada_Modbus.Record_IO (Float_Reg);

   --  Record with mixed Float32 and Register_Value fields (5 registers)
   type Mixed_Reg is record
      Temperature : IEEE_Float_32;   --  Register 0-1
      Pressure    : IEEE_Float_32;   --  Register 2-3
      Status      : Register_Value;  --  Register 4
   end record with Size => 5 * 16;

   for Mixed_Reg use record
      Temperature at 0 range 0 .. 31;
      Pressure    at 4 range 0 .. 31;
      Status      at 8 range 0 .. 15;
   end record;

   package Mixed_IO is new Ada_Modbus.Record_IO (Mixed_Reg);

   --  Test: Float32 From_Registers with Big_Endian word order
   --  IEEE 754: 50.0 = 0x42480000 => High=0x4248, Low=0x0000
   procedure Test_Float32_From_Registers (T : in out Test_Case'Class);
   procedure Test_Float32_From_Registers (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Regs : constant Float_IO.Map_Registers :=
        [0 => 16#4248#, 1 => 16#0000#];
      Data : Float_Reg;
   begin
      Data := Float_IO.From_Registers
        (Regs, Pairs => [1 => 0], Order => Big_Endian);
      Assert (Data.Value = 50.0,
              "Float should be 50.0, got " &
              IEEE_Float_32'Image (Data.Value));
   end Test_Float32_From_Registers;

   --  Test: Float32 To_Registers with Big_Endian word order
   procedure Test_Float32_To_Registers (T : in out Test_Case'Class);
   procedure Test_Float32_To_Registers (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Data : constant Float_Reg := (Value => 50.0);
      Regs : Float_IO.Map_Registers;
   begin
      Regs := Float_IO.To_Registers
        (Data, Pairs => [1 => 0], Order => Big_Endian);
      Assert (Regs (0) = 16#4248#,
              "Reg 0 should be 16#4248#, got " &
              Register_Value'Image (Regs (0)));
      Assert (Regs (1) = 16#0000#,
              "Reg 1 should be 16#0000#, got " &
              Register_Value'Image (Regs (1)));
   end Test_Float32_To_Registers;

   --  Test: Float32 round-trip preserves value
   procedure Test_Float32_Round_Trip (T : in out Test_Case'Class);
   procedure Test_Float32_Round_Trip (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Original : constant Float_Reg := (Value => -123.456);
      Regs   : Float_IO.Map_Registers;
      Result : Float_Reg;
   begin
      Regs := Float_IO.To_Registers
        (Original, Pairs => [1 => 0], Order => Big_Endian);
      Result := Float_IO.From_Registers
        (Regs, Pairs => [1 => 0], Order => Big_Endian);
      Assert (Result.Value = Original.Value,
              "Round-trip: expected " &
              IEEE_Float_32'Image (Original.Value) & ", got " &
              IEEE_Float_32'Image (Result.Value));
   end Test_Float32_Round_Trip;

   --  Test: Mixed record with Float32 and Register_Value
   procedure Test_Mixed_Float_Register (T : in out Test_Case'Class);
   procedure Test_Mixed_Float_Register (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      --  50.0 = 0x42480000, 100.0 = 0x42C80000
      Regs : constant Mixed_IO.Map_Registers :=
        [0 => 16#4248#, 1 => 16#0000#,
         2 => 16#42C8#, 3 => 16#0000#,
         4 => 16#ABCD#];
      Data : Mixed_Reg;
   begin
      Data := Mixed_IO.From_Registers
        (Regs, Pairs => [1 => 0, 2 => 2], Order => Big_Endian);
      Assert (Data.Temperature = 50.0,
              "Temperature should be 50.0, got " &
              IEEE_Float_32'Image (Data.Temperature));
      Assert (Data.Pressure = 100.0,
              "Pressure should be 100.0, got " &
              IEEE_Float_32'Image (Data.Pressure));
      Assert (Data.Status = 16#ABCD#,
              "Status should be 16#ABCD#, got " &
              Register_Value'Image (Data.Status));
   end Test_Mixed_Float_Register;

   --  Test: Mid_Little_Endian (CDAB) word order
   --  50.0 = 0x42480000 => CDAB: Low word first => [0x0000, 0x4248]
   procedure Test_Float32_Mid_Little_Endian (T : in out Test_Case'Class);
   procedure Test_Float32_Mid_Little_Endian (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Regs : constant Float_IO.Map_Registers :=
        [0 => 16#0000#, 1 => 16#4248#];
      Data : Float_Reg;
   begin
      Data := Float_IO.From_Registers
        (Regs, Pairs => [1 => 0], Order => Mid_Little_Endian);
      Assert (Data.Value = 50.0,
              "Float (CDAB) should be 50.0, got " &
              IEEE_Float_32'Image (Data.Value));
   end Test_Float32_Mid_Little_Endian;

   --  Test: Field_Sizes From_Registers with Big_Endian
   --  Same as Test_Mixed_Float_Register but using Field_Sizes API
   procedure Test_Field_Sizes_From (T : in out Test_Case'Class);
   procedure Test_Field_Sizes_From (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      use Mixed_IO;
      Regs : constant Mixed_IO.Map_Registers :=
        [0 => 16#4248#, 1 => 16#0000#,
         2 => 16#42C8#, 3 => 16#0000#,
         4 => 16#ABCD#];
      Data : Mixed_Reg;
      Layout : constant Field_Sizes := [Bits_32, Bits_32, Bits_16];
   begin
      Data := Mixed_IO.From_Registers (Regs, Layout, Big_Endian);
      Assert (Data.Temperature = 50.0,
              "Temperature should be 50.0, got " &
              IEEE_Float_32'Image (Data.Temperature));
      Assert (Data.Pressure = 100.0,
              "Pressure should be 100.0, got " &
              IEEE_Float_32'Image (Data.Pressure));
      Assert (Data.Status = 16#ABCD#,
              "Status should be 16#ABCD#, got " &
              Register_Value'Image (Data.Status));
   end Test_Field_Sizes_From;

   --  Test: Field_Sizes To_Registers round-trip
   procedure Test_Field_Sizes_Round_Trip (T : in out Test_Case'Class);
   procedure Test_Field_Sizes_Round_Trip (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      use Mixed_IO;
      Original : constant Mixed_Reg :=
        (Temperature => 50.0, Pressure => 100.0, Status => 16#1234#);
      Layout : constant Field_Sizes := [Bits_32, Bits_32, Bits_16];
      Regs   : Mixed_IO.Map_Registers;
      Result : Mixed_Reg;
   begin
      Regs := Mixed_IO.To_Registers (Original, Layout, Big_Endian);
      --  Verify wire format: 50.0 = 0x42480000
      Assert (Regs (0) = 16#4248#, "Reg 0 should be high word of 50.0");
      Assert (Regs (1) = 16#0000#, "Reg 1 should be low word of 50.0");

      Result := Mixed_IO.From_Registers (Regs, Layout, Big_Endian);
      Assert (Result.Temperature = Original.Temperature,
              "Temperature mismatch after round-trip");
      Assert (Result.Pressure = Original.Pressure,
              "Pressure mismatch after round-trip");
      Assert (Result.Status = Original.Status,
              "Status mismatch after round-trip");
   end Test_Field_Sizes_Round_Trip;

   --  Test: Field_Sizes with Mid_Little_Endian (CDAB)
   procedure Test_Field_Sizes_CDAB (T : in out Test_Case'Class);
   procedure Test_Field_Sizes_CDAB (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      use Float_IO;
      --  50.0 = 0x42480000, CDAB: Low word first => [0x0000, 0x4248]
      Regs : constant Float_IO.Map_Registers :=
        [0 => 16#0000#, 1 => 16#4248#];
      Layout : constant Field_Sizes := [1 => Bits_32];
      Data : Float_Reg;
   begin
      Data := Float_IO.From_Registers (Regs, Layout, Mid_Little_Endian);
      Assert (Data.Value = 50.0,
              "Float (CDAB) should be 50.0, got " &
              IEEE_Float_32'Image (Data.Value));
   end Test_Field_Sizes_CDAB;

   --  Test: Register_Count_Of
   procedure Test_Register_Count_Of (T : in out Test_Case'Class);
   procedure Test_Register_Count_Of (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      use Mixed_IO;
   begin
      Assert (Register_Count_Of ([1 => Bits_16]) = 1,
              "Single Bits_16 = 1 register");
      Assert (Register_Count_Of ([1 => Bits_32]) = 2,
              "Single Bits_32 = 2 registers");
      Assert (Register_Count_Of ([Bits_32, Bits_32, Bits_16]) = 5,
              "2x Bits_32 + 1x Bits_16 = 5 registers");
      Assert (Register_Count_Of ([Bits_16, Bits_16, Bits_16]) = 3,
              "3x Bits_16 = 3 registers");
   end Test_Register_Count_Of;

   --  Test: To_Registers with Mid_Little_Endian
   procedure Test_Float32_To_Mid_Little_Endian (T : in out Test_Case'Class);
   procedure Test_Float32_To_Mid_Little_Endian (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      Data : constant Float_Reg := (Value => 50.0);
      Regs : Float_IO.Map_Registers;
   begin
      Regs := Float_IO.To_Registers
        (Data, Pairs => [1 => 0], Order => Mid_Little_Endian);
      Assert (Regs (0) = 16#0000#,
              "CDAB Reg 0 should be 16#0000#, got " &
              Register_Value'Image (Regs (0)));
      Assert (Regs (1) = 16#4248#,
              "CDAB Reg 1 should be 16#4248#, got " &
              Register_Value'Image (Regs (1)));
   end Test_Float32_To_Mid_Little_Endian;

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
      Registration.Register_Routine (T, Test_Float32_From_Registers'Access,
                                     "Float32 From_Registers (Big_Endian)");
      Registration.Register_Routine (T, Test_Float32_To_Registers'Access,
                                     "Float32 To_Registers (Big_Endian)");
      Registration.Register_Routine (T, Test_Float32_Round_Trip'Access,
                                     "Float32 round-trip");
      Registration.Register_Routine (T, Test_Mixed_Float_Register'Access,
                                     "Mixed Float32 + Register_Value");
      Registration.Register_Routine (T, Test_Float32_Mid_Little_Endian'Access,
                                     "Float32 Mid_Little_Endian (CDAB)");
      Registration.Register_Routine (T, Test_Float32_To_Mid_Little_Endian'Access,
                                     "Float32 To Mid_Little_Endian (CDAB)");
      Registration.Register_Routine (T, Test_Field_Sizes_From'Access,
                                     "Field_Sizes From_Registers");
      Registration.Register_Routine (T, Test_Field_Sizes_Round_Trip'Access,
                                     "Field_Sizes round-trip");
      Registration.Register_Routine (T, Test_Field_Sizes_CDAB'Access,
                                     "Field_Sizes Mid_Little_Endian (CDAB)");
      Registration.Register_Routine (T, Test_Register_Count_Of'Access,
                                     "Register_Count_Of");
   end Register_Tests;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      S : constant AUnit.Test_Suites.Access_Test_Suite :=
        new AUnit.Test_Suites.Test_Suite;
   begin
      S.Add_Test (new Record_IO_Test_Case);
      return S;
   end Suite;

end Test_Record_IO;
