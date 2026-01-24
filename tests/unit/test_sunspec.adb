--  Test_SunSpec - SunSpec energy package tests implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.Protocol; use Ada_Modbus.Protocol;
with Ada_Modbus.Energy.SunSpec; use Ada_Modbus.Energy.SunSpec;
with Ada_Modbus.Energy.SunSpec.Inverter; use Ada_Modbus.Energy.SunSpec.Inverter;
with Ada_Modbus.Energy.SunSpec.Common; use Ada_Modbus.Energy.SunSpec.Common;

package body Test_SunSpec is

   type SunSpec_Test_Case is new Test_Case with null record;

   overriding function Name (T : SunSpec_Test_Case) return AUnit.Message_String is
     (AUnit.Format ("SunSpec Energy Package Tests"));

   overriding procedure Register_Tests (T : in out SunSpec_Test_Case);

   --  Test: Scale factor application
   procedure Test_Apply_Scale (T : in out Test_Case'Class);
   procedure Test_Apply_Scale (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      --  Scale factor 0: no scaling
      Assert (Apply_Scale (100, 0) = 100.0, "SF=0 should not scale");

      --  Scale factor 1: multiply by 10
      Assert (Apply_Scale (100, 1) = 1000.0, "SF=1 should multiply by 10");

      --  Scale factor -1: divide by 10
      Assert (Apply_Scale (100, -1) = 10.0, "SF=-1 should divide by 10");

      --  Scale factor 2: multiply by 100
      Assert (Apply_Scale (50, 2) = 5000.0, "SF=2 should multiply by 100");

      --  Scale factor -2: divide by 100
      Assert (Apply_Scale (1000, -2) = 10.0, "SF=-2 should divide by 100");
   end Test_Apply_Scale;

   --  Test: SunSpec identifier check request encoding
   procedure Test_Check_SunSpec_Request (T : in Out Test_Case'Class);
   procedure Test_Check_SunSpec_Request (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer : PDU_Buffer;
      Length : Natural;
   begin
      Encode_Check_SunSpec_Request
        (Base_Address => 40000,
         Buffer       => Buffer,
         Length       => Length);

      Assert (Length = 5, "Request length should be 5");
      Assert (Buffer (0) = 16#03#, "FC should be Read Holding Registers (0x03)");
      --  Address 40000 = 0x9C40
      Assert (Buffer (1) = 16#9C#, "Address high byte should be 0x9C");
      Assert (Buffer (2) = 16#40#, "Address low byte should be 0x40");
      --  Quantity = 2
      Assert (Buffer (3) = 16#00#, "Quantity high should be 0x00");
      Assert (Buffer (4) = 16#02#, "Quantity low should be 0x02");
   end Test_Check_SunSpec_Request;

   --  Test: SunSpec identifier response decoding (valid)
   procedure Test_Check_SunSpec_Response_Valid (T : in Out Test_Case'Class);
   procedure Test_Check_SunSpec_Response_Valid (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer   : PDU_Buffer := [others => 0];
      Is_Valid : Boolean;
      Result   : Status;
   begin
      --  Build valid response: FC=03, ByteCount=4, "SunS" (0x53756E53)
      Buffer (0) := 16#03#;  --  FC
      Buffer (1) := 16#04#;  --  Byte count
      Buffer (2) := 16#53#;  --  'S'
      Buffer (3) := 16#75#;  --  'u'
      Buffer (4) := 16#6E#;  --  'n'
      Buffer (5) := 16#53#;  --  'S'

      Decode_Check_SunSpec_Response
        (Buffer   => Buffer,
         Length   => 6,
         Is_Valid => Is_Valid,
         Result   => Result);

      Assert (Result = Success, "Decode should succeed");
      Assert (Is_Valid, "Should detect valid SunSpec device");
   end Test_Check_SunSpec_Response_Valid;

   --  Test: SunSpec identifier response decoding (invalid)
   procedure Test_Check_SunSpec_Response_Invalid (T : in Out Test_Case'Class);
   procedure Test_Check_SunSpec_Response_Invalid (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer   : PDU_Buffer := [others => 0];
      Is_Valid : Boolean;
      Result   : Status;
   begin
      --  Build invalid response: wrong identifier
      Buffer (0) := 16#03#;  --  FC
      Buffer (1) := 16#04#;  --  Byte count
      Buffer (2) := 16#00#;  --  Wrong
      Buffer (3) := 16#00#;
      Buffer (4) := 16#00#;
      Buffer (5) := 16#00#;

      Decode_Check_SunSpec_Response
        (Buffer   => Buffer,
         Length   => 6,
         Is_Valid => Is_Valid,
         Result   => Result);

      Assert (Result = Success, "Decode should succeed");
      Assert (not Is_Valid, "Should detect non-SunSpec device");
   end Test_Check_SunSpec_Response_Invalid;

   --  Test: Model header request encoding
   procedure Test_Read_Model_Header_Request (T : in Out Test_Case'Class);
   procedure Test_Read_Model_Header_Request (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer : PDU_Buffer;
      Length : Natural;
   begin
      --  Read header at offset 2 (after SunS identifier)
      Encode_Read_Model_Header_Request
        (Base_Address => 40000,
         Offset       => 2,
         Buffer       => Buffer,
         Length       => Length);

      Assert (Length = 5, "Request length should be 5");
      Assert (Buffer (0) = 16#03#, "FC should be Read Holding Registers");
      --  Address 40002 = 0x9C42
      Assert (Buffer (1) = 16#9C#, "Address high byte should be 0x9C");
      Assert (Buffer (2) = 16#42#, "Address low byte should be 0x42");
   end Test_Read_Model_Header_Request;

   --  Test: Model header response decoding
   procedure Test_Decode_Model_Header (T : in Out Test_Case'Class);
   procedure Test_Decode_Model_Header (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer : PDU_Buffer := [others => 0];
      Header : Model_Header;
      Result : Status;
   begin
      --  Build response: Model ID = 1 (Common), Length = 66
      Buffer (0) := 16#03#;  --  FC
      Buffer (1) := 16#04#;  --  Byte count
      Buffer (2) := 16#00#;  --  Model ID high
      Buffer (3) := 16#01#;  --  Model ID low (1 = Common)
      Buffer (4) := 16#00#;  --  Length high
      Buffer (5) := 16#42#;  --  Length low (66)

      Decode_Model_Header_Response
        (Buffer => Buffer,
         Length => 6,
         Header => Header,
         Result => Result);

      Assert (Result = Success, "Decode should succeed");
      Assert (Header.ID = 1, "Model ID should be 1 (Common)");
      Assert (Header.Length = 66, "Model length should be 66");
   end Test_Decode_Model_Header;

   --  Test: String decoding from registers
   procedure Test_Decode_String (T : in Out Test_Case'Class);
   procedure Test_Decode_String (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Registers : Register_Array (0 .. 3);
      Result    : SunSpec_String;
      Len       : Natural;
   begin
      --  "SMA" encoded as 16-bit registers (big-endian)
      Registers (0) := 16#534D#;  --  "SM"
      Registers (1) := 16#4100#;  --  "A" + NUL
      Registers (2) := 16#0000#;  --  NUL terminator
      Registers (3) := 16#0000#;

      Decode_String (Registers, Result, Len);

      Assert (Len = 3, "String length should be 3");
      Assert (Result (1 .. 3) = "SMA", "String should be 'SMA'");
   end Test_Decode_String;

   --  Test: Scale factor lookup table bounds
   procedure Test_Scale_Multipliers (T : in Out Test_Case'Class);
   procedure Test_Scale_Multipliers (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      --  Test extreme scale factors
      Assert (Scale_Multipliers (-10) = 1.0E-10, "SF=-10 multiplier");
      Assert (Scale_Multipliers (10) = 1.0E10, "SF=10 multiplier");
      Assert (Scale_Multipliers (0) = 1.0, "SF=0 multiplier");
   end Test_Scale_Multipliers;

   --  Test: Signed scale factor application (for temperatures)
   procedure Test_Apply_Scale_Signed (T : in Out Test_Case'Class);
   procedure Test_Apply_Scale_Signed (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      --  Positive value (47 degrees with SF=0)
      Assert (Apply_Scale_Signed (47, 0) = 47.0,
              "Positive value should work like unsigned");

      --  Positive value with scale factor
      Assert (Apply_Scale_Signed (470, -1) = 47.0,
              "Positive value with negative SF");

      --  Negative value: -5 degrees stored as 65531 (two's complement)
      --  65536 - 5 = 65531
      Assert (Apply_Scale_Signed (65531, 0) = -5.0,
              "Negative value (two's complement) should be -5");

      --  Negative value with scale factor: -1.5 degrees
      --  -15 as unsigned = 65521, SF=-1 -> -1.5
      Assert (Apply_Scale_Signed (65521, -1) = -1.5,
              "Negative value with SF=-1");

      --  Edge case: exactly 32767 (max positive int16)
      Assert (Apply_Scale_Signed (32767, 0) = 32767.0,
              "Max positive int16 should stay positive");

      --  Edge case: 32768 (min negative int16 = -32768)
      Assert (Apply_Scale_Signed (32768, 0) = -32768.0,
              "32768 as unsigned should be -32768 signed");
   end Test_Apply_Scale_Signed;

   --  Test: Inverter AC measurements decode
   procedure Test_Inverter_AC_Decode (T : in Out Test_Case'Class);
   procedure Test_Inverter_AC_Decode (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer : PDU_Buffer := [others => 0];
      Current, Voltage, Power, Frequency : Float;
      Result : Status;
   begin
      --  Build simulated Modbus response for 16 registers
      --  FC=03, ByteCount=32, then 16 registers (big-endian)
      Buffer (0) := 16#03#;  --  FC
      Buffer (1) := 32;      --  Byte count (16 regs * 2)

      --  Register 0 (offset 2): AC Current = 1234 (12.34A with SF=-2)
      Buffer (2) := 16#04#; Buffer (3) := 16#D2#;  -- 1234

      --  Registers 1-3: Phase currents (skip)
      --  Register 4 (offset 10): Current SF = -2 (0xFFFE as two's complement)
      Buffer (10) := 16#FF#; Buffer (11) := 16#FE#;  -- -2

      --  Registers 5-7: Line-line voltages (skip)
      --  Register 8 (offset 18): Voltage AN = 2300 (230.0V with SF=-1)
      Buffer (18) := 16#08#; Buffer (19) := 16#FC#;  -- 2300

      --  Registers 9-10: Voltage BN, CN (skip)
      --  Register 11 (offset 24): Voltage SF = -1 (0xFFFF)
      Buffer (24) := 16#FF#; Buffer (25) := 16#FF#;  -- -1

      --  Register 12 (offset 26): AC Power = 5000 (5000W with SF=0)
      Buffer (26) := 16#13#; Buffer (27) := 16#88#;  -- 5000

      --  Register 13 (offset 28): Power SF = 0
      Buffer (28) := 16#00#; Buffer (29) := 16#00#;  -- 0

      --  Register 14 (offset 30): Frequency = 5000 (50.00Hz with SF=-2)
      Buffer (30) := 16#13#; Buffer (31) := 16#88#;  -- 5000

      --  Register 15 (offset 32): Frequency SF = -2
      Buffer (32) := 16#FF#; Buffer (33) := 16#FE#;  -- -2

      Decode_AC_Measurements_Response
        (Buffer       => Buffer,
         Length       => 34,
         Current_A    => Current,
         Voltage_V    => Voltage,
         Power_W      => Power,
         Frequency_Hz => Frequency,
         Result       => Result);

      Assert (Result = Success, "Decode should succeed");
      Assert (abs (Current - 12.34) < 0.01, "Current should be ~12.34A");
      Assert (abs (Voltage - 230.0) < 0.1, "Voltage should be ~230V");
      Assert (abs (Power - 5000.0) < 0.1, "Power should be 5000W");
      Assert (abs (Frequency - 50.0) < 0.01, "Frequency should be 50Hz");
   end Test_Inverter_AC_Decode;

   --  Test: Inverter DC measurements decode
   procedure Test_Inverter_DC_Decode (T : in Out Test_Case'Class);
   procedure Test_Inverter_DC_Decode (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer : PDU_Buffer := [others => 0];
      Current, Voltage, Power : Float;
      Result : Status;
   begin
      --  Build response for 6 registers
      Buffer (0) := 16#03#;  --  FC
      Buffer (1) := 12;      --  Byte count (6 regs * 2)

      --  Register 0: DC Current = 850 (8.50A with SF=-2)
      Buffer (2) := 16#03#; Buffer (3) := 16#52#;  -- 850

      --  Register 1: Current SF = -2
      Buffer (4) := 16#FF#; Buffer (5) := 16#FE#;  -- -2

      --  Register 2: DC Voltage = 4500 (450.0V with SF=-1)
      Buffer (6) := 16#11#; Buffer (7) := 16#94#;  -- 4500

      --  Register 3: Voltage SF = -1
      Buffer (8) := 16#FF#; Buffer (9) := 16#FF#;  -- -1

      --  Register 4: DC Power = 3825 (3825W with SF=0)
      Buffer (10) := 16#0E#; Buffer (11) := 16#F1#;  -- 3825

      --  Register 5: Power SF = 0
      Buffer (12) := 16#00#; Buffer (13) := 16#00#;  -- 0

      Decode_DC_Measurements_Response
        (Buffer    => Buffer,
         Length    => 14,
         Current_A => Current,
         Voltage_V => Voltage,
         Power_W   => Power,
         Result    => Result);

      Assert (Result = Success, "Decode should succeed");
      Assert (abs (Current - 8.5) < 0.01, "DC Current should be ~8.5A");
      Assert (abs (Voltage - 450.0) < 0.1, "DC Voltage should be ~450V");
      Assert (abs (Power - 3825.0) < 0.1, "DC Power should be 3825W");
   end Test_Inverter_DC_Decode;

   --  Test: Inverter energy decode (32-bit value)
   procedure Test_Inverter_Energy_Decode (T : in Out Test_Case'Class);
   procedure Test_Inverter_Energy_Decode (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer : PDU_Buffer := [others => 0];
      Energy : Float;
      Result : Status;
   begin
      --  Build response for 3 registers (32-bit energy + SF)
      Buffer (0) := 16#03#;  --  FC
      Buffer (1) := 6;       --  Byte count (3 regs * 2)

      --  Energy = 12345678 Wh (0x00BC614E)
      --  High word at reg 0, low word at reg 1
      Buffer (2) := 16#00#; Buffer (3) := 16#BC#;  -- High: 0x00BC = 188
      Buffer (4) := 16#61#; Buffer (5) := 16#4E#;  -- Low:  0x614E = 24910
      --  188 * 65536 + 24910 = 12345678

      --  SF = 0
      Buffer (6) := 16#00#; Buffer (7) := 16#00#;

      Decode_Energy_Response
        (Buffer    => Buffer,
         Length    => 8,
         Energy_Wh => Energy,
         Result    => Result);

      Assert (Result = Success, "Decode should succeed");
      Assert (abs (Energy - 12345678.0) < 1.0, "Energy should be 12345678 Wh");
   end Test_Inverter_Energy_Decode;

   --  Test: Inverter state decode
   procedure Test_Inverter_State_Decode (T : in Out Test_Case'Class);
   procedure Test_Inverter_State_Decode (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer : PDU_Buffer := [others => 0];
      State  : Inverter_State;
      Result : Status;
   begin
      Buffer (0) := 16#03#;  --  FC
      Buffer (1) := 4;       --  Byte count

      --  State = 4 (Running/MPPT)
      Buffer (2) := 16#00#; Buffer (3) := 16#04#;

      --  Vendor state (ignored)
      Buffer (4) := 16#00#; Buffer (5) := 16#00#;

      Decode_State_Response
        (Buffer => Buffer,
         Length => 6,
         State  => State,
         Result => Result);

      Assert (Result = Success, "Decode should succeed");
      Assert (State = Running, "State should be Running (MPPT)");

      --  Test Fault state (7)
      Buffer (3) := 16#07#;
      Decode_State_Response (Buffer, 6, State, Result);
      Assert (State = Fault, "State should be Fault");

      --  Test Sleeping state (2)
      Buffer (3) := 16#02#;
      Decode_State_Response (Buffer, 6, State, Result);
      Assert (State = Sleeping, "State should be Sleeping");
   end Test_Inverter_State_Decode;

   --  Test: Common model request encoding
   procedure Test_Common_Model_Requests (T : in Out Test_Case'Class);
   procedure Test_Common_Model_Requests (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer : PDU_Buffer;
      Length : Natural;
   begin
      --  Test manufacturer request (base=40000, model_offset=2)
      --  Should read from 40000 + 2 + 2 = 40004
      Encode_Read_Manufacturer_Request
        (Base_Address => 40000,
         Model_Offset => 2,
         Buffer       => Buffer,
         Length       => Length);

      Assert (Length = 5, "Request length should be 5");
      Assert (Buffer (0) = 16#03#, "FC should be 0x03");
      --  Address 40004 = 0x9C44
      Assert (Buffer (1) = 16#9C#, "Address high");
      Assert (Buffer (2) = 16#44#, "Address low");
      --  Quantity = 16 (32 chars = 16 registers)
      Assert (Buffer (4) = 16#10#, "Quantity should be 16");
   end Test_Common_Model_Requests;

   ---------------------
   -- Register_Tests --
   ---------------------

   overriding procedure Register_Tests (T : in out SunSpec_Test_Case) is
   begin
      Registration.Register_Routine (T, Test_Apply_Scale'Access, "Apply_Scale");
      Registration.Register_Routine (T, Test_Check_SunSpec_Request'Access,
                          "Encode_Check_SunSpec_Request");
      Registration.Register_Routine (T, Test_Check_SunSpec_Response_Valid'Access,
                          "Decode_Check_SunSpec_Response (valid)");
      Registration.Register_Routine (T, Test_Check_SunSpec_Response_Invalid'Access,
                          "Decode_Check_SunSpec_Response (invalid)");
      Registration.Register_Routine (T, Test_Read_Model_Header_Request'Access,
                          "Encode_Read_Model_Header_Request");
      Registration.Register_Routine (T, Test_Decode_Model_Header'Access,
                          "Decode_Model_Header_Response");
      Registration.Register_Routine (T, Test_Decode_String'Access,
                          "Decode_String");
      Registration.Register_Routine (T, Test_Scale_Multipliers'Access,
                          "Scale_Multipliers lookup table");
      --  Additional tests for signed values and Inverter model
      Registration.Register_Routine (T, Test_Apply_Scale_Signed'Access,
                          "Apply_Scale_Signed (signed int16)");
      Registration.Register_Routine (T, Test_Inverter_AC_Decode'Access,
                          "Inverter AC Measurements Decode");
      Registration.Register_Routine (T, Test_Inverter_DC_Decode'Access,
                          "Inverter DC Measurements Decode");
      Registration.Register_Routine (T, Test_Inverter_Energy_Decode'Access,
                          "Inverter Energy Decode (32-bit)");
      Registration.Register_Routine (T, Test_Inverter_State_Decode'Access,
                          "Inverter State Decode");
      Registration.Register_Routine (T, Test_Common_Model_Requests'Access,
                          "Common Model Request Encoding");
   end Register_Tests;

   -----------
   -- Suite --
   -----------

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      S : constant AUnit.Test_Suites.Access_Test_Suite :=
        new AUnit.Test_Suites.Test_Suite;
   begin
      S.Add_Test (new SunSpec_Test_Case);
      return S;
   end Suite;

end Test_SunSpec;
