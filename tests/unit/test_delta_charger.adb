--  Test_Delta_Charger - Delta AC Max Basic EV-Charger tests implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.Protocol; use Ada_Modbus.Protocol;
with Ada_Modbus.Energy.Delta_Charger; use Ada_Modbus.Energy.Delta_Charger;
with Interfaces; use Interfaces;

package body Test_Delta_Charger is

   type Delta_Test_Case is new Test_Case with null record;

   overriding function Name (T : Delta_Test_Case) return AUnit.Message_String is
     (AUnit.Format ("Delta AC Max Basic Tests"));

   overriding procedure Register_Tests (T : in out Delta_Test_Case);

   ---------------------------------------------------------------------------
   --  Encoding Tests
   ---------------------------------------------------------------------------

   --  Test: Read EVSE Status Request encoding
   procedure Test_Encode_EVSE_Status_Request (T : in Out Test_Case'Class);
   procedure Test_Encode_EVSE_Status_Request (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer : PDU_Buffer;
      Length : Natural;
   begin
      Encode_Read_EVSE_Status_Request (Buffer, Length);

      Assert (Length = 5, "Request length should be 5");
      Assert (Buffer (0) = 16#04#, "FC should be Read Input Registers (0x04)");
      --  Address 1000 = 0x03E8
      Assert (Buffer (1) = 16#03#, "Address high byte should be 0x03");
      Assert (Buffer (2) = 16#E8#, "Address low byte should be 0xE8");
      --  Quantity = 21
      Assert (Buffer (3) = 16#00#, "Quantity high should be 0x00");
      Assert (Buffer (4) = 16#15#, "Quantity low should be 0x15 (21)");
   end Test_Encode_EVSE_Status_Request;

   --  Test: Read Charger Info Request encoding
   procedure Test_Encode_Charger_Info_Request (T : in Out Test_Case'Class);
   procedure Test_Encode_Charger_Info_Request (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer : PDU_Buffer;
      Length : Natural;
   begin
      Encode_Read_Charger_Info_Request (Buffer, Length);

      Assert (Length = 5, "Request length should be 5");
      Assert (Buffer (0) = 16#04#, "FC should be Read Input Registers (0x04)");
      --  Address 100 = 0x0064
      Assert (Buffer (1) = 16#00#, "Address high byte should be 0x00");
      Assert (Buffer (2) = 16#64#, "Address low byte should be 0x64");
      --  Quantity = 3
      Assert (Buffer (3) = 16#00#, "Quantity high should be 0x00");
      Assert (Buffer (4) = 16#03#, "Quantity low should be 0x03");
   end Test_Encode_Charger_Info_Request;

   --  Test: Read Comm Settings Request encoding
   procedure Test_Encode_Comm_Settings_Request (T : in Out Test_Case'Class);
   procedure Test_Encode_Comm_Settings_Request (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer : PDU_Buffer;
      Length : Natural;
   begin
      Encode_Read_Comm_Settings_Request (Buffer, Length);

      Assert (Length = 5, "Request length should be 5");
      Assert (Buffer (0) = 16#03#, "FC should be Read Holding Registers (0x03)");
      --  Address 201 = 0x00C9
      Assert (Buffer (1) = 16#00#, "Address high byte should be 0x00");
      Assert (Buffer (2) = 16#C9#, "Address low byte should be 0xC9");
      --  Quantity = 4
      Assert (Buffer (3) = 16#00#, "Quantity high should be 0x00");
      Assert (Buffer (4) = 16#04#, "Quantity low should be 0x04");
   end Test_Encode_Comm_Settings_Request;

   --  Test: Set Power Limit Request encoding
   procedure Test_Encode_Set_Power_Limit (T : in Out Test_Case'Class);
   procedure Test_Encode_Set_Power_Limit (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer : PDU_Buffer;
      Length : Natural;
   begin
      --  Set power limit to 11000W (0x00002AF8)
      Encode_Set_Power_Limit_Request (11000, Buffer, Length);

      Assert (Buffer (0) = 16#10#, "FC should be Write Multiple Registers (0x10)");
      --  Address 1600 = 0x0640
      Assert (Buffer (1) = 16#06#, "Address high byte should be 0x06");
      Assert (Buffer (2) = 16#40#, "Address low byte should be 0x40");
      --  Quantity = 2
      Assert (Buffer (3) = 16#00#, "Quantity high should be 0x00");
      Assert (Buffer (4) = 16#02#, "Quantity low should be 0x02");
      --  Byte count = 4
      Assert (Buffer (5) = 16#04#, "Byte count should be 4");
      --  Value: 11000 = 0x00002AF8 (Big Endian: High word 0x0000, Low word 0x2AF8)
      Assert (Buffer (6) = 16#00#, "High word high byte");
      Assert (Buffer (7) = 16#00#, "High word low byte");
      Assert (Buffer (8) = 16#2A#, "Low word high byte");
      Assert (Buffer (9) = 16#F8#, "Low word low byte");
   end Test_Encode_Set_Power_Limit;

   --  Test: Set Suspend Request encoding
   procedure Test_Encode_Set_Suspend (T : in Out Test_Case'Class);
   procedure Test_Encode_Set_Suspend (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer : PDU_Buffer;
      Length : Natural;
   begin
      --  Suspend charging
      Encode_Set_Suspend_Request (Suspend => True, Buffer => Buffer, Length => Length);

      Assert (Length = 5, "Request length should be 5");
      Assert (Buffer (0) = 16#06#, "FC should be Write Single Register (0x06)");
      --  Address 1602 = 0x0642
      Assert (Buffer (1) = 16#06#, "Address high byte should be 0x06");
      Assert (Buffer (2) = 16#42#, "Address low byte should be 0x42");
      --  Value = 1 (suspended)
      Assert (Buffer (3) = 16#00#, "Value high should be 0x00");
      Assert (Buffer (4) = 16#01#, "Value low should be 0x01");

      --  Resume charging
      Encode_Set_Suspend_Request (Suspend => False, Buffer => Buffer, Length => Length);
      Assert (Buffer (4) = 16#00#, "Value low should be 0x00 for resume");
   end Test_Encode_Set_Suspend;

   --  Test: Set Communication Timeout Request encoding
   procedure Test_Encode_Set_Comm_Timeout (T : in Out Test_Case'Class);
   procedure Test_Encode_Set_Comm_Timeout (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer : PDU_Buffer;
      Length : Natural;
   begin
      Encode_Set_Comm_Timeout_Request (Timeout_Seconds => 60, Buffer => Buffer, Length => Length);

      Assert (Length = 5, "Request length should be 5");
      Assert (Buffer (0) = 16#06#, "FC should be Write Single Register (0x06)");
      --  Address 202 = 0x00CA
      Assert (Buffer (1) = 16#00#, "Address high byte should be 0x00");
      Assert (Buffer (2) = 16#CA#, "Address low byte should be 0xCA");
      --  Value = 60
      Assert (Buffer (3) = 16#00#, "Value high should be 0x00");
      Assert (Buffer (4) = 16#3C#, "Value low should be 0x3C (60)");
   end Test_Encode_Set_Comm_Timeout;

   --  Test: Set Communication Timeout Enabled Request encoding
   procedure Test_Encode_Set_Comm_Timeout_Enabled (T : in Out Test_Case'Class);
   procedure Test_Encode_Set_Comm_Timeout_Enabled (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer : PDU_Buffer;
      Length : Natural;
   begin
      Encode_Set_Comm_Timeout_Enabled_Request (Enabled => True, Buffer => Buffer, Length => Length);

      Assert (Length = 5, "Request length should be 5");
      Assert (Buffer (0) = 16#06#, "FC should be Write Single Register (0x06)");
      --  Address 201 = 0x00C9
      Assert (Buffer (1) = 16#00#, "Address high byte should be 0x00");
      Assert (Buffer (2) = 16#C9#, "Address low byte should be 0xC9");
      --  Value = 1 (enabled)
      Assert (Buffer (3) = 16#00#, "Value high should be 0x00");
      Assert (Buffer (4) = 16#01#, "Value low should be 0x01");
   end Test_Encode_Set_Comm_Timeout_Enabled;

   ---------------------------------------------------------------------------
   --  Decoding Tests
   ---------------------------------------------------------------------------

   --  Test: Decode EVSE Status Response
   procedure Test_Decode_EVSE_Status (T : in Out Test_Case'Class);
   procedure Test_Decode_EVSE_Status (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer : PDU_Buffer := [others => 0];
      Status : EVSE_Status;
      Result : Ada_Modbus.Status;
   begin
      --  Build response: FC=04, ByteCount=42 (21 registers * 2)
      Buffer (0) := 16#04#;  --  FC
      Buffer (1) := 42;      --  Byte count

      --  Offset 0 (EVSE State): 4 = Charging
      Buffer (2) := 16#00#; Buffer (3) := 16#04#;
      --  Offset 1 (Charge State): 7 = Charging L123
      Buffer (4) := 16#00#; Buffer (5) := 16#07#;
      --  Offset 2 (unused): 0
      Buffer (6) := 16#00#; Buffer (7) := 16#00#;
      --  Offset 3-4 (Voltage FLOAT32 raw): 0x4364 0x0000 (approx 228V)
      Buffer (8) := 16#43#; Buffer (9) := 16#64#;
      Buffer (10) := 16#00#; Buffer (11) := 16#00#;
      --  Offset 5-6 (Power UINT32): 7500W = 0x00001D4C
      Buffer (12) := 16#00#; Buffer (13) := 16#00#;
      Buffer (14) := 16#1D#; Buffer (15) := 16#4C#;
      --  Offset 7-8 (Current FLOAT32 raw): 0x4248 0x0000 (approx 50A)
      Buffer (16) := 16#42#; Buffer (17) := 16#48#;
      Buffer (18) := 16#00#; Buffer (19) := 16#00#;
      --  Offset 9-10 (Output Power FLOAT32 raw): 0x45EA 0x6000 (approx 7500W)
      Buffer (20) := 16#45#; Buffer (21) := 16#EA#;
      Buffer (22) := 16#60#; Buffer (23) := 16#00#;
      --  Offset 11 (SOC): 850 = 85.0%
      Buffer (24) := 16#03#; Buffer (25) := 16#52#;
      --  Offset 12-16 (unused): 0
      Buffer (26) := 16#00#; Buffer (27) := 16#00#;
      Buffer (28) := 16#00#; Buffer (29) := 16#00#;
      Buffer (30) := 16#00#; Buffer (31) := 16#00#;
      Buffer (32) := 16#00#; Buffer (33) := 16#00#;
      Buffer (34) := 16#00#; Buffer (35) := 16#00#;
      --  Offset 17-18 (Charging Time UINT32): 3600s = 0x00000E10
      Buffer (36) := 16#00#; Buffer (37) := 16#00#;
      Buffer (38) := 16#0E#; Buffer (39) := 16#10#;
      --  Offset 19-20 (Energy UINT32): 5500Wh = 0x0000157C
      Buffer (40) := 16#00#; Buffer (41) := 16#00#;
      Buffer (42) := 16#15#; Buffer (43) := 16#7C#;

      Decode_EVSE_Status_Response
        (Buffer => Buffer,
         Length => 44,
         Status => Status,
         Result => Result);

      Assert (Result = Success, "Decode should succeed");
      Assert (Status.State = Charging, "EVSE State should be Charging");
      Assert (Status.Charge = Charging_L123, "Charge State should be Charging_L123");
      Assert (Status.Power_W = 7500, "Power should be 7500W");
      Assert (Status.SOC_Percent_x10 = 850, "SOC should be 850 (85.0%)");
      Assert (Status.Charging_Time_S = 3600, "Charging time should be 3600s");
      Assert (Status.Energy_Wh = 5500, "Energy should be 5500Wh");
      --  Verify raw float arrays are populated
      Assert (Status.Voltage_Raw (0) = 16#4364#, "Voltage raw high word");
      Assert (Status.Current_Raw (0) = 16#4248#, "Current raw high word");
   end Test_Decode_EVSE_Status;

   --  Test: Decode Charger Info Response
   procedure Test_Decode_Charger_Info (T : in Out Test_Case'Class);
   procedure Test_Decode_Charger_Info (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer : PDU_Buffer := [others => 0];
      Info   : Charger_Info;
      Result : Ada_Modbus.Status;
   begin
      --  Build response: FC=04, ByteCount=6 (3 registers * 2)
      Buffer (0) := 16#04#;  --  FC
      Buffer (1) := 6;       --  Byte count
      --  Register 100 (Charger State): 1 = Operational
      Buffer (2) := 16#00#; Buffer (3) := 16#01#;
      --  Register 101 (unused): 0
      Buffer (4) := 16#00#; Buffer (5) := 16#00#;
      --  Register 102 (EVSE Count): 2
      Buffer (6) := 16#00#; Buffer (7) := 16#02#;

      Decode_Charger_Info_Response
        (Buffer => Buffer,
         Length => 8,
         Info   => Info,
         Result => Result);

      Assert (Result = Success, "Decode should succeed");
      Assert (Info.State = Operational, "Charger State should be Operational");
      Assert (Info.EVSE_Count = 2, "EVSE Count should be 2");
   end Test_Decode_Charger_Info;

   --  Test: Decode Charger Info with Faulted state
   procedure Test_Decode_Charger_Info_Faulted (T : in Out Test_Case'Class);
   procedure Test_Decode_Charger_Info_Faulted (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer : PDU_Buffer := [others => 0];
      Info   : Charger_Info;
      Result : Ada_Modbus.Status;
   begin
      Buffer (0) := 16#04#;
      Buffer (1) := 6;
      --  Charger State: 10 = Faulted
      Buffer (2) := 16#00#; Buffer (3) := 16#0A#;
      Buffer (4) := 16#00#; Buffer (5) := 16#00#;
      Buffer (6) := 16#00#; Buffer (7) := 16#01#;

      Decode_Charger_Info_Response (Buffer, 8, Info, Result);

      Assert (Result = Success, "Decode should succeed");
      Assert (Info.State = Faulted, "Charger State should be Faulted");
   end Test_Decode_Charger_Info_Faulted;

   --  Test: Decode Communication Settings Response
   procedure Test_Decode_Comm_Settings (T : in Out Test_Case'Class);
   procedure Test_Decode_Comm_Settings (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer   : PDU_Buffer := [others => 0];
      Settings : Comm_Settings;
      Result   : Ada_Modbus.Status;
   begin
      --  Build response: FC=03, ByteCount=8 (4 registers * 2)
      Buffer (0) := 16#03#;  --  FC
      Buffer (1) := 8;       --  Byte count
      --  Register 201 (Timeout Enabled): 1 = enabled
      Buffer (2) := 16#00#; Buffer (3) := 16#01#;
      --  Register 202 (Timeout): 120 seconds
      Buffer (4) := 16#00#; Buffer (5) := 16#78#;
      --  Register 203-204 (Fallback Power UINT32): 3680W = 0x00000E60
      Buffer (6) := 16#00#; Buffer (7) := 16#00#;
      Buffer (8) := 16#0E#; Buffer (9) := 16#60#;

      Decode_Comm_Settings_Response
        (Buffer   => Buffer,
         Length   => 10,
         Settings => Settings,
         Result   => Result);

      Assert (Result = Success, "Decode should succeed");
      Assert (Settings.Timeout_Enabled, "Timeout should be enabled");
      Assert (Settings.Timeout_Seconds = 120, "Timeout should be 120s");
      Assert (Settings.Fallback_Power = 3680, "Fallback power should be 3680W");
   end Test_Decode_Comm_Settings;

   ---------------------------------------------------------------------------
   --  State Conversion Tests
   ---------------------------------------------------------------------------

   --  Test: EVSE State conversion
   procedure Test_To_EVSE_State (T : in Out Test_Case'Class);
   procedure Test_To_EVSE_State (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert (To_EVSE_State (0) = Unavailable, "0 = Unavailable");
      Assert (To_EVSE_State (1) = Available, "1 = Available");
      Assert (To_EVSE_State (2) = Occupied, "2 = Occupied");
      Assert (To_EVSE_State (3) = Preparing, "3 = Preparing");
      Assert (To_EVSE_State (4) = Charging, "4 = Charging");
      Assert (To_EVSE_State (5) = Finishing, "5 = Finishing");
      Assert (To_EVSE_State (6) = Suspended_EV, "6 = Suspended_EV");
      Assert (To_EVSE_State (7) = Suspended_EVSE, "7 = Suspended_EVSE");
      Assert (To_EVSE_State (8) = Not_Ready, "8 = Not_Ready");
      Assert (To_EVSE_State (9) = Faulted, "9 = Faulted");
      Assert (To_EVSE_State (99) = Unavailable, "Unknown = Unavailable");
   end Test_To_EVSE_State;

   --  Test: Charger State conversion
   procedure Test_To_Charger_State (T : in Out Test_Case'Class);
   procedure Test_To_Charger_State (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert (To_Charger_State (0) = Not_Ready, "0 = Not_Ready");
      Assert (To_Charger_State (1) = Operational, "1 = Operational");
      Assert (To_Charger_State (10) = Faulted, "10 = Faulted");
      Assert (To_Charger_State (255) = Not_Responding, "255 = Not_Responding");
      Assert (To_Charger_State (99) = Not_Ready, "Unknown = Not_Ready");
   end Test_To_Charger_State;

   --  Test: Charge State conversion
   procedure Test_To_Charge_State (T : in Out Test_Case'Class);
   procedure Test_To_Charge_State (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert (To_Charge_State (0) = Not_Charging, "0 = Not_Charging");
      Assert (To_Charge_State (1) = Charging_L1, "1 = Charging_L1");
      Assert (To_Charge_State (2) = Charging_L2, "2 = Charging_L2");
      Assert (To_Charge_State (3) = Charging_L3, "3 = Charging_L3");
      Assert (To_Charge_State (4) = Charging_L12, "4 = Charging_L12");
      Assert (To_Charge_State (5) = Charging_L23, "5 = Charging_L23");
      Assert (To_Charge_State (6) = Charging_L13, "6 = Charging_L13");
      Assert (To_Charge_State (7) = Charging_L123, "7 = Charging_L123");
      Assert (To_Charge_State (99) = Not_Charging, "Unknown = Not_Charging");
   end Test_To_Charge_State;

   ---------------------------------------------------------------------------
   --  Utility Function Tests
   ---------------------------------------------------------------------------

   --  Test: UINT32 decode/encode
   procedure Test_Uint32_Conversion (T : in Out Test_Case'Class);
   procedure Test_Uint32_Conversion (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      High_Word, Low_Word : Register_Value;
      Value : Unsigned_32;
   begin
      --  Test decode
      Value := Decode_Uint32 (High_Word => 16#0001#, Low_Word => 16#E240#);
      Assert (Value = 123456, "Decode 123456");

      Value := Decode_Uint32 (High_Word => 16#0000#, Low_Word => 16#0000#);
      Assert (Value = 0, "Decode 0");

      Value := Decode_Uint32 (High_Word => 16#FFFF#, Low_Word => 16#FFFF#);
      Assert (Value = 4294967295, "Decode max uint32");

      --  Test encode
      Encode_Uint32 (123456, High_Word, Low_Word);
      Assert (High_Word = 16#0001#, "Encode high word");
      Assert (Low_Word = 16#E240#, "Encode low word");

      Encode_Uint32 (0, High_Word, Low_Word);
      Assert (High_Word = 0 and Low_Word = 0, "Encode 0");
   end Test_Uint32_Conversion;

   --  Test: SOC percentage conversion
   procedure Test_To_SOC_Percent (T : in Out Test_Case'Class);
   procedure Test_To_SOC_Percent (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert (To_SOC_Percent (0) = 0, "0 -> 0%");
      Assert (To_SOC_Percent (500) = 50, "500 -> 50%");
      Assert (To_SOC_Percent (1000) = 100, "1000 -> 100%");
      Assert (To_SOC_Percent (999) = 99, "999 -> 99%");
   end Test_To_SOC_Percent;

   ---------------------
   -- Register_Tests --
   ---------------------

   overriding procedure Register_Tests (T : in out Delta_Test_Case) is
   begin
      --  Encoding tests
      Registration.Register_Routine (T, Test_Encode_EVSE_Status_Request'Access,
                          "Encode_Read_EVSE_Status_Request");
      Registration.Register_Routine (T, Test_Encode_Charger_Info_Request'Access,
                          "Encode_Read_Charger_Info_Request");
      Registration.Register_Routine (T, Test_Encode_Comm_Settings_Request'Access,
                          "Encode_Read_Comm_Settings_Request");
      Registration.Register_Routine (T, Test_Encode_Set_Power_Limit'Access,
                          "Encode_Set_Power_Limit_Request");
      Registration.Register_Routine (T, Test_Encode_Set_Suspend'Access,
                          "Encode_Set_Suspend_Request");
      Registration.Register_Routine (T, Test_Encode_Set_Comm_Timeout'Access,
                          "Encode_Set_Comm_Timeout_Request");
      Registration.Register_Routine (T, Test_Encode_Set_Comm_Timeout_Enabled'Access,
                          "Encode_Set_Comm_Timeout_Enabled_Request");

      --  Decoding tests
      Registration.Register_Routine (T, Test_Decode_EVSE_Status'Access,
                          "Decode_EVSE_Status_Response");
      Registration.Register_Routine (T, Test_Decode_Charger_Info'Access,
                          "Decode_Charger_Info_Response");
      Registration.Register_Routine (T, Test_Decode_Charger_Info_Faulted'Access,
                          "Decode_Charger_Info_Response (Faulted)");
      Registration.Register_Routine (T, Test_Decode_Comm_Settings'Access,
                          "Decode_Comm_Settings_Response");

      --  State conversion tests
      Registration.Register_Routine (T, Test_To_EVSE_State'Access,
                          "To_EVSE_State conversion");
      Registration.Register_Routine (T, Test_To_Charger_State'Access,
                          "To_Charger_State conversion");
      Registration.Register_Routine (T, Test_To_Charge_State'Access,
                          "To_Charge_State conversion");

      --  Utility function tests
      Registration.Register_Routine (T, Test_Uint32_Conversion'Access,
                          "UINT32 encode/decode");
      Registration.Register_Routine (T, Test_To_SOC_Percent'Access,
                          "To_SOC_Percent conversion");
   end Register_Tests;

   -----------
   -- Suite --
   -----------

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      S : constant AUnit.Test_Suites.Access_Test_Suite :=
        new AUnit.Test_Suites.Test_Suite;
   begin
      S.Add_Test (new Delta_Test_Case);
      return S;
   end Suite;

end Test_Delta_Charger;
