--  Test_Gateway - Gateway unit tests implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.Protocol; use Ada_Modbus.Protocol;
with Ada_Modbus.Protocol.TCP; use Ada_Modbus.Protocol.TCP;
with Ada_Modbus.Protocol.RTU; use Ada_Modbus.Protocol.RTU;
with Ada_Modbus.Gateway;

package body Test_Gateway is

   type Gateway_Test_Case is new Test_Case with null record;

   overriding function Name (T : Gateway_Test_Case) return AUnit.Message_String is
     (AUnit.Format ("Gateway Tests"));

   overriding procedure Register_Tests (T : in out Gateway_Test_Case);

   --  Test: TCP to RTU conversion
   procedure Test_TCP_To_RTU (T : in out Test_Case'Class);
   procedure Test_TCP_To_RTU (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      --  Build a valid TCP frame first
      TCP_PDU     : PDU_Buffer := [others => 0];
      TCP_ADU_Buf : Protocol.TCP.ADU_Buffer;
      TCP_Len     : Natural;
      RTU_ADU_Buf : Protocol.RTU.ADU_Buffer;
      RTU_Len     : Natural;
      Trans       : Transaction_Id;
      Result      : Status;
   begin
      --  FC03: Read 10 holding registers starting at 0
      TCP_PDU (0) := 16#03#;
      TCP_PDU (1) := 16#00#;
      TCP_PDU (2) := 16#00#;
      TCP_PDU (3) := 16#00#;
      TCP_PDU (4) := 16#0A#;

      --  Build TCP frame with Transaction=42, Unit=1
      Protocol.TCP.Build_Frame
        (TCP_ADU_Buf, TCP_Len,
         Transaction => 42, Unit => 1,
         PDU => TCP_PDU, PDU_Length => 5);

      --  Convert TCP -> RTU
      Gateway.TCP_To_RTU (TCP_ADU_Buf, TCP_Len, RTU_ADU_Buf, RTU_Len,
                          Trans, Result);

      Assert (Result = Success, "TCP_To_RTU should succeed");
      Assert (Trans = 42, "Transaction ID should be preserved");

      --  RTU frame: Slave(1) + PDU(5) + CRC(2) = 8 bytes
      Assert (RTU_Len = 8, "RTU frame should be 8 bytes");

      --  Check slave address
      Assert (RTU_ADU_Buf (0) = 16#01#, "RTU slave should be 1");

      --  Check PDU content
      Assert (RTU_ADU_Buf (1) = 16#03#, "RTU FC should be 0x03");
      Assert (RTU_ADU_Buf (2) = 16#00#, "Start addr high");
      Assert (RTU_ADU_Buf (3) = 16#00#, "Start addr low");
      Assert (RTU_ADU_Buf (4) = 16#00#, "Quantity high");
      Assert (RTU_ADU_Buf (5) = 16#0A#, "Quantity low");

      --  Verify CRC is valid
      Assert (Protocol.RTU.Verify_CRC (RTU_ADU_Buf (0 .. RTU_Len - 1)),
              "RTU CRC should be valid");
   end Test_TCP_To_RTU;

   --  Test: RTU to TCP conversion
   procedure Test_RTU_To_TCP (T : in out Test_Case'Class);
   procedure Test_RTU_To_TCP (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      --  Build a valid RTU frame first
      RTU_PDU     : PDU_Buffer := [others => 0];
      RTU_ADU_Buf : Protocol.RTU.ADU_Buffer;
      RTU_Len     : Natural;
      TCP_ADU_Buf : Protocol.TCP.ADU_Buffer;
      TCP_Len     : Natural;
      Result      : Status;
   begin
      --  FC03 response: 03 14 (20 bytes = 10 registers)
      RTU_PDU (0) := 16#03#;
      RTU_PDU (1) := 16#14#;
      --  10 register values (20 bytes of zeros)

      --  Build RTU frame with Slave=1
      Protocol.RTU.Build_Frame
        (RTU_ADU_Buf, RTU_Len,
         Slave => 1, PDU => RTU_PDU, PDU_Length => 22);

      --  Convert RTU -> TCP with Transaction=42
      Gateway.RTU_To_TCP (RTU_ADU_Buf, RTU_Len, 42,
                          TCP_ADU_Buf, TCP_Len, Result);

      Assert (Result = Success, "RTU_To_TCP should succeed");

      --  TCP frame: MBAP(7) + PDU(22) = 29 bytes
      Assert (TCP_Len = 29, "TCP frame should be 29 bytes");

      --  Check Transaction ID = 42 = 0x002A
      Assert (TCP_ADU_Buf (0) = 16#00#, "Trans ID high");
      Assert (TCP_ADU_Buf (1) = 16#2A#, "Trans ID low");

      --  Check Protocol ID = 0
      Assert (TCP_ADU_Buf (2) = 16#00# and TCP_ADU_Buf (3) = 16#00#,
              "Protocol ID should be 0");

      --  Check Unit ID
      Assert (TCP_ADU_Buf (6) = 16#01#, "Unit should be 1");

      --  Check PDU FC
      Assert (TCP_ADU_Buf (7) = 16#03#, "TCP FC should be 0x03");
   end Test_RTU_To_TCP;

   --  Test: Round-trip TCP -> RTU -> TCP
   procedure Test_Round_Trip (T : in out Test_Case'Class);
   procedure Test_Round_Trip (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      PDU         : PDU_Buffer := [others => 0];
      Orig_TCP    : Protocol.TCP.ADU_Buffer;
      Orig_Len    : Natural;
      RTU_ADU_Buf : Protocol.RTU.ADU_Buffer;
      RTU_Len     : Natural;
      Back_TCP    : Protocol.TCP.ADU_Buffer;
      Back_Len    : Natural;
      Trans       : Transaction_Id;
      Result      : Status;
   begin
      --  FC06: Write single register at addr 100 with value 42
      PDU (0) := 16#06#;
      PDU (1) := 16#00#;
      PDU (2) := 16#64#;
      PDU (3) := 16#00#;
      PDU (4) := 16#2A#;

      --  Build original TCP frame
      Protocol.TCP.Build_Frame
        (Orig_TCP, Orig_Len,
         Transaction => 99, Unit => 5,
         PDU => PDU, PDU_Length => 5);

      --  TCP -> RTU
      Gateway.TCP_To_RTU (Orig_TCP, Orig_Len, RTU_ADU_Buf, RTU_Len,
                          Trans, Result);
      Assert (Result = Success, "TCP->RTU should succeed");

      --  RTU -> TCP (simulating response = echo for FC06)
      Gateway.RTU_To_TCP (RTU_ADU_Buf, RTU_Len, Trans,
                          Back_TCP, Back_Len, Result);
      Assert (Result = Success, "RTU->TCP should succeed");

      --  Verify same length
      Assert (Back_Len = Orig_Len, "Round-trip should preserve length");

      --  Verify Transaction ID preserved
      Assert (Back_TCP (0) = Orig_TCP (0) and Back_TCP (1) = Orig_TCP (1),
              "Transaction ID preserved");

      --  Verify Unit ID preserved
      Assert (Back_TCP (6) = Orig_TCP (6), "Unit ID preserved");

      --  Verify PDU preserved
      for I in 7 .. Orig_Len - 1 loop
         Assert (Back_TCP (I) = Orig_TCP (I),
                 "PDU byte" & I'Image & " preserved");
      end loop;
   end Test_Round_Trip;

   --  Test: Invalid TCP frame
   procedure Test_Invalid_TCP_Frame (T : in out Test_Case'Class);
   procedure Test_Invalid_TCP_Frame (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      TCP_ADU_Buf : constant Protocol.TCP.ADU_Buffer := [others => 0];
      RTU_ADU_Buf : Protocol.RTU.ADU_Buffer;
      RTU_Len     : Natural;
      Trans       : Transaction_Id;
      Result      : Status;
   begin
      --  Pass a too-short frame (only 3 bytes)
      Gateway.TCP_To_RTU (TCP_ADU_Buf, 3, RTU_ADU_Buf, RTU_Len,
                          Trans, Result);

      Assert (Result /= Success, "Should fail on invalid TCP frame");
   end Test_Invalid_TCP_Frame;

   --  Test: Invalid RTU frame (bad CRC)
   procedure Test_Invalid_RTU_Frame (T : in out Test_Case'Class);
   procedure Test_Invalid_RTU_Frame (T : in out Test_Case'Class) is
      pragma Unreferenced (T);
      RTU_ADU_Buf : Protocol.RTU.ADU_Buffer := [others => 0];
      TCP_ADU_Buf : Protocol.TCP.ADU_Buffer;
      TCP_Len     : Natural;
      Result      : Status;
   begin
      --  Set up an RTU frame with invalid CRC
      RTU_ADU_Buf (0) := 16#01#;  --  Slave 1
      RTU_ADU_Buf (1) := 16#03#;  --  FC 03
      RTU_ADU_Buf (2) := 16#00#;  --  Bad CRC
      RTU_ADU_Buf (3) := 16#00#;  --  Bad CRC

      Gateway.RTU_To_TCP (RTU_ADU_Buf, 4, 1,
                          TCP_ADU_Buf, TCP_Len, Result);

      Assert (Result /= Success, "Should fail on invalid RTU frame");
   end Test_Invalid_RTU_Frame;

   overriding procedure Register_Tests (T : in out Gateway_Test_Case) is
   begin
      Registration.Register_Routine (T, Test_TCP_To_RTU'Access,
                                     "TCP to RTU conversion");
      Registration.Register_Routine (T, Test_RTU_To_TCP'Access,
                                     "RTU to TCP conversion");
      Registration.Register_Routine (T, Test_Round_Trip'Access,
                                     "TCP -> RTU -> TCP round-trip");
      Registration.Register_Routine (T, Test_Invalid_TCP_Frame'Access,
                                     "Invalid TCP frame handling");
      Registration.Register_Routine (T, Test_Invalid_RTU_Frame'Access,
                                     "Invalid RTU frame handling");
   end Register_Tests;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      S : constant AUnit.Test_Suites.Access_Test_Suite :=
        new AUnit.Test_Suites.Test_Suite;
   begin
      S.Add_Test (new Gateway_Test_Case);
      return S;
   end Suite;

end Test_Gateway;
