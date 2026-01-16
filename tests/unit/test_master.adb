--  Test_Master - Master (client) unit tests implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Interfaces; use Interfaces;
with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.Protocol;
with Ada_Modbus.Protocol.RTU;
with Ada_Modbus.Protocol.TCP;
with Ada_Modbus.Master;

package body Test_Master is

   --  Mock transport state
   Mock_Send_Buffer   : Byte_Array (0 .. 511) := [others => 0];
   Mock_Send_Length   : Natural := 0;
   Mock_Recv_Buffer   : Byte_Array (0 .. 511) := [others => 0];
   Mock_Recv_Length   : Natural := 0;
   Mock_Recv_Pos      : Natural := 0;
   Mock_Time          : Unsigned_32 := 0;
   Mock_Send_Fail     : Boolean := False;
   Mock_Recv_Timeout  : Boolean := False;

   --  Mock transport context (not used, state is global for simplicity)
   type Mock_Context is null record;

   --  Mock transport functions
   function Mock_Send
     (Ctx  : in out Mock_Context;
      Data : Byte_Array) return Natural
   is
      pragma Unreferenced (Ctx);
   begin
      if Mock_Send_Fail then
         return 0;
      end if;
      for I in Data'Range loop
         Mock_Send_Buffer (Mock_Send_Length + I - Data'First) := Data (I);
      end loop;
      Mock_Send_Length := Mock_Send_Length + Data'Length;
      return Data'Length;
   end Mock_Send;

   function Mock_Receive
     (Ctx        : in Out Mock_Context;
      Buffer     : out Byte_Array;
      Max_Length : Natural;
      Timeout_Ms : Natural) return Natural
   is
      pragma Unreferenced (Ctx, Timeout_Ms);
      Bytes_To_Copy : Natural;
   begin
      Buffer := [others => 0];
      if Mock_Recv_Timeout then
         return 0;
      end if;
      Bytes_To_Copy := Natural'Min (Max_Length, Mock_Recv_Length - Mock_Recv_Pos);
      for I in 0 .. Bytes_To_Copy - 1 loop
         Buffer (Buffer'First + I) := Mock_Recv_Buffer (Mock_Recv_Pos + I);
      end loop;
      Mock_Recv_Pos := Mock_Recv_Pos + Bytes_To_Copy;
      return Bytes_To_Copy;
   end Mock_Receive;

   function Mock_Get_Tick_Ms return Unsigned_32 is
   begin
      return Mock_Time;
   end Mock_Get_Tick_Ms;

   --  Reset mock state
   procedure Reset_Mocks is
   begin
      Mock_Send_Buffer := [others => 0];
      Mock_Send_Length := 0;
      Mock_Recv_Buffer := [others => 0];
      Mock_Recv_Length := 0;
      Mock_Recv_Pos := 0;
      Mock_Time := 0;
      Mock_Send_Fail := False;
      Mock_Recv_Timeout := False;
   end Reset_Mocks;

   --  Instantiate Master with mock transport
   package Mock_Master is new Ada_Modbus.Master
     (Transport_Context => Mock_Context,
      Send              => Mock_Send,
      Receive           => Mock_Receive,
      Get_Tick_Ms       => Mock_Get_Tick_Ms);

   --  Global mock context for tests
   Test_Transport : Mock_Context;

   use type Mock_Master.Protocol_Mode;

   type Master_Test_Case is new Test_Case with null record;

   overriding function Name (T : Master_Test_Case) return AUnit.Message_String is
     (AUnit.Format ("Master (Client) Tests"));

   overriding procedure Register_Tests (T : in Out Master_Test_Case);

   --  Test: Initialize master context
   procedure Test_Initialize (T : in Out Test_Case'Class);
   procedure Test_Initialize (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Ctx    : Mock_Master.Master_Context;
      Config : Mock_Master.Master_Config;
   begin
      Config.Mode := Mock_Master.TCP;
      Config.Default_Slave := 5;
      Config.Default_Timeout := 2000;

      Mock_Master.Initialize (Ctx, Config, Test_Transport);

      Assert (Ctx.Config.Mode = Mock_Master.TCP, "Mode should be TCP");
      Assert (Ctx.Config.Default_Slave = 5, "Default slave should be 5");
      Assert (Ctx.Config.Default_Timeout = 2000, "Default timeout should be 2000");
   end Test_Initialize;

   --  Test: Read Holding Registers with RTU
   procedure Test_Read_Holding_Registers_RTU (T : in Out Test_Case'Class);
   procedure Test_Read_Holding_Registers_RTU (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Ctx    : Mock_Master.Master_Context;
      Config : Mock_Master.Master_Config;
      Values : Register_Array (0 .. 9);
      Result : Status;
      Resp_PDU : Protocol.PDU_Buffer := [others => 0];
      Resp_ADU : Protocol.RTU.ADU_Buffer;
      Resp_Len : Natural;
   begin
      Reset_Mocks;

      Config.Mode := Mock_Master.RTU;
      Config.Default_Slave := 1;
      Mock_Master.Initialize (Ctx, Config, Test_Transport);

      --  Prepare mock response: FC03, ByteCount=4, Data (2 registers)
      Resp_PDU (0) := 16#03#;  --  FC
      Resp_PDU (1) := 4;       --  Byte count
      Resp_PDU (2) := 16#12#;
      Resp_PDU (3) := 16#34#;
      Resp_PDU (4) := 16#56#;
      Resp_PDU (5) := 16#78#;

      Protocol.RTU.Build_Frame (Resp_ADU, Resp_Len, Slave => 1, PDU => Resp_PDU, PDU_Length => 6);

      --  Set up mock receive buffer
      for I in 0 .. Resp_Len - 1 loop
         Mock_Recv_Buffer (I) := Resp_ADU (I);
      end loop;
      Mock_Recv_Length := Resp_Len;

      Result := Mock_Master.Read_Holding_Registers
        (Ctx, Slave => 1, Start_Address => 0, Quantity => 2, Values => Values);

      Assert (Result = Success, "Read should succeed, got " & Result'Image);
      Assert (Values (0) = 16#1234#, "First register should be 0x1234");
      Assert (Values (1) = 16#5678#, "Second register should be 0x5678");
   end Test_Read_Holding_Registers_RTU;

   --  Test: Read Holding Registers with TCP
   procedure Test_Read_Holding_Registers_TCP (T : in Out Test_Case'Class);
   procedure Test_Read_Holding_Registers_TCP (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Ctx    : Mock_Master.Master_Context;
      Config : Mock_Master.Master_Config;
      Values : Register_Array (0 .. 9);
      Result : Status;
      Resp_PDU : Protocol.PDU_Buffer := [others => 0];
      Resp_ADU : Protocol.TCP.ADU_Buffer;
      Resp_Len : Natural;
   begin
      Reset_Mocks;

      Config.Mode := Mock_Master.TCP;
      Mock_Master.Initialize (Ctx, Config, Test_Transport);

      --  Transaction ID will be 1 (first transaction)
      Resp_PDU (0) := 16#03#;
      Resp_PDU (1) := 2;       --  Byte count = 1 register
      Resp_PDU (2) := 16#AB#;
      Resp_PDU (3) := 16#CD#;

      Protocol.TCP.Build_Frame (Resp_ADU, Resp_Len, Transaction => 1, Unit => 1,
                                PDU => Resp_PDU, PDU_Length => 4);

      for I in 0 .. Resp_Len - 1 loop
         Mock_Recv_Buffer (I) := Resp_ADU (I);
      end loop;
      Mock_Recv_Length := Resp_Len;

      Result := Mock_Master.Read_Holding_Registers
        (Ctx, Slave => 1, Start_Address => 100, Quantity => 1, Values => Values);

      Assert (Result = Success, "Read should succeed, got " & Result'Image);
      Assert (Values (0) = 16#ABCD#, "Register should be 0xABCD");
   end Test_Read_Holding_Registers_TCP;

   --  Test: Write Single Register
   procedure Test_Write_Single_Register (T : in Out Test_Case'Class);
   procedure Test_Write_Single_Register (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Ctx    : Mock_Master.Master_Context;
      Config : Mock_Master.Master_Config;
      Result : Status;
      Resp_PDU : Protocol.PDU_Buffer := [others => 0];
      Resp_ADU : Protocol.RTU.ADU_Buffer;
      Resp_Len : Natural;
   begin
      Reset_Mocks;

      Config.Mode := Mock_Master.RTU;
      Mock_Master.Initialize (Ctx, Config, Test_Transport);

      --  Echo response for FC06
      Resp_PDU (0) := 16#06#;
      Resp_PDU (1) := 16#00#;
      Resp_PDU (2) := 16#0A#;  --  Address 10
      Resp_PDU (3) := 16#12#;
      Resp_PDU (4) := 16#34#;  --  Value 0x1234

      Protocol.RTU.Build_Frame (Resp_ADU, Resp_Len, Slave => 1, PDU => Resp_PDU, PDU_Length => 5);

      for I in 0 .. Resp_Len - 1 loop
         Mock_Recv_Buffer (I) := Resp_ADU (I);
      end loop;
      Mock_Recv_Length := Resp_Len;

      Result := Mock_Master.Write_Single_Register
        (Ctx, Slave => 1, Address => 10, Value => 16#1234#);

      Assert (Result = Success, "Write should succeed");
      --  Verify request was sent (check FC in send buffer)
      Assert (Mock_Send_Buffer (1) = 16#06#, "Sent FC should be 0x06");
   end Test_Write_Single_Register;

   --  Test: Timeout handling
   procedure Test_Timeout (T : in Out Test_Case'Class);
   procedure Test_Timeout (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Ctx    : Mock_Master.Master_Context;
      Config : Mock_Master.Master_Config;
      Values : Register_Array (0 .. 9);
      Result : Status;
   begin
      Reset_Mocks;
      Mock_Recv_Timeout := True;  --  Simulate timeout

      Config.Mode := Mock_Master.RTU;
      Mock_Master.Initialize (Ctx, Config, Test_Transport);

      Result := Mock_Master.Read_Holding_Registers
        (Ctx, Slave => 1, Start_Address => 0, Quantity => 1, Values => Values);

      Assert (Result = Timeout, "Should return Timeout");
   end Test_Timeout;

   --  Test: Exception response handling
   procedure Test_Exception_Response (T : in Out Test_Case'Class);
   procedure Test_Exception_Response (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Ctx    : Mock_Master.Master_Context;
      Config : Mock_Master.Master_Config;
      Values : Register_Array (0 .. 9);
      Result : Status;
      Resp_PDU : Protocol.PDU_Buffer := [others => 0];
      Resp_ADU : Protocol.RTU.ADU_Buffer;
      Resp_Len : Natural;
   begin
      Reset_Mocks;

      Config.Mode := Mock_Master.RTU;
      Mock_Master.Initialize (Ctx, Config, Test_Transport);

      --  Exception response
      Resp_PDU (0) := 16#83#;  --  FC03 + 0x80
      Resp_PDU (1) := 16#02#;  --  Illegal Address

      Protocol.RTU.Build_Frame (Resp_ADU, Resp_Len, Slave => 1, PDU => Resp_PDU, PDU_Length => 2);

      for I in 0 .. Resp_Len - 1 loop
         Mock_Recv_Buffer (I) := Resp_ADU (I);
      end loop;
      Mock_Recv_Length := Resp_Len;

      Result := Mock_Master.Read_Holding_Registers
        (Ctx, Slave => 1, Start_Address => 0, Quantity => 1, Values => Values);

      Assert (Result = Exception_Illegal_Address, "Should return Illegal Address exception");
   end Test_Exception_Response;

   --  Test: Write Single Coil
   procedure Test_Write_Single_Coil (T : in Out Test_Case'Class);
   procedure Test_Write_Single_Coil (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Ctx    : Mock_Master.Master_Context;
      Config : Mock_Master.Master_Config;
      Result : Status;
      Resp_PDU : Protocol.PDU_Buffer := [others => 0];
      Resp_ADU : Protocol.RTU.ADU_Buffer;
      Resp_Len : Natural;
   begin
      Reset_Mocks;

      Config.Mode := Mock_Master.RTU;
      Mock_Master.Initialize (Ctx, Config, Test_Transport);

      --  Echo response for FC05
      Resp_PDU (0) := 16#05#;
      Resp_PDU (1) := 16#00#;
      Resp_PDU (2) := 16#05#;  --  Address 5
      Resp_PDU (3) := 16#FF#;
      Resp_PDU (4) := 16#00#;  --  ON

      Protocol.RTU.Build_Frame (Resp_ADU, Resp_Len, Slave => 1, PDU => Resp_PDU, PDU_Length => 5);

      for I in 0 .. Resp_Len - 1 loop
         Mock_Recv_Buffer (I) := Resp_ADU (I);
      end loop;
      Mock_Recv_Length := Resp_Len;

      Result := Mock_Master.Write_Single_Coil
        (Ctx, Slave => 1, Address => 5, Value => True);

      Assert (Result = Success, "Write coil should succeed");
   end Test_Write_Single_Coil;

   --  Test: Read Coils
   procedure Test_Read_Coils (T : in Out Test_Case'Class);
   procedure Test_Read_Coils (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Ctx    : Mock_Master.Master_Context;
      Config : Mock_Master.Master_Config;
      Values : Coil_Array (0 .. 15);
      Result : Status;
      Resp_PDU : Protocol.PDU_Buffer := [others => 0];
      Resp_ADU : Protocol.RTU.ADU_Buffer;
      Resp_Len : Natural;
   begin
      Reset_Mocks;

      Config.Mode := Mock_Master.RTU;
      Mock_Master.Initialize (Ctx, Config, Test_Transport);

      --  Response for FC01
      Resp_PDU (0) := 16#01#;
      Resp_PDU (1) := 1;       --  Byte count
      Resp_PDU (2) := 16#A5#;  --  Coil data: 10100101

      Protocol.RTU.Build_Frame (Resp_ADU, Resp_Len, Slave => 1, PDU => Resp_PDU, PDU_Length => 3);

      for I in 0 .. Resp_Len - 1 loop
         Mock_Recv_Buffer (I) := Resp_ADU (I);
      end loop;
      Mock_Recv_Length := Resp_Len;

      Result := Mock_Master.Read_Coils
        (Ctx, Slave => 1, Start_Address => 0, Quantity => 8, Values => Values);

      Assert (Result = Success, "Read coils should succeed");
      Assert (Values (0) = True, "Coil 0 should be ON");
      Assert (Values (1) = False, "Coil 1 should be OFF");
      Assert (Values (2) = True, "Coil 2 should be ON");
   end Test_Read_Coils;

   overriding procedure Register_Tests (T : in Out Master_Test_Case) is
   begin
      Registration.Register_Routine (T, Test_Initialize'Access, "Initialize Master");
      Registration.Register_Routine (T, Test_Read_Holding_Registers_RTU'Access, "Read Holding Registers RTU");
      Registration.Register_Routine (T, Test_Read_Holding_Registers_TCP'Access, "Read Holding Registers TCP");
      Registration.Register_Routine (T, Test_Write_Single_Register'Access, "Write Single Register");
      Registration.Register_Routine (T, Test_Timeout'Access, "Timeout Handling");
      Registration.Register_Routine (T, Test_Exception_Response'Access, "Exception Response");
      Registration.Register_Routine (T, Test_Write_Single_Coil'Access, "Write Single Coil");
      Registration.Register_Routine (T, Test_Read_Coils'Access, "Read Coils");
   end Register_Tests;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      S : constant AUnit.Test_Suites.Access_Test_Suite := new AUnit.Test_Suites.Test_Suite;
   begin
      S.Add_Test (new Master_Test_Case);
      return S;
   end Suite;

end Test_Master;
