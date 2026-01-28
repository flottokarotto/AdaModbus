--  Test_Slave - Slave processing unit tests implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Interfaces;
with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.Slave; use Ada_Modbus.Slave;
with Ada_Modbus.Protocol.RTU;
with Ada_Modbus.Protocol.TCP;
with Ada_Modbus.Protocol.ASCII;

package body Test_Slave is

   --  Mock storage for testing
   Mock_Registers : Register_Array (0 .. 99) := [others => 0];
   Mock_Coils     : Coil_Array (0 .. 99) := [others => False];

   --  Mock callbacks
   function Mock_Read_Holding_Registers
     (Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array) return Status
   is
   begin
      if Natural (Start_Address) + Natural (Quantity) > 100 then
         return Exception_Illegal_Address;
      end if;
      for I in 0 .. Natural (Quantity) - 1 loop
         Values (I) := Mock_Registers (Natural (Start_Address) + I);
      end loop;
      return Success;
   end Mock_Read_Holding_Registers;

   function Mock_Write_Single_Register
     (Address : Register_Address;
      Value   : Register_Value) return Status
   is
   begin
      if Natural (Address) >= 100 then
         return Exception_Illegal_Address;
      end if;
      Mock_Registers (Natural (Address)) := Value;
      return Success;
   end Mock_Write_Single_Register;

   function Mock_Read_Coils
     (Start_Address : Coil_Address;
      Quantity      : Coil_Count;
      Values        : out Coil_Array) return Status
   is
   begin
      if Natural (Start_Address) + Natural (Quantity) > 100 then
         return Exception_Illegal_Address;
      end if;
      for I in 0 .. Natural (Quantity) - 1 loop
         Values (I) := Mock_Coils (Natural (Start_Address) + I);
      end loop;
      return Success;
   end Mock_Read_Coils;

   function Mock_Write_Single_Coil
     (Address : Coil_Address;
      Value   : Coil_Value) return Status
   is
   begin
      if Natural (Address) >= 100 then
         return Exception_Illegal_Address;
      end if;
      Mock_Coils (Natural (Address)) := Value;
      return Success;
   end Mock_Write_Single_Coil;

   function Mock_Write_Multiple_Registers
     (Start_Address : Register_Address;
      Values        : Register_Array) return Status
   is
   begin
      if Natural (Start_Address) + Values'Length > 100 then
         return Exception_Illegal_Address;
      end if;
      for I in Values'Range loop
         Mock_Registers (Natural (Start_Address) + I - Values'First) := Values (I);
      end loop;
      return Success;
   end Mock_Write_Multiple_Registers;

   --  Mock callbacks for new FCs
   Mock_Exception_Status : Byte := 16#A5#;

   function Mock_Read_Exception_Status
     (Exception_Status : out Byte) return Status
   is
   begin
      Exception_Status := Mock_Exception_Status;
      return Success;
   end Mock_Read_Exception_Status;

   function Mock_Diagnostics
     (Sub_Function : Interfaces.Unsigned_16;
      Data_In      : Interfaces.Unsigned_16;
      Data_Out     : out Interfaces.Unsigned_16) return Status
   is
      pragma Unreferenced (Sub_Function);
   begin
      --  Echo back data for sub-function 0 (Return Query Data)
      Data_Out := Data_In;
      return Success;
   end Mock_Diagnostics;

   Mock_Server_Id : Byte := 16#42#;

   function Mock_Report_Server_Id
     (Server_Id     : out Byte;
      Run_Indicator : out Boolean;
      Add_Data      : out Byte_Array;
      Add_Data_Len  : out Natural) return Status
   is
   begin
      Server_Id := Mock_Server_Id;
      Run_Indicator := True;
      Add_Data (Add_Data'First) := Character'Pos ('T');
      Add_Data (Add_Data'First + 1) := Character'Pos ('S');
      Add_Data (Add_Data'First + 2) := Character'Pos ('T');
      Add_Data_Len := 3;
      return Success;
   end Mock_Report_Server_Id;

   function Mock_Mask_Write_Register
     (Address  : Register_Address;
      And_Mask : Register_Value;
      Or_Mask  : Register_Value) return Status
   is
      use type Interfaces.Unsigned_16;
      Current : Interfaces.Unsigned_16;
   begin
      if Natural (Address) >= 100 then
         return Exception_Illegal_Address;
      end if;
      --  Result = (Current AND And_Mask) OR (Or_Mask AND (NOT And_Mask))
      Current := Interfaces.Unsigned_16 (Mock_Registers (Natural (Address)));
      Current := (Current and Interfaces.Unsigned_16 (And_Mask)) or
                 (Interfaces.Unsigned_16 (Or_Mask) and (not Interfaces.Unsigned_16 (And_Mask)));
      Mock_Registers (Natural (Address)) := Register_Value (Current);
      return Success;
   end Mock_Mask_Write_Register;

   function Mock_Read_Write_Registers
     (Read_Start    : Register_Address;
      Read_Quantity : Register_Count;
      Read_Values   : out Register_Array;
      Write_Start   : Register_Address;
      Write_Values  : Register_Array) return Status
   is
   begin
      --  First perform write
      if Natural (Write_Start) + Write_Values'Length > 100 then
         return Exception_Illegal_Address;
      end if;
      for I in Write_Values'Range loop
         Mock_Registers (Natural (Write_Start) + I - Write_Values'First) := Write_Values (I);
      end loop;

      --  Then perform read
      if Natural (Read_Start) + Natural (Read_Quantity) > 100 then
         return Exception_Illegal_Address;
      end if;
      for I in 0 .. Natural (Read_Quantity) - 1 loop
         Read_Values (I) := Mock_Registers (Natural (Read_Start) + I);
      end loop;
      return Success;
   end Mock_Read_Write_Registers;

   type Slave_Test_Case is new Test_Case with null record;

   overriding function Name (T : Slave_Test_Case) return AUnit.Message_String is
     (AUnit.Format ("Slave Processing Tests"));

   overriding procedure Register_Tests (T : in out Slave_Test_Case);

   --  Test: Read Holding Registers via RTU
   procedure Test_Read_Holding_Registers_RTU (T : in Out Test_Case'Class);
   procedure Test_Read_Holding_Registers_RTU (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      Req_PDU  : Protocol.PDU_Buffer := [others => 0];
   begin
      --  Setup mock data
      Mock_Registers (0) := 16#1234#;
      Mock_Registers (1) := 16#5678#;

      --  Setup config
      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Read_Holding_Registers := Mock_Read_Holding_Registers'Access;

      --  Build RTU request: FC03, Start=0, Qty=2
      Req_PDU (0) := 16#03#;  --  FC
      Req_PDU (1) := 16#00#;  --  Start high
      Req_PDU (2) := 16#00#;  --  Start low
      Req_PDU (3) := 16#00#;  --  Qty high
      Req_PDU (4) := 16#02#;  --  Qty low

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => Req_PDU, PDU_Length => 5);

      --  Copy to request
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send response");
      Assert (Resp_Len > 0, "Response should not be empty");
      --  Response PDU: FC + ByteCount + Data
      --  Verify register values in response (after slave ID, before CRC)
      Assert (Response (1) = 16#03#, "FC should be 0x03");
      Assert (Response (2) = 4, "Byte count should be 4");
      Assert (Response (3) = 16#12# and Response (4) = 16#34#, "First register should be 0x1234");
      Assert (Response (5) = 16#56# and Response (6) = 16#78#, "Second register should be 0x5678");
   end Test_Read_Holding_Registers_RTU;

   --  Test: Write Single Register via RTU
   procedure Test_Write_Single_Register_RTU (T : in Out Test_Case'Class);
   procedure Test_Write_Single_Register_RTU (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      Req_PDU  : Protocol.PDU_Buffer := [others => 0];
   begin
      Mock_Registers (10) := 0;

      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Write_Single_Register := Mock_Write_Single_Register'Access;

      --  Build RTU request: FC06, Addr=10, Value=0xABCD
      Req_PDU (0) := 16#06#;
      Req_PDU (1) := 16#00#;
      Req_PDU (2) := 16#0A#;  --  Address 10
      Req_PDU (3) := 16#AB#;
      Req_PDU (4) := 16#CD#;

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => Req_PDU, PDU_Length => 5);

      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send response");
      Assert (Mock_Registers (10) = 16#ABCD#, "Register should be written");
   end Test_Write_Single_Register_RTU;

   --  Test: Illegal function (callback not registered)
   procedure Test_Illegal_Function (T : in Out Test_Case'Class);
   procedure Test_Illegal_Function (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      Req_PDU  : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;
      --  No callbacks registered

      Req_PDU (0) := 16#03#;  --  FC03 but no callback
      Req_PDU (1) := 16#00#;
      Req_PDU (2) := 16#00#;
      Req_PDU (3) := 16#00#;
      Req_PDU (4) := 16#01#;

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => Req_PDU, PDU_Length => 5);

      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send exception response");
      Assert (Response (1) = 16#83#, "FC should be 0x83 (exception)");
      Assert (Response (2) = 16#01#, "Exception code should be 01 (Illegal Function)");
   end Test_Illegal_Function;

   --  Test: Broadcast (no response)
   procedure Test_Broadcast_No_Response (T : in Out Test_Case'Class);
   procedure Test_Broadcast_No_Response (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      Req_PDU  : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Write_Single_Register := Mock_Write_Single_Register'Access;

      Req_PDU (0) := 16#06#;
      Req_PDU (1) := 16#00#;
      Req_PDU (2) := 16#00#;
      Req_PDU (3) := 16#12#;
      Req_PDU (4) := 16#34#;

      --  Broadcast address = 0
      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 0, PDU => Req_PDU, PDU_Length => 5);

      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (not Send, "Should NOT send response to broadcast");
      Assert (Mock_Registers (0) = 16#1234#, "Register should still be written");
   end Test_Broadcast_No_Response;

   --  Test: Wrong slave ID (no response)
   procedure Test_Wrong_Slave_ID (T : in Out Test_Case'Class);
   procedure Test_Wrong_Slave_ID (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      Req_PDU  : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;  --  We are slave 1
      Config.Callbacks.Read_Holding_Registers := Mock_Read_Holding_Registers'Access;

      Req_PDU (0) := 16#03#;
      Req_PDU (1) := 16#00#;
      Req_PDU (2) := 16#00#;
      Req_PDU (3) := 16#00#;
      Req_PDU (4) := 16#01#;

      --  Request to slave 5, not us
      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 5, PDU => Req_PDU, PDU_Length => 5);

      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (not Send, "Should NOT respond to request for different slave");
   end Test_Wrong_Slave_ID;

   --  Test: TCP mode processing
   procedure Test_TCP_Processing (T : in Out Test_Case'Class);
   procedure Test_TCP_Processing (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.TCP.ADU_Buffer;
      Req_Len  : Natural;
      Req_PDU  : Protocol.PDU_Buffer := [others => 0];
   begin
      Mock_Registers (5) := 16#BEEF#;

      Config.Mode := TCP;
      Config.Unit_Id := 1;
      Config.Callbacks.Read_Holding_Registers := Mock_Read_Holding_Registers'Access;

      Req_PDU (0) := 16#03#;
      Req_PDU (1) := 16#00#;
      Req_PDU (2) := 16#05#;  --  Start 5
      Req_PDU (3) := 16#00#;
      Req_PDU (4) := 16#01#;  --  Qty 1

      Protocol.TCP.Build_Frame (Req_ADU, Req_Len, Transaction => 42, Unit => 1,
                                PDU => Req_PDU, PDU_Length => 5);

      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send TCP response");
      --  MBAP header: TransID(2) + ProtoID(2) + Len(2) + Unit(1) + PDU
      Assert (Response (0) = 16#00# and Response (1) = 16#2A#, "Transaction ID should be 42");
      Assert (Response (7) = 16#03#, "FC should be 0x03");
      Assert (Response (9) = 16#BE# and Response (10) = 16#EF#, "Register value");
   end Test_TCP_Processing;

   --  Test: Read Coils
   procedure Test_Read_Coils (T : in Out Test_Case'Class);
   procedure Test_Read_Coils (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      Req_PDU  : Protocol.PDU_Buffer := [others => 0];
   begin
      Mock_Coils (0) := True;
      Mock_Coils (1) := False;
      Mock_Coils (2) := True;
      Mock_Coils (7) := True;

      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Read_Coils := Mock_Read_Coils'Access;

      --  FC01, Start=0, Qty=8
      Req_PDU (0) := 16#01#;
      Req_PDU (1) := 16#00#;
      Req_PDU (2) := 16#00#;
      Req_PDU (3) := 16#00#;
      Req_PDU (4) := 16#08#;

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => Req_PDU, PDU_Length => 5);

      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send response");
      Assert (Response (1) = 16#01#, "FC should be 0x01");
      Assert (Response (2) = 1, "Byte count should be 1");
      --  Coils: bit0=1, bit1=0, bit2=1, bit7=1 => 0x85
      Assert (Response (3) = 16#85#, "Coil data should be 0x85");
   end Test_Read_Coils;

   --  Test: Write Multiple Registers
   procedure Test_Write_Multiple_Registers (T : in Out Test_Case'Class);
   procedure Test_Write_Multiple_Registers (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      Req_PDU  : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Write_Multiple_Registers := Mock_Write_Multiple_Registers'Access;

      --  FC16, Start=20, Qty=2, ByteCount=4, Values
      Req_PDU (0) := 16#10#;
      Req_PDU (1) := 16#00#;
      Req_PDU (2) := 16#14#;  --  Start 20
      Req_PDU (3) := 16#00#;
      Req_PDU (4) := 16#02#;  --  Qty 2
      Req_PDU (5) := 16#04#;  --  Byte count
      Req_PDU (6) := 16#11#;
      Req_PDU (7) := 16#22#;
      Req_PDU (8) := 16#33#;
      Req_PDU (9) := 16#44#;

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => Req_PDU, PDU_Length => 10);

      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send response");
      Assert (Mock_Registers (20) = 16#1122#, "First register should be written");
      Assert (Mock_Registers (21) = 16#3344#, "Second register should be written");
   end Test_Write_Multiple_Registers;

   --  Test: Read Exception Status (FC 07)
   procedure Test_Read_Exception_Status (T : in Out Test_Case'Class);
   procedure Test_Read_Exception_Status (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      Req_PDU  : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Read_Exception_Status := Mock_Read_Exception_Status'Access;

      Mock_Exception_Status := 16#A5#;

      --  FC 07 - just function code
      Req_PDU (0) := 16#07#;

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => Req_PDU, PDU_Length => 1);

      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send response");
      Assert (Response (1) = 16#07#, "FC should be 0x07");
      Assert (Response (2) = 16#A5#, "Exception status should be 0xA5");
   end Test_Read_Exception_Status;

   --  Test: Diagnostics (FC 08)
   procedure Test_Diagnostics (T : in Out Test_Case'Class);
   procedure Test_Diagnostics (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      Req_PDU  : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Diagnostics := Mock_Diagnostics'Access;

      --  FC 08, SubFunction 0, Data 0x1234
      Req_PDU (0) := 16#08#;
      Req_PDU (1) := 16#00#;
      Req_PDU (2) := 16#00#;  --  Sub-function 0
      Req_PDU (3) := 16#12#;
      Req_PDU (4) := 16#34#;  --  Data

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => Req_PDU, PDU_Length => 5);

      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send response");
      Assert (Response (1) = 16#08#, "FC should be 0x08");
      --  Data should be echoed back
      Assert (Response (4) = 16#12# and Response (5) = 16#34#, "Data should be echoed");
   end Test_Diagnostics;

   --  Test: Report Server ID (FC 17)
   procedure Test_Report_Server_Id (T : in Out Test_Case'Class);
   procedure Test_Report_Server_Id (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      Req_PDU  : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Report_Server_Id := Mock_Report_Server_Id'Access;

      Mock_Server_Id := 16#42#;

      --  FC 17 - just function code
      Req_PDU (0) := 16#11#;

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => Req_PDU, PDU_Length => 1);

      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send response");
      Assert (Response (1) = 16#11#, "FC should be 0x11");
      Assert (Response (2) = 5, "Byte count should be 5 (1+1+3)");
      Assert (Response (3) = 16#42#, "Server ID should be 0x42");
      Assert (Response (4) = 16#FF#, "Run indicator should be 0xFF (ON)");
   end Test_Report_Server_Id;

   --  Test: Mask Write Register (FC 22)
   procedure Test_Mask_Write_Register (T : in Out Test_Case'Class);
   procedure Test_Mask_Write_Register (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      Req_PDU  : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Mask_Write_Register := Mock_Mask_Write_Register'Access;

      --  Set initial value: 0xFF00
      Mock_Registers (10) := 16#FF00#;

      --  FC 22, Address 10, And_Mask=0xF0F0, Or_Mask=0x0F0F
      --  Result = (0xFF00 AND 0xF0F0) OR (0x0F0F AND NOT 0xF0F0)
      --         = 0xF000 OR (0x0F0F AND 0x0F0F) = 0xF000 OR 0x0F0F = 0xFF0F
      Req_PDU (0) := 16#16#;  --  FC 22
      Req_PDU (1) := 16#00#;
      Req_PDU (2) := 16#0A#;  --  Address 10
      Req_PDU (3) := 16#F0#;
      Req_PDU (4) := 16#F0#;  --  And_Mask 0xF0F0
      Req_PDU (5) := 16#0F#;
      Req_PDU (6) := 16#0F#;  --  Or_Mask 0x0F0F

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => Req_PDU, PDU_Length => 7);

      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send response");
      Assert (Response (1) = 16#16#, "FC should be 0x16");
      Assert (Mock_Registers (10) = 16#FF0F#, "Register should be masked to 0xFF0F");
   end Test_Mask_Write_Register;

   --  Test: Read/Write Multiple Registers (FC 23)
   procedure Test_Read_Write_Registers (T : in Out Test_Case'Class);
   procedure Test_Read_Write_Registers (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      Req_PDU  : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Read_Write_Registers := Mock_Read_Write_Registers'Access;

      --  Set some initial values for reading
      Mock_Registers (0) := 16#1111#;
      Mock_Registers (1) := 16#2222#;

      --  FC 23: Read 2 regs from addr 0, Write 1 reg to addr 10
      Req_PDU (0) := 16#17#;  --  FC 23
      Req_PDU (1) := 16#00#;
      Req_PDU (2) := 16#00#;  --  Read start 0
      Req_PDU (3) := 16#00#;
      Req_PDU (4) := 16#02#;  --  Read qty 2
      Req_PDU (5) := 16#00#;
      Req_PDU (6) := 16#0A#;  --  Write start 10
      Req_PDU (7) := 16#00#;
      Req_PDU (8) := 16#01#;  --  Write qty 1
      Req_PDU (9) := 16#02#;  --  Write byte count
      Req_PDU (10) := 16#AA#;
      Req_PDU (11) := 16#BB#;  --  Write value 0xAABB

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => Req_PDU, PDU_Length => 12);

      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send response");
      Assert (Response (1) = 16#17#, "FC should be 0x17");
      Assert (Response (2) = 4, "Byte count should be 4 (2 registers)");
      Assert (Response (3) = 16#11# and Response (4) = 16#11#, "First read value should be 0x1111");
      Assert (Response (5) = 16#22# and Response (6) = 16#22#, "Second read value should be 0x2222");
      Assert (Mock_Registers (10) = 16#AABB#, "Write value should be stored");
   end Test_Read_Write_Registers;

   --  Mock callbacks for additional FCs
   function Mock_Read_Discrete_Inputs
     (Start_Address : Coil_Address;
      Quantity      : Coil_Count;
      Values        : out Coil_Array) return Status
   is
   begin
      if Natural (Start_Address) + Natural (Quantity) > 100 then
         return Exception_Illegal_Address;
      end if;
      for I in 0 .. Natural (Quantity) - 1 loop
         Values (I) := Mock_Coils (Natural (Start_Address) + I);
      end loop;
      return Success;
   end Mock_Read_Discrete_Inputs;

   function Mock_Read_Input_Registers
     (Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array) return Status
   is
   begin
      if Natural (Start_Address) + Natural (Quantity) > 100 then
         return Exception_Illegal_Address;
      end if;
      for I in 0 .. Natural (Quantity) - 1 loop
         Values (I) := Mock_Registers (Natural (Start_Address) + I);
      end loop;
      return Success;
   end Mock_Read_Input_Registers;

   function Mock_Write_Multiple_Coils
     (Start_Address : Coil_Address;
      Values        : Coil_Array) return Status
   is
   begin
      if Natural (Start_Address) + Values'Length > 100 then
         return Exception_Illegal_Address;
      end if;
      for I in Values'Range loop
         Mock_Coils (Natural (Start_Address) + I - Values'First) := Values (I);
      end loop;
      return Success;
   end Mock_Write_Multiple_Coils;

   --  Test: Read Discrete Inputs (FC02)
   procedure Test_Read_Discrete_Inputs (T : in Out Test_Case'Class);
   procedure Test_Read_Discrete_Inputs (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      --  Reset mocks
      Mock_Registers := [others => 0];
      Mock_Coils := [others => False];
      Mock_Coils (0) := True;
      Mock_Coils (2) := True;

      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Read_Discrete_Inputs := Mock_Read_Discrete_Inputs'Access;

      --  FC02: Read Discrete Inputs at address 0, quantity 8
      PDU (0) := 16#02#;
      PDU (1) := 0; PDU (2) := 0;    --  Address 0
      PDU (3) := 0; PDU (4) := 8;    --  Quantity 8

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 5);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send response");
      Assert (Response (1) = 16#02#, "FC should be 0x02");
      Assert (Response (2) = 1, "Byte count should be 1");
      Assert ((Response (3) and 16#05#) = 16#05#, "Bits 0 and 2 should be set");
   end Test_Read_Discrete_Inputs;

   --  Test: Read Input Registers (FC04)
   procedure Test_Read_Input_Registers (T : in Out Test_Case'Class);
   procedure Test_Read_Input_Registers (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      --  Reset mocks
      Mock_Registers := [others => 0];
      Mock_Coils := [others => False];
      Mock_Registers (0) := 16#1234#;
      Mock_Registers (1) := 16#5678#;

      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Read_Input_Registers := Mock_Read_Input_Registers'Access;

      --  FC04: Read Input Registers at address 0, quantity 2
      PDU (0) := 16#04#;
      PDU (1) := 0; PDU (2) := 0;    --  Address 0
      PDU (3) := 0; PDU (4) := 2;    --  Quantity 2

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 5);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send response");
      Assert (Response (1) = 16#04#, "FC should be 0x04");
      Assert (Response (2) = 4, "Byte count should be 4");
   end Test_Read_Input_Registers;

   --  Test: Write Single Coil (FC05)
   procedure Test_Write_Single_Coil (T : in Out Test_Case'Class);
   procedure Test_Write_Single_Coil (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      --  Reset mocks
      Mock_Registers := [others => 0];
      Mock_Coils := [others => False];

      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Write_Single_Coil := Mock_Write_Single_Coil'Access;

      --  FC05: Write Single Coil at address 5, value ON
      PDU (0) := 16#05#;
      PDU (1) := 0; PDU (2) := 5;       --  Address 5
      PDU (3) := 16#FF#; PDU (4) := 0;  --  Value ON

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 5);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send response");
      Assert (Response (1) = 16#05#, "FC should be 0x05");
      Assert (Mock_Coils (5) = True, "Coil 5 should be ON");
   end Test_Write_Single_Coil;

   --  Test: Write Multiple Coils (FC15)
   procedure Test_Write_Multiple_Coils (T : in Out Test_Case'Class);
   procedure Test_Write_Multiple_Coils (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      --  Reset mocks
      Mock_Registers := [others => 0];
      Mock_Coils := [others => False];

      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Write_Multiple_Coils := Mock_Write_Multiple_Coils'Access;

      --  FC15: Write Multiple Coils at address 0, quantity 8
      PDU (0) := 16#0F#;
      PDU (1) := 0; PDU (2) := 0;    --  Address 0
      PDU (3) := 0; PDU (4) := 8;    --  Quantity 8
      PDU (5) := 1;                  --  Byte count
      PDU (6) := 16#A5#;             --  Coil data: 10100101

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 7);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send response");
      Assert (Response (1) = 16#0F#, "FC should be 0x0F");
      Assert (Mock_Coils (0) = True, "Coil 0 should be ON");
      Assert (Mock_Coils (2) = True, "Coil 2 should be ON");
   end Test_Write_Multiple_Coils;

   --  Test: Invalid quantity error
   procedure Test_Invalid_Quantity (T : in Out Test_Case'Class);
   procedure Test_Invalid_Quantity (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      --  Reset mocks
      Mock_Registers := [others => 0];
      Mock_Coils := [others => False];

      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Read_Holding_Registers := Mock_Read_Holding_Registers'Access;

      --  FC03 with quantity 0 (invalid)
      PDU (0) := 16#03#;
      PDU (1) := 0; PDU (2) := 0;    --  Address 0
      PDU (3) := 0; PDU (4) := 0;    --  Quantity 0 (invalid!)

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 5);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send error response");
      Assert (Response (1) = 16#83#, "FC should be 0x83 (exception)");
      Assert (Response (2) = 16#03#, "Exception code should be Illegal Value");
   end Test_Invalid_Quantity;

   --  Test: Write Single Coil OFF (FC05 with 0x0000)
   procedure Test_Write_Single_Coil_Off (T : in Out Test_Case'Class);
   procedure Test_Write_Single_Coil_Off (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      Mock_Coils := [others => False];
      Mock_Coils (5) := True;  --  Start with coil ON

      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Write_Single_Coil := Mock_Write_Single_Coil'Access;

      --  FC05: Write Single Coil at address 5, value OFF (0x0000)
      PDU (0) := 16#05#;
      PDU (1) := 0; PDU (2) := 5;       --  Address 5
      PDU (3) := 16#00#; PDU (4) := 0;  --  Value OFF

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 5);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send response");
      Assert (Response (1) = 16#05#, "FC should be 0x05");
      Assert (Mock_Coils (5) = False, "Coil 5 should be OFF");
   end Test_Write_Single_Coil_Off;

   --  Test: Write Single Coil with invalid value
   procedure Test_Write_Single_Coil_Invalid (T : in Out Test_Case'Class);
   procedure Test_Write_Single_Coil_Invalid (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      Mock_Coils := [others => False];

      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Write_Single_Coil := Mock_Write_Single_Coil'Access;

      --  FC05: Write Single Coil with invalid value (0x1234 - not 0xFF00 or 0x0000)
      PDU (0) := 16#05#;
      PDU (1) := 0; PDU (2) := 5;       --  Address 5
      PDU (3) := 16#12#; PDU (4) := 16#34#;  --  Invalid value

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 5);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send error response");
      Assert (Response (1) = 16#85#, "FC should be 0x85 (exception)");
      Assert (Response (2) = 16#03#, "Exception code should be Illegal Value");
   end Test_Write_Single_Coil_Invalid;

   --  Test: Illegal address error
   procedure Test_Illegal_Address (T : in Out Test_Case'Class);
   procedure Test_Illegal_Address (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      --  Reset mocks
      Mock_Registers := [others => 0];
      Mock_Coils := [others => False];

      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Read_Holding_Registers := Mock_Read_Holding_Registers'Access;

      --  FC03 with address beyond range
      PDU (0) := 16#03#;
      PDU (1) := 16#FF#; PDU (2) := 16#00#;  --  Address 65280
      PDU (3) := 0; PDU (4) := 1;            --  Quantity 1

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 5);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send error response");
      Assert (Response (1) = 16#83#, "FC should be 0x83 (exception)");
      Assert (Response (2) = 16#02#, "Exception code should be Illegal Address");
   end Test_Illegal_Address;

   --  Test: ASCII mode processing
   procedure Test_ASCII_Processing (T : in Out Test_Case'Class);
   procedure Test_ASCII_Processing (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 511) := [others => 0];
      Response : Byte_Array (0 .. 511);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_Frame : Protocol.ASCII.Frame_Buffer;
      Req_Len  : Natural;
      Req_PDU  : Protocol.PDU_Buffer := [others => 0];
   begin
      Mock_Registers := [others => 0];
      Mock_Registers (0) := 16#ABCD#;

      Config.Mode := Ada_Modbus.Slave.ASCII;
      Config.Unit_Id := 1;
      Config.Callbacks.Read_Holding_Registers := Mock_Read_Holding_Registers'Access;

      --  FC03: Read 1 register from address 0
      Req_PDU (0) := 16#03#;
      Req_PDU (1) := 0; Req_PDU (2) := 0;  --  Address 0
      Req_PDU (3) := 0; Req_PDU (4) := 1;  --  Quantity 1

      Protocol.ASCII.Build_Frame (Req_Frame, Req_Len, Slave => 1, PDU => Req_PDU, PDU_Length => 5);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_Frame (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send ASCII response");
      Assert (Resp_Len > 0, "Response should not be empty");
      --  ASCII response starts with ':'
      Assert (Response (0) = Character'Pos (':'), "ASCII frame should start with ':'");
   end Test_ASCII_Processing;

   --  Test: ASCII broadcast (no response)
   procedure Test_ASCII_Broadcast (T : in Out Test_Case'Class);
   procedure Test_ASCII_Broadcast (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 511) := [others => 0];
      Response : Byte_Array (0 .. 511);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_Frame : Protocol.ASCII.Frame_Buffer;
      Req_Len  : Natural;
      Req_PDU  : Protocol.PDU_Buffer := [others => 0];
   begin
      Mock_Registers := [others => 0];

      Config.Mode := Ada_Modbus.Slave.ASCII;
      Config.Unit_Id := 1;
      Config.Callbacks.Write_Single_Register := Mock_Write_Single_Register'Access;

      --  FC06: Write register 10 = 0x1234
      Req_PDU (0) := 16#06#;
      Req_PDU (1) := 0; Req_PDU (2) := 10;  --  Address 10
      Req_PDU (3) := 16#12#; Req_PDU (4) := 16#34#;  --  Value

      --  Broadcast address = 0
      Protocol.ASCII.Build_Frame (Req_Frame, Req_Len, Slave => 0, PDU => Req_PDU, PDU_Length => 5);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_Frame (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (not Send, "Should NOT send response to broadcast");
      Assert (Mock_Registers (10) = 16#1234#, "Register should still be written");
   end Test_ASCII_Broadcast;

   --  Mock callback that returns an error
   function Mock_Read_Registers_Error
     (Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array) return Status
   is
      pragma Unreferenced (Start_Address, Quantity);
   begin
      Values := [others => 0];
      return Exception_Slave_Failure;
   end Mock_Read_Registers_Error;

   --  Test: Callback returns error
   procedure Test_Callback_Error (T : in Out Test_Case'Class);
   procedure Test_Callback_Error (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Read_Holding_Registers := Mock_Read_Registers_Error'Access;

      --  FC03: Read registers (callback will fail)
      PDU (0) := 16#03#;
      PDU (1) := 0; PDU (2) := 0;
      PDU (3) := 0; PDU (4) := 1;

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 5);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send error response");
      Assert (Response (1) = 16#83#, "FC should be 0x83 (exception)");
      Assert (Response (2) = 16#04#, "Exception code should be Slave Failure");
   end Test_Callback_Error;

   --  Test: Quantity too large (over 125 registers)
   procedure Test_Quantity_Too_Large (T : in Out Test_Case'Class);
   procedure Test_Quantity_Too_Large (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Read_Holding_Registers := Mock_Read_Holding_Registers'Access;

      --  FC03 with quantity 126 (over max 125)
      PDU (0) := 16#03#;
      PDU (1) := 0; PDU (2) := 0;
      PDU (3) := 0; PDU (4) := 126;  --  Quantity 126 (invalid)

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 5);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send error response");
      Assert (Response (1) = 16#83#, "FC should be 0x83 (exception)");
      Assert (Response (2) = 16#03#, "Exception code should be Illegal Value");
   end Test_Quantity_Too_Large;

   --  Test: Coil quantity too large (over 2000)
   procedure Test_Coil_Quantity_Too_Large (T : in Out Test_Case'Class);
   procedure Test_Coil_Quantity_Too_Large (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Read_Coils := Mock_Read_Coils'Access;

      --  FC01 with quantity 2001 (over max 2000)
      PDU (0) := 16#01#;
      PDU (1) := 0; PDU (2) := 0;
      PDU (3) := 16#07#; PDU (4) := 16#D1#;  --  Quantity 2001 (invalid)

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 5);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send error response");
      Assert (Response (1) = 16#81#, "FC should be 0x81 (exception)");
      Assert (Response (2) = 16#03#, "Exception code should be Illegal Value");
   end Test_Coil_Quantity_Too_Large;

   --  Mock callback that returns an error for coils
   function Mock_Read_Coils_Error
     (Start_Address : Coil_Address;
      Quantity      : Coil_Count;
      Values        : out Coil_Array) return Status
   is
      pragma Unreferenced (Start_Address, Quantity);
   begin
      Values := [others => False];
      return Exception_Slave_Failure;
   end Mock_Read_Coils_Error;

   --  Mock callback for discrete inputs error
   function Mock_Read_Discrete_Inputs_Error
     (Start_Address : Coil_Address;
      Quantity      : Coil_Count;
      Values        : out Coil_Array) return Status
   is
      pragma Unreferenced (Start_Address, Quantity);
   begin
      Values := [others => False];
      return Exception_Illegal_Address;
   end Mock_Read_Discrete_Inputs_Error;

   --  Mock callback for input registers error
   function Mock_Read_Input_Registers_Error
     (Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array) return Status
   is
      pragma Unreferenced (Start_Address, Quantity);
   begin
      Values := [others => 0];
      return Exception_Illegal_Address;
   end Mock_Read_Input_Registers_Error;

   --  Test: Read Coils callback error
   procedure Test_Read_Coils_Error (T : in Out Test_Case'Class);
   procedure Test_Read_Coils_Error (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Read_Coils := Mock_Read_Coils_Error'Access;

      PDU (0) := 16#01#;
      PDU (1) := 0; PDU (2) := 0;
      PDU (3) := 0; PDU (4) := 8;

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 5);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send error response");
      Assert (Response (1) = 16#81#, "FC should be 0x81 (exception)");
      Assert (Response (2) = 16#04#, "Exception code should be Slave Failure");
   end Test_Read_Coils_Error;

   --  Test: Read Discrete Inputs callback error
   procedure Test_Read_Discrete_Inputs_Error (T : in Out Test_Case'Class);
   procedure Test_Read_Discrete_Inputs_Error (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Read_Discrete_Inputs := Mock_Read_Discrete_Inputs_Error'Access;

      PDU (0) := 16#02#;
      PDU (1) := 0; PDU (2) := 0;
      PDU (3) := 0; PDU (4) := 8;

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 5);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send error response");
      Assert (Response (1) = 16#82#, "FC should be 0x82 (exception)");
      Assert (Response (2) = 16#02#, "Exception code should be Illegal Address");
   end Test_Read_Discrete_Inputs_Error;

   --  Test: Read Input Registers callback error
   procedure Test_Read_Input_Registers_Error (T : in Out Test_Case'Class);
   procedure Test_Read_Input_Registers_Error (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Read_Input_Registers := Mock_Read_Input_Registers_Error'Access;

      PDU (0) := 16#04#;
      PDU (1) := 0; PDU (2) := 0;
      PDU (3) := 0; PDU (4) := 1;

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 5);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send error response");
      Assert (Response (1) = 16#84#, "FC should be 0x84 (exception)");
      Assert (Response (2) = 16#02#, "Exception code should be Illegal Address");
   end Test_Read_Input_Registers_Error;

   --  Test: FC02 null callback
   procedure Test_FC02_Null_Callback (T : in Out Test_Case'Class);
   procedure Test_FC02_Null_Callback (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;
      --  No callback registered

      PDU (0) := 16#02#;
      PDU (1) := 0; PDU (2) := 0;
      PDU (3) := 0; PDU (4) := 8;

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 5);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send error response");
      Assert (Response (1) = 16#82#, "FC should be 0x82 (exception)");
      Assert (Response (2) = 16#01#, "Exception code should be Illegal Function");
   end Test_FC02_Null_Callback;

   --  Test: FC04 null callback
   procedure Test_FC04_Null_Callback (T : in Out Test_Case'Class);
   procedure Test_FC04_Null_Callback (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;

      PDU (0) := 16#04#;
      PDU (1) := 0; PDU (2) := 0;
      PDU (3) := 0; PDU (4) := 1;

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 5);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send error response");
      Assert (Response (1) = 16#84#, "FC should be 0x84 (exception)");
      Assert (Response (2) = 16#01#, "Exception code should be Illegal Function");
   end Test_FC04_Null_Callback;

   --  Test: FC05 null callback
   procedure Test_FC05_Null_Callback (T : in Out Test_Case'Class);
   procedure Test_FC05_Null_Callback (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;

      PDU (0) := 16#05#;
      PDU (1) := 0; PDU (2) := 0;
      PDU (3) := 16#FF#; PDU (4) := 0;

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 5);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send error response");
      Assert (Response (1) = 16#85#, "FC should be 0x85 (exception)");
      Assert (Response (2) = 16#01#, "Exception code should be Illegal Function");
   end Test_FC05_Null_Callback;

   --  Test: FC06 null callback
   procedure Test_FC06_Null_Callback (T : in Out Test_Case'Class);
   procedure Test_FC06_Null_Callback (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;

      PDU (0) := 16#06#;
      PDU (1) := 0; PDU (2) := 0;
      PDU (3) := 16#12#; PDU (4) := 16#34#;

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 5);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send error response");
      Assert (Response (1) = 16#86#, "FC should be 0x86 (exception)");
      Assert (Response (2) = 16#01#, "Exception code should be Illegal Function");
   end Test_FC06_Null_Callback;

   --  Test: FC15 null callback
   procedure Test_FC15_Null_Callback (T : in Out Test_Case'Class);
   procedure Test_FC15_Null_Callback (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;

      PDU (0) := 16#0F#;
      PDU (1) := 0; PDU (2) := 0;
      PDU (3) := 0; PDU (4) := 8;
      PDU (5) := 1;
      PDU (6) := 16#FF#;

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 7);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send error response");
      Assert (Response (1) = 16#8F#, "FC should be 0x8F (exception)");
      Assert (Response (2) = 16#01#, "Exception code should be Illegal Function");
   end Test_FC15_Null_Callback;

   --  Test: FC16 null callback
   procedure Test_FC16_Null_Callback (T : in Out Test_Case'Class);
   procedure Test_FC16_Null_Callback (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;

      PDU (0) := 16#10#;
      PDU (1) := 0; PDU (2) := 0;
      PDU (3) := 0; PDU (4) := 1;
      PDU (5) := 2;
      PDU (6) := 16#12#; PDU (7) := 16#34#;

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 8);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send error response");
      Assert (Response (1) = 16#90#, "FC should be 0x90 (exception)");
      Assert (Response (2) = 16#01#, "Exception code should be Illegal Function");
   end Test_FC16_Null_Callback;

   --  Test: Short PDU for FC01
   procedure Test_Short_PDU_FC01 (T : in Out Test_Case'Class);
   procedure Test_Short_PDU_FC01 (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Read_Coils := Mock_Read_Coils'Access;

      --  Only FC + 2 bytes (need 5)
      PDU (0) := 16#01#;
      PDU (1) := 0; PDU (2) := 0;

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 3);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send error response");
      Assert (Response (1) = 16#81#, "FC should be 0x81 (exception)");
      Assert (Response (2) = 16#03#, "Exception code should be Illegal Value");
   end Test_Short_PDU_FC01;

   --  Test: Short PDU for FC02
   procedure Test_Short_PDU_FC02 (T : in Out Test_Case'Class);
   procedure Test_Short_PDU_FC02 (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Read_Discrete_Inputs := Mock_Read_Discrete_Inputs'Access;

      PDU (0) := 16#02#;
      PDU (1) := 0; PDU (2) := 0;

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 3);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send error response");
      Assert (Response (1) = 16#82#, "FC should be 0x82 (exception)");
      Assert (Response (2) = 16#03#, "Exception code should be Illegal Value");
   end Test_Short_PDU_FC02;

   --  Mock callbacks for write errors
   function Mock_Write_Single_Coil_Error
     (Address : Coil_Address;
      Value   : Coil_Value) return Status
   is
      pragma Unreferenced (Address, Value);
   begin
      return Exception_Slave_Failure;
   end Mock_Write_Single_Coil_Error;

   function Mock_Write_Single_Register_Error
     (Address : Register_Address;
      Value   : Register_Value) return Status
   is
      pragma Unreferenced (Address, Value);
   begin
      return Exception_Slave_Failure;
   end Mock_Write_Single_Register_Error;

   function Mock_Write_Multiple_Coils_Error
     (Start_Address : Coil_Address;
      Values        : Coil_Array) return Status
   is
      pragma Unreferenced (Start_Address, Values);
   begin
      return Exception_Slave_Failure;
   end Mock_Write_Multiple_Coils_Error;

   function Mock_Write_Multiple_Registers_Error
     (Start_Address : Register_Address;
      Values        : Register_Array) return Status
   is
      pragma Unreferenced (Start_Address, Values);
   begin
      return Exception_Slave_Failure;
   end Mock_Write_Multiple_Registers_Error;

   --  Test: FC01 null callback
   procedure Test_FC01_Null_Callback (T : in Out Test_Case'Class);
   procedure Test_FC01_Null_Callback (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;

      PDU (0) := 16#01#;
      PDU (1) := 0; PDU (2) := 0;
      PDU (3) := 0; PDU (4) := 8;

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 5);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send error response");
      Assert (Response (1) = 16#81#, "FC should be 0x81 (exception)");
      Assert (Response (2) = 16#01#, "Exception code should be Illegal Function");
   end Test_FC01_Null_Callback;

   --  Test: FC03 short PDU
   procedure Test_Short_PDU_FC03 (T : in Out Test_Case'Class);
   procedure Test_Short_PDU_FC03 (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Read_Holding_Registers := Mock_Read_Holding_Registers'Access;

      PDU (0) := 16#03#;
      PDU (1) := 0; PDU (2) := 0;

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 3);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send error response");
      Assert (Response (1) = 16#83#, "FC should be 0x83 (exception)");
      Assert (Response (2) = 16#03#, "Exception code should be Illegal Value");
   end Test_Short_PDU_FC03;

   --  Test: FC04 short PDU
   procedure Test_Short_PDU_FC04 (T : in Out Test_Case'Class);
   procedure Test_Short_PDU_FC04 (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Read_Input_Registers := Mock_Read_Input_Registers'Access;

      PDU (0) := 16#04#;
      PDU (1) := 0; PDU (2) := 0;

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 3);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send error response");
      Assert (Response (1) = 16#84#, "FC should be 0x84 (exception)");
      Assert (Response (2) = 16#03#, "Exception code should be Illegal Value");
   end Test_Short_PDU_FC04;

   --  Test: FC04 invalid quantity
   procedure Test_FC04_Invalid_Quantity (T : in Out Test_Case'Class);
   procedure Test_FC04_Invalid_Quantity (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Read_Input_Registers := Mock_Read_Input_Registers'Access;

      PDU (0) := 16#04#;
      PDU (1) := 0; PDU (2) := 0;
      PDU (3) := 0; PDU (4) := 0;  --  Quantity 0

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 5);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send error response");
      Assert (Response (1) = 16#84#, "FC should be 0x84 (exception)");
      Assert (Response (2) = 16#03#, "Exception code should be Illegal Value");
   end Test_FC04_Invalid_Quantity;

   --  Test: FC05 short PDU
   procedure Test_Short_PDU_FC05 (T : in Out Test_Case'Class);
   procedure Test_Short_PDU_FC05 (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Write_Single_Coil := Mock_Write_Single_Coil'Access;

      PDU (0) := 16#05#;
      PDU (1) := 0; PDU (2) := 0;

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 3);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send error response");
      Assert (Response (1) = 16#85#, "FC should be 0x85 (exception)");
      Assert (Response (2) = 16#03#, "Exception code should be Illegal Value");
   end Test_Short_PDU_FC05;

   --  Test: FC05 callback error
   procedure Test_FC05_Callback_Error (T : in Out Test_Case'Class);
   procedure Test_FC05_Callback_Error (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Write_Single_Coil := Mock_Write_Single_Coil_Error'Access;

      PDU (0) := 16#05#;
      PDU (1) := 0; PDU (2) := 0;
      PDU (3) := 16#FF#; PDU (4) := 0;

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 5);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send error response");
      Assert (Response (1) = 16#85#, "FC should be 0x85 (exception)");
      Assert (Response (2) = 16#04#, "Exception code should be Slave Failure");
   end Test_FC05_Callback_Error;

   --  Test: FC06 short PDU
   procedure Test_Short_PDU_FC06 (T : in Out Test_Case'Class);
   procedure Test_Short_PDU_FC06 (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Write_Single_Register := Mock_Write_Single_Register'Access;

      PDU (0) := 16#06#;
      PDU (1) := 0; PDU (2) := 0;

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 3);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send error response");
      Assert (Response (1) = 16#86#, "FC should be 0x86 (exception)");
      Assert (Response (2) = 16#03#, "Exception code should be Illegal Value");
   end Test_Short_PDU_FC06;

   --  Test: FC06 callback error
   procedure Test_FC06_Callback_Error (T : in Out Test_Case'Class);
   procedure Test_FC06_Callback_Error (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Write_Single_Register := Mock_Write_Single_Register_Error'Access;

      PDU (0) := 16#06#;
      PDU (1) := 0; PDU (2) := 0;
      PDU (3) := 16#12#; PDU (4) := 16#34#;

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 5);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send error response");
      Assert (Response (1) = 16#86#, "FC should be 0x86 (exception)");
      Assert (Response (2) = 16#04#, "Exception code should be Slave Failure");
   end Test_FC06_Callback_Error;

   --  Test: FC15 short PDU
   procedure Test_Short_PDU_FC15 (T : in Out Test_Case'Class);
   procedure Test_Short_PDU_FC15 (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Write_Multiple_Coils := Mock_Write_Multiple_Coils'Access;

      PDU (0) := 16#0F#;
      PDU (1) := 0; PDU (2) := 0;
      PDU (3) := 0; PDU (4) := 8;

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 5);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send error response");
      Assert (Response (1) = 16#8F#, "FC should be 0x8F (exception)");
      Assert (Response (2) = 16#03#, "Exception code should be Illegal Value");
   end Test_Short_PDU_FC15;

   --  Test: FC15 invalid quantity
   procedure Test_FC15_Invalid_Quantity (T : in Out Test_Case'Class);
   procedure Test_FC15_Invalid_Quantity (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Write_Multiple_Coils := Mock_Write_Multiple_Coils'Access;

      PDU (0) := 16#0F#;
      PDU (1) := 0; PDU (2) := 0;
      PDU (3) := 0; PDU (4) := 0;  --  Quantity 0
      PDU (5) := 0;
      PDU (6) := 0;

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 7);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send error response");
      Assert (Response (1) = 16#8F#, "FC should be 0x8F (exception)");
      Assert (Response (2) = 16#03#, "Exception code should be Illegal Value");
   end Test_FC15_Invalid_Quantity;

   --  Test: FC15 byte count mismatch
   procedure Test_FC15_Byte_Count_Mismatch (T : in Out Test_Case'Class);
   procedure Test_FC15_Byte_Count_Mismatch (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Write_Multiple_Coils := Mock_Write_Multiple_Coils'Access;

      --  Quantity 8 needs byte count 1, but we say 2
      PDU (0) := 16#0F#;
      PDU (1) := 0; PDU (2) := 0;
      PDU (3) := 0; PDU (4) := 8;
      PDU (5) := 2;  --  Wrong byte count (should be 1)
      PDU (6) := 16#FF#;
      PDU (7) := 16#FF#;

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 8);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send error response");
      Assert (Response (1) = 16#8F#, "FC should be 0x8F (exception)");
      Assert (Response (2) = 16#03#, "Exception code should be Illegal Value");
   end Test_FC15_Byte_Count_Mismatch;

   --  Test: FC15 callback error
   procedure Test_FC15_Callback_Error (T : in Out Test_Case'Class);
   procedure Test_FC15_Callback_Error (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Write_Multiple_Coils := Mock_Write_Multiple_Coils_Error'Access;

      PDU (0) := 16#0F#;
      PDU (1) := 0; PDU (2) := 0;
      PDU (3) := 0; PDU (4) := 8;
      PDU (5) := 1;
      PDU (6) := 16#FF#;

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 7);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send error response");
      Assert (Response (1) = 16#8F#, "FC should be 0x8F (exception)");
      Assert (Response (2) = 16#04#, "Exception code should be Slave Failure");
   end Test_FC15_Callback_Error;

   --  Test: FC16 short PDU
   procedure Test_Short_PDU_FC16 (T : in Out Test_Case'Class);
   procedure Test_Short_PDU_FC16 (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Write_Multiple_Registers := Mock_Write_Multiple_Registers'Access;

      PDU (0) := 16#10#;
      PDU (1) := 0; PDU (2) := 0;
      PDU (3) := 0; PDU (4) := 1;

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 5);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send error response");
      Assert (Response (1) = 16#90#, "FC should be 0x90 (exception)");
      Assert (Response (2) = 16#03#, "Exception code should be Illegal Value");
   end Test_Short_PDU_FC16;

   --  Test: FC16 invalid quantity
   procedure Test_FC16_Invalid_Quantity (T : in Out Test_Case'Class);
   procedure Test_FC16_Invalid_Quantity (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Write_Multiple_Registers := Mock_Write_Multiple_Registers'Access;

      PDU (0) := 16#10#;
      PDU (1) := 0; PDU (2) := 0;
      PDU (3) := 0; PDU (4) := 0;  --  Quantity 0
      PDU (5) := 0;
      PDU (6) := 0; PDU (7) := 0;

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 8);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send error response");
      Assert (Response (1) = 16#90#, "FC should be 0x90 (exception)");
      Assert (Response (2) = 16#03#, "Exception code should be Illegal Value");
   end Test_FC16_Invalid_Quantity;

   --  Test: FC16 byte count mismatch
   procedure Test_FC16_Byte_Count_Mismatch (T : in Out Test_Case'Class);
   procedure Test_FC16_Byte_Count_Mismatch (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Write_Multiple_Registers := Mock_Write_Multiple_Registers'Access;

      --  Quantity 1 needs byte count 2, but we say 4
      PDU (0) := 16#10#;
      PDU (1) := 0; PDU (2) := 0;
      PDU (3) := 0; PDU (4) := 1;
      PDU (5) := 4;  --  Wrong byte count (should be 2)
      PDU (6) := 16#12#; PDU (7) := 16#34#;
      PDU (8) := 16#56#; PDU (9) := 16#78#;

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 10);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send error response");
      Assert (Response (1) = 16#90#, "FC should be 0x90 (exception)");
      Assert (Response (2) = 16#03#, "Exception code should be Illegal Value");
   end Test_FC16_Byte_Count_Mismatch;

   --  Test: FC16 callback error
   procedure Test_FC16_Callback_Error (T : in Out Test_Case'Class);
   procedure Test_FC16_Callback_Error (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Write_Multiple_Registers := Mock_Write_Multiple_Registers_Error'Access;

      PDU (0) := 16#10#;
      PDU (1) := 0; PDU (2) := 0;
      PDU (3) := 0; PDU (4) := 1;
      PDU (5) := 2;
      PDU (6) := 16#12#; PDU (7) := 16#34#;

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 8);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send error response");
      Assert (Response (1) = 16#90#, "FC should be 0x90 (exception)");
      Assert (Response (2) = 16#04#, "Exception code should be Slave Failure");
   end Test_FC16_Callback_Error;

   --  Test: FC07 null callback
   procedure Test_FC07_Null_Callback (T : in Out Test_Case'Class);
   procedure Test_FC07_Null_Callback (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;

      PDU (0) := 16#07#;

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 1);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send error response");
      Assert (Response (1) = 16#87#, "FC should be 0x87 (exception)");
      Assert (Response (2) = 16#01#, "Exception code should be Illegal Function");
   end Test_FC07_Null_Callback;

   --  Test: FC08 null callback
   procedure Test_FC08_Null_Callback (T : in Out Test_Case'Class);
   procedure Test_FC08_Null_Callback (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;

      PDU (0) := 16#08#;
      PDU (1) := 0; PDU (2) := 0;
      PDU (3) := 16#12#; PDU (4) := 16#34#;

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 5);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send error response");
      Assert (Response (1) = 16#88#, "FC should be 0x88 (exception)");
      Assert (Response (2) = 16#01#, "Exception code should be Illegal Function");
   end Test_FC08_Null_Callback;

   --  Test: FC08 short PDU
   procedure Test_FC08_Short_PDU (T : in Out Test_Case'Class);
   procedure Test_FC08_Short_PDU (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Diagnostics := Mock_Diagnostics'Access;

      PDU (0) := 16#08#;
      PDU (1) := 0; PDU (2) := 0;

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 3);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send error response");
      Assert (Response (1) = 16#88#, "FC should be 0x88 (exception)");
      Assert (Response (2) = 16#03#, "Exception code should be Illegal Value");
   end Test_FC08_Short_PDU;

   --  Test: FC17 null callback
   procedure Test_FC17_Null_Callback (T : in Out Test_Case'Class);
   procedure Test_FC17_Null_Callback (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;

      PDU (0) := 16#11#;  --  FC 17

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 1);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send error response");
      Assert (Response (1) = 16#91#, "FC should be 0x91 (exception)");
      Assert (Response (2) = 16#01#, "Exception code should be Illegal Function");
   end Test_FC17_Null_Callback;

   --  Test: FC22 null callback
   procedure Test_FC22_Null_Callback (T : in Out Test_Case'Class);
   procedure Test_FC22_Null_Callback (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;

      PDU (0) := 16#16#;  --  FC 22
      PDU (1) := 0; PDU (2) := 0;
      PDU (3) := 16#FF#; PDU (4) := 16#FF#;
      PDU (5) := 0; PDU (6) := 0;

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 7);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send error response");
      Assert (Response (1) = 16#96#, "FC should be 0x96 (exception)");
      Assert (Response (2) = 16#01#, "Exception code should be Illegal Function");
   end Test_FC22_Null_Callback;

   --  Test: FC22 short PDU
   procedure Test_FC22_Short_PDU (T : in Out Test_Case'Class);
   procedure Test_FC22_Short_PDU (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Mask_Write_Register := Mock_Mask_Write_Register'Access;

      PDU (0) := 16#16#;  --  FC 22
      PDU (1) := 0; PDU (2) := 0;
      PDU (3) := 16#FF#; PDU (4) := 16#FF#;

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 5);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send error response");
      Assert (Response (1) = 16#96#, "FC should be 0x96 (exception)");
      Assert (Response (2) = 16#03#, "Exception code should be Illegal Value");
   end Test_FC22_Short_PDU;

   --  Test: FC23 null callback
   procedure Test_FC23_Null_Callback (T : in Out Test_Case'Class);
   procedure Test_FC23_Null_Callback (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;

      PDU (0) := 16#17#;  --  FC 23
      PDU (1) := 0; PDU (2) := 0;   --  Read start
      PDU (3) := 0; PDU (4) := 1;   --  Read qty
      PDU (5) := 0; PDU (6) := 0;   --  Write start
      PDU (7) := 0; PDU (8) := 1;   --  Write qty
      PDU (9) := 2;                  --  Byte count
      PDU (10) := 16#12#; PDU (11) := 16#34#;

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 12);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send error response");
      Assert (Response (1) = 16#97#, "FC should be 0x97 (exception)");
      Assert (Response (2) = 16#01#, "Exception code should be Illegal Function");
   end Test_FC23_Null_Callback;

   --  Test: FC23 short PDU
   procedure Test_FC23_Short_PDU (T : in Out Test_Case'Class);
   procedure Test_FC23_Short_PDU (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Read_Write_Registers := Mock_Read_Write_Registers'Access;

      PDU (0) := 16#17#;  --  FC 23
      PDU (1) := 0; PDU (2) := 0;
      PDU (3) := 0; PDU (4) := 1;
      PDU (5) := 0; PDU (6) := 0;
      PDU (7) := 0; PDU (8) := 1;

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 9);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send error response");
      Assert (Response (1) = 16#97#, "FC should be 0x97 (exception)");
      Assert (Response (2) = 16#03#, "Exception code should be Illegal Value");
   end Test_FC23_Short_PDU;

   --  Test: FC15 data too short
   procedure Test_FC15_Data_Too_Short (T : in Out Test_Case'Class);
   procedure Test_FC15_Data_Too_Short (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Write_Multiple_Coils := Mock_Write_Multiple_Coils'Access;

      --  Byte count says 2, but only 1 byte of data provided
      PDU (0) := 16#0F#;
      PDU (1) := 0; PDU (2) := 0;
      PDU (3) := 0; PDU (4) := 16;  --  Quantity 16 (needs 2 bytes)
      PDU (5) := 2;                   --  Byte count 2
      PDU (6) := 16#FF#;              --  Only 1 byte of data!

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 7);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send error response");
      Assert (Response (1) = 16#8F#, "FC should be 0x8F (exception)");
      Assert (Response (2) = 16#03#, "Exception code should be Illegal Value");
   end Test_FC15_Data_Too_Short;

   --  Test: FC16 data too short
   procedure Test_FC16_Data_Too_Short (T : in Out Test_Case'Class);
   procedure Test_FC16_Data_Too_Short (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Write_Multiple_Registers := Mock_Write_Multiple_Registers'Access;

      --  Byte count says 4, but only 2 bytes of data provided
      PDU (0) := 16#10#;
      PDU (1) := 0; PDU (2) := 0;
      PDU (3) := 0; PDU (4) := 2;   --  Quantity 2 (needs 4 bytes)
      PDU (5) := 4;                  --  Byte count 4
      PDU (6) := 16#12#; PDU (7) := 16#34#;  --  Only 2 bytes of data!

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 8);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send error response");
      Assert (Response (1) = 16#90#, "FC should be 0x90 (exception)");
      Assert (Response (2) = 16#03#, "Exception code should be Illegal Value");
   end Test_FC16_Data_Too_Short;

   --  Test: FC02 invalid quantity
   procedure Test_FC02_Invalid_Quantity (T : in Out Test_Case'Class);
   procedure Test_FC02_Invalid_Quantity (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;
      Config.Callbacks.Read_Discrete_Inputs := Mock_Read_Discrete_Inputs'Access;

      --  Quantity 0 (invalid)
      PDU (0) := 16#02#;
      PDU (1) := 0; PDU (2) := 0;
      PDU (3) := 0; PDU (4) := 0;

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 5);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send error response");
      Assert (Response (1) = 16#82#, "FC should be 0x82 (exception)");
      Assert (Response (2) = 16#03#, "Exception code should be Illegal Value");
   end Test_FC02_Invalid_Quantity;

   --  Test: Unknown function code
   procedure Test_Unknown_Function_Code (T : in Out Test_Case'Class);
   procedure Test_Unknown_Function_Code (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Config   : Slave_Config;
      Request  : Byte_Array (0 .. 255) := [others => 0];
      Response : Byte_Array (0 .. 255);
      Resp_Len : Natural;
      Send     : Boolean;
      Req_ADU  : Protocol.RTU.ADU_Buffer;
      Req_Len  : Natural;
      PDU      : Protocol.PDU_Buffer := [others => 0];
   begin
      Config.Mode := RTU;
      Config.Unit_Id := 1;

      --  FC 99 - unknown function code
      PDU (0) := 16#63#;  --  99
      PDU (1) := 0; PDU (2) := 0;

      Protocol.RTU.Build_Frame (Req_ADU, Req_Len, Slave => 1, PDU => PDU, PDU_Length => 3);
      for I in 0 .. Req_Len - 1 loop
         Request (I) := Req_ADU (I);
      end loop;

      Process_Request (Config, Request, Req_Len, Response, Resp_Len, Send);

      Assert (Send, "Should send error response");
      Assert (Response (1) = 16#E3#, "FC should be 0xE3 (99 + 0x80)");
      Assert (Response (2) = 16#01#, "Exception code should be Illegal Function");
   end Test_Unknown_Function_Code;

   overriding procedure Register_Tests (T : in Out Slave_Test_Case) is
   begin
      Registration.Register_Routine (T, Test_Read_Holding_Registers_RTU'Access, "Read Holding Registers RTU");
      Registration.Register_Routine (T, Test_Write_Single_Register_RTU'Access, "Write Single Register RTU");
      Registration.Register_Routine (T, Test_Illegal_Function'Access, "Illegal Function Exception");
      Registration.Register_Routine (T, Test_Broadcast_No_Response'Access, "Broadcast No Response");
      Registration.Register_Routine (T, Test_Wrong_Slave_ID'Access, "Wrong Slave ID");
      Registration.Register_Routine (T, Test_TCP_Processing'Access, "TCP Processing");
      Registration.Register_Routine (T, Test_Read_Coils'Access, "Read Coils");
      Registration.Register_Routine (T, Test_Write_Multiple_Registers'Access, "Write Multiple Registers");
      --  New FC tests
      Registration.Register_Routine (T, Test_Read_Exception_Status'Access, "Read Exception Status (FC 07)");
      Registration.Register_Routine (T, Test_Diagnostics'Access, "Diagnostics (FC 08)");
      Registration.Register_Routine (T, Test_Report_Server_Id'Access, "Report Server ID (FC 17)");
      Registration.Register_Routine (T, Test_Mask_Write_Register'Access, "Mask Write Register (FC 22)");
      Registration.Register_Routine (T, Test_Read_Write_Registers'Access, "Read/Write Registers (FC 23)");
      --  Additional coverage tests
      Registration.Register_Routine (T, Test_Read_Discrete_Inputs'Access, "Read Discrete Inputs (FC 02)");
      Registration.Register_Routine (T, Test_Read_Input_Registers'Access, "Read Input Registers (FC 04)");
      Registration.Register_Routine (T, Test_Write_Single_Coil'Access, "Write Single Coil (FC 05)");
      Registration.Register_Routine (T, Test_Write_Multiple_Coils'Access, "Write Multiple Coils (FC 15)");
      Registration.Register_Routine (T, Test_Invalid_Quantity'Access, "Invalid Quantity Error");
      Registration.Register_Routine (T, Test_Illegal_Address'Access, "Illegal Address Error");
      --  Additional coverage tests
      Registration.Register_Routine (T, Test_Write_Single_Coil_Off'Access, "Write Single Coil OFF");
      Registration.Register_Routine (T, Test_Write_Single_Coil_Invalid'Access, "Write Single Coil Invalid Value");
      Registration.Register_Routine (T, Test_ASCII_Processing'Access, "ASCII Mode Processing");
      Registration.Register_Routine (T, Test_ASCII_Broadcast'Access, "ASCII Broadcast");
      Registration.Register_Routine (T, Test_Callback_Error'Access, "Callback Error Response");
      Registration.Register_Routine (T, Test_Quantity_Too_Large'Access, "Quantity Too Large");
      Registration.Register_Routine (T, Test_Coil_Quantity_Too_Large'Access, "Coil Quantity Too Large");
      Registration.Register_Routine (T, Test_Unknown_Function_Code'Access, "Unknown Function Code");
      --  Error path tests
      Registration.Register_Routine (T, Test_Read_Coils_Error'Access, "Read Coils Callback Error");
      Registration.Register_Routine (T, Test_Read_Discrete_Inputs_Error'Access, "Read Discrete Inputs Callback Error");
      Registration.Register_Routine (T, Test_Read_Input_Registers_Error'Access, "Read Input Registers Callback Error");
      Registration.Register_Routine (T, Test_FC02_Null_Callback'Access, "FC02 Null Callback");
      Registration.Register_Routine (T, Test_FC04_Null_Callback'Access, "FC04 Null Callback");
      Registration.Register_Routine (T, Test_FC05_Null_Callback'Access, "FC05 Null Callback");
      Registration.Register_Routine (T, Test_FC06_Null_Callback'Access, "FC06 Null Callback");
      Registration.Register_Routine (T, Test_FC15_Null_Callback'Access, "FC15 Null Callback");
      Registration.Register_Routine (T, Test_FC16_Null_Callback'Access, "FC16 Null Callback");
      Registration.Register_Routine (T, Test_Short_PDU_FC01'Access, "Short PDU FC01");
      Registration.Register_Routine (T, Test_Short_PDU_FC02'Access, "Short PDU FC02");
      Registration.Register_Routine (T, Test_FC02_Invalid_Quantity'Access, "FC02 Invalid Quantity");
      Registration.Register_Routine (T, Test_FC01_Null_Callback'Access, "FC01 Null Callback");
      Registration.Register_Routine (T, Test_Short_PDU_FC03'Access, "Short PDU FC03");
      Registration.Register_Routine (T, Test_Short_PDU_FC04'Access, "Short PDU FC04");
      Registration.Register_Routine (T, Test_FC04_Invalid_Quantity'Access, "FC04 Invalid Quantity");
      Registration.Register_Routine (T, Test_Short_PDU_FC05'Access, "Short PDU FC05");
      Registration.Register_Routine (T, Test_FC05_Callback_Error'Access, "FC05 Callback Error");
      Registration.Register_Routine (T, Test_Short_PDU_FC06'Access, "Short PDU FC06");
      Registration.Register_Routine (T, Test_FC06_Callback_Error'Access, "FC06 Callback Error");
      Registration.Register_Routine (T, Test_Short_PDU_FC15'Access, "Short PDU FC15");
      Registration.Register_Routine (T, Test_FC15_Invalid_Quantity'Access, "FC15 Invalid Quantity");
      Registration.Register_Routine (T, Test_FC15_Byte_Count_Mismatch'Access, "FC15 Byte Count Mismatch");
      Registration.Register_Routine (T, Test_FC15_Callback_Error'Access, "FC15 Callback Error");
      Registration.Register_Routine (T, Test_Short_PDU_FC16'Access, "Short PDU FC16");
      Registration.Register_Routine (T, Test_FC16_Invalid_Quantity'Access, "FC16 Invalid Quantity");
      Registration.Register_Routine (T, Test_FC16_Byte_Count_Mismatch'Access, "FC16 Byte Count Mismatch");
      Registration.Register_Routine (T, Test_FC16_Callback_Error'Access, "FC16 Callback Error");
      --  Special FC tests
      Registration.Register_Routine (T, Test_FC07_Null_Callback'Access, "FC07 Null Callback");
      Registration.Register_Routine (T, Test_FC08_Null_Callback'Access, "FC08 Null Callback");
      Registration.Register_Routine (T, Test_FC08_Short_PDU'Access, "FC08 Short PDU");
      Registration.Register_Routine (T, Test_FC17_Null_Callback'Access, "FC17 Null Callback");
      Registration.Register_Routine (T, Test_FC22_Null_Callback'Access, "FC22 Null Callback");
      Registration.Register_Routine (T, Test_FC22_Short_PDU'Access, "FC22 Short PDU");
      Registration.Register_Routine (T, Test_FC23_Null_Callback'Access, "FC23 Null Callback");
      Registration.Register_Routine (T, Test_FC23_Short_PDU'Access, "FC23 Short PDU");
      Registration.Register_Routine (T, Test_FC15_Data_Too_Short'Access, "FC15 Data Too Short");
      Registration.Register_Routine (T, Test_FC16_Data_Too_Short'Access, "FC16 Data Too Short");
   end Register_Tests;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      S : constant AUnit.Test_Suites.Access_Test_Suite := new AUnit.Test_Suites.Test_Suite;
   begin
      S.Add_Test (new Slave_Test_Case);
      return S;
   end Suite;

end Test_Slave;
