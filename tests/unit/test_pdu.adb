--  Test_PDU - PDU encoding/decoding tests implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Interfaces; use type Interfaces.Unsigned_16;
with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.Protocol; use Ada_Modbus.Protocol;

package body Test_PDU is

   type PDU_Test_Case is new Test_Case with null record;

   overriding function Name (T : PDU_Test_Case) return AUnit.Message_String is
     (AUnit.Format ("PDU Encoding/Decoding Tests"));

   overriding procedure Register_Tests (T : in Out PDU_Test_Case);

   --  Test: Read Holding Registers request encoding
   procedure Test_Read_Registers_Request (T : in Out Test_Case'Class);
   procedure Test_Read_Registers_Request (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer : PDU_Buffer;
      Length : Natural;
   begin
      Encode_Read_Registers_Request
        (Buffer, Length, FC_Read_Holding_Registers,
         Start_Address => 0, Quantity => 10);

      Assert (Length = 5, "Request length should be 5");
      Assert (Buffer (0) = 16#03#, "FC should be 0x03");
      Assert (Buffer (1) = 16#00#, "Start addr high should be 0x00");
      Assert (Buffer (2) = 16#00#, "Start addr low should be 0x00");
      Assert (Buffer (3) = 16#00#, "Quantity high should be 0x00");
      Assert (Buffer (4) = 16#0A#, "Quantity low should be 0x0A");
   end Test_Read_Registers_Request;

   --  Test: Read Bits (Coils) request encoding
   procedure Test_Read_Bits_Request (T : in Out Test_Case'Class);
   procedure Test_Read_Bits_Request (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer : PDU_Buffer;
      Length : Natural;
   begin
      Encode_Read_Bits_Request
        (Buffer, Length, FC_Read_Coils,
         Start_Address => 100, Quantity => 16);

      Assert (Length = 5, "Request length should be 5");
      Assert (Buffer (0) = 16#01#, "FC should be 0x01");
      Assert (Buffer (1) = 16#00#, "Start addr high should be 0x00");
      Assert (Buffer (2) = 16#64#, "Start addr low should be 0x64");
      Assert (Buffer (3) = 16#00#, "Quantity high should be 0x00");
      Assert (Buffer (4) = 16#10#, "Quantity low should be 0x10");
   end Test_Read_Bits_Request;

   --  Test: Write Single Register request encoding
   procedure Test_Write_Single_Register_Request (T : in Out Test_Case'Class);
   procedure Test_Write_Single_Register_Request (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer : PDU_Buffer;
      Length : Natural;
   begin
      Encode_Write_Single_Register_Request
        (Buffer, Length, Address => 100, Value => 16#1234#);

      Assert (Length = 5, "Request length should be 5");
      Assert (Buffer (0) = 16#06#, "FC should be 0x06");
      Assert (Buffer (1) = 16#00#, "Address high should be 0x00");
      Assert (Buffer (2) = 16#64#, "Address low should be 0x64");
      Assert (Buffer (3) = 16#12#, "Value high should be 0x12");
      Assert (Buffer (4) = 16#34#, "Value low should be 0x34");
   end Test_Write_Single_Register_Request;

   --  Test: Write Single Coil request encoding
   procedure Test_Write_Single_Coil_Request (T : in Out Test_Case'Class);
   procedure Test_Write_Single_Coil_Request (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer : PDU_Buffer;
      Length : Natural;
   begin
      --  Test ON value
      Encode_Write_Single_Coil_Request (Buffer, Length, Address => 5, Value => True);

      Assert (Length = 5, "Request length should be 5");
      Assert (Buffer (0) = 16#05#, "FC should be 0x05");
      Assert (Buffer (3) = 16#FF#, "ON value high should be 0xFF");
      Assert (Buffer (4) = 16#00#, "ON value low should be 0x00");

      --  Test OFF value
      Encode_Write_Single_Coil_Request (Buffer, Length, Address => 5, Value => False);
      Assert (Buffer (3) = 16#00#, "OFF value high should be 0x00");
      Assert (Buffer (4) = 16#00#, "OFF value low should be 0x00");
   end Test_Write_Single_Coil_Request;

   --  Test: Exception code conversion (all codes)
   procedure Test_Exception_Codes (T : in Out Test_Case'Class);
   procedure Test_Exception_Codes (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      --  To_Exception_Byte - all exception codes
      Assert (To_Exception_Byte (Exception_Illegal_Function) = 16#01#,
              "Illegal Function should be 0x01");
      Assert (To_Exception_Byte (Exception_Illegal_Address) = 16#02#,
              "Illegal Address should be 0x02");
      Assert (To_Exception_Byte (Exception_Illegal_Value) = 16#03#,
              "Illegal Value should be 0x03");
      Assert (To_Exception_Byte (Exception_Slave_Failure) = 16#04#,
              "Slave Failure should be 0x04");
      Assert (To_Exception_Byte (Exception_Acknowledge) = 16#05#,
              "Acknowledge should be 0x05");
      Assert (To_Exception_Byte (Exception_Slave_Busy) = 16#06#,
              "Slave Busy should be 0x06");
      Assert (To_Exception_Byte (Exception_Gateway_Path) = 16#0A#,
              "Gateway Path should be 0x0A");
      Assert (To_Exception_Byte (Exception_Gateway_Target) = 16#0B#,
              "Gateway Target should be 0x0B");

      --  From_Exception_Byte - all exception codes
      Assert (From_Exception_Byte (16#01#) = Exception_Illegal_Function,
              "0x01 should be Illegal Function");
      Assert (From_Exception_Byte (16#02#) = Exception_Illegal_Address,
              "0x02 should be Illegal Address");
      Assert (From_Exception_Byte (16#03#) = Exception_Illegal_Value,
              "0x03 should be Illegal Value");
      Assert (From_Exception_Byte (16#04#) = Exception_Slave_Failure,
              "0x04 should be Slave Failure");
      Assert (From_Exception_Byte (16#05#) = Exception_Acknowledge,
              "0x05 should be Acknowledge");
      Assert (From_Exception_Byte (16#06#) = Exception_Slave_Busy,
              "0x06 should be Slave Busy");
      Assert (From_Exception_Byte (16#0A#) = Exception_Gateway_Path,
              "0x0A should be Gateway Path");
      Assert (From_Exception_Byte (16#0B#) = Exception_Gateway_Target,
              "0x0B should be Gateway Target");
      --  Unknown code defaults to Illegal Function
      Assert (From_Exception_Byte (16#FF#) = Exception_Illegal_Function,
              "Unknown code should default to Illegal Function");
   end Test_Exception_Codes;

   --  Test: Write Multiple Coils encoding
   procedure Test_Write_Multiple_Coils_Request (T : in Out Test_Case'Class);
   procedure Test_Write_Multiple_Coils_Request (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer : PDU_Buffer;
      Length : Natural;
      Coils  : constant Coil_Array (0 .. 9) :=
        [True, False, True, True, False, False, True, True, True, False];
   begin
      Encode_Write_Multiple_Coils_Request (Buffer, Length, Start_Address => 20, Values => Coils);

      Assert (Buffer (0) = 16#0F#, "FC should be 0x0F");
      Assert (Buffer (1) = 16#00# and Buffer (2) = 16#14#, "Start address should be 20");
      Assert (Buffer (3) = 16#00# and Buffer (4) = 16#0A#, "Quantity should be 10");
      Assert (Buffer (5) = 2, "Byte count should be 2");
      --  Coils packed LSB first: bits 0-7 = 11001101 = 0xCD, bits 8-9 = 01 = 0x01
      Assert (Buffer (6) = 16#CD#, "First byte should be 0xCD");
      Assert (Buffer (7) = 16#01#, "Second byte should be 0x01");
      Assert (Length = 8, "Length should be 8");
   end Test_Write_Multiple_Coils_Request;

   --  Test: Write Multiple Registers encoding
   procedure Test_Write_Multiple_Registers_Request (T : in Out Test_Case'Class);
   procedure Test_Write_Multiple_Registers_Request (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer : PDU_Buffer;
      Length : Natural;
      Regs   : constant Register_Array (0 .. 2) := [16#1234#, 16#5678#, 16#9ABC#];
   begin
      Encode_Write_Multiple_Registers_Request (Buffer, Length, Start_Address => 10, Values => Regs);

      Assert (Buffer (0) = 16#10#, "FC should be 0x10");
      Assert (Buffer (1) = 16#00# and Buffer (2) = 16#0A#, "Start address should be 10");
      Assert (Buffer (3) = 16#00# and Buffer (4) = 16#03#, "Quantity should be 3");
      Assert (Buffer (5) = 6, "Byte count should be 6");
      Assert (Buffer (6) = 16#12# and Buffer (7) = 16#34#, "First register");
      Assert (Buffer (8) = 16#56# and Buffer (9) = 16#78#, "Second register");
      Assert (Buffer (10) = 16#9A# and Buffer (11) = 16#BC#, "Third register");
      Assert (Length = 12, "Length should be 12");
   end Test_Write_Multiple_Registers_Request;

   --  Test: Decode Read Registers Response
   procedure Test_Decode_Read_Registers_Response (T : in Out Test_Case'Class);
   procedure Test_Decode_Read_Registers_Response (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer : PDU_Buffer := [others => 0];
      Values : Register_Array (0 .. 9);
      Count  : Natural;
      Result : Status;
   begin
      --  FC 03 response: FC + ByteCount + Data
      Buffer (0) := 16#03#;  --  FC
      Buffer (1) := 4;       --  Byte count = 2 registers
      Buffer (2) := 16#12#;  --  Reg 0 high
      Buffer (3) := 16#34#;  --  Reg 0 low
      Buffer (4) := 16#AB#;  --  Reg 1 high
      Buffer (5) := 16#CD#;  --  Reg 1 low

      Decode_Read_Registers_Response (Buffer, 6, Values, Count, Result);

      Assert (Result = Success, "Decode should succeed");
      Assert (Count = 2, "Count should be 2");
      Assert (Values (0) = 16#1234#, "First register should be 0x1234");
      Assert (Values (1) = 16#ABCD#, "Second register should be 0xABCD");
   end Test_Decode_Read_Registers_Response;

   --  Test: Decode Read Bits Response
   procedure Test_Decode_Read_Bits_Response (T : in Out Test_Case'Class);
   procedure Test_Decode_Read_Bits_Response (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer : PDU_Buffer := [others => 0];
      Values : Coil_Array (0 .. 15);
      Count  : Natural;
      Result : Status;
   begin
      --  FC 01 response: FC + ByteCount + CoilData
      Buffer (0) := 16#01#;  --  FC
      Buffer (1) := 2;       --  Byte count
      Buffer (2) := 16#CD#;  --  Coils 0-7: 11001101
      Buffer (3) := 16#01#;  --  Coils 8-15: 00000001

      Decode_Read_Bits_Response (Buffer, 4, Values, Count, Result);

      Assert (Result = Success, "Decode should succeed");
      Assert (Count = 16, "Count should be 16");
      Assert (Values (0) = True, "Coil 0 should be ON");
      Assert (Values (1) = False, "Coil 1 should be OFF");
      Assert (Values (2) = True, "Coil 2 should be ON");
      Assert (Values (3) = True, "Coil 3 should be ON");
      Assert (Values (7) = True, "Coil 7 should be ON");
      Assert (Values (8) = True, "Coil 8 should be ON");
      Assert (Values (9) = False, "Coil 9 should be OFF");
   end Test_Decode_Read_Bits_Response;

   --  Test: Decode Write Single Response
   procedure Test_Decode_Write_Single_Response (T : in Out Test_Case'Class);
   procedure Test_Decode_Write_Single_Response (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer  : PDU_Buffer := [others => 0];
      Address : Register_Address;
      Value   : Register_Value;
      Result  : Status;
   begin
      --  FC 06 response (echo): FC + Address + Value
      Buffer (0) := 16#06#;
      Buffer (1) := 16#00#;
      Buffer (2) := 16#64#;  --  Address 100
      Buffer (3) := 16#12#;
      Buffer (4) := 16#34#;  --  Value 0x1234

      Decode_Write_Single_Response (Buffer, 5, Address, Value, Result);

      Assert (Result = Success, "Decode should succeed");
      Assert (Address = 100, "Address should be 100");
      Assert (Value = 16#1234#, "Value should be 0x1234");
   end Test_Decode_Write_Single_Response;

   --  Test: Decode Write Multiple Response
   procedure Test_Decode_Write_Multiple_Response (T : in Out Test_Case'Class);
   procedure Test_Decode_Write_Multiple_Response (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer   : PDU_Buffer := [others => 0];
      Address  : Register_Address;
      Quantity : Natural;
      Result   : Status;
   begin
      --  FC 10 response: FC + StartAddress + Quantity
      Buffer (0) := 16#10#;
      Buffer (1) := 16#00#;
      Buffer (2) := 16#0A#;  --  Start address 10
      Buffer (3) := 16#00#;
      Buffer (4) := 16#05#;  --  Quantity 5

      Decode_Write_Multiple_Response (Buffer, 5, Address, Quantity, Result);

      Assert (Result = Success, "Decode should succeed");
      Assert (Address = 10, "Address should be 10");
      Assert (Quantity = 5, "Quantity should be 5");
   end Test_Decode_Write_Multiple_Response;

   --  Test: Decode Exception Response
   procedure Test_Decode_Exception_Response (T : in Out Test_Case'Class);
   procedure Test_Decode_Exception_Response (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer : PDU_Buffer := [others => 0];
      Result : Status;
   begin
      --  Exception response: FC+0x80 + ExceptionCode
      Buffer (0) := 16#83#;  --  FC 03 + 0x80
      Buffer (1) := 16#02#;  --  Illegal Address

      Decode_Exception_Response (Buffer, 2, Result);

      Assert (Result = Exception_Illegal_Address, "Should be Illegal Address exception");
   end Test_Decode_Exception_Response;

   --  Test: Decode response detects exception
   procedure Test_Decode_Detects_Exception (T : in Out Test_Case'Class);
   procedure Test_Decode_Detects_Exception (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer : PDU_Buffer := [others => 0];
      Values : Register_Array (0 .. 9);
      Count  : Natural;
      Result : Status;
   begin
      --  Exception response in read registers
      Buffer (0) := 16#83#;  --  FC 03 + 0x80 = exception
      Buffer (1) := 16#01#;  --  Illegal Function

      Decode_Read_Registers_Response (Buffer, 2, Values, Count, Result);

      Assert (Result = Exception_Illegal_Function, "Should detect exception");
      Assert (Count = 0, "Count should be 0 on exception");
   end Test_Decode_Detects_Exception;

   --  Test: Decode with frame error (too short)
   procedure Test_Decode_Frame_Error (T : in Out Test_Case'Class);
   procedure Test_Decode_Frame_Error (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer  : PDU_Buffer := [others => 0];
      Address : Register_Address;
      Value   : Register_Value;
      Result  : Status;
   begin
      Buffer (0) := 16#06#;

      Decode_Write_Single_Response (Buffer, 3, Address, Value, Result);  --  Too short

      Assert (Result = Frame_Error, "Should return Frame_Error");
   end Test_Decode_Frame_Error;

   --  Test: Read Exception Status (FC 07) encode/decode
   procedure Test_Read_Exception_Status (T : in Out Test_Case'Class);
   procedure Test_Read_Exception_Status (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer : PDU_Buffer := [others => 0];
      Length : Natural;
      Exc_Status : Byte;
      Result : Status;
   begin
      --  Encode request (just FC)
      Encode_Read_Exception_Status_Request (Buffer, Length);
      Assert (Length = 1, "Request length should be 1");
      Assert (Buffer (0) = 16#07#, "FC should be 0x07");

      --  Decode response
      Buffer (0) := 16#07#;  --  FC
      Buffer (1) := 16#A5#;  --  Exception status
      Decode_Read_Exception_Status_Response (Buffer, 2, Exc_Status, Result);
      Assert (Result = Success, "Decode should succeed");
      Assert (Exc_Status = 16#A5#, "Exception status should be 0xA5");

      --  Decode too short
      Decode_Read_Exception_Status_Response (Buffer, 1, Exc_Status, Result);
      Assert (Result = Frame_Error, "Should fail with too short frame");
   end Test_Read_Exception_Status;

   --  Test: Diagnostics (FC 08) encode/decode
   procedure Test_Diagnostics (T : in Out Test_Case'Class);
   procedure Test_Diagnostics (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer : PDU_Buffer := [others => 0];
      Length : Natural;
      Sub_Function : Interfaces.Unsigned_16;
      Data_Out : Interfaces.Unsigned_16;
      Result : Status;
   begin
      --  Encode request with sub-function 0 (Return Query Data) and data 0x1234
      Encode_Diagnostics_Request (Buffer, Length, 0, 16#1234#);
      Assert (Length = 5, "Request length should be 5");
      Assert (Buffer (0) = 16#08#, "FC should be 0x08");
      Assert (Buffer (1) = 16#00# and Buffer (2) = 16#00#, "Sub-function should be 0x0000");
      Assert (Buffer (3) = 16#12# and Buffer (4) = 16#34#, "Data should be 0x1234");

      --  Decode response
      Buffer (0) := 16#08#;
      Buffer (1) := 16#00#;
      Buffer (2) := 16#00#;  --  Sub-function 0
      Buffer (3) := 16#12#;
      Buffer (4) := 16#34#;  --  Data echoed back
      Decode_Diagnostics_Response (Buffer, 5, Sub_Function, Data_Out, Result);
      Assert (Result = Success, "Decode should succeed");
      Assert (Sub_Function = 0, "Sub-function should be 0");
      Assert (Data_Out = 16#1234#, "Data should be 0x1234");

      --  Decode too short
      Decode_Diagnostics_Response (Buffer, 3, Sub_Function, Data_Out, Result);
      Assert (Result = Frame_Error, "Should fail with too short frame");
   end Test_Diagnostics;

   --  Test: Report Server ID (FC 17) encode/decode
   procedure Test_Report_Server_Id (T : in Out Test_Case'Class);
   procedure Test_Report_Server_Id (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer : PDU_Buffer := [others => 0];
      Length : Natural;
      Server_Id : Byte;
      Run_Indicator : Boolean;
      Add_Data : Byte_Array (0 .. 9);
      Add_Data_Len : Natural;
      Result : Status;
   begin
      --  Encode request (just FC)
      Encode_Report_Server_Id_Request (Buffer, Length);
      Assert (Length = 1, "Request length should be 1");
      Assert (Buffer (0) = 16#11#, "FC should be 0x11 (17)");

      --  Decode response with server ID = 1, running, and additional data "TEST"
      Buffer (0) := 16#11#;  --  FC
      Buffer (1) := 6;       --  Byte count: 1 + 1 + 4 = 6
      Buffer (2) := 16#01#;  --  Server ID
      Buffer (3) := 16#FF#;  --  Run indicator = ON
      Buffer (4) := Character'Pos ('T');
      Buffer (5) := Character'Pos ('E');
      Buffer (6) := Character'Pos ('S');
      Buffer (7) := Character'Pos ('T');

      Add_Data := [others => 0];
      Decode_Report_Server_Id_Response
        (Buffer, 8, Server_Id, Run_Indicator, Add_Data, Add_Data_Len, Result);

      Assert (Result = Success, "Decode should succeed");
      Assert (Server_Id = 16#01#, "Server ID should be 0x01");
      Assert (Run_Indicator, "Run indicator should be True");
      Assert (Add_Data_Len = 4, "Additional data length should be 4");
      Assert (Add_Data (0) = Character'Pos ('T'), "First byte should be 'T'");

      --  Test with run indicator OFF
      Buffer (3) := 16#00#;
      Decode_Report_Server_Id_Response
        (Buffer, 8, Server_Id, Run_Indicator, Add_Data, Add_Data_Len, Result);
      Assert (not Run_Indicator, "Run indicator should be False");

      --  Decode too short
      Decode_Report_Server_Id_Response
        (Buffer, 2, Server_Id, Run_Indicator, Add_Data, Add_Data_Len, Result);
      Assert (Result = Frame_Error, "Should fail with too short frame");
   end Test_Report_Server_Id;

   --  Test: Mask Write Register (FC 22) encode/decode
   procedure Test_Mask_Write_Register (T : in Out Test_Case'Class);
   procedure Test_Mask_Write_Register (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer : PDU_Buffer := [others => 0];
      Length : Natural;
      Address : Register_Address;
      And_Mask : Register_Value;
      Or_Mask : Register_Value;
      Result : Status;
   begin
      --  Encode request: address 100, AND mask 0xFF00, OR mask 0x00FF
      Encode_Mask_Write_Register_Request
        (Buffer, Length, Address => 100, And_Mask => 16#FF00#, Or_Mask => 16#00FF#);

      Assert (Length = 7, "Request length should be 7");
      Assert (Buffer (0) = 16#16#, "FC should be 0x16 (22)");
      Assert (Buffer (1) = 16#00# and Buffer (2) = 16#64#, "Address should be 100");
      Assert (Buffer (3) = 16#FF# and Buffer (4) = 16#00#, "AND mask should be 0xFF00");
      Assert (Buffer (5) = 16#00# and Buffer (6) = 16#FF#, "OR mask should be 0x00FF");

      --  Decode response (echo)
      Buffer (0) := 16#16#;
      Buffer (1) := 16#00#;
      Buffer (2) := 16#64#;  --  Address 100
      Buffer (3) := 16#FF#;
      Buffer (4) := 16#00#;  --  AND mask
      Buffer (5) := 16#00#;
      Buffer (6) := 16#FF#;  --  OR mask

      Decode_Mask_Write_Register_Response (Buffer, 7, Address, And_Mask, Or_Mask, Result);
      Assert (Result = Success, "Decode should succeed");
      Assert (Address = 100, "Address should be 100");
      Assert (And_Mask = 16#FF00#, "AND mask should be 0xFF00");
      Assert (Or_Mask = 16#00FF#, "OR mask should be 0x00FF");

      --  Decode too short
      Decode_Mask_Write_Register_Response (Buffer, 5, Address, And_Mask, Or_Mask, Result);
      Assert (Result = Frame_Error, "Should fail with too short frame");
   end Test_Mask_Write_Register;

   --  Test: Read/Write Multiple Registers (FC 23) encode/decode
   procedure Test_Read_Write_Registers (T : in Out Test_Case'Class);
   procedure Test_Read_Write_Registers (T : in Out Test_Case'Class) is
      pragma Unreferenced (T);
      Buffer : PDU_Buffer := [others => 0];
      Length : Natural;
      Write_Values : constant Register_Array (0 .. 1) := [16#1234#, 16#5678#];
      Read_Values : Register_Array (0 .. 4);
      Count : Natural;
      Result : Status;
   begin
      --  Encode request: Read 3 regs from addr 10, Write 2 regs to addr 20
      Encode_Read_Write_Registers_Request
        (Buffer, Length,
         Read_Start    => 10,
         Read_Quantity => 3,
         Write_Start   => 20,
         Write_Values  => Write_Values);

      Assert (Buffer (0) = 16#17#, "FC should be 0x17 (23)");
      Assert (Buffer (1) = 16#00# and Buffer (2) = 16#0A#, "Read start should be 10");
      Assert (Buffer (3) = 16#00# and Buffer (4) = 16#03#, "Read quantity should be 3");
      Assert (Buffer (5) = 16#00# and Buffer (6) = 16#14#, "Write start should be 20");
      Assert (Buffer (7) = 16#00# and Buffer (8) = 16#02#, "Write quantity should be 2");
      Assert (Buffer (9) = 4, "Write byte count should be 4");
      Assert (Buffer (10) = 16#12# and Buffer (11) = 16#34#, "First write value");
      Assert (Buffer (12) = 16#56# and Buffer (13) = 16#78#, "Second write value");
      Assert (Length = 14, "Request length should be 14");

      --  Decode response: FC + ByteCount + 3 registers
      Buffer (0) := 16#17#;  --  FC
      Buffer (1) := 6;       --  Byte count (3 regs * 2)
      Buffer (2) := 16#AA#;
      Buffer (3) := 16#BB#;  --  Reg 0
      Buffer (4) := 16#CC#;
      Buffer (5) := 16#DD#;  --  Reg 1
      Buffer (6) := 16#EE#;
      Buffer (7) := 16#FF#;  --  Reg 2

      Read_Values := [others => 0];
      Decode_Read_Write_Registers_Response (Buffer, 8, Read_Values, Count, Result);

      Assert (Result = Success, "Decode should succeed");
      Assert (Count = 3, "Count should be 3");
      Assert (Read_Values (0) = 16#AABB#, "First register should be 0xAABB");
      Assert (Read_Values (1) = 16#CCDD#, "Second register should be 0xCCDD");
      Assert (Read_Values (2) = 16#EEFF#, "Third register should be 0xEEFF");

      --  Decode too short
      Decode_Read_Write_Registers_Response (Buffer, 1, Read_Values, Count, Result);
      Assert (Result = Frame_Error, "Should fail with too short frame");
   end Test_Read_Write_Registers;

   overriding procedure Register_Tests (T : in Out PDU_Test_Case) is
   begin
      Registration.Register_Routine (T, Test_Read_Registers_Request'Access, "Read Registers Request");
      Registration.Register_Routine (T, Test_Read_Bits_Request'Access, "Read Bits Request");
      Registration.Register_Routine (T, Test_Write_Single_Register_Request'Access, "Write Single Register Request");
      Registration.Register_Routine (T, Test_Write_Single_Coil_Request'Access, "Write Single Coil Request");
      Registration.Register_Routine (T, Test_Write_Multiple_Coils_Request'Access, "Write Multiple Coils Request");
      Registration.Register_Routine (T, Test_Write_Multiple_Registers_Request'Access, "Write Multiple Registers Request");
      Registration.Register_Routine (T, Test_Exception_Codes'Access, "Exception Codes (all)");
      Registration.Register_Routine (T, Test_Decode_Read_Registers_Response'Access, "Decode Read Registers Response");
      Registration.Register_Routine (T, Test_Decode_Read_Bits_Response'Access, "Decode Read Bits Response");
      Registration.Register_Routine (T, Test_Decode_Write_Single_Response'Access, "Decode Write Single Response");
      Registration.Register_Routine (T, Test_Decode_Write_Multiple_Response'Access, "Decode Write Multiple Response");
      Registration.Register_Routine (T, Test_Decode_Exception_Response'Access, "Decode Exception Response");
      Registration.Register_Routine (T, Test_Decode_Detects_Exception'Access, "Decode Detects Exception");
      Registration.Register_Routine (T, Test_Decode_Frame_Error'Access, "Decode Frame Error");
      --  New FC tests
      Registration.Register_Routine (T, Test_Read_Exception_Status'Access, "Read Exception Status (FC 07)");
      Registration.Register_Routine (T, Test_Diagnostics'Access, "Diagnostics (FC 08)");
      Registration.Register_Routine (T, Test_Report_Server_Id'Access, "Report Server ID (FC 17)");
      Registration.Register_Routine (T, Test_Mask_Write_Register'Access, "Mask Write Register (FC 22)");
      Registration.Register_Routine (T, Test_Read_Write_Registers'Access, "Read/Write Registers (FC 23)");
   end Register_Tests;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      S : constant AUnit.Test_Suites.Access_Test_Suite := new AUnit.Test_Suites.Test_Suite;
   begin
      S.Add_Test (new PDU_Test_Case);
      return S;
   end Suite;

end Test_PDU;
