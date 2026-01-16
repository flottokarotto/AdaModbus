--  Slave_Example - Demonstrates Modbus Slave/Server usage
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  This example shows how to:
--  - Set up a Modbus slave with callbacks
--  - Process a request and generate a response

with Ada.Text_IO; use Ada.Text_IO;
with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.Slave; use Ada_Modbus.Slave;
with Ada_Modbus.Protocol.RTU;

procedure Slave_Example is

   --  Simulated holding registers (100 registers)
   Holding_Registers : Register_Array (0 .. 99) := [others => 0];

   --  Simulated coils (100 coils)
   Coils : Coil_Array (0 .. 99) := [others => False];

   -------------------------
   --  Callback functions --
   -------------------------

   function Read_Holding_Registers_CB
     (Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array) return Status
   is
      Start : constant Natural := Natural (Start_Address);
   begin
      Put_Line ("Read Holding Registers: Start=" & Start'Image &
                ", Qty=" & Quantity'Image);

      --  Check address range
      if Start + Natural (Quantity) > Holding_Registers'Length then
         return Exception_Illegal_Address;
      end if;

      --  Copy values
      for I in 0 .. Natural (Quantity) - 1 loop
         Values (Values'First + I) := Holding_Registers (Start + I);
      end loop;

      return Success;
   end Read_Holding_Registers_CB;

   function Write_Single_Register_CB
     (Address : Register_Address;
      Value   : Register_Value) return Status
   is
      Addr : constant Natural := Natural (Address);
   begin
      Put_Line ("Write Single Register: Addr=" & Addr'Image &
                ", Value=" & Value'Image);

      if Addr >= Holding_Registers'Length then
         return Exception_Illegal_Address;
      end if;

      Holding_Registers (Addr) := Value;
      return Success;
   end Write_Single_Register_CB;

   function Read_Coils_CB
     (Start_Address : Coil_Address;
      Quantity      : Coil_Count;
      Values        : out Coil_Array) return Status
   is
      Start : constant Natural := Natural (Start_Address);
   begin
      Put_Line ("Read Coils: Start=" & Start'Image &
                ", Qty=" & Quantity'Image);

      if Start + Natural (Quantity) > Coils'Length then
         return Exception_Illegal_Address;
      end if;

      for I in 0 .. Natural (Quantity) - 1 loop
         Values (Values'First + I) := Coils (Start + I);
      end loop;

      return Success;
   end Read_Coils_CB;

   function Write_Single_Coil_CB
     (Address : Coil_Address;
      Value   : Coil_Value) return Status
   is
      Addr : constant Natural := Natural (Address);
   begin
      Put_Line ("Write Single Coil: Addr=" & Addr'Image &
                ", Value=" & Value'Image);

      if Addr >= Coils'Length then
         return Exception_Illegal_Address;
      end if;

      Coils (Addr) := Value;
      return Success;
   end Write_Single_Coil_CB;

   --  Slave configuration
   Config : constant Slave_Config :=
     (Mode      => RTU,
      Unit_Id   => 1,
      Callbacks =>
        (Read_Holding_Registers  => Read_Holding_Registers_CB'Unrestricted_Access,
         Write_Single_Register   => Write_Single_Register_CB'Unrestricted_Access,
         Read_Coils              => Read_Coils_CB'Unrestricted_Access,
         Write_Single_Coil       => Write_Single_Coil_CB'Unrestricted_Access,
         Read_Discrete_Inputs    => null,
         Read_Input_Registers    => null,
         Write_Multiple_Coils    => null,
         Write_Multiple_Registers => null,
         Read_Exception_Status   => null,
         Diagnostics             => null,
         Report_Server_Id        => null,
         Mask_Write_Register     => null,
         Read_Write_Registers    => null));

   --  Build a test request (Read 10 Holding Registers from address 0)
   procedure Build_Test_Request
     (Request : out Byte_Array;
      Length  : out Natural)
   is
      PDU : Protocol.PDU_Buffer;
      PDU_Len : Natural;
      ADU : Protocol.RTU.ADU_Buffer;
      ADU_Len : Natural;
   begin
      --  FC 03 (Read Holding Registers), Start=0, Quantity=10
      Protocol.Encode_Read_Registers_Request
        (PDU, PDU_Len, FC_Read_Holding_Registers,
         Start_Address => 0, Quantity => 10);

      --  Build RTU frame
      Protocol.RTU.Build_Frame (ADU, ADU_Len, Slave => 1, PDU => PDU, PDU_Length => PDU_Len);

      --  Copy to output
      for I in 0 .. ADU_Len - 1 loop
         Request (Request'First + I) := ADU (I);
      end loop;
      Length := ADU_Len;
   end Build_Test_Request;

   procedure Print_Hex (Data : Byte_Array; Length : Natural) is
   begin
      for I in 0 .. Length - 1 loop
         declare
            B : constant Byte := Data (Data'First + I);
            Hex : constant String := B'Image;
         begin
            Put (Hex & " ");
         end;
      end loop;
      New_Line;
   end Print_Hex;

   Request_Buffer  : Byte_Array (0 .. 255) := [others => 0];
   Request_Length  : Natural;
   Response_Buffer : Byte_Array (0 .. 255) := [others => 0];
   Response_Length : Natural;
   Send_Response   : Boolean;

begin
   Put_Line ("=== Modbus Slave Example ===");
   New_Line;

   --  Initialize some test data
   Holding_Registers (0) := 16#1234#;
   Holding_Registers (1) := 16#5678#;
   Holding_Registers (2) := 16#ABCD#;
   Coils (0) := True;
   Coils (5) := True;

   --  Build and process a test request
   Build_Test_Request (Request_Buffer, Request_Length);

   Put ("Request frame:  ");
   Print_Hex (Request_Buffer, Request_Length);

   --  Process the request
   Process_Request
     (Config,
      Request_Buffer, Request_Length,
      Response_Buffer, Response_Length,
      Send_Response);

   if Send_Response then
      Put ("Response frame: ");
      Print_Hex (Response_Buffer, Response_Length);
   else
      Put_Line ("No response (broadcast or error)");
   end if;

   New_Line;
   Put_Line ("=== Done ===");
end Slave_Example;
