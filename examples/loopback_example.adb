--  Loopback_Example - Demonstrates Master and Slave communication
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  This example shows a complete Modbus transaction:
--  - Master builds a request
--  - Request is processed by Slave
--  - Slave generates response
--  - Master parses response
--
--  Uses in-memory loopback (no actual transport)

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.Slave; use Ada_Modbus.Slave;
with Ada_Modbus.Master;

procedure Loopback_Example is

   --  Loopback buffers
   Tx_Buffer : Byte_Array (0 .. 255) := [others => 0];
   Tx_Length : Natural := 0;
   Rx_Buffer : Byte_Array (0 .. 255) := [others => 0];
   Rx_Length : Natural := 0;

   --  Slave data
   Holding_Registers : Register_Array (0 .. 99) := [others => 0];

   --  Slave callback
   function Read_Holding_Registers_CB
     (Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array) return Status
   is
      Start : constant Natural := Natural (Start_Address);
   begin
      if Start + Natural (Quantity) > Holding_Registers'Length then
         return Exception_Illegal_Address;
      end if;
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
      if Addr >= Holding_Registers'Length then
         return Exception_Illegal_Address;
      end if;
      Holding_Registers (Addr) := Value;
      return Success;
   end Write_Single_Register_CB;

   Slave_Config_Val : constant Slave_Config :=
     (Mode      => RTU,
      Unit_Id   => 1,
      Callbacks =>
        (Read_Holding_Registers   => Read_Holding_Registers_CB'Unrestricted_Access,
         Write_Single_Register    => Write_Single_Register_CB'Unrestricted_Access,
         Read_Coils               => null,
         Read_Discrete_Inputs     => null,
         Read_Input_Registers     => null,
         Write_Single_Coil        => null,
         Write_Multiple_Coils     => null,
         Write_Multiple_Registers => null,
         Read_Exception_Status    => null,
         Diagnostics              => null,
         Report_Server_Id         => null,
         Mask_Write_Register      => null,
         Read_Write_Registers     => null));

   --  Mock transport for Master (uses global buffers, no real context needed)
   type Null_Context is null record;
   Dummy_Ctx : Null_Context;

   function Send
     (Ctx  : in Out Null_Context;
      Data : Byte_Array) return Natural
   is
      pragma Unreferenced (Ctx);
   begin
      --  Copy data to Tx buffer
      for I in Data'Range loop
         Tx_Buffer (Tx_Buffer'First + I - Data'First) := Data (I);
      end loop;
      Tx_Length := Data'Length;
      return Data'Length;
   end Send;

   function Receive
     (Ctx        : in out Null_Context;
      Buffer     : out Byte_Array;
      Max_Length : Natural;
      Timeout_Ms : Natural) return Natural
   is
      pragma Unreferenced (Ctx, Timeout_Ms);
      Len : constant Natural := Natural'Min (Rx_Length, Max_Length);
   begin
      Buffer := [others => 0];
      for I in 0 .. Len - 1 loop
         Buffer (Buffer'First + I) := Rx_Buffer (I);
      end loop;
      return Len;
   end Receive;

   Tick_Counter : Unsigned_32 := 0;

   function Get_Tick_Ms return Unsigned_32 is
   begin
      Tick_Counter := Tick_Counter + 1;
      return Tick_Counter;
   end Get_Tick_Ms;

   --  Instantiate Master
   package My_Master is new Ada_Modbus.Master
     (Transport_Context => Null_Context,
      Send              => Send,
      Receive           => Receive,
      Get_Tick_Ms       => Get_Tick_Ms);

   Master_Ctx : My_Master.Master_Context;

   --  Process loopback
   procedure Process_Loopback is
      Send_Response : Boolean;
   begin
      --  Slave processes the request from Tx buffer
      Process_Request
        (Slave_Config_Val,
         Tx_Buffer, Tx_Length,
         Rx_Buffer, Rx_Length,
         Send_Response);
   end Process_Loopback;

   Values : Register_Array (0 .. 9);
   Result : Status;

begin
   Put_Line ("=== Loopback Example ===");
   New_Line;

   --  Initialize slave data
   Holding_Registers (0) := 16#0001#;
   Holding_Registers (1) := 16#0002#;
   Holding_Registers (2) := 16#0003#;
   Holding_Registers (3) := 16#0004#;
   Holding_Registers (4) := 16#0005#;

   --  Initialize Master
   My_Master.Initialize
     (Master_Ctx,
      (Mode => My_Master.RTU, Default_Slave => 1, Default_Timeout => 1000),
      Dummy_Ctx);

   Put_Line ("--- Test 1: Read 5 Holding Registers ---");

   --  Master sends read request (this fills Tx_Buffer)
   Result := My_Master.Read_Holding_Registers
     (Master_Ctx, Slave => 1, Start_Address => 0, Quantity => 5,
      Values => Values, Timeout_Ms => 1000);

   --  Simulate: Slave processes request
   Process_Loopback;

   --  Check result
   if Result = Success then
      Put_Line ("Read request: SUCCESS");
   else
      Put_Line ("Read request: " & Result'Image);
   end if;
   Put_Line ("Slave Registers: " &
             Holding_Registers (0)'Image &
             Holding_Registers (1)'Image &
             Holding_Registers (2)'Image &
             Holding_Registers (3)'Image &
             Holding_Registers (4)'Image);

   New_Line;
   Put_Line ("--- Test 2: Write Single Register ---");

   Result := My_Master.Write_Single_Register
     (Master_Ctx, Slave => 1, Address => 10, Value => 16#ABCD#,
      Timeout_Ms => 1000);

   Process_Loopback;

   --  Check result
   if Result = Success then
      Put_Line ("Write request: SUCCESS");
   else
      Put_Line ("Write request: " & Result'Image);
   end if;
   Put_Line ("Register 10 after write: " & Holding_Registers (10)'Image);

   New_Line;
   Put_Line ("=== Done ===");
end Loopback_Example;
