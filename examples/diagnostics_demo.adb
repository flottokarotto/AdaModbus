--  Diagnostics_Demo - Demonstrate Modbus diagnostic function codes
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Tests FC 07 (Read Exception Status), FC 08 (Diagnostics),
--  FC 17 (Report Server ID), FC 22 (Mask Write Register),
--  and FC 23 (Read/Write Multiple Registers).
--
--  Usage: diagnostics_demo [host] [port] [slave_id]
--  Example: diagnostics_demo 192.168.1.100 502 1

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Exceptions;
with Interfaces; use Interfaces;

with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.Transport.TCP; use Ada_Modbus.Transport.TCP;
with Ada_Modbus.Master;

procedure Diagnostics_Demo is

   --  Use access type for limited TCP_Connection
   type Connection_Access is access all TCP_Connection;
   Connection : aliased TCP_Connection;
   Conn_Ptr   : constant Connection_Access := Connection'Access;

   function Send
     (Ctx  : in Out Connection_Access;
      Data : Byte_Array) return Natural
   is
   begin
      return Ada_Modbus.Transport.TCP.Send (Ctx.all, Data);
   end Send;

   function Receive
     (Ctx        : in Out Connection_Access;
      Buffer     : out Byte_Array;
      Max_Length : Natural;
      Timeout_Ms : Natural) return Natural
   is
   begin
      return Ada_Modbus.Transport.TCP.Receive (Ctx.all, Buffer, Max_Length, Timeout_Ms);
   end Receive;

   function Get_Tick_Ms return Unsigned_32 is (0);

   package My_Master is new Ada_Modbus.Master
     (Transport_Context => Connection_Access,
      Send              => Send,
      Receive           => Receive,
      Get_Tick_Ms       => Get_Tick_Ms);

   Master_Ctx : My_Master.Master_Context;
   Config     : My_Master.Master_Config;
   Result     : Status;

   Host     : String (1 .. 256) := [others => ' '];
   Host_Len : Natural := 9;
   Port     : Natural := 1502;
   Slave    : Unit_Id := 1;

   procedure Print_Separator is
   begin
      Put_Line ("────────────────────────────────────────────────────────────");
   end Print_Separator;

   procedure Test_Read_Exception_Status is
      Exception_Status : Byte;
   begin
      Put_Line ("FC 07: Read Exception Status");
      Put_Line ("  Reading device exception status...");

      Result := My_Master.Read_Exception_Status (Master_Ctx, Slave, Exception_Status);

      if Result = Success then
         Put_Line ("  Status: SUCCESS");
         Put_Line ("  Exception Status: " & Exception_Status'Image &
                   " (0x" & Byte'Image (Exception_Status) & ")");

         --  Decode individual bits
         Put ("  Bits: ");
         for I in 0 .. 7 loop
            if (Exception_Status and Byte (2 ** I)) /= 0 then
               Put ("1");
            else
               Put ("0");
            end if;
         end loop;
         New_Line;
      else
         Put_Line ("  Status: FAILED - " & Result'Image);
      end if;
   end Test_Read_Exception_Status;

   procedure Test_Diagnostics_Echo is
      Data_Out : Unsigned_16;
      Test_Value : constant Unsigned_16 := 16#ABCD#;
   begin
      Put_Line ("FC 08: Diagnostics - Return Query Data (Echo Test)");
      Put_Line ("  Sending test value: 0x" & Test_Value'Image);

      --  Sub-function 0x0000 = Return Query Data
      Result := My_Master.Diagnostics
        (Master_Ctx, Slave,
         Sub_Function => 16#0000#,
         Data_In      => Test_Value,
         Data_Out     => Data_Out);

      if Result = Success then
         Put_Line ("  Status: SUCCESS");
         Put_Line ("  Received: 0x" & Data_Out'Image);
         if Data_Out = Test_Value then
            Put_Line ("  Echo verified: PASS");
         else
            Put_Line ("  Echo verified: FAIL (mismatch)");
         end if;
      else
         Put_Line ("  Status: FAILED - " & Result'Image);
      end if;
   end Test_Diagnostics_Echo;

   procedure Test_Diagnostics_Counters is
      Data_Out : Unsigned_16;
   begin
      Put_Line ("FC 08: Diagnostics - Read Counters");

      --  Sub-function 0x000B = Return Bus Message Count
      Result := My_Master.Diagnostics
        (Master_Ctx, Slave,
         Sub_Function => 16#000B#,
         Data_In      => 0,
         Data_Out     => Data_Out);

      if Result = Success then
         Put_Line ("  Bus Message Count: " & Data_Out'Image);
      else
         Put_Line ("  Bus Message Count: N/A (" & Result'Image & ")");
      end if;

      --  Sub-function 0x000C = Return Bus Communication Error Count
      Result := My_Master.Diagnostics
        (Master_Ctx, Slave,
         Sub_Function => 16#000C#,
         Data_In      => 0,
         Data_Out     => Data_Out);

      if Result = Success then
         Put_Line ("  Bus Comm Error Count: " & Data_Out'Image);
      else
         Put_Line ("  Bus Comm Error Count: N/A (" & Result'Image & ")");
      end if;

      --  Sub-function 0x000E = Return Slave Message Count
      Result := My_Master.Diagnostics
        (Master_Ctx, Slave,
         Sub_Function => 16#000E#,
         Data_In      => 0,
         Data_Out     => Data_Out);

      if Result = Success then
         Put_Line ("  Slave Message Count: " & Data_Out'Image);
      else
         Put_Line ("  Slave Message Count: N/A (" & Result'Image & ")");
      end if;
   end Test_Diagnostics_Counters;

   procedure Test_Report_Server_Id is
      Server_Id     : Byte;
      Run_Indicator : Boolean;
      Add_Data      : Byte_Array (0 .. 31);
      Add_Data_Len  : Natural;
   begin
      Put_Line ("FC 17: Report Server ID");
      Put_Line ("  Querying device identification...");

      Result := My_Master.Report_Server_Id
        (Master_Ctx, Slave, Server_Id, Run_Indicator, Add_Data, Add_Data_Len);

      if Result = Success then
         Put_Line ("  Status: SUCCESS");
         Put_Line ("  Server ID: " & Server_Id'Image);
         Put_Line ("  Run Indicator: " & (if Run_Indicator then "ON (0xFF)" else "OFF (0x00)"));
         if Add_Data_Len > 0 then
            Put ("  Additional Data (" & Add_Data_Len'Image & " bytes): ");
            for I in 0 .. Add_Data_Len - 1 loop
               Put (Add_Data (I)'Image & " ");
            end loop;
            New_Line;
         end if;
      else
         Put_Line ("  Status: FAILED - " & Result'Image);
      end if;
   end Test_Report_Server_Id;

   procedure Test_Mask_Write_Register is
      Read_Values  : Register_Array (0 .. 0);
      Original_Val : Register_Value;
   begin
      Put_Line ("FC 22: Mask Write Register");
      Put_Line ("  This modifies register using AND/OR masks.");
      Put_Line ("  Formula: Result = (Current AND And_Mask) OR (Or_Mask AND NOT And_Mask)");
      New_Line;

      --  First read current value
      Result := My_Master.Read_Holding_Registers
        (Master_Ctx, Slave, Start_Address => 0, Quantity => 1, Values => Read_Values);

      if Result /= Success then
         Put_Line ("  Could not read current value: " & Result'Image);
         return;
      end if;

      Original_Val := Read_Values (0);
      Put_Line ("  Current value at register 0: " & Original_Val'Image);

      --  Set bit 0 (Or_Mask = 0x0001, And_Mask = 0xFFFF to keep all other bits)
      Put_Line ("  Setting bit 0 (AND=0xFFFF, OR=0x0001)...");

      Result := My_Master.Mask_Write_Register
        (Master_Ctx, Slave,
         Address  => 0,
         And_Mask => 16#FFFF#,
         Or_Mask  => 16#0001#);

      if Result = Success then
         Put_Line ("  Status: SUCCESS");

         --  Read back to verify
         Result := My_Master.Read_Holding_Registers
           (Master_Ctx, Slave, Start_Address => 0, Quantity => 1, Values => Read_Values);

         if Result = Success then
            Put_Line ("  New value: " & Read_Values (0)'Image);
         end if;
      else
         Put_Line ("  Status: FAILED - " & Result'Image);
      end if;

      --  Clear bit 0 (And_Mask = 0xFFFE to clear bit 0, Or_Mask = 0x0000)
      Put_Line ("  Clearing bit 0 (AND=0xFFFE, OR=0x0000)...");

      Result := My_Master.Mask_Write_Register
        (Master_Ctx, Slave,
         Address  => 0,
         And_Mask => 16#FFFE#,
         Or_Mask  => 16#0000#);

      if Result = Success then
         Put_Line ("  Status: SUCCESS");

         Result := My_Master.Read_Holding_Registers
           (Master_Ctx, Slave, Start_Address => 0, Quantity => 1, Values => Read_Values);

         if Result = Success then
            Put_Line ("  New value: " & Read_Values (0)'Image);
         end if;
      else
         Put_Line ("  Status: FAILED - " & Result'Image);
      end if;
   end Test_Mask_Write_Register;

   procedure Test_Read_Write_Registers is
      Read_Values  : Register_Array (0 .. 4);
      Write_Values : constant Register_Array (0 .. 2) := [100, 200, 300];
   begin
      Put_Line ("FC 23: Read/Write Multiple Registers");
      Put_Line ("  Performs atomic write then read in single transaction.");
      New_Line;

      Put_Line ("  Writing [100, 200, 300] to registers 10-12");
      Put_Line ("  Reading 5 registers from address 0");

      Result := My_Master.Read_Write_Multiple_Registers
        (Master_Ctx, Slave,
         Read_Start    => 0,
         Read_Quantity => 5,
         Read_Values   => Read_Values,
         Write_Start   => 10,
         Write_Values  => Write_Values);

      if Result = Success then
         Put_Line ("  Status: SUCCESS");
         Put ("  Read values: ");
         for I in Read_Values'Range loop
            Put (Read_Values (I)'Image & " ");
         end loop;
         New_Line;

         --  Verify written values
         declare
            Verify : Register_Array (0 .. 2);
         begin
            Result := My_Master.Read_Holding_Registers
              (Master_Ctx, Slave, Start_Address => 10, Quantity => 3, Values => Verify);

            if Result = Success then
               Put ("  Written values verified: ");
               for I in Verify'Range loop
                  Put (Verify (I)'Image & " ");
               end loop;
               New_Line;
            end if;
         end;
      else
         Put_Line ("  Status: FAILED - " & Result'Image);
      end if;
   end Test_Read_Write_Registers;

begin
   Put_Line ("╔══════════════════════════════════════════════════════════════╗");
   Put_Line ("║         Modbus Diagnostics Function Code Demo                ║");
   Put_Line ("╚══════════════════════════════════════════════════════════════╝");
   New_Line;

   --  Parse arguments
   Host (1 .. 9) := "127.0.0.1";

   if Ada.Command_Line.Argument_Count >= 1 then
      declare
         Arg : constant String := Ada.Command_Line.Argument (1);
      begin
         Host_Len := Arg'Length;
         Host (1 .. Host_Len) := Arg;
      end;
   end if;

   if Ada.Command_Line.Argument_Count >= 2 then
      Port := Natural'Value (Ada.Command_Line.Argument (2));
   end if;

   if Ada.Command_Line.Argument_Count >= 3 then
      Slave := Unit_Id (Natural'Value (Ada.Command_Line.Argument (3)));
   end if;

   Put_Line ("Host:     " & Host (1 .. Host_Len));
   Put_Line ("Port:     " & Port'Image);
   Put_Line ("Slave ID: " & Slave'Image);
   New_Line;

   --  Connect
   Put ("Connecting... ");
   Connect (Connection, Host (1 .. Host_Len), Port, 5.0, Result);
   if Result /= Success then
      Put_Line ("FAILED: " & Result'Image);
      return;
   end if;
   Put_Line ("OK");
   New_Line;

   --  Initialize master
   Config.Mode := My_Master.TCP;
   Config.Default_Timeout := 2000;
   My_Master.Initialize (Master_Ctx, Config, Conn_Ptr);

   --  Run tests
   Print_Separator;
   Test_Read_Exception_Status;
   Print_Separator;
   Test_Diagnostics_Echo;
   Print_Separator;
   Test_Diagnostics_Counters;
   Print_Separator;
   Test_Report_Server_Id;
   Print_Separator;
   Test_Mask_Write_Register;
   Print_Separator;
   Test_Read_Write_Registers;
   Print_Separator;

   New_Line;
   Put_Line ("All diagnostic tests completed.");

   Disconnect (Connection);

exception
   when E : others =>
      Put_Line ("Exception: " & Ada.Exceptions.Exception_Message (E));
      Disconnect (Connection);
end Diagnostics_Demo;
