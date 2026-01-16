--  TCP_Master - Modbus TCP Master (Client) demo
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  A simple Modbus TCP client that:
--  - Connects to a Modbus TCP server
--  - Provides interactive commands
--  - Demonstrates Master API usage
--
--  Usage: tcp_master [host] [port]
--  Default: localhost 1502

with Ada.Calendar;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Exceptions;
with Interfaces; use Interfaces;
with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.Master;
with Ada_Modbus.Transport.TCP;

procedure TCP_Master is

   use Ada_Modbus.Transport.TCP;

   --  Connection and access type for transport context
   type Connection_Access is access all TCP_Connection;
   Connection : aliased TCP_Connection;
   Conn_Ptr   : constant Connection_Access := Connection'Access;

   --  Transport callbacks for Master generic
   function Send_Data
     (Ctx  : in out Connection_Access;
      Data : Byte_Array) return Natural is
   begin
      return Send (Ctx.all, Data);
   end Send_Data;

   function Receive_Data
     (Ctx        : in Out Connection_Access;
      Buffer     : out Byte_Array;
      Max_Length : Natural;
      Timeout_Ms : Natural) return Natural
   is
   begin
      return Receive (Ctx.all, Buffer, Max_Length, Timeout_Ms);
   end Receive_Data;

   function Get_Tick return Unsigned_32 is
      use Ada.Calendar;
      Now     : constant Time := Clock;
      Seconds : constant Duration := Now - Time_Of (1970, 1, 1, 0.0);
   begin
      return Unsigned_32 (Seconds * 1000.0) mod Unsigned_32'Last;
   end Get_Tick;

   --  Instantiate Master with TCP mode
   package My_Master is new Ada_Modbus.Master
     (Transport_Context => Connection_Access,
      Send              => Send_Data,
      Receive           => Receive_Data,
      Get_Tick_Ms       => Get_Tick);

   Ctx : My_Master.Master_Context;

   Host : String (1 .. 256) := [others => ' '];
   Host_Len : Natural := 9;  --  "localhost"
   Port : Natural := 1502;
   Result : Status;

   procedure Print_Help is
   begin
      New_Line;
      Put_Line ("Commands:");
      Put_Line ("  rhr <start> <count>   - Read Holding Registers (FC03)");
      Put_Line ("  rir <start> <count>   - Read Input Registers (FC04)");
      Put_Line ("  wsr <addr> <value>    - Write Single Register (FC06)");
      Put_Line ("  wmr <start> <v1> ...  - Write Multiple Registers (FC16)");
      Put_Line ("  rc <start> <count>    - Read Coils (FC01)");
      Put_Line ("  wsc <addr> <0|1>      - Write Single Coil (FC05)");
      Put_Line ("  help                  - Show this help");
      Put_Line ("  quit                  - Exit program");
      New_Line;
   end Print_Help;

   procedure Print_Status (S : Status) is
   begin
      case S is
         when Success => Put_Line ("  Status: Success");
         when Timeout => Put_Line ("  Status: Timeout");
         when CRC_Error => Put_Line ("  Status: CRC Error");
         when Frame_Error => Put_Line ("  Status: Frame Error");
         when Invalid_Response => Put_Line ("  Status: Invalid Response");
         when Exception_Illegal_Function => Put_Line ("  Status: Illegal Function");
         when Exception_Illegal_Address => Put_Line ("  Status: Illegal Address");
         when Exception_Illegal_Value => Put_Line ("  Status: Illegal Value");
         when Exception_Slave_Failure => Put_Line ("  Status: Slave Failure");
         when others => Put_Line ("  Status: " & S'Image);
      end case;
   end Print_Status;

   procedure Do_Read_Holding_Registers (Start : Natural; Count : Natural) is
      Values : Register_Array (0 .. Count - 1);
   begin
      Put_Line ("Reading" & Count'Image & " holding registers from" & Start'Image);
      Result := My_Master.Read_Holding_Registers
        (Ctx, Slave => 1,
         Start_Address => Register_Address (Start),
         Quantity => Register_Count (Count),
         Values => Values,
         Timeout_Ms => 3000);

      Print_Status (Result);
      if Result = Success then
         for I in Values'Range loop
            Put_Line ("  [" & Natural'Image (Start + I) & "] =" & Values (I)'Image);
         end loop;
      end if;
   end Do_Read_Holding_Registers;

   procedure Do_Read_Input_Registers (Start : Natural; Count : Natural) is
      Values : Register_Array (0 .. Count - 1);
   begin
      Put_Line ("Reading" & Count'Image & " input registers from" & Start'Image);
      Result := My_Master.Read_Input_Registers
        (Ctx, Slave => 1,
         Start_Address => Register_Address (Start),
         Quantity => Register_Count (Count),
         Values => Values,
         Timeout_Ms => 3000);

      Print_Status (Result);
      if Result = Success then
         for I in Values'Range loop
            Put_Line ("  [" & Natural'Image (Start + I) & "] =" & Values (I)'Image);
         end loop;
      end if;
   end Do_Read_Input_Registers;

   procedure Do_Write_Single_Register (Addr : Natural; Value : Natural) is
   begin
      Put_Line ("Writing" & Value'Image & " to register" & Addr'Image);
      Result := My_Master.Write_Single_Register
        (Ctx, Slave => 1,
         Address => Register_Address (Addr),
         Value => Register_Value (Value),
         Timeout_Ms => 3000);
      Print_Status (Result);
   end Do_Write_Single_Register;

   procedure Do_Read_Coils (Start : Natural; Count : Natural) is
      Values : Coil_Array (0 .. Count - 1);
   begin
      Put_Line ("Reading" & Count'Image & " coils from" & Start'Image);
      Result := My_Master.Read_Coils
        (Ctx, Slave => 1,
         Start_Address => Coil_Address (Start),
         Quantity => Coil_Count (Count),
         Values => Values,
         Timeout_Ms => 3000);

      Print_Status (Result);
      if Result = Success then
         for I in Values'Range loop
            Put_Line ("  [" & Natural'Image (Start + I) & "] = " &
                     (if Values (I) then "ON" else "OFF"));
         end loop;
      end if;
   end Do_Read_Coils;

   procedure Do_Write_Single_Coil (Addr : Natural; Value : Boolean) is
   begin
      Put_Line ("Writing " & (if Value then "ON" else "OFF") &
               " to coil" & Addr'Image);
      Result := My_Master.Write_Single_Coil
        (Ctx, Slave => 1,
         Address => Coil_Address (Addr),
         Value => Value,
         Timeout_Ms => 3000);
      Print_Status (Result);
   end Do_Write_Single_Coil;

   Line : String (1 .. 256);
   Last : Natural;

begin
   Put_Line ("=== Modbus TCP Master Demo ===");
   New_Line;

   --  Parse command line
   Host (1 .. 9) := "localhost";
   if Ada.Command_Line.Argument_Count >= 1 then
      declare
         Arg : constant String := Ada.Command_Line.Argument (1);
      begin
         Host (1 .. Arg'Length) := Arg;
         Host_Len := Arg'Length;
      end;
   end if;
   if Ada.Command_Line.Argument_Count >= 2 then
      Port := Natural'Value (Ada.Command_Line.Argument (2));
   end if;

   --  Connect to server
   Put_Line ("Connecting to " & Host (1 .. Host_Len) & ":" & Port'Image & "...");
   Connect (Connection, Host (1 .. Host_Len), Port, 5.0, Result);

   if Result /= Success then
      Put_Line ("Connection failed: " & Last_Error (Connection));
      return;
   end if;

   Put_Line ("Connected!");

   --  Initialize Master context
   My_Master.Initialize
     (Ctx,
      (Mode => My_Master.TCP, Default_Slave => 1, Default_Timeout => 3000),
      Conn_Ptr);

   Print_Help;

   --  Interactive command loop
   loop
      Put ("modbus> ");
      Get_Line (Line, Last);

      exit when Last >= 4 and then Line (1 .. 4) = "quit";
      exit when Last >= 4 and then Line (1 .. 4) = "exit";

      if Last >= 4 and then Line (1 .. 4) = "help" then
         Print_Help;

      elsif Last >= 3 and then Line (1 .. 3) = "rhr" then
         --  Read Holding Registers: rhr <start> <count>
         declare
            Start_Pos : Natural := 5;
            Space_Pos : Natural;
            Start : Natural;
            Count : Natural;
         begin
            --  Skip whitespace
            while Start_Pos <= Last and then Line (Start_Pos) = ' ' loop
               Start_Pos := Start_Pos + 1;
            end loop;
            --  Find space
            Space_Pos := Start_Pos;
            while Space_Pos <= Last and then Line (Space_Pos) /= ' ' loop
               Space_Pos := Space_Pos + 1;
            end loop;
            Start := Natural'Value (Line (Start_Pos .. Space_Pos - 1));
            Count := Natural'Value (Line (Space_Pos + 1 .. Last));
            Do_Read_Holding_Registers (Start, Count);
         exception
            when others => Put_Line ("Usage: rhr <start> <count>");
         end;

      elsif Last >= 3 and then Line (1 .. 3) = "rir" then
         --  Read Input Registers
         declare
            Start_Pos : Natural := 5;
            Space_Pos : Natural;
            Start : Natural;
            Count : Natural;
         begin
            while Start_Pos <= Last and then Line (Start_Pos) = ' ' loop
               Start_Pos := Start_Pos + 1;
            end loop;
            Space_Pos := Start_Pos;
            while Space_Pos <= Last and then Line (Space_Pos) /= ' ' loop
               Space_Pos := Space_Pos + 1;
            end loop;
            Start := Natural'Value (Line (Start_Pos .. Space_Pos - 1));
            Count := Natural'Value (Line (Space_Pos + 1 .. Last));
            Do_Read_Input_Registers (Start, Count);
         exception
            when others => Put_Line ("Usage: rir <start> <count>");
         end;

      elsif Last >= 3 and then Line (1 .. 3) = "wsr" then
         --  Write Single Register
         declare
            Start_Pos : Natural := 5;
            Space_Pos : Natural;
            Addr : Natural;
            Value : Natural;
         begin
            while Start_Pos <= Last and then Line (Start_Pos) = ' ' loop
               Start_Pos := Start_Pos + 1;
            end loop;
            Space_Pos := Start_Pos;
            while Space_Pos <= Last and then Line (Space_Pos) /= ' ' loop
               Space_Pos := Space_Pos + 1;
            end loop;
            Addr := Natural'Value (Line (Start_Pos .. Space_Pos - 1));
            Value := Natural'Value (Line (Space_Pos + 1 .. Last));
            Do_Write_Single_Register (Addr, Value);
         exception
            when others => Put_Line ("Usage: wsr <addr> <value>");
         end;

      elsif Last >= 2 and then Line (1 .. 2) = "rc" then
         --  Read Coils
         declare
            Start_Pos : Natural := 4;
            Space_Pos : Natural;
            Start : Natural;
            Count : Natural;
         begin
            while Start_Pos <= Last and then Line (Start_Pos) = ' ' loop
               Start_Pos := Start_Pos + 1;
            end loop;
            Space_Pos := Start_Pos;
            while Space_Pos <= Last and then Line (Space_Pos) /= ' ' loop
               Space_Pos := Space_Pos + 1;
            end loop;
            Start := Natural'Value (Line (Start_Pos .. Space_Pos - 1));
            Count := Natural'Value (Line (Space_Pos + 1 .. Last));
            Do_Read_Coils (Start, Count);
         exception
            when others => Put_Line ("Usage: rc <start> <count>");
         end;

      elsif Last >= 3 and then Line (1 .. 3) = "wsc" then
         --  Write Single Coil
         declare
            Start_Pos : Natural := 5;
            Space_Pos : Natural;
            Addr : Natural;
            Value : Natural;
         begin
            while Start_Pos <= Last and then Line (Start_Pos) = ' ' loop
               Start_Pos := Start_Pos + 1;
            end loop;
            Space_Pos := Start_Pos;
            while Space_Pos <= Last and then Line (Space_Pos) /= ' ' loop
               Space_Pos := Space_Pos + 1;
            end loop;
            Addr := Natural'Value (Line (Start_Pos .. Space_Pos - 1));
            Value := Natural'Value (Line (Space_Pos + 1 .. Last));
            Do_Write_Single_Coil (Addr, Value /= 0);
         exception
            when others => Put_Line ("Usage: wsc <addr> <0|1>");
         end;

      elsif Last > 0 then
         Put_Line ("Unknown command. Type 'help' for commands.");
      end if;
   end loop;

   Disconnect (Connection);
   Put_Line ("Disconnected. Goodbye!");

exception
   when E : others =>
      Put_Line ("Exception: " & Ada.Exceptions.Exception_Message (E));
      Disconnect (Connection);
end TCP_Master;
