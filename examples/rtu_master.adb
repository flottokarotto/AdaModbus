--  RTU_Master - Modbus RTU Master (Client) demo
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  A simple Modbus RTU client that:
--  - Connects via serial port (COM port / TTY)
--  - Provides interactive commands
--  - Demonstrates Master API with RTU framing
--
--  Usage: rtu_master <port> [baud]
--  Examples:
--    Windows: rtu_master COM3 9600
--    Linux:   rtu_master /dev/ttyUSB0 19200

with Ada.Calendar;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Exceptions;
with Interfaces; use Interfaces;
with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.Master;
with Ada_Modbus.Transport.Serial;

procedure RTU_Master is

   use Ada_Modbus.Transport.Serial;

   --  Connection and access type for transport context
   type Connection_Access is access all Serial_Connection;
   Connection : aliased Serial_Connection;
   Conn_Ptr   : constant Connection_Access := Connection'Access;

   --  Transport callbacks for Master generic
   function Send_Data
     (Ctx  : in Out Connection_Access;
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

   --  Instantiate Master with RTU mode
   package My_Master is new Ada_Modbus.Master
     (Transport_Context => Connection_Access,
      Send              => Send_Data,
      Receive           => Receive_Data,
      Get_Tick_Ms       => Get_Tick);

   Ctx : My_Master.Master_Context;

   Port_Name : String (1 .. 256) := [others => ' '];
   Port_Len  : Natural := 0;
   Baud      : Baud_Rate := B9600;
   Slave_Id  : Unit_Id := 1;
   Result    : Status;

   function Parse_Baud (S : String) return Baud_Rate is
      Val : constant Natural := Natural'Value (S);
   begin
      case Val is
         when 1200   => return B1200;
         when 2400   => return B2400;
         when 4800   => return B4800;
         when 9600   => return B9600;
         when 19200  => return B19200;
         when 38400  => return B38400;
         when 57600  => return B57600;
         when 115200 => return B115200;
         when others => return B9600;
      end case;
   exception
      when others => return B9600;
   end Parse_Baud;

   procedure Print_Help is
   begin
      New_Line;
      Put_Line ("Commands:");
      Put_Line ("  rhr <start> <count>   - Read Holding Registers (FC03)");
      Put_Line ("  rir <start> <count>   - Read Input Registers (FC04)");
      Put_Line ("  wsr <addr> <value>    - Write Single Register (FC06)");
      Put_Line ("  rc <start> <count>    - Read Coils (FC01)");
      Put_Line ("  wsc <addr> <0|1>      - Write Single Coil (FC05)");
      Put_Line ("  slave <id>            - Set slave address (1-247)");
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
        (Ctx, Slave => Slave_Id,
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
        (Ctx, Slave => Slave_Id,
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
        (Ctx, Slave => Slave_Id,
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
        (Ctx, Slave => Slave_Id,
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
        (Ctx, Slave => Slave_Id,
         Address => Coil_Address (Addr),
         Value => Value,
         Timeout_Ms => 3000);
      Print_Status (Result);
   end Do_Write_Single_Coil;

   Line : String (1 .. 256);
   Last : Natural;

begin
   Put_Line ("=== Modbus RTU Master Demo ===");
   New_Line;

   --  Parse command line
   if Ada.Command_Line.Argument_Count < 1 then
      Put_Line ("Usage: rtu_master <port> [baud]");
      Put_Line ("  port: COM3 (Windows) or /dev/ttyUSB0 (Linux)");
      Put_Line ("  baud: 1200|2400|4800|9600|19200|38400|57600|115200");
      Put_Line ("        (default: 9600)");
      return;
   end if;

   declare
      Arg1 : constant String := Ada.Command_Line.Argument (1);
   begin
      Port_Len := Arg1'Length;
      Port_Name (1 .. Port_Len) := Arg1;
   end;

   if Ada.Command_Line.Argument_Count >= 2 then
      Baud := Parse_Baud (Ada.Command_Line.Argument (2));
   end if;

   --  Open serial port
   Put_Line ("Opening " & Port_Name (1 .. Port_Len) &
             " at " & Baud_Rate'Image (Baud) & "...");
   Open (Connection, Port_Name (1 .. Port_Len),
         (Rate => Baud, Parity => None, Stop_Bits => One, Data_Bits => Eight),
         Result);

   if Result /= Success then
      Put_Line ("Failed to open port: " & Last_Error (Connection));
      return;
   end if;

   Put_Line ("Connected!");

   --  Initialize Master context
   My_Master.Initialize
     (Ctx,
      (Mode => My_Master.RTU, Default_Slave => Slave_Id, Default_Timeout => 3000),
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

      elsif Last >= 5 and then Line (1 .. 5) = "slave" then
         --  Set slave ID
         declare
            Start_Pos : Natural := 7;
         begin
            while Start_Pos <= Last and then Line (Start_Pos) = ' ' loop
               Start_Pos := Start_Pos + 1;
            end loop;
            Slave_Id := Unit_Id (Natural'Value (Line (Start_Pos .. Last)));
            Put_Line ("Slave ID set to" & Slave_Id'Image);
         exception
            when others => Put_Line ("Usage: slave <1-247>");
         end;

      elsif Last >= 3 and then Line (1 .. 3) = "rhr" then
         --  Read Holding Registers: rhr <start> <count>
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

   Close (Connection);
   Put_Line ("Disconnected. Goodbye!");

exception
   when E : others =>
      Put_Line ("Exception: " & Ada.Exceptions.Exception_Message (E));
      Close (Connection);
end RTU_Master;
