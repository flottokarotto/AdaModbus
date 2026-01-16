--  Data_Logger - Modbus Data Logger
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Periodically reads registers and outputs to console/CSV format.
--  Useful for monitoring, trend analysis, and debugging.
--
--  Usage: data_logger [host] [port] [slave] [interval_ms]
--  Example: data_logger 192.168.1.100 502 1 1000

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting;
with Interfaces; use Interfaces;

with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.Transport.TCP; use Ada_Modbus.Transport.TCP;
with Ada_Modbus.Master;

procedure Data_Logger is

   --  Configuration
   Num_Registers : constant := 10;
   Num_Coils     : constant := 16;

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

   Host        : String (1 .. 256) := [others => ' '];
   Host_Len    : Natural := 9;
   Port        : Natural := 1502;
   Slave       : Unit_Id := 1;
   Interval_Ms : Natural := 1000;

   Registers   : Register_Array (0 .. Num_Registers - 1);
   Coils       : Coil_Array (0 .. Num_Coils - 1);

   Sample_Count : Natural := 0;
   Error_Count  : Natural := 0;

   CSV_Mode : Boolean := False;

   function Timestamp return String is
      Now : constant Time := Clock;
   begin
      return Ada.Calendar.Formatting.Image (Now, Include_Time_Fraction => True);
   end Timestamp;

   procedure Print_CSV_Header is
   begin
      Put ("Timestamp,Sample");
      for I in 0 .. Num_Registers - 1 loop
         Put (",Reg" & I'Image);
      end loop;
      for I in 0 .. Num_Coils - 1 loop
         Put (",Coil" & I'Image);
      end loop;
      New_Line;
   end Print_CSV_Header;

   procedure Print_CSV_Row is
   begin
      Put (Timestamp & "," & Sample_Count'Image);
      for I in Registers'Range loop
         Put ("," & Registers (I)'Image);
      end loop;
      for I in Coils'Range loop
         Put ("," & (if Coils (I) then "1" else "0"));
      end loop;
      New_Line;
   end Print_CSV_Row;

   procedure Print_Human_Readable is
   begin
      Put_Line ("─────────────────────────────────────────────────────────────");
      Put_Line ("Sample #" & Sample_Count'Image & " at " & Timestamp);
      New_Line;

      Put_Line ("Holding Registers (0-" & Natural'Image (Num_Registers - 1) & "):");
      Put ("  ");
      for I in Registers'Range loop
         Put (Registers (I)'Image);
         if I < Registers'Last then
            Put (", ");
         end if;
      end loop;
      New_Line;
      New_Line;

      Put_Line ("Coils (0-" & Natural'Image (Num_Coils - 1) & "):");
      Put ("  ");
      for I in Coils'Range loop
         Put ((if Coils (I) then "1" else "0"));
         if (I + 1) mod 8 = 0 and I < Coils'Last then
            Put (" ");
         end if;
      end loop;
      New_Line;
   end Print_Human_Readable;

   procedure Read_And_Log is
   begin
      Sample_Count := Sample_Count + 1;

      --  Read holding registers
      Result := My_Master.Read_Holding_Registers
        (Master_Ctx, Slave,
         Start_Address => 0,
         Quantity      => Register_Count (Num_Registers),
         Values        => Registers);

      if Result /= Success then
         Error_Count := Error_Count + 1;
         if not CSV_Mode then
            Put_Line ("ERROR reading registers: " & Result'Image);
         end if;
         return;
      end if;

      --  Read coils
      Result := My_Master.Read_Coils
        (Master_Ctx, Slave,
         Start_Address => 0,
         Quantity      => Coil_Count (Num_Coils),
         Values        => Coils);

      if Result /= Success then
         Error_Count := Error_Count + 1;
         if not CSV_Mode then
            Put_Line ("ERROR reading coils: " & Result'Image);
         end if;
         return;
      end if;

      --  Output data
      if CSV_Mode then
         Print_CSV_Row;
      else
         Print_Human_Readable;
      end if;
   end Read_And_Log;

   procedure Print_Usage is
   begin
      Put_Line ("Usage: data_logger [options] [host] [port] [slave] [interval_ms]");
      Put_Line ("  --csv        Output in CSV format");
      Put_Line ("  host         Modbus TCP host (default: 127.0.0.1)");
      Put_Line ("  port         TCP port (default: 1502)");
      Put_Line ("  slave        Slave ID 1-247 (default: 1)");
      Put_Line ("  interval_ms  Poll interval in ms (default: 1000)");
      New_Line;
      Put_Line ("Example: data_logger 192.168.1.100 502 1 1000");
      Put_Line ("         data_logger --csv 192.168.1.100 502 1 500 > log.csv");
   end Print_Usage;

   Arg_Index : Natural := 1;

begin
   --  Parse arguments
   Host (1 .. 9) := "127.0.0.1";

   while Arg_Index <= Ada.Command_Line.Argument_Count loop
      declare
         Arg : constant String := Ada.Command_Line.Argument (Arg_Index);
      begin
         if Arg = "--csv" then
            CSV_Mode := True;
         elsif Arg = "--help" or Arg = "-h" then
            Print_Usage;
            return;
         elsif Arg_Index = 1 or (Arg_Index = 2 and CSV_Mode) then
            Host_Len := Arg'Length;
            Host (1 .. Host_Len) := Arg;
         elsif Arg_Index = 2 or (Arg_Index = 3 and CSV_Mode) then
            Port := Natural'Value (Arg);
         elsif Arg_Index = 3 or (Arg_Index = 4 and CSV_Mode) then
            Slave := Unit_Id (Natural'Value (Arg));
         elsif Arg_Index = 4 or (Arg_Index = 5 and CSV_Mode) then
            Interval_Ms := Natural'Value (Arg);
         end if;
      end;
      Arg_Index := Arg_Index + 1;
   end loop;

   if not CSV_Mode then
      Put_Line ("╔══════════════════════════════════════════════════════════════╗");
      Put_Line ("║                    Modbus Data Logger                        ║");
      Put_Line ("╚══════════════════════════════════════════════════════════════╝");
      New_Line;
      Put_Line ("Host:     " & Host (1 .. Host_Len));
      Put_Line ("Port:     " & Port'Image);
      Put_Line ("Slave:    " & Slave'Image);
      Put_Line ("Interval: " & Interval_Ms'Image & " ms");
      New_Line;
      Put ("Connecting... ");
   end if;

   --  Connect
   Connect (Connection, Host (1 .. Host_Len), Port, 5.0, Result);
   if Result /= Success then
      if CSV_Mode then
         Put_Line ("# ERROR: Connection failed: " & Result'Image);
      else
         Put_Line ("FAILED: " & Result'Image);
      end if;
      return;
   end if;

   if not CSV_Mode then
      Put_Line ("OK");
      Put_Line ("Logging started. Press Ctrl+C to stop.");
      New_Line;
   end if;

   --  Initialize master
   Config.Mode := My_Master.TCP;
   Config.Default_Timeout := 1000;
   My_Master.Initialize (Master_Ctx, Config, Conn_Ptr);

   --  Print CSV header if in CSV mode
   if CSV_Mode then
      Print_CSV_Header;
   end if;

   --  Main logging loop
   loop
      Read_And_Log;

      --  Wait for next sample
      delay Duration (Interval_Ms) / 1000.0;
   end loop;

exception
   when E : others =>
      if not CSV_Mode then
         New_Line;
         Put_Line ("─────────────────────────────────────────────────────────────");
         Put_Line ("Logger stopped: " & Ada.Exceptions.Exception_Message (E));
         Put_Line ("Total samples: " & Sample_Count'Image);
         Put_Line ("Total errors:  " & Error_Count'Image);
      else
         Put_Line ("# Stopped: " & Ada.Exceptions.Exception_Message (E));
      end if;
      Disconnect (Connection);
end Data_Logger;
