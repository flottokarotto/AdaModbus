--  Multi_Slave_Poll - Poll multiple Modbus slaves cyclically
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Demonstrates typical SCADA/HMI polling pattern.
--  Polls multiple slaves in sequence and displays results.
--
--  Usage: multi_slave_poll [host] [port] [slave_count]
--  Example: multi_slave_poll 192.168.1.100 502 5

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Calendar;
with Interfaces; use Interfaces;

with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.Transport.TCP; use Ada_Modbus.Transport.TCP;
with Ada_Modbus.Master;

procedure Multi_Slave_Poll is

   --  Configuration
   Max_Slaves : constant := 16;
   Poll_Interval_Ms : constant := 1000;  --  1 second between poll cycles

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

   function Get_Tick_Ms return Unsigned_32 is
      use Ada.Calendar;
      Now     : constant Time := Clock;
      Seconds : constant Duration := Now - Time_Of (1970, 1, 1, 0.0);
   begin
      return Unsigned_32 (Seconds * 1000.0) mod Unsigned_32'Last;
   end Get_Tick_Ms;

   package My_Master is new Ada_Modbus.Master
     (Transport_Context => Connection_Access,
      Send              => Send,
      Receive           => Receive,
      Get_Tick_Ms       => Get_Tick_Ms);

   --  Slave data structure
   type Slave_Status is (Online, Offline, Error);

   type Slave_Record is record
      Slave_Unit   : Unit_Id := 0;
      Status       : Slave_Status := Offline;
      Registers    : Register_Array (0 .. 9) := [others => 0];
      Coils        : Coil_Array (0 .. 7) := [others => False];
      Poll_Count   : Natural := 0;
      Error_Count  : Natural := 0;
      Last_Error   : Ada_Modbus.Status := Success;
   end record;

   Slaves : array (1 .. Max_Slaves) of Slave_Record;
   Slave_Count : Natural := 3;

   Master_Ctx : My_Master.Master_Context;
   Config     : My_Master.Master_Config;
   Result     : Ada_Modbus.Status;

   Host     : String (1 .. 256) := [others => ' '];
   Host_Len : Natural := 0;
   Port     : Natural := 1502;

   Poll_Cycle : Natural := 0;

   procedure Clear_Screen is
   begin
      --  ANSI escape sequence to clear screen (works on most terminals)
      Put (ASCII.ESC & "[2J" & ASCII.ESC & "[H");
   end Clear_Screen;

   procedure Print_Header is
   begin
      Put_Line ("╔══════════════════════════════════════════════════════════════╗");
      Put_Line ("║           Multi-Slave Modbus Polling Monitor                 ║");
      Put_Line ("╠══════════════════════════════════════════════════════════════╣");
      Put_Line ("║ Host: " & Host (1 .. Host_Len) & ":" & Port'Image &
                "  Cycle: " & Poll_Cycle'Image & "                          ║");
      Put_Line ("╚══════════════════════════════════════════════════════════════╝");
      New_Line;
   end Print_Header;

   procedure Print_Slave_Status (Idx : Positive) is
      S : Slave_Record renames Slaves (Idx);
   begin
      Put ("Slave " & S.Slave_Unit'Image & ": ");

      case S.Status is
         when Online =>
            Put ("[ONLINE ] ");
         when Offline =>
            Put ("[OFFLINE] ");
         when Error =>
            Put ("[ERROR  ] ");
      end case;

      Put ("Polls=" & S.Poll_Count'Image & " Errs=" & S.Error_Count'Image);

      if S.Status = Online then
         New_Line;
         Put ("  Registers: ");
         for I in 0 .. 4 loop
            Put (S.Registers (I)'Image & " ");
         end loop;
         New_Line;
         Put ("  Coils:     ");
         for I in 0 .. 7 loop
            Put ((if S.Coils (I) then "1" else "0"));
         end loop;
      elsif S.Status = Error then
         Put (" Last: " & S.Last_Error'Image);
      end if;

      New_Line;
   end Print_Slave_Status;

   procedure Poll_Slave (Idx : Positive) is
      S : Slave_Record renames Slaves (Idx);
   begin
      S.Poll_Count := S.Poll_Count + 1;

      --  Read holding registers
      Result := My_Master.Read_Holding_Registers
        (Master_Ctx, Slave => S.Slave_Unit, Start_Address => 0,
         Quantity => 5, Values => S.Registers);

      if Result /= Success then
         S.Error_Count := S.Error_Count + 1;
         S.Last_Error := Result;
         if Result = Timeout then
            S.Status := Offline;
         else
            S.Status := Error;
         end if;
         return;
      end if;

      --  Read coils
      Result := My_Master.Read_Coils
        (Master_Ctx, Slave => S.Slave_Unit, Start_Address => 0,
         Quantity => 8, Values => S.Coils);

      if Result /= Success then
         S.Error_Count := S.Error_Count + 1;
         S.Last_Error := Result;
         S.Status := Error;
         return;
      end if;

      S.Status := Online;
   end Poll_Slave;

begin
   --  Parse arguments
   if Ada.Command_Line.Argument_Count >= 1 then
      declare
         Arg : constant String := Ada.Command_Line.Argument (1);
      begin
         Host_Len := Arg'Length;
         Host (1 .. Host_Len) := Arg;
      end;
   else
      Host_Len := 9;
      Host (1 .. Host_Len) := "127.0.0.1";
   end if;

   if Ada.Command_Line.Argument_Count >= 2 then
      Port := Natural'Value (Ada.Command_Line.Argument (2));
   end if;

   if Ada.Command_Line.Argument_Count >= 3 then
      Slave_Count := Natural'Min (Max_Slaves,
                                  Natural'Value (Ada.Command_Line.Argument (3)));
   end if;

   --  Initialize slaves
   for I in 1 .. Slave_Count loop
      Slaves (I).Slave_Unit := Unit_Id (I);
   end loop;

   Put_Line ("=== Multi-Slave Polling Demo ===");
   Put_Line ("Connecting to " & Host (1 .. Host_Len) & ":" & Port'Image);
   Put_Line ("Polling " & Slave_Count'Image & " slaves");
   New_Line;

   --  Connect
   Connect (Connection, Host (1 .. Host_Len), Port, 5.0, Result);
   if Result /= Success then
      Put_Line ("ERROR: Connection failed: " & Result'Image);
      return;
   end if;

   Put_Line ("Connected! Starting poll loop...");
   Put_Line ("Press Ctrl+C to exit.");
   New_Line;

   --  Initialize master
   Config.Mode := My_Master.TCP;
   Config.Default_Timeout := 500;
   My_Master.Initialize (Master_Ctx, Config, Conn_Ptr);

   --  Main polling loop
   loop
      Poll_Cycle := Poll_Cycle + 1;

      Clear_Screen;
      Print_Header;

      --  Poll each slave
      for I in 1 .. Slave_Count loop
         Poll_Slave (I);
         Print_Slave_Status (I);
         New_Line;
      end loop;

      --  Summary
      declare
         Online_Count : Natural := 0;
         Total_Errors : Natural := 0;
      begin
         for I in 1 .. Slave_Count loop
            if Slaves (I).Status = Online then
               Online_Count := Online_Count + 1;
            end if;
            Total_Errors := Total_Errors + Slaves (I).Error_Count;
         end loop;

         Put_Line ("────────────────────────────────────────");
         Put_Line ("Summary: " & Online_Count'Image & "/" & Slave_Count'Image &
                   " online, " & Total_Errors'Image & " total errors");
      end;

      --  Wait before next cycle (simplified delay)
      delay Duration (Poll_Interval_Ms) / 1000.0;
   end loop;

exception
   when E : others =>
      Put_Line ("Exception: " & Ada.Exceptions.Exception_Message (E));
      Disconnect (Connection);
end Multi_Slave_Poll;
