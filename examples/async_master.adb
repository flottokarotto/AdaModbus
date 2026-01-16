--  Async_Master - Async Master Demo using polling-based non-blocking API
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Demonstrates the async API by polling multiple slaves concurrently.
--  Usage: async_master [host] [port] [slave_count]
--  Default: localhost 1502 3

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Exceptions;
with Interfaces; use Interfaces;

with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.Transport.TCP; use Ada_Modbus.Transport.TCP;
with Ada_Modbus.Master;
with Ada_Modbus.Master.Async;

procedure Async_Master is

   --  Configuration
   Default_Host : constant String := "127.0.0.1";
   Default_Port : constant := 1502;
   Default_Slave_Count : constant := 3;

   --  Use access type for limited TCP_Connection
   type Connection_Access is access all TCP_Connection;
   Connection : aliased TCP_Connection;
   Conn_Ptr   : constant Connection_Access := Connection'Access;

   function Send
     (Ctx  : in out Connection_Access;
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

   --  Simple tick counter
   Tick_Counter : Unsigned_32 := 0;

   function Get_Tick_Ms return Unsigned_32 is
   begin
      Tick_Counter := Tick_Counter + 1;
      return Tick_Counter;
   end Get_Tick_Ms;

   --  Instantiate Master with access type
   package My_Master is new Ada_Modbus.Master
     (Transport_Context => Connection_Access,
      Send              => Send,
      Receive           => Receive,
      Get_Tick_Ms       => Get_Tick_Ms);

   --  Instantiate Async extension
   package My_Async is new My_Master.Async (Max_Pending_Requests => 8);

   use type My_Async.Response_Status;

   --  Response tracking
   type Slave_Data is record
      Slave_Id      : Unit_Id := 0;
      Pending       : Boolean := False;
      Handle        : My_Async.Request_Handle := 1;
      Response_Count: Natural := 0;
      Error_Count   : Natural := 0;
      Last_Value    : Register_Value := 0;
   end record;

   Slaves : array (1 .. 10) of Slave_Data;
   Slave_Count : Natural := Default_Slave_Count;
   Total_Requests : Natural := 0;
   Total_Responses : Natural := 0;

   --  Callback for register reads
   procedure On_Read_Response
     (Handle         : My_Async.Request_Handle;
      Resp_Status    : My_Async.Response_Status;
      Slave          : Unit_Id;
      Values         : Register_Array;
      Exception_Code : Byte)
   is
      pragma Unreferenced (Handle, Exception_Code);
   begin
      for I in 1 .. Slave_Count loop
         if Slaves (I).Slave_Id = Slave then
            Slaves (I).Pending := False;
            if Resp_Status = My_Async.Response_Success then
               Slaves (I).Response_Count := Slaves (I).Response_Count + 1;
               if Values'Length > 0 then
                  Slaves (I).Last_Value := Values (Values'First);
               end if;
               Total_Responses := Total_Responses + 1;
               Put_Line ("  Slave " & Slave'Image & ": Value = " &
                         Slaves (I).Last_Value'Image);
            else
               Slaves (I).Error_Count := Slaves (I).Error_Count + 1;
               Put_Line ("  Slave " & Slave'Image & ": " &
                         (if Resp_Status = My_Async.Response_Timeout then "TIMEOUT"
                          elsif Resp_Status = My_Async.Response_Exception then "EXCEPTION"
                          else "ERROR"));
            end if;
            exit;
         end if;
      end loop;
   end On_Read_Response;

   Master_Ctx : My_Master.Master_Context;
   Async_Ctx  : My_Async.Async_Context;
   Config     : My_Master.Master_Config;
   Result     : Status;
   Success_Flag : Boolean;

   Host : String (1 .. 256) := [others => ' '];
   Host_Len : Natural := 0;
   Port : Natural := Default_Port;

   Poll_Rounds : constant := 5;

begin
   Put_Line ("=== Async Master Demo ===");
   Put_Line ("Demonstrates non-blocking polling of multiple slaves");
   New_Line;

   --  Parse command line
   if Ada.Command_Line.Argument_Count >= 1 then
      declare
         Arg : constant String := Ada.Command_Line.Argument (1);
      begin
         Host_Len := Arg'Length;
         Host (1 .. Host_Len) := Arg;
      end;
   else
      Host_Len := Default_Host'Length;
      Host (1 .. Host_Len) := Default_Host;
   end if;

   if Ada.Command_Line.Argument_Count >= 2 then
      Port := Natural'Value (Ada.Command_Line.Argument (2));
   end if;

   if Ada.Command_Line.Argument_Count >= 3 then
      Slave_Count := Natural'Min (10, Natural'Value (Ada.Command_Line.Argument (3)));
   end if;

   Put_Line ("Connecting to " & Host (1 .. Host_Len) & ":" & Port'Image);
   Put_Line ("Polling " & Slave_Count'Image & " slaves");
   New_Line;

   --  Connect
   Connect (Connection, Host (1 .. Host_Len), Port, 5.0, Result);
   if Result /= Success then
      Put_Line ("ERROR: Connection failed: " & Result'Image);
      return;
   end if;

   Put_Line ("Connected!");
   New_Line;

   --  Initialize master and async context
   Config.Mode := My_Master.TCP;
   Config.Default_Timeout := 500;  --  Short timeout for demo
   My_Master.Initialize (Master_Ctx, Config, Conn_Ptr);
   My_Async.Initialize (Async_Ctx, Master_Ctx);

   --  Initialize slave tracking
   for I in 1 .. Slave_Count loop
      Slaves (I).Slave_Id := Unit_Id (I);
   end loop;

   --  Polling loop
   for Round in 1 .. Poll_Rounds loop
      Put_Line ("--- Poll Round " & Round'Image & " ---");

      --  Start async requests for all slaves
      for I in 1 .. Slave_Count loop
         if not Slaves (I).Pending then
            Success_Flag := My_Async.Read_Holding_Registers_Async
              (Async_Ctx,
               Slave         => Slaves (I).Slave_Id,
               Start_Address => 0,
               Quantity      => 1,
               On_Response   => On_Read_Response'Unrestricted_Access,
               Handle        => Slaves (I).Handle);

            if Success_Flag then
               Slaves (I).Pending := True;
               Total_Requests := Total_Requests + 1;
            else
               Put_Line ("  Failed to start request for slave " & I'Image);
            end if;
         end if;
      end loop;

      --  Process responses until all complete or timeout
      declare
         Max_Iterations : constant := 1000;
         Iteration : Natural := 0;
         All_Done : Boolean;
      begin
         loop
            My_Async.Process_Pending (Async_Ctx);

            --  Check if all requests completed
            All_Done := True;
            for I in 1 .. Slave_Count loop
               if Slaves (I).Pending then
                  All_Done := False;
                  exit;
               end if;
            end loop;

            exit when All_Done;

            Iteration := Iteration + 1;
            exit when Iteration >= Max_Iterations;

            --  Small delay simulation
            Tick_Counter := Tick_Counter + 10;
         end loop;
      end;

      New_Line;
   end loop;

   --  Summary
   Put_Line ("=== Summary ===");
   Put_Line ("Total Requests:  " & Total_Requests'Image);
   Put_Line ("Total Responses: " & Total_Responses'Image);
   New_Line;

   for I in 1 .. Slave_Count loop
      Put_Line ("Slave " & Slaves (I).Slave_Id'Image & ":" &
                " OK=" & Slaves (I).Response_Count'Image &
                " Errors=" & Slaves (I).Error_Count'Image);
   end loop;

   Disconnect (Connection);
   Put_Line ("Done.");

exception
   when E : others =>
      Put_Line ("Exception: " & Ada.Exceptions.Exception_Message (E));
      Disconnect (Connection);
end Async_Master;
