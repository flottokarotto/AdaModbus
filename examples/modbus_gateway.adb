--  Modbus_Gateway - TCP to RTU Gateway
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Bridges Modbus TCP clients to Modbus RTU devices on serial port.
--  Accepts TCP connections and forwards requests to RTU slaves.
--
--  Usage: modbus_gateway <serial_port> [tcp_port] [baud]
--  Example: modbus_gateway COM3 502 9600
--           modbus_gateway /dev/ttyUSB0 1502 19200

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Exceptions;

with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.Protocol;
with Ada_Modbus.Protocol.TCP;
with Ada_Modbus.Protocol.RTU;
with Ada_Modbus.Transport.TCP; use Ada_Modbus.Transport.TCP;
with Ada_Modbus.Transport.Serial; use Ada_Modbus.Transport.Serial;

procedure Modbus_Gateway is

   --  Configuration
   Default_TCP_Port : constant := 502;
   Default_Baud     : constant Baud_Rate := B9600;
   RTU_Timeout_Ms   : constant := 1000;

   --  Connections
   TCP_Server     : aliased TCP_Connection;
   TCP_Client     : aliased TCP_Connection;
   Serial_Conn    : aliased Serial_Connection;
   Has_Client     : Boolean := False;

   --  Buffers
   TCP_Request    : Byte_Array (0 .. Protocol.TCP.Max_ADU_Size - 1) := [others => 0];
   TCP_Response   : Byte_Array (0 .. Protocol.TCP.Max_ADU_Size - 1) := [others => 0];
   RTU_Request    : Byte_Array (0 .. Protocol.RTU.Max_ADU_Size - 1) := [others => 0];
   RTU_Response   : Byte_Array (0 .. Protocol.RTU.Max_ADU_Size - 1) := [others => 0];

   --  Statistics
   Request_Count  : Natural := 0;
   Success_Count  : Natural := 0;
   Error_Count    : Natural := 0;

   Serial_Port    : String (1 .. 256) := [others => ' '];
   Serial_Len     : Natural := 0;
   TCP_Port       : Natural := Default_TCP_Port;
   Baud           : Baud_Rate := Default_Baud;
   Result         : Status;

   function Parse_Baud (S : String) return Baud_Rate is
   begin
      case Natural'Value (S) is
         when 9600   => return B9600;
         when 19200  => return B19200;
         when 38400  => return B38400;
         when 57600  => return B57600;
         when 115200 => return B115200;
         when others => return B9600;
      end case;
   end Parse_Baud;

   procedure Print_Usage is
   begin
      Put_Line ("Usage: modbus_gateway <serial_port> [tcp_port] [baud]");
      Put_Line ("  serial_port - Serial port for RTU devices (COM3, /dev/ttyUSB0)");
      Put_Line ("  tcp_port    - TCP port to listen on (default: 502)");
      Put_Line ("  baud        - Serial baud rate (default: 9600)");
      New_Line;
      Put_Line ("Example: modbus_gateway COM3 502 9600");
   end Print_Usage;

   procedure Handle_Request is
      TCP_Len      : Natural;
      RTU_Len      : Natural;
      Trans_Id     : Protocol.TCP.Transaction_Id;
      Unit_Id_Val  : Unit_Id;
      PDU          : Protocol.PDU_Buffer;
      PDU_Len      : Natural;
      Parse_Result : Status;
      RTU_Slave    : Unit_Id;
      RTU_PDU      : Protocol.PDU_Buffer;
      RTU_PDU_Len  : Natural;
      Recv_Len     : Natural;
   begin
      --  Receive TCP request
      TCP_Len := Receive (TCP_Client, TCP_Request,
                          Protocol.TCP.Max_ADU_Size, 100);

      if TCP_Len = 0 then
         return;  --  No data or client disconnected
      end if;

      Request_Count := Request_Count + 1;

      --  Parse TCP frame
      Protocol.TCP.Parse_Frame (TCP_Request, TCP_Len, Trans_Id, Unit_Id_Val,
                                PDU, PDU_Len, Parse_Result);

      if Parse_Result /= Success then
         Put_Line ("  Error: Invalid TCP frame: " & Parse_Result'Image);
         Error_Count := Error_Count + 1;
         return;
      end if;

      Put ("Request #" & Request_Count'Image &
           " [Trans=" & Trans_Id'Image &
           " Unit=" & Unit_Id_Val'Image &
           " FC=" & PDU (0)'Image & "]");

      --  Build RTU frame
      Protocol.RTU.Build_Frame (RTU_Request, RTU_Len, Unit_Id_Val, PDU, PDU_Len);

      --  Send to RTU device
      declare
         Sent : constant Natural := Send (Serial_Conn, RTU_Request (0 .. RTU_Len - 1));
      begin
         if Sent /= RTU_Len then
            Put_Line (" -> Serial send failed");
            Error_Count := Error_Count + 1;
            --  Send exception response
            return;
         end if;
      end;

      --  Wait for RTU response
      Recv_Len := Receive (Serial_Conn, RTU_Response,
                           Protocol.RTU.Max_ADU_Size, RTU_Timeout_Ms);

      if Recv_Len = 0 then
         Put_Line (" -> RTU Timeout");
         Error_Count := Error_Count + 1;
         --  Could send gateway timeout exception (code 11)
         return;
      end if;

      --  Parse RTU response
      Protocol.RTU.Parse_Frame (RTU_Response, Recv_Len, RTU_Slave,
                                RTU_PDU, RTU_PDU_Len, Parse_Result);

      if Parse_Result /= Success then
         Put_Line (" -> RTU parse error: " & Parse_Result'Image);
         Error_Count := Error_Count + 1;
         return;
      end if;

      --  Build TCP response with same transaction ID
      Protocol.TCP.Build_Frame (TCP_Response, TCP_Len, Trans_Id, RTU_Slave,
                                RTU_PDU, RTU_PDU_Len);

      --  Send TCP response
      declare
         Sent : constant Natural := Send (TCP_Client, TCP_Response (0 .. TCP_Len - 1));
      begin
         if Sent = TCP_Len then
            Put_Line (" -> OK");
            Success_Count := Success_Count + 1;
         else
            Put_Line (" -> TCP send failed");
            Error_Count := Error_Count + 1;
         end if;
      end;
   end Handle_Request;

begin
   Put_Line ("=== Modbus TCP-RTU Gateway ===");
   New_Line;

   --  Parse arguments
   if Ada.Command_Line.Argument_Count < 1 then
      Print_Usage;
      return;
   end if;

   declare
      Arg : constant String := Ada.Command_Line.Argument (1);
   begin
      Serial_Len := Arg'Length;
      Serial_Port (1 .. Serial_Len) := Arg;
   end;

   if Ada.Command_Line.Argument_Count >= 2 then
      TCP_Port := Natural'Value (Ada.Command_Line.Argument (2));
   end if;

   if Ada.Command_Line.Argument_Count >= 3 then
      Baud := Parse_Baud (Ada.Command_Line.Argument (3));
   end if;

   Put_Line ("Serial Port: " & Serial_Port (1 .. Serial_Len));
   Put_Line ("Baud Rate:   " & Baud'Image);
   Put_Line ("TCP Port:    " & TCP_Port'Image);
   New_Line;

   --  Open serial port
   Open (Serial_Conn, Serial_Port (1 .. Serial_Len),
         (Rate => Baud, Parity => None, Stop_Bits => One, Data_Bits => Eight),
         Result);
   if Result /= Success then
      Put_Line ("ERROR: Failed to open serial port: " & Result'Image);
      return;
   end if;
   Put_Line ("Serial port opened.");

   --  Start TCP server
   Listen (TCP_Server, TCP_Port, Result);
   if Result /= Success then
      Put_Line ("ERROR: Failed to start TCP server: " & Result'Image);
      Close (Serial_Conn);
      return;
   end if;

   Put_Line ("TCP server listening on port " & TCP_Port'Image);
   Put_Line ("Gateway ready. Press Ctrl+C to exit.");
   New_Line;

   --  Main loop
   loop
      --  Accept new client if none connected
      if not Has_Client then
         Put ("Waiting for TCP client... ");
         Accept_Connection (TCP_Server, TCP_Client, Result);
         if Result = Success then
            Has_Client := True;
            Put_Line ("Connected!");
         else
            Put_Line ("Accept failed: " & Result'Image);
         end if;
      end if;

      --  Handle requests from connected client
      if Has_Client then
         begin
            Handle_Request;
         exception
            when others =>
               Put_Line ("Client disconnected.");
               Has_Client := False;
               Disconnect (TCP_Client);
         end;
      end if;
   end loop;

exception
   when E : others =>
      Put_Line ("Exception: " & Ada.Exceptions.Exception_Message (E));
      New_Line;
      Put_Line ("=== Statistics ===");
      Put_Line ("Requests:  " & Request_Count'Image);
      Put_Line ("Success:   " & Success_Count'Image);
      Put_Line ("Errors:    " & Error_Count'Image);
      Close (Serial_Conn);
      Disconnect (TCP_Server);
end Modbus_Gateway;
