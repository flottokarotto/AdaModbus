--  Ada_Modbus.Transport.TCP - TCP socket transport backend
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Provides TCP/IP transport using GNAT.Sockets (cross-platform)
--  Can be used as client (connect to server) or server (accept connections)

with GNAT.Sockets;

package Ada_Modbus.Transport.TCP is

   --  TCP Connection handle
   type TCP_Connection is limited private;

   --  Connection state
   type Connection_State is (Disconnected, Connected, Listening, Error);

   --  Get current connection state
   function State (Conn : TCP_Connection) return Connection_State;

   --  Get last error message
   function Last_Error (Conn : TCP_Connection) return String;

   -----------------------
   --  Client functions --
   -----------------------

   --  Connect to a Modbus TCP server
   procedure Connect
     (Conn    : in out TCP_Connection;
      Host    : String;
      Port    : Natural := 502;
      Timeout : Duration := 5.0;
      Result  : out Status);

   --  Disconnect from server
   procedure Disconnect (Conn : in out TCP_Connection);

   -----------------------
   --  Server functions --
   -----------------------

   --  Start listening for connections
   procedure Listen
     (Conn   : in out TCP_Connection;
      Port   : Natural := 502;
      Result : out Status);

   --  Accept incoming connection (blocking)
   procedure Accept_Connection
     (Server : TCP_Connection;
      Client : out TCP_Connection;
      Result : out Status);

   --  Close server socket
   procedure Close_Server (Conn : in out TCP_Connection);

   ------------------------
   --  I/O functions     --
   ------------------------

   --  Send data (for use with Master generic)
   function Send (Conn : in out TCP_Connection; Data : Byte_Array) return Natural;

   --  Receive data with timeout (for use with Master generic)
   function Receive
     (Conn       : in out TCP_Connection;
      Buffer     : out Byte_Array;
      Max_Length : Natural;
      Timeout_Ms : Natural) return Natural;

   --  Send complete Modbus TCP frame
   procedure Send_Frame
     (Conn   : in out TCP_Connection;
      Frame  : Byte_Array;
      Result : out Status);

   --  Receive complete Modbus TCP frame (reads MBAP header first to get length)
   procedure Receive_Frame
     (Conn       : in out TCP_Connection;
      Buffer     : out Byte_Array;
      Length     : out Natural;
      Timeout_Ms : Natural;
      Result     : out Status);

private

   type TCP_Connection is limited record
      Socket       : GNAT.Sockets.Socket_Type := GNAT.Sockets.No_Socket;
      Server_Socket : GNAT.Sockets.Socket_Type := GNAT.Sockets.No_Socket;
      Address      : GNAT.Sockets.Sock_Addr_Type;
      Current_State : Connection_State := Disconnected;
      Error_Msg    : String (1 .. 256) := [others => ' '];
      Error_Len    : Natural := 0;
   end record;

end Ada_Modbus.Transport.TCP;
