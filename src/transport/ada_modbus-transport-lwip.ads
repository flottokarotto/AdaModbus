--  Ada_Modbus.Transport.LwIP - LwIP socket transport for embedded systems
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Provides TCP transport using LwIP sockets API.
--  Designed for Cortex-M4 and similar embedded targets.
--
--  Prerequisites:
--  - LwIP configured with LWIP_SOCKET=1 (BSD sockets API)
--  - lwip/sockets.h available

with Interfaces; use Interfaces;
with Interfaces.C; use Interfaces.C;

package Ada_Modbus.Transport.LwIP is

   --  Socket handle (maps to LwIP socket descriptor)
   type Socket_Handle is new Integer;

   Invalid_Socket : constant Socket_Handle := -1;

   --  Connection state
   type Connection_State is (Disconnected, Connected, Error);

   --  TCP Connection
   type TCP_Connection is record
      Socket : Socket_Handle := Invalid_Socket;
      State  : Connection_State := Disconnected;
   end record;

   -----------------------
   --  Client functions --
   -----------------------

   --  Connect to a Modbus TCP server
   procedure Connect
     (Conn    : in out TCP_Connection;
      IP_Addr : Unsigned_32;  --  IP address in network byte order
      Port    : Unsigned_16;  --  Port in host byte order
      Result  : out Status);

   --  Disconnect from server
   procedure Disconnect (Conn : in out TCP_Connection);

   -----------------------
   --  Server functions --
   -----------------------

   --  Bind and listen on port
   procedure Listen
     (Conn   : in Out TCP_Connection;
      Port   : Unsigned_16;
      Result : out Status);

   --  Accept incoming connection (blocking)
   procedure Accept_Connection
     (Server : TCP_Connection;
      Client : out TCP_Connection;
      Result : out Status);

   ------------------------
   --  I/O functions     --
   ------------------------

   --  Send data (for use with Master generic)
   function Send
     (Conn : in Out TCP_Connection;
      Data : Byte_Array) return Natural;

   --  Receive data with timeout (for use with Master generic)
   function Receive
     (Conn       : in Out TCP_Connection;
      Buffer     : out Byte_Array;
      Max_Length : Natural;
      Timeout_Ms : Natural) return Natural;

   --  Helper: Make IP address from octets
   function Make_IP_Addr (A, B, C, D : Unsigned_8) return Unsigned_32;

private

   --  LwIP socket constants
   AF_INET     : constant := 2;
   SOCK_STREAM : constant := 1;
   IPPROTO_TCP : constant := 6;
   SOL_SOCKET  : constant := 16#FFF#;
   SO_RCVTIMEO : constant := 16#1006#;

   --  LwIP sockaddr_in structure (simplified)
   type Sockaddr_In is record
      Sin_Len    : Unsigned_8 := 0;
      Sin_Family : Unsigned_8 := Unsigned_8 (AF_INET);
      Sin_Port   : Unsigned_16 := 0;  --  Network byte order
      Sin_Addr   : Unsigned_32 := 0;  --  Network byte order
      Sin_Zero   : Unsigned_64 := 0;
   end record
     with Convention => C;

   --  LwIP timeval structure
   type Timeval is record
      Tv_Sec  : long := 0;
      Tv_Usec : long := 0;
   end record
     with Convention => C;

   --  LwIP Socket API imports
   function LWIP_Socket
     (Domain   : int;
      Typ      : int;
      Protocol : int) return int
     with Import, Convention => C, External_Name => "lwip_socket";

   function LWIP_Connect
     (S       : int;
      Name    : access Sockaddr_In;
      Namelen : int) return int
     with Import, Convention => C, External_Name => "lwip_connect";

   function LWIP_Bind
     (S       : int;
      Name    : access Sockaddr_In;
      Namelen : int) return int
     with Import, Convention => C, External_Name => "lwip_bind";

   function LWIP_Listen
     (S       : int;
      Backlog : int) return int
     with Import, Convention => C, External_Name => "lwip_listen";

   function LWIP_Accept
     (S       : int;
      Addr    : access Sockaddr_In;
      Addrlen : access int) return int
     with Import, Convention => C, External_Name => "lwip_accept";

   function LWIP_Send
     (S     : int;
      Data  : System.Address;
      Size  : size_t;
      Flags : int) return int
     with Import, Convention => C, External_Name => "lwip_send";

   function LWIP_Recv
     (S      : int;
      Mem    : System.Address;
      Len    : size_t;
      Flags  : int) return int
     with Import, Convention => C, External_Name => "lwip_recv";

   function LWIP_Close (S : int) return int
     with Import, Convention => C, External_Name => "lwip_close";

   function LWIP_Setsockopt
     (S       : int;
      Level   : int;
      Optname : int;
      Optval  : System.Address;
      Optlen  : int) return int
     with Import, Convention => C, External_Name => "lwip_setsockopt";

   function LWIP_Htons (N : Unsigned_16) return Unsigned_16
     with Import, Convention => C, External_Name => "lwip_htons";

   function LWIP_Htonl (N : Unsigned_32) return Unsigned_32
     with Import, Convention => C, External_Name => "lwip_htonl";

end Ada_Modbus.Transport.LwIP;
