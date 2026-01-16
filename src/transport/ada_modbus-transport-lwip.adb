--  Ada_Modbus.Transport.LwIP - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with System;

package body Ada_Modbus.Transport.LwIP is

   -------------
   -- Connect --
   -------------

   procedure Connect
     (Conn    : in Out TCP_Connection;
      IP_Addr : Unsigned_32;
      Port    : Unsigned_16;
      Result  : out Status)
   is
      Sock : int;
      Addr : aliased Sockaddr_In;
      Ret  : int;
   begin
      --  Create socket
      Sock := LWIP_Socket (AF_INET, SOCK_STREAM, IPPROTO_TCP);
      if Sock < 0 then
         Conn.State := Error;
         Result := Frame_Error;
         return;
      end if;

      Conn.Socket := Socket_Handle (Sock);

      --  Setup address
      Addr.Sin_Family := Unsigned_8 (AF_INET);
      Addr.Sin_Port := LWIP_Htons (Port);
      Addr.Sin_Addr := IP_Addr;  --  Already in network byte order

      --  Connect
      Ret := LWIP_Connect (Sock, Addr'Access, Sockaddr_In'Size / 8);
      if Ret < 0 then
         Disconnect (Conn);
         Result := Frame_Error;
         return;
      end if;

      Conn.State := Connected;
      Result := Success;
   end Connect;

   ----------------
   -- Disconnect --
   ----------------

   procedure Disconnect (Conn : in Out TCP_Connection) is
      Ret : int;
      pragma Unreferenced (Ret);
   begin
      if Conn.Socket /= Invalid_Socket then
         Ret := LWIP_Close (int (Conn.Socket));
         Conn.Socket := Invalid_Socket;
      end if;
      Conn.State := Disconnected;
   end Disconnect;

   ------------
   -- Listen --
   ------------

   procedure Listen
     (Conn   : in Out TCP_Connection;
      Port   : Unsigned_16;
      Result : out Status)
   is
      Sock : int;
      Addr : aliased Sockaddr_In;
      Ret  : int;
   begin
      --  Create socket
      Sock := LWIP_Socket (AF_INET, SOCK_STREAM, IPPROTO_TCP);
      if Sock < 0 then
         Conn.State := Error;
         Result := Frame_Error;
         return;
      end if;

      Conn.Socket := Socket_Handle (Sock);

      --  Bind to port
      Addr.Sin_Family := Unsigned_8 (AF_INET);
      Addr.Sin_Port := LWIP_Htons (Port);
      Addr.Sin_Addr := 0;  --  INADDR_ANY

      Ret := LWIP_Bind (Sock, Addr'Access, Sockaddr_In'Size / 8);
      if Ret < 0 then
         Disconnect (Conn);
         Result := Frame_Error;
         return;
      end if;

      --  Start listening (backlog = 1 for embedded)
      Ret := LWIP_Listen (Sock, 1);
      if Ret < 0 then
         Disconnect (Conn);
         Result := Frame_Error;
         return;
      end if;

      Conn.State := Connected;  --  Listening state
      Result := Success;
   end Listen;

   -----------------------
   -- Accept_Connection --
   -----------------------

   procedure Accept_Connection
     (Server : TCP_Connection;
      Client : out TCP_Connection;
      Result : out Status)
   is
      Client_Sock : int;
      Client_Addr : aliased Sockaddr_In;
      Addr_Len    : aliased int := Sockaddr_In'Size / 8;
   begin
      Client.Socket := Invalid_Socket;
      Client.State := Disconnected;

      if Server.Socket = Invalid_Socket then
         Result := Frame_Error;
         return;
      end if;

      Client_Sock := LWIP_Accept
        (int (Server.Socket), Client_Addr'Access, Addr_Len'Access);

      if Client_Sock < 0 then
         Result := Frame_Error;
         return;
      end if;

      Client.Socket := Socket_Handle (Client_Sock);
      Client.State := Connected;
      Result := Success;
   end Accept_Connection;

   ----------
   -- Send --
   ----------

   function Send
     (Conn : in Out TCP_Connection;
      Data : Byte_Array) return Natural
   is
      Sent : int;
   begin
      if Conn.Socket = Invalid_Socket or else Conn.State /= Connected then
         return 0;
      end if;

      Sent := LWIP_Send
        (int (Conn.Socket),
         Data (Data'First)'Address,
         Data'Length,
         0);

      if Sent < 0 then
         Conn.State := Error;
         return 0;
      end if;

      return Natural (Sent);
   end Send;

   -------------
   -- Receive --
   -------------

   function Receive
     (Conn       : in Out TCP_Connection;
      Buffer     : out Byte_Array;
      Max_Length : Natural;
      Timeout_Ms : Natural) return Natural
   is
      Timeout : aliased Timeval;
      Ret     : int;
      Received : int;
      Len     : size_t;
   begin
      Buffer := [others => 0];

      if Conn.Socket = Invalid_Socket or else Conn.State /= Connected then
         return 0;
      end if;

      --  Set receive timeout
      Timeout.Tv_Sec := long (Timeout_Ms / 1000);
      Timeout.Tv_Usec := long ((Timeout_Ms mod 1000) * 1000);

      Ret := LWIP_Setsockopt
        (int (Conn.Socket),
         SOL_SOCKET,
         SO_RCVTIMEO,
         Timeout'Address,
         Timeval'Size / 8);

      if Ret < 0 then
         return 0;
      end if;

      --  Receive data
      Len := size_t (Natural'Min (Max_Length, Buffer'Length));
      Received := LWIP_Recv
        (int (Conn.Socket),
         Buffer (Buffer'First)'Address,
         Len,
         0);

      if Received <= 0 then
         --  Timeout or error
         return 0;
      end if;

      return Natural (Received);
   end Receive;

   ------------------
   -- Make_IP_Addr --
   ------------------

   function Make_IP_Addr (A, B, C, D : Unsigned_8) return Unsigned_32 is
   begin
      --  Return IP in network byte order (big-endian)
      return Unsigned_32 (A)
        or Shift_Left (Unsigned_32 (B), 8)
        or Shift_Left (Unsigned_32 (C), 16)
        or Shift_Left (Unsigned_32 (D), 24);
   end Make_IP_Addr;

end Ada_Modbus.Transport.LwIP;
