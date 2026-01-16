--  Ada_Modbus.Transport.TCP - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with Ada.Streams;
with GNAT.Sockets; use GNAT.Sockets;

package body Ada_Modbus.Transport.TCP is

   procedure Set_Error (Conn : in out TCP_Connection; Msg : String) is
      Len : constant Natural := Natural'Min (Msg'Length, Conn.Error_Msg'Length);
   begin
      Conn.Error_Msg := [others => ' '];
      Conn.Error_Msg (1 .. Len) := Msg (Msg'First .. Msg'First + Len - 1);
      Conn.Error_Len := Len;
      Conn.Current_State := Error;
   end Set_Error;

   -----------
   -- State --
   -----------

   function State (Conn : TCP_Connection) return Connection_State is
   begin
      return Conn.Current_State;
   end State;

   ----------------
   -- Last_Error --
   ----------------

   function Last_Error (Conn : TCP_Connection) return String is
   begin
      return Conn.Error_Msg (1 .. Conn.Error_Len);
   end Last_Error;

   -------------
   -- Connect --
   -------------

   procedure Connect
     (Conn    : in out TCP_Connection;
      Host    : String;
      Port    : Natural := 502;
      Timeout : Duration := 5.0;
      Result  : out Status)
   is
      pragma Unreferenced (Timeout);
      Address : Sock_Addr_Type;
   begin
      --  Create socket
      Create_Socket (Conn.Socket, Family_Inet, Socket_Stream);

      --  Resolve host
      Address.Addr := Addresses (Get_Host_By_Name (Host), 1);
      Address.Port := Port_Type (Port);

      --  Connect
      Connect_Socket (Conn.Socket, Address);

      Conn.Address := Address;
      Conn.Current_State := Connected;
      Result := Success;
   exception
      when Socket_Error =>
         Set_Error (Conn, "Socket error during connect");
         if Conn.Socket /= No_Socket then
            Close_Socket (Conn.Socket);
            Conn.Socket := No_Socket;
         end if;
         Result := Frame_Error;
      when Host_Error =>
         Set_Error (Conn, "Host not found: " & Host);
         Result := Frame_Error;
   end Connect;

   ----------------
   -- Disconnect --
   ----------------

   procedure Disconnect (Conn : in out TCP_Connection) is
   begin
      if Conn.Socket /= No_Socket then
         begin
            Shutdown_Socket (Conn.Socket, Shut_Read_Write);
         exception
            when Socket_Error => null;
         end;
         Close_Socket (Conn.Socket);
         Conn.Socket := No_Socket;
      end if;
      Conn.Current_State := Disconnected;
   end Disconnect;

   ------------
   -- Listen --
   ------------

   procedure Listen
     (Conn   : in out TCP_Connection;
      Port   : Natural := 502;
      Result : out Status)
   is
      Address : Sock_Addr_Type;
   begin
      Create_Socket (Conn.Server_Socket, Family_Inet, Socket_Stream);

      --  Allow address reuse
      Set_Socket_Option (Conn.Server_Socket, Socket_Level, (Reuse_Address, True));

      Address.Addr := Any_Inet_Addr;
      Address.Port := Port_Type (Port);

      Bind_Socket (Conn.Server_Socket, Address);
      Listen_Socket (Conn.Server_Socket, 5);

      Conn.Current_State := Listening;
      Result := Success;
   exception
      when Socket_Error =>
         Set_Error (Conn, "Socket error during listen");
         if Conn.Server_Socket /= No_Socket then
            Close_Socket (Conn.Server_Socket);
            Conn.Server_Socket := No_Socket;
         end if;
         Result := Frame_Error;
   end Listen;

   -----------------------
   -- Accept_Connection --
   -----------------------

   procedure Accept_Connection
     (Server : TCP_Connection;
      Client : out TCP_Connection;
      Result : out Status)
   is
   begin
      Accept_Socket (Server.Server_Socket, Client.Socket, Client.Address);
      Client.Current_State := Connected;
      Result := Success;
   exception
      when Socket_Error =>
         Client.Current_State := Error;
         Result := Frame_Error;
   end Accept_Connection;

   ------------------
   -- Close_Server --
   ------------------

   procedure Close_Server (Conn : in out TCP_Connection) is
   begin
      if Conn.Server_Socket /= No_Socket then
         Close_Socket (Conn.Server_Socket);
         Conn.Server_Socket := No_Socket;
      end if;
      Conn.Current_State := Disconnected;
   end Close_Server;

   ----------
   -- Send --
   ----------

   function Send (Conn : in out TCP_Connection; Data : Byte_Array) return Natural is
      use Ada.Streams;
      Buffer : Stream_Element_Array (1 .. Stream_Element_Offset (Data'Length));
      Last   : Stream_Element_Offset;
   begin
      if Conn.Current_State /= Connected then
         return 0;
      end if;

      --  Convert to stream elements
      for I in Data'Range loop
         Buffer (Stream_Element_Offset (I - Data'First + 1)) := Stream_Element (Data (I));
      end loop;

      Send_Socket (Conn.Socket, Buffer, Last);
      return Natural (Last);
   exception
      when Socket_Error =>
         Conn.Current_State := Error;
         return 0;
   end Send;

   -------------
   -- Receive --
   -------------

   function Receive
     (Conn       : in out TCP_Connection;
      Buffer     : out Byte_Array;
      Max_Length : Natural;
      Timeout_Ms : Natural) return Natural
   is
      use Ada.Streams;
      Recv_Buffer : Stream_Element_Array (1 .. Stream_Element_Offset (Max_Length));
      Last        : Stream_Element_Offset;
      Selector    : Selector_Type;
      R_Set       : Socket_Set_Type;
      W_Set       : Socket_Set_Type;
      Status_Sel  : Selector_Status;
      Timeout_Dur : constant Duration := Duration (Timeout_Ms) / 1000.0;
   begin
      Buffer := [others => 0];

      if Conn.Current_State /= Connected then
         return 0;
      end if;

      --  Use selector for timeout
      Create_Selector (Selector);
      Empty (R_Set);
      Empty (W_Set);
      Set (R_Set, Conn.Socket);

      Check_Selector (Selector, R_Set, W_Set, Status_Sel, Timeout_Dur);
      Close_Selector (Selector);

      if Status_Sel = Expired then
         return 0;  --  Timeout
      end if;

      if Status_Sel = Completed and then Is_Set (R_Set, Conn.Socket) then
         Receive_Socket (Conn.Socket, Recv_Buffer, Last);

         if Last < 1 then
            --  Connection closed
            Conn.Current_State := Disconnected;
            return 0;
         end if;

         --  Convert from stream elements
         for I in 1 .. Last loop
            Buffer (Buffer'First + Natural (I) - 1) := Byte (Recv_Buffer (I));
         end loop;

         return Natural (Last);
      end if;

      return 0;
   exception
      when Socket_Error =>
         Conn.Current_State := Error;
         return 0;
   end Receive;

   ----------------
   -- Send_Frame --
   ----------------

   procedure Send_Frame
     (Conn   : in out TCP_Connection;
      Frame  : Byte_Array;
      Result : out Status)
   is
      Sent : constant Natural := Send (Conn, Frame);
   begin
      if Sent = Frame'Length then
         Result := Success;
      else
         Result := Frame_Error;
      end if;
   end Send_Frame;

   -------------------
   -- Receive_Frame --
   -------------------

   procedure Receive_Frame
     (Conn       : in out TCP_Connection;
      Buffer     : out Byte_Array;
      Length     : out Natural;
      Timeout_Ms : Natural;
      Result     : out Status)
   is
      Header      : Byte_Array (0 .. 5);
      Header_Recv : Natural;
      Data_Length : Natural;
      Data_Recv   : Natural;
   begin
      Buffer := [others => 0];
      Length := 0;

      --  Receive MBAP header (6 bytes: Trans ID + Proto ID + Length)
      Header_Recv := Receive (Conn, Header, 6, Timeout_Ms);

      if Header_Recv < 6 then
         Result := Timeout;
         return;
      end if;

      --  Extract length from header (bytes 4-5, big endian)
      Data_Length := Natural (Header (4)) * 256 + Natural (Header (5));

      --  Receive remaining data (Unit ID + PDU)
      declare
         Data_Buffer : Byte_Array (0 .. Data_Length - 1);
      begin
         Data_Recv := Receive (Conn, Data_Buffer, Data_Length, Timeout_Ms);

         if Data_Recv < Data_Length then
            Result := Timeout;
            return;
         end if;

         --  Combine header and data into buffer
         Buffer (Buffer'First .. Buffer'First + 5) := Header;
         for I in 0 .. Data_Length - 1 loop
            Buffer (Buffer'First + 6 + I) := Data_Buffer (I);
         end loop;

         Length := 6 + Data_Length;
         Result := Success;
      end;
   exception
      when others =>
         Result := Frame_Error;
   end Receive_Frame;

end Ada_Modbus.Transport.TCP;
