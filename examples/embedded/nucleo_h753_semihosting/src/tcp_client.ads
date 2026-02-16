--  TCP_Client - Blocking TCP Client using LwIP
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Provides a blocking TCP client API on top of LwIP's callback-based raw API.
--  Uses polling to wait for connection/data.

with Interfaces; use Interfaces;
with Ada_Modbus; use Ada_Modbus;

package TCP_Client is

   --  Maximum receive buffer size
   Max_RX_Size : constant := 512;

   --  Connection state
   type Connection_State is
     (Disconnected,
      Connecting,
      Connected,
      Error);

   --  Initialize TCP client and LwIP stack
   procedure Initialize
     (Local_IP   : Unsigned_32;
      Netmask    : Unsigned_32;
      Gateway    : Unsigned_32);

   --  Poll network (call frequently from main loop)
   procedure Poll;

   --  Connect to remote host
   procedure Connect
     (Remote_IP  : Unsigned_32;
      Remote_Port : Unsigned_16;
      Timeout_Ms : Unsigned_32;
      Result     : out Status);

   --  Disconnect
   procedure Disconnect;

   --  Check if connected
   function Is_Connected return Boolean;

   --  Get connection state
   function Get_State return Connection_State;

   --  Send data
   procedure Send
     (Data   : Byte_Array;
      Result : out Status);

   --  Receive data (blocking with timeout)
   procedure Receive
     (Data       : out Byte_Array;
      Length     : out Natural;
      Timeout_Ms : Unsigned_32;
      Result     : out Status);

   --  Send and receive (for Modbus TCP transaction)
   procedure Transceive
     (TX_Data    : Byte_Array;
      RX_Data    : out Byte_Array;
      RX_Length  : out Natural;
      Timeout_Ms : Unsigned_32;
      Result     : out Status);

   --  Check Ethernet link status
   function Link_Is_Up return Boolean;

end TCP_Client;
