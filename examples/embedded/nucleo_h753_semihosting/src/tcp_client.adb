--  TCP_Client - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with System;
with Interfaces.C; use type Interfaces.C.int;
with LwIP_Bindings; use LwIP_Bindings;
with STM32H7_HAL;

package body TCP_Client is

   --  Internal state
   Current_State : Connection_State := Disconnected;
   Current_PCB   : TCP_PCB_Ptr := null;

   --  Network interface
   Netif_Struct  : aliased Netif_T;
   Netif_IP      : IP4_Addr_T;
   Netif_Mask    : IP4_Addr_T;
   Netif_GW      : IP4_Addr_T;

   --  Receive buffer
   RX_Buffer : Byte_Array (0 .. Max_RX_Size - 1) := [others => 0];
   RX_Len    : Natural := 0;
   RX_Ready  : Boolean := False;

   ----------------------
   --  Callback Stubs  --
   ----------------------

   --  Called when connection is established
   function Connect_Callback
     (Arg : System.Address;
      PCB : TCP_PCB_Ptr;
      Err : Err_T) return Err_T
     with Convention => C;

   function Connect_Callback
     (Arg : System.Address;
      PCB : TCP_PCB_Ptr;
      Err : Err_T) return Err_T
   is
      pragma Unreferenced (Arg, PCB);
   begin
      if Err = ERR_OK then
         Current_State := Connected;
      else
         Current_State := Error;
      end if;
      return ERR_OK;
   end Connect_Callback;

   --  Called when data is received
   function Recv_Callback
     (Arg : System.Address;
      PCB : TCP_PCB_Ptr;
      P   : Pbuf_Ptr;
      Err : Err_T) return Err_T
     with Convention => C;

   function Recv_Callback
     (Arg : System.Address;
      PCB : TCP_PCB_Ptr;
      P   : Pbuf_Ptr;
      Err : Err_T) return Err_T
   is
      pragma Unreferenced (Arg, Err);
      Len      : Unsigned_16;
      Copied   : Unsigned_16;
      Dummy    : Unsigned_8;
   begin
      if P = null then
         --  Connection closed by remote
         Current_State := Disconnected;
         return ERR_OK;
      end if;

      --  Get data length
      Len := Pbuf_Get_Tot_Len (P);
      if Len > Unsigned_16 (Max_RX_Size) then
         Len := Unsigned_16 (Max_RX_Size);
      end if;

      --  Copy data to buffer
      Copied := Pbuf_Copy_Partial (P, RX_Buffer'Address, Len, 0);
      RX_Len := Natural (Copied);
      RX_Ready := True;

      --  Acknowledge received data
      TCP_Recved (PCB, Len);

      --  Free pbuf
      Dummy := Pbuf_Free (P);

      return ERR_OK;
   end Recv_Callback;

   --  Called on error
   procedure Err_Callback (Arg : System.Address; Err : Err_T)
     with Convention => C;

   procedure Err_Callback (Arg : System.Address; Err : Err_T) is
      pragma Unreferenced (Arg, Err);
   begin
      Current_State := Error;
      Current_PCB := null;
   end Err_Callback;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Local_IP   : Unsigned_32;
      Netmask    : Unsigned_32;
      Gateway    : Unsigned_32)
   is
   begin
      --  Store network config
      Netif_IP := (Addr => Local_IP);
      Netif_Mask := (Addr => Netmask);
      Netif_GW := (Addr => Gateway);

      --  Initialize LwIP stack
      LwIP_Init;

      --  Add network interface
      Netif_Add (Netif_Struct'Access, Netif_IP, Netif_Mask, Netif_GW);

      --  Set as default and bring up
      Netif_Set_Default (Netif_Struct'Access);
      Netif_Set_Up (Netif_Struct'Access);

      Current_State := Disconnected;
   end Initialize;

   ----------
   -- Poll --
   ----------

   procedure Poll is
   begin
      --  Process received Ethernet frames
      Ethernetif_Input (Netif_Struct'Access);

      --  Process LwIP timeouts
      Sys_Check_Timeouts;
   end Poll;

   -------------
   -- Connect --
   -------------

   procedure Connect
     (Remote_IP  : Unsigned_32;
      Remote_Port : Unsigned_16;
      Timeout_Ms : Unsigned_32;
      Result     : out Status)
   is
      IP : aliased IP4_Addr_T := (Addr => Remote_IP);
      Err : Err_T;
      Start_Time : constant Unsigned_32 := STM32H7_HAL.Get_Tick;
   begin
      --  Disconnect if already connected
      if Current_PCB /= null then
         Disconnect;
      end if;

      --  Create new TCP PCB
      Current_PCB := TCP_New;
      if Current_PCB = null then
         Result := Buffer_Too_Small;
         return;
      end if;

      --  Set callbacks
      TCP_Arg (Current_PCB, System.Null_Address);
      TCP_Recv (Current_PCB, Recv_Callback'Address);
      TCP_Err (Current_PCB, Err_Callback'Address);

      --  Start connection
      Current_State := Connecting;

      Err := TCP_Connect (Current_PCB, IP'Access, Remote_Port, Connect_Callback'Address);
      if Err /= ERR_OK then
         TCP_Abort (Current_PCB);
         Current_PCB := null;
         Current_State := Disconnected;
         Result := Timeout;
         return;
      end if;

      --  Wait for connection with timeout
      while Current_State = Connecting loop
         Poll;

         if STM32H7_HAL.Get_Tick - Start_Time > Timeout_Ms then
            TCP_Abort (Current_PCB);
            Current_PCB := null;
            Current_State := Disconnected;
            Result := Timeout;
            return;
         end if;
      end loop;

      if Current_State = Connected then
         Result := Success;
      else
         Result := Timeout;
      end if;
   end Connect;

   ----------------
   -- Disconnect --
   ----------------

   procedure Disconnect is
      Err : Err_T;
   begin
      if Current_PCB /= null then
         Err := TCP_Close (Current_PCB);
         if Err /= ERR_OK then
            TCP_Abort (Current_PCB);
         end if;
         Current_PCB := null;
      end if;
      Current_State := Disconnected;
      RX_Ready := False;
      RX_Len := 0;
   end Disconnect;

   ------------------
   -- Is_Connected --
   ------------------

   function Is_Connected return Boolean is
   begin
      return Current_State = Connected;
   end Is_Connected;

   ---------------
   -- Get_State --
   ---------------

   function Get_State return Connection_State is
   begin
      return Current_State;
   end Get_State;

   ----------
   -- Send --
   ----------

   procedure Send
     (Data   : Byte_Array;
      Result : out Status)
   is
      Err : Err_T;
   begin
      if Current_State /= Connected then
         Result := Invalid_Request;
         return;
      end if;

      --  Write data to send buffer
      Err := TCP_Write (Current_PCB, Data'Address, Unsigned_16 (Data'Length),
                        TCP_WRITE_FLAG_COPY);
      if Err /= ERR_OK then
         Result := Buffer_Too_Small;
         return;
      end if;

      --  Trigger sending
      Err := TCP_Output (Current_PCB);
      if Err /= ERR_OK then
         Result := Timeout;
         return;
      end if;

      Result := Success;
   end Send;

   -------------
   -- Receive --
   -------------

   procedure Receive
     (Data       : out Byte_Array;
      Length     : out Natural;
      Timeout_Ms : Unsigned_32;
      Result     : out Status)
   is
      Start_Time : constant Unsigned_32 := STM32H7_HAL.Get_Tick;
   begin
      Data := [others => 0];
      Length := 0;

      if Current_State /= Connected then
         Result := Invalid_Request;
         return;
      end if;

      --  Clear receive ready flag
      RX_Ready := False;

      --  Wait for data with timeout
      while not RX_Ready loop
         Poll;

         if Current_State /= Connected then
            Result := Timeout;
            return;
         end if;

         if STM32H7_HAL.Get_Tick - Start_Time > Timeout_Ms then
            Result := Timeout;
            return;
         end if;
      end loop;

      --  Copy received data
      if RX_Len > Data'Length then
         Length := Data'Length;
      else
         Length := RX_Len;
      end if;

      for I in 0 .. Length - 1 loop
         Data (Data'First + I) := RX_Buffer (I);
      end loop;

      RX_Ready := False;
      Result := Success;
   end Receive;

   ---------------
   -- Transceive --
   ---------------

   procedure Transceive
     (TX_Data    : Byte_Array;
      RX_Data    : out Byte_Array;
      RX_Length  : out Natural;
      Timeout_Ms : Unsigned_32;
      Result     : out Status)
   is
   begin
      --  Send data
      Send (TX_Data, Result);
      if Result /= Success then
         RX_Data := [others => 0];
         RX_Length := 0;
         return;
      end if;

      --  Receive response
      Receive (RX_Data, RX_Length, Timeout_Ms, Result);
   end Transceive;

   ----------------
   -- Link_Is_Up --
   ----------------

   function Link_Is_Up return Boolean is
   begin
      return Ethernetif_Link_Status /= 0;
   end Link_Is_Up;

end TCP_Client;
