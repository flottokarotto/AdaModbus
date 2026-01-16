--  Modbus_TCP_Slave - Embedded Modbus TCP Slave for Cortex-M4
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  This example demonstrates a Modbus TCP slave (server) running on
--  a Cortex-M4 microcontroller with LwIP TCP/IP stack.
--
--  Features:
--  - Holding Registers (FC03/FC06/FC16): 100 registers
--  - Input Registers (FC04): 16 registers (e.g., ADC values)
--  - Coils (FC01/FC05/FC15): 32 coils (e.g., GPIO outputs)
--  - Discrete Inputs (FC02): 32 inputs (e.g., GPIO inputs)
--
--  Integration:
--  1. Call Modbus_Init once at startup (after LwIP is initialized)
--  2. Call Modbus_Poll periodically from main loop
--  3. Access data via Holding_Registers, Input_Registers, etc.

with Interfaces; use Interfaces;
with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.Slave; use Ada_Modbus.Slave;
with Ada_Modbus.Protocol.TCP;
with Ada_Modbus.Transport.LwIP; use Ada_Modbus.Transport.LwIP;

package body Modbus_TCP_Slave is

   --  Server and client sockets
   Server_Socket : TCP_Connection;
   Client_Socket : TCP_Connection;
   Has_Client    : Boolean := False;

   --  Receive/Transmit buffers
   Rx_Buffer : Byte_Array (0 .. 260) := [others => 0];
   Tx_Buffer : Byte_Array (0 .. 260) := [others => 0];

   --  Data stores (directly accessible from application)
   Holding_Registers : Register_Array (0 .. 99) := [others => 0];
   Input_Registers   : Register_Array (0 .. 15) := [others => 0];
   Coil_Status       : Coil_Array (0 .. 31) := [others => False];
   Discrete_Inputs   : Coil_Array (0 .. 31) := [others => False];

   -----------------------
   --  Slave Callbacks  --
   -----------------------

   function Read_Holding_Registers_CB
     (Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array) return Status
   is
      Start : constant Natural := Natural (Start_Address);
   begin
      if Start + Natural (Quantity) > Holding_Registers'Length then
         return Exception_Illegal_Address;
      end if;

      for I in 0 .. Natural (Quantity) - 1 loop
         Values (Values'First + I) := Holding_Registers (Start + I);
      end loop;

      return Success;
   end Read_Holding_Registers_CB;

   function Read_Input_Registers_CB
     (Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array) return Status
   is
      Start : constant Natural := Natural (Start_Address);
   begin
      if Start + Natural (Quantity) > Input_Registers'Length then
         return Exception_Illegal_Address;
      end if;

      for I in 0 .. Natural (Quantity) - 1 loop
         Values (Values'First + I) := Input_Registers (Start + I);
      end loop;

      return Success;
   end Read_Input_Registers_CB;

   function Write_Single_Register_CB
     (Address : Register_Address;
      Value   : Register_Value) return Status
   is
      Addr : constant Natural := Natural (Address);
   begin
      if Addr >= Holding_Registers'Length then
         return Exception_Illegal_Address;
      end if;

      Holding_Registers (Addr) := Value;
      return Success;
   end Write_Single_Register_CB;

   function Write_Multiple_Registers_CB
     (Start_Address : Register_Address;
      Values        : Register_Array) return Status
   is
      Start : constant Natural := Natural (Start_Address);
   begin
      if Start + Values'Length > Holding_Registers'Length then
         return Exception_Illegal_Address;
      end if;

      for I in Values'Range loop
         Holding_Registers (Start + I - Values'First) := Values (I);
      end loop;

      return Success;
   end Write_Multiple_Registers_CB;

   function Read_Coils_CB
     (Start_Address : Coil_Address;
      Quantity      : Coil_Count;
      Values        : out Coil_Array) return Status
   is
      Start : constant Natural := Natural (Start_Address);
   begin
      if Start + Natural (Quantity) > Coil_Status'Length then
         return Exception_Illegal_Address;
      end if;

      for I in 0 .. Natural (Quantity) - 1 loop
         Values (Values'First + I) := Coil_Status (Start + I);
      end loop;

      return Success;
   end Read_Coils_CB;

   function Read_Discrete_Inputs_CB
     (Start_Address : Coil_Address;
      Quantity      : Coil_Count;
      Values        : out Coil_Array) return Status
   is
      Start : constant Natural := Natural (Start_Address);
   begin
      if Start + Natural (Quantity) > Discrete_Inputs'Length then
         return Exception_Illegal_Address;
      end if;

      for I in 0 .. Natural (Quantity) - 1 loop
         Values (Values'First + I) := Discrete_Inputs (Start + I);
      end loop;

      return Success;
   end Read_Discrete_Inputs_CB;

   function Write_Single_Coil_CB
     (Address : Coil_Address;
      Value   : Coil_Value) return Status
   is
      Addr : constant Natural := Natural (Address);
   begin
      if Addr >= Coil_Status'Length then
         return Exception_Illegal_Address;
      end if;

      Coil_Status (Addr) := Value;
      return Success;
   end Write_Single_Coil_CB;

   function Write_Multiple_Coils_CB
     (Start_Address : Coil_Address;
      Values        : Coil_Array) return Status
   is
      Start : constant Natural := Natural (Start_Address);
   begin
      if Start + Values'Length > Coil_Status'Length then
         return Exception_Illegal_Address;
      end if;

      for I in Values'Range loop
         Coil_Status (Start + I - Values'First) := Values (I);
      end loop;

      return Success;
   end Write_Multiple_Coils_CB;

   --  Slave configuration
   Slave_Config_Val : constant Slave_Config :=
     (Mode      => TCP,
      Unit_Id   => 1,
      Callbacks =>
        (Read_Holding_Registers   => Read_Holding_Registers_CB'Access,
         Read_Input_Registers     => Read_Input_Registers_CB'Access,
         Write_Single_Register    => Write_Single_Register_CB'Access,
         Write_Multiple_Registers => Write_Multiple_Registers_CB'Access,
         Read_Coils               => Read_Coils_CB'Access,
         Read_Discrete_Inputs     => Read_Discrete_Inputs_CB'Access,
         Write_Single_Coil        => Write_Single_Coil_CB'Access,
         Write_Multiple_Coils     => Write_Multiple_Coils_CB'Access,
         Read_Exception_Status    => null,
         Diagnostics              => null,
         Report_Server_Id         => null,
         Mask_Write_Register      => null,
         Read_Write_Registers     => null));

   -----------------
   -- Modbus_Init --
   -----------------

   procedure Modbus_Init (Port : Unsigned_16; Result : out Status) is
   begin
      Has_Client := False;

      --  Start listening on specified port
      Listen (Server_Socket, Port, Result);
   end Modbus_Init;

   -----------------
   -- Modbus_Poll --
   -----------------

   procedure Modbus_Poll is
      Result        : Status;
      Rx_Len        : Natural;
      Tx_Len        : Natural;
      Send_Response : Boolean;
      Bytes_Sent    : Natural;
   begin
      --  If no client, try to accept one (non-blocking would be better)
      if not Has_Client then
         --  Note: This is blocking! For production, use select() or
         --  check socket state before accepting
         Accept_Connection (Server_Socket, Client_Socket, Result);
         if Result = Success then
            Has_Client := True;
         end if;
         return;
      end if;

      --  Try to receive data from client (with short timeout)
      Rx_Len := Receive (Client_Socket, Rx_Buffer, Rx_Buffer'Length, 10);

      if Rx_Len = 0 then
         --  No data or timeout - check if connection is still alive
         --  For simplicity, we keep the connection
         return;
      end if;

      --  Process the Modbus request
      Process_Request
        (Slave_Config_Val,
         Rx_Buffer, Rx_Len,
         Tx_Buffer, Tx_Len,
         Send_Response);

      --  Send response if needed
      if Send_Response and then Tx_Len > 0 then
         Bytes_Sent := Send (Client_Socket, Tx_Buffer (0 .. Tx_Len - 1));

         if Bytes_Sent = 0 then
            --  Send failed, disconnect client
            Disconnect (Client_Socket);
            Has_Client := False;
         end if;
      end if;
   end Modbus_Poll;

   ----------------------
   -- Data Accessors   --
   ----------------------

   function Get_Holding_Register (Index : Natural) return Register_Value is
   begin
      if Index <= Holding_Registers'Last then
         return Holding_Registers (Index);
      else
         return 0;
      end if;
   end Get_Holding_Register;

   procedure Set_Holding_Register (Index : Natural; Value : Register_Value) is
   begin
      if Index <= Holding_Registers'Last then
         Holding_Registers (Index) := Value;
      end if;
   end Set_Holding_Register;

   procedure Set_Input_Register (Index : Natural; Value : Register_Value) is
   begin
      if Index <= Input_Registers'Last then
         Input_Registers (Index) := Value;
      end if;
   end Set_Input_Register;

   function Get_Coil (Index : Natural) return Boolean is
   begin
      if Index <= Coil_Status'Last then
         return Coil_Status (Index);
      else
         return False;
      end if;
   end Get_Coil;

   procedure Set_Discrete_Input (Index : Natural; Value : Boolean) is
   begin
      if Index <= Discrete_Inputs'Last then
         Discrete_Inputs (Index) := Value;
      end if;
   end Set_Discrete_Input;

end Modbus_TCP_Slave;
