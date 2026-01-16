--  Minimal_RTU_Slave - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with Ada_Modbus.Protocol.RTU;

package body Minimal_RTU_Slave is

   --  Buffers
   Request_Buffer  : Byte_Array (0 .. 255) := [others => 0];
   Response_Buffer : Byte_Array (0 .. 255) := [others => 0];

   --  Slave configuration
   Slave_Config_Val : constant Slave_Config :=
     (Mode      => RTU,
      Unit_Id   => Unit_Id_Value,
      Callbacks =>
        (Read_Holding_Registers   => Read_Holding_Registers_CB'Access,
         Read_Input_Registers     => Read_Input_Registers_CB'Access,
         Write_Single_Register    => Write_Single_Register_CB'Access,
         Write_Multiple_Registers => Write_Multiple_Registers_CB'Access,
         Read_Coils               => Read_Coils_CB'Access,
         Read_Discrete_Inputs     => null,
         Write_Single_Coil        => Write_Single_Coil_CB'Access,
         Write_Multiple_Coils     => null,
         Read_Exception_Status    => null,
         Diagnostics              => null,
         Report_Server_Id         => null,
         Mask_Write_Register      => null,
         Read_Write_Registers     => null));

   --  Hardware abstraction (implement these for your platform)
   --  These are weak symbols that should be overridden

   function UART_Receive
     (Buffer     : out Byte_Array;
      Max_Length : Natural;
      Timeout_Ms : Natural) return Natural
     with Import, Convention => C, External_Name => "uart_receive";
   --  Implement this to receive data from UART
   --  Return number of bytes received, 0 on timeout

   function UART_Send (Data : Byte_Array) return Natural
     with Import, Convention => C, External_Name => "uart_send";
   --  Implement this to send data via UART
   --  Return number of bytes sent

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Request_Count := 0;
      Error_Count := 0;

      --  Initialize registers with default values
      Holding_Registers := [others => 0];
      Input_Registers := [others => 0];
      Coils := [others => False];
   end Initialize;

   ----------
   -- Poll --
   ----------

   function Poll return Boolean is
      Request_Length  : Natural;
      Response_Length : Natural;
      Send_Response   : Boolean;
      Sent            : Natural;
   begin
      --  Try to receive a request (non-blocking with short timeout)
      Request_Length := UART_Receive (Request_Buffer, Request_Buffer'Length, 10);

      if Request_Length = 0 then
         return False;  --  No data received
      end if;

      Request_Count := Request_Count + 1;

      --  Process the request
      Process_Request (Slave_Config_Val,
                       Request_Buffer, Request_Length,
                       Response_Buffer, Response_Length,
                       Send_Response);

      --  Send response if needed
      if Send_Response and Response_Length > 0 then
         Sent := UART_Send (Response_Buffer (0 .. Response_Length - 1));
         if Sent /= Response_Length then
            Error_Count := Error_Count + 1;
         end if;
      end if;

      return True;
   end Poll;

   --------------------------------
   -- Read_Holding_Registers_CB --
   --------------------------------

   function Read_Holding_Registers_CB
     (Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array) return Status
   is
      Start : constant Natural := Natural (Start_Address);
   begin
      if Start + Natural (Quantity) - 1 > Holding_Registers'Last then
         return Exception_Illegal_Address;
      end if;

      for I in 0 .. Natural (Quantity) - 1 loop
         Values (Values'First + I) := Holding_Registers (Start + I);
      end loop;

      return Success;
   end Read_Holding_Registers_CB;

   ------------------------------
   -- Read_Input_Registers_CB --
   ------------------------------

   function Read_Input_Registers_CB
     (Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array) return Status
   is
      Start : constant Natural := Natural (Start_Address);
   begin
      if Start + Natural (Quantity) - 1 > Input_Registers'Last then
         return Exception_Illegal_Address;
      end if;

      for I in 0 .. Natural (Quantity) - 1 loop
         Values (Values'First + I) := Input_Registers (Start + I);
      end loop;

      return Success;
   end Read_Input_Registers_CB;

   -----------------------------
   -- Write_Single_Register_CB --
   -----------------------------

   function Write_Single_Register_CB
     (Address : Register_Address;
      Value   : Register_Value) return Status
   is
      Addr : constant Natural := Natural (Address);
   begin
      if Addr > Holding_Registers'Last then
         return Exception_Illegal_Address;
      end if;

      Holding_Registers (Addr) := Value;
      return Success;
   end Write_Single_Register_CB;

   ---------------------------------
   -- Write_Multiple_Registers_CB --
   ---------------------------------

   function Write_Multiple_Registers_CB
     (Start_Address : Register_Address;
      Values        : Register_Array) return Status
   is
      Start : constant Natural := Natural (Start_Address);
   begin
      if Start + Values'Length - 1 > Holding_Registers'Last then
         return Exception_Illegal_Address;
      end if;

      for I in Values'Range loop
         Holding_Registers (Start + I - Values'First) := Values (I);
      end loop;

      return Success;
   end Write_Multiple_Registers_CB;

   -------------------
   -- Read_Coils_CB --
   -------------------

   function Read_Coils_CB
     (Start_Address : Coil_Address;
      Quantity      : Coil_Count;
      Values        : out Coil_Array) return Status
   is
      Start : constant Natural := Natural (Start_Address);
   begin
      if Start + Natural (Quantity) - 1 > Coils'Last then
         return Exception_Illegal_Address;
      end if;

      for I in 0 .. Natural (Quantity) - 1 loop
         Values (Values'First + I) := Coils (Start + I);
      end loop;

      return Success;
   end Read_Coils_CB;

   -------------------------
   -- Write_Single_Coil_CB --
   -------------------------

   function Write_Single_Coil_CB
     (Address : Coil_Address;
      Value   : Coil_Value) return Status
   is
      Addr : constant Natural := Natural (Address);
   begin
      if Addr > Coils'Last then
         return Exception_Illegal_Address;
      end if;

      Coils (Addr) := Value;
      return Success;
   end Write_Single_Coil_CB;

end Minimal_RTU_Slave;
