--  RTU_Slave - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with Interfaces; use Interfaces;
with Ada_Modbus.Slave; use Ada_Modbus.Slave;
with STM32_SysTick;

package body RTU_Slave is

   --  Buffers
   Request_Buffer  : Byte_Array (0 .. 255) := [others => 0];
   Response_Buffer : Byte_Array (0 .. 255) := [others => 0];

   --  Convert between Byte_Array types
   function To_UART_Buffer (Data : Byte_Array) return LM3S_UART.Byte_Array is
      Result : LM3S_UART.Byte_Array (Data'Range);
   begin
      for I in Data'Range loop
         Result (I) := Unsigned_8 (Data (I));
      end loop;
      return Result;
   end To_UART_Buffer;

   function From_UART_Buffer (Data : LM3S_UART.Byte_Array) return Byte_Array is
      Result : Byte_Array (Data'Range);
   begin
      for I in Data'Range loop
         Result (I) := Byte (Data (I));
      end loop;
      return Result;
   end From_UART_Buffer;

   --  Callbacks
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

   --  Slave configuration
   Slave_Config : constant Ada_Modbus.Slave.Slave_Config :=
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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      --  Initialize SysTick for timing
      STM32_SysTick.Initialize;

      --  Initialize UART
      LM3S_UART.Initialize (UART_Port, LM3S_UART.B9600);

      --  Reset counters
      Request_Count := 0;
      Error_Count := 0;

      --  Initialize with some test data
      for I in Holding_Registers'Range loop
         Holding_Registers (I) := Register_Value (I * 100);
      end loop;

      for I in Input_Registers'Range loop
         Input_Registers (I) := Register_Value (I * 10 + 1000);
      end loop;

      for I in 0 .. 7 loop
         Coils (I) := (I mod 2) = 0;
      end loop;
   end Initialize;

   ----------
   -- Poll --
   ----------

   function Poll return Boolean is
      UART_Buffer : LM3S_UART.Byte_Array (0 .. 255);
      Request_Length  : Natural;
      Response_Length : Natural;
      Send_Response   : Boolean;
      Sent            : Natural;
      More_Data       : Natural;
   begin
      --  Check if any data is available (non-blocking check)
      if not LM3S_UART.RX_Available (UART_Port) then
         return False;
      end if;

      --  Data is available, receive with longer timeout to get complete frame
      --  First receive gets initial data
      Request_Length := LM3S_UART.Receive
        (UART_Port, UART_Buffer, UART_Buffer'Length, 50);

      if Request_Length = 0 then
         return False;  --  No data received
      end if;

      --  Try to receive more data with short timeout (inter-frame gap detection)
      --  This ensures we get the complete frame
      loop
         declare
            Temp_Buffer : LM3S_UART.Byte_Array (0 .. 255);
         begin
            More_Data := LM3S_UART.Receive
              (UART_Port, Temp_Buffer, Temp_Buffer'Length, 5);
            if More_Data > 0 and Request_Length + More_Data <= UART_Buffer'Length then
               UART_Buffer (Request_Length .. Request_Length + More_Data - 1) :=
                 Temp_Buffer (0 .. More_Data - 1);
               Request_Length := Request_Length + More_Data;
            else
               exit;
            end if;
         end;
      end loop;

      --  Convert to Modbus buffer
      Request_Buffer (0 .. Request_Length - 1) :=
        From_UART_Buffer (UART_Buffer (0 .. Request_Length - 1));

      Request_Count := Request_Count + 1;

      --  Process the request
      Process_Request (Slave_Config,
                       Request_Buffer, Request_Length,
                       Response_Buffer, Response_Length,
                       Send_Response);

      --  Send response if needed
      if Send_Response and Response_Length > 0 then
         Sent := LM3S_UART.Send
           (UART_Port,
            To_UART_Buffer (Response_Buffer (0 .. Response_Length - 1)));
         if Sent /= Response_Length then
            Error_Count := Error_Count + 1;
         end if;
      end if;

      return True;
   end Poll;

end RTU_Slave;
