--  RTU_Master - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with Interfaces; use Interfaces;
with Ada_Modbus.Master;
with STM32_SysTick;

package body RTU_Master is

   --  UART context type (just a placeholder for this example)
   type UART_Context is null record;
   UART_Ctx : aliased UART_Context;

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

   --  Transport callbacks for Master generic
   function Send_Data
     (Ctx  : in out UART_Context;
      Data : Byte_Array) return Natural
   is
      pragma Unreferenced (Ctx);
   begin
      return LM3S_UART.Send (UART_Port, To_UART_Buffer (Data));
   end Send_Data;

   function Receive_Data
     (Ctx        : in Out UART_Context;
      Buffer     : out Byte_Array;
      Max_Length : Natural;
      Timeout_Ms : Natural) return Natural
   is
      pragma Unreferenced (Ctx);
      UART_Buffer : LM3S_UART.Byte_Array (0 .. Max_Length - 1);
      Received : Natural;
   begin
      Buffer := [others => 0];
      Received := LM3S_UART.Receive (UART_Port, UART_Buffer, Max_Length, Timeout_Ms);
      if Received > 0 then
         Buffer (Buffer'First .. Buffer'First + Received - 1) :=
           From_UART_Buffer (UART_Buffer (0 .. Received - 1));
      end if;
      return Received;
   end Receive_Data;

   function Get_Tick return Unsigned_32 is
   begin
      return STM32_SysTick.Get_Tick_Ms;
   end Get_Tick;

   --  Instantiate Master generic
   package My_Master is new Ada_Modbus.Master
     (Transport_Context => UART_Context,
      Send              => Send_Data,
      Receive           => Receive_Data,
      Get_Tick_Ms       => Get_Tick);

   Master_Ctx : My_Master.Master_Context;
   Config     : My_Master.Master_Config;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      --  Initialize SysTick for timing
      STM32_SysTick.Initialize;

      --  Initialize UART
      LM3S_UART.Initialize (UART_Port, LM3S_UART.B9600);

      --  Initialize Master context
      Config.Mode := My_Master.RTU;
      Config.Default_Slave := Default_Slave;
      Config.Default_Timeout := 1000;
      My_Master.Initialize (Master_Ctx, Config, UART_Ctx);

      --  Reset counters
      Request_Count := 0;
      Response_Count := 0;
      Error_Count := 0;
      Timeout_Count := 0;
   end Initialize;

   ----------------------------
   -- Read_Holding_Registers --
   ----------------------------

   function Read_Holding_Registers
     (Slave         : Unit_Id;
      Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array;
      Timeout_Ms    : Natural := 1000) return Status
   is
      Result : Status;
   begin
      Request_Count := Request_Count + 1;

      Result := My_Master.Read_Holding_Registers
        (Master_Ctx, Slave, Start_Address, Quantity, Values, Timeout_Ms);

      if Result = Success then
         Response_Count := Response_Count + 1;
      elsif Result = Timeout then
         Timeout_Count := Timeout_Count + 1;
      else
         Error_Count := Error_Count + 1;
      end if;

      return Result;
   end Read_Holding_Registers;

   --------------------------
   -- Read_Input_Registers --
   --------------------------

   function Read_Input_Registers
     (Slave         : Unit_Id;
      Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array;
      Timeout_Ms    : Natural := 1000) return Status
   is
      Result : Status;
   begin
      Request_Count := Request_Count + 1;

      Result := My_Master.Read_Input_Registers
        (Master_Ctx, Slave, Start_Address, Quantity, Values, Timeout_Ms);

      if Result = Success then
         Response_Count := Response_Count + 1;
      elsif Result = Timeout then
         Timeout_Count := Timeout_Count + 1;
      else
         Error_Count := Error_Count + 1;
      end if;

      return Result;
   end Read_Input_Registers;

   ---------------------------
   -- Write_Single_Register --
   ---------------------------

   function Write_Single_Register
     (Slave      : Unit_Id;
      Address    : Register_Address;
      Value      : Register_Value;
      Timeout_Ms : Natural := 1000) return Status
   is
      Result : Status;
   begin
      Request_Count := Request_Count + 1;

      Result := My_Master.Write_Single_Register
        (Master_Ctx, Slave, Address, Value, Timeout_Ms);

      if Result = Success then
         Response_Count := Response_Count + 1;
      elsif Result = Timeout then
         Timeout_Count := Timeout_Count + 1;
      else
         Error_Count := Error_Count + 1;
      end if;

      return Result;
   end Write_Single_Register;

   ----------------
   -- Read_Coils --
   ----------------

   function Read_Coils
     (Slave         : Unit_Id;
      Start_Address : Coil_Address;
      Quantity      : Coil_Count;
      Values        : out Coil_Array;
      Timeout_Ms    : Natural := 1000) return Status
   is
      Result : Status;
   begin
      Request_Count := Request_Count + 1;

      Result := My_Master.Read_Coils
        (Master_Ctx, Slave, Start_Address, Quantity, Values, Timeout_Ms);

      if Result = Success then
         Response_Count := Response_Count + 1;
      elsif Result = Timeout then
         Timeout_Count := Timeout_Count + 1;
      else
         Error_Count := Error_Count + 1;
      end if;

      return Result;
   end Read_Coils;

   ----------------------
   -- Write_Single_Coil --
   ----------------------

   function Write_Single_Coil
     (Slave      : Unit_Id;
      Address    : Coil_Address;
      Value      : Coil_Value;
      Timeout_Ms : Natural := 1000) return Status
   is
      Result : Status;
   begin
      Request_Count := Request_Count + 1;

      Result := My_Master.Write_Single_Coil
        (Master_Ctx, Slave, Address, Value, Timeout_Ms);

      if Result = Success then
         Response_Count := Response_Count + 1;
      elsif Result = Timeout then
         Timeout_Count := Timeout_Count + 1;
      else
         Error_Count := Error_Count + 1;
      end if;

      return Result;
   end Write_Single_Coil;

end RTU_Master;
