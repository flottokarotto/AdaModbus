--  RTU_Master - Modbus RTU Master for LM3S6965 (QEMU)
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Minimal RTU master implementation using LM3S UART.

with Ada_Modbus; use Ada_Modbus;
with LM3S_UART;

package RTU_Master is

   --  Configuration
   UART_Port     : constant LM3S_UART.UART_Peripheral := LM3S_UART.UART0;
   Default_Slave : constant Unit_Id := 1;

   --  Statistics
   Request_Count  : Natural := 0;
   Response_Count : Natural := 0;
   Error_Count    : Natural := 0;
   Timeout_Count  : Natural := 0;

   --  Initialize master (call once at startup)
   procedure Initialize;

   --  Read holding registers from slave
   function Read_Holding_Registers
     (Slave         : Unit_Id;
      Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array;
      Timeout_Ms    : Natural := 1000) return Status;

   --  Read input registers from slave
   function Read_Input_Registers
     (Slave         : Unit_Id;
      Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array;
      Timeout_Ms    : Natural := 1000) return Status;

   --  Write single register to slave
   function Write_Single_Register
     (Slave      : Unit_Id;
      Address    : Register_Address;
      Value      : Register_Value;
      Timeout_Ms : Natural := 1000) return Status;

   --  Read coils from slave
   function Read_Coils
     (Slave         : Unit_Id;
      Start_Address : Coil_Address;
      Quantity      : Coil_Count;
      Values        : out Coil_Array;
      Timeout_Ms    : Natural := 1000) return Status;

   --  Write single coil to slave
   function Write_Single_Coil
     (Slave      : Unit_Id;
      Address    : Coil_Address;
      Value      : Coil_Value;
      Timeout_Ms : Natural := 1000) return Status;

end RTU_Master;
