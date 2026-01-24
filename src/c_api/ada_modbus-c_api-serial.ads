--  Ada_Modbus.C_API.Serial - Serial/RTU C bindings
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Serial port and Modbus RTU API for C developers.

with Interfaces.C.Strings;
with System;

package Ada_Modbus.C_API.Serial is

   --  Opaque handle for serial connection
   subtype C_Serial_Handle is System.Address;

   --  Parity settings
   C_Parity_None : constant int := 0;
   C_Parity_Even : constant int := 1;
   C_Parity_Odd  : constant int := 2;

   --  Stop bits
   C_Stop_1 : constant int := 1;
   C_Stop_2 : constant int := 2;

   --  Connection state
   C_Serial_Disconnected : constant int := 0;
   C_Serial_Connected    : constant int := 1;
   C_Serial_Error        : constant int := 2;

   ---------------------
   --  Serial Port API
   ---------------------

   --  Create a new serial connection handle
   function Modbus_Serial_Create return C_Serial_Handle
     with Export, Convention => C, External_Name => "modbus_serial_create";

   --  Destroy a serial connection handle
   procedure Modbus_Serial_Destroy (Handle : C_Serial_Handle)
     with Export, Convention => C, External_Name => "modbus_serial_destroy";

   --  Open serial port
   --  port: "COM1" (Windows) or "/dev/ttyUSB0" (Linux)
   --  baud: 9600, 19200, 38400, 57600, 115200
   --  data_bits: 7 or 8
   --  parity: C_Parity_None, C_Parity_Even, C_Parity_Odd
   --  stop_bits: C_Stop_1, C_Stop_2
   function Modbus_Serial_Open
     (Handle    : C_Serial_Handle;
      Port      : Interfaces.C.Strings.chars_ptr;
      Baud      : int;
      Data_Bits : int;
      Parity    : int;
      Stop_Bits : int) return C_Status
     with Export, Convention => C, External_Name => "modbus_serial_open";

   --  Close serial port
   procedure Modbus_Serial_Close (Handle : C_Serial_Handle)
     with Export, Convention => C, External_Name => "modbus_serial_close";

   --  Get connection state
   function Modbus_Serial_State (Handle : C_Serial_Handle) return int
     with Export, Convention => C, External_Name => "modbus_serial_state";

   --  Get last error message
   function Modbus_Serial_Last_Error
     (Handle : C_Serial_Handle) return Interfaces.C.Strings.chars_ptr
     with Export, Convention => C, External_Name => "modbus_serial_last_error";

   ---------------------
   --  RTU Master API
   ---------------------

   --  Opaque handle for RTU master context
   subtype C_RTU_Master_Handle is System.Address;

   --  Create RTU master context
   function Modbus_RTU_Master_Create
     (Serial_Handle : C_Serial_Handle;
      Unit_Id       : unsigned_char;
      Timeout_Ms    : int) return C_RTU_Master_Handle
     with Export, Convention => C, External_Name => "modbus_rtu_master_create";

   --  Destroy RTU master context
   procedure Modbus_RTU_Master_Destroy (Handle : C_RTU_Master_Handle)
     with Export, Convention => C, External_Name => "modbus_rtu_master_destroy";

   --  FC 03: Read Holding Registers (RTU)
   function Modbus_RTU_Read_Holding_Registers
     (Handle        : C_RTU_Master_Handle;
      Slave         : unsigned_char;
      Start_Address : unsigned_short;
      Quantity      : unsigned_short;
      Values        : access unsigned_short;
      Timeout_Ms    : int) return C_Status
     with Export, Convention => C,
          External_Name => "modbus_rtu_read_holding_registers";

   --  FC 04: Read Input Registers (RTU)
   function Modbus_RTU_Read_Input_Registers
     (Handle        : C_RTU_Master_Handle;
      Slave         : unsigned_char;
      Start_Address : unsigned_short;
      Quantity      : unsigned_short;
      Values        : access unsigned_short;
      Timeout_Ms    : int) return C_Status
     with Export, Convention => C,
          External_Name => "modbus_rtu_read_input_registers";

   --  FC 06: Write Single Register (RTU)
   function Modbus_RTU_Write_Single_Register
     (Handle     : C_RTU_Master_Handle;
      Slave      : unsigned_char;
      Address    : unsigned_short;
      Value      : unsigned_short;
      Timeout_Ms : int) return C_Status
     with Export, Convention => C,
          External_Name => "modbus_rtu_write_single_register";

   --  FC 16: Write Multiple Registers (RTU)
   function Modbus_RTU_Write_Multiple_Registers
     (Handle        : C_RTU_Master_Handle;
      Slave         : unsigned_char;
      Start_Address : unsigned_short;
      Quantity      : unsigned_short;
      Values        : access unsigned_short;
      Timeout_Ms    : int) return C_Status
     with Export, Convention => C,
          External_Name => "modbus_rtu_write_multiple_registers";

   --  FC 01: Read Coils (RTU)
   function Modbus_RTU_Read_Coils
     (Handle        : C_RTU_Master_Handle;
      Slave         : unsigned_char;
      Start_Address : unsigned_short;
      Quantity      : unsigned_short;
      Values        : access unsigned_char;
      Timeout_Ms    : int) return C_Status
     with Export, Convention => C,
          External_Name => "modbus_rtu_read_coils";

   --  FC 05: Write Single Coil (RTU)
   function Modbus_RTU_Write_Single_Coil
     (Handle     : C_RTU_Master_Handle;
      Slave      : unsigned_char;
      Address    : unsigned_short;
      Value      : int;
      Timeout_Ms : int) return C_Status
     with Export, Convention => C,
          External_Name => "modbus_rtu_write_single_coil";

end Ada_Modbus.C_API.Serial;
