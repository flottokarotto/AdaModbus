--  Ada_Modbus.Slave - Modbus Slave (Server) implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Callback-based slave implementation.
--  User provides callbacks for each supported function code.
--  Unimplemented callbacks (null) result in Exception Code 01 (Illegal Function).

with Interfaces;
with Ada_Modbus.Protocol;

package Ada_Modbus.Slave is

   pragma Preelaborate;

   --  Callback types for each function code
   --  Return Success or appropriate exception status

   --  Read Coils (FC 01)
   type Read_Coils_Callback is access function
     (Start_Address : Coil_Address;
      Quantity      : Coil_Count;
      Values        : out Coil_Array) return Status;

   --  Read Discrete Inputs (FC 02)
   type Read_Discrete_Inputs_Callback is access function
     (Start_Address : Coil_Address;
      Quantity      : Coil_Count;
      Values        : out Coil_Array) return Status;

   --  Read Holding Registers (FC 03)
   type Read_Holding_Registers_Callback is access function
     (Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array) return Status;

   --  Read Input Registers (FC 04)
   type Read_Input_Registers_Callback is access function
     (Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array) return Status;

   --  Write Single Coil (FC 05)
   type Write_Single_Coil_Callback is access function
     (Address : Coil_Address;
      Value   : Coil_Value) return Status;

   --  Write Single Register (FC 06)
   type Write_Single_Register_Callback is access function
     (Address : Register_Address;
      Value   : Register_Value) return Status;

   --  Write Multiple Coils (FC 15)
   type Write_Multiple_Coils_Callback is access function
     (Start_Address : Coil_Address;
      Values        : Coil_Array) return Status;

   --  Write Multiple Registers (FC 16)
   type Write_Multiple_Registers_Callback is access function
     (Start_Address : Register_Address;
      Values        : Register_Array) return Status;

   --  Read Exception Status (FC 07)
   --  Returns 8-bit exception status
   type Read_Exception_Status_Callback is access function
     (Exception_Status : out Byte) return Status;

   --  Diagnostics (FC 08)
   --  Sub_Function 0x0000 = Return Query Data (echo)
   type Diagnostics_Callback is access function
     (Sub_Function : Interfaces.Unsigned_16;
      Data_In      : Interfaces.Unsigned_16;
      Data_Out     : out Interfaces.Unsigned_16) return Status;

   --  Report Server ID (FC 17)
   --  Returns server identification and run status
   type Report_Server_Id_Callback is access function
     (Server_Id     : out Byte;
      Run_Indicator : out Boolean;
      Add_Data      : out Byte_Array;
      Add_Data_Len  : out Natural) return Status;

   --  Mask Write Register (FC 22)
   --  Result = (Current AND And_Mask) OR (Or_Mask AND (NOT And_Mask))
   type Mask_Write_Register_Callback is access function
     (Address  : Register_Address;
      And_Mask : Register_Value;
      Or_Mask  : Register_Value) return Status;

   --  Read/Write Multiple Registers (FC 23)
   --  Performs write then read in single transaction
   type Read_Write_Registers_Callback is access function
     (Read_Start    : Register_Address;
      Read_Quantity : Register_Count;
      Read_Values   : out Register_Array;
      Write_Start   : Register_Address;
      Write_Values  : Register_Array) return Status;

   --  Callback record - null means function not supported
   type Slave_Callbacks is record
      Read_Coils               : Read_Coils_Callback := null;
      Read_Discrete_Inputs     : Read_Discrete_Inputs_Callback := null;
      Read_Holding_Registers   : Read_Holding_Registers_Callback := null;
      Read_Input_Registers     : Read_Input_Registers_Callback := null;
      Write_Single_Coil        : Write_Single_Coil_Callback := null;
      Write_Single_Register    : Write_Single_Register_Callback := null;
      Write_Multiple_Coils     : Write_Multiple_Coils_Callback := null;
      Write_Multiple_Registers : Write_Multiple_Registers_Callback := null;
      Read_Exception_Status    : Read_Exception_Status_Callback := null;
      Diagnostics              : Diagnostics_Callback := null;
      Report_Server_Id         : Report_Server_Id_Callback := null;
      Mask_Write_Register      : Mask_Write_Register_Callback := null;
      Read_Write_Registers     : Read_Write_Registers_Callback := null;
   end record;

   --  Protocol mode
   type Protocol_Mode is (RTU, ASCII, TCP);

   --  Slave configuration
   type Slave_Config is record
      Mode      : Protocol_Mode := RTU;
      Unit_Id   : Ada_Modbus.Unit_Id := 1;
      Callbacks : Slave_Callbacks;
   end record;

   --  Process a received request and generate response
   --  Returns True if response should be sent, False for broadcast (no response)
   procedure Process_Request
     (Config         : Slave_Config;
      Request_Frame  : Byte_Array;
      Request_Length : Natural;
      Response_Frame : out Byte_Array;
      Response_Length : out Natural;
      Send_Response  : out Boolean);

private

   --  Process PDU and generate response PDU
   procedure Process_PDU
     (Config       : Slave_Config;
      Request_PDU  : Protocol.PDU_Buffer;
      Request_Len  : Natural;
      Response_PDU : out Protocol.PDU_Buffer;
      Response_Len : out Natural);

   --  Build exception response PDU
   procedure Build_Exception_Response
     (FC           : Function_Code;
      Exception_St : Status;
      Response     : out Protocol.PDU_Buffer;
      Response_Len : out Natural);

end Ada_Modbus.Slave;
