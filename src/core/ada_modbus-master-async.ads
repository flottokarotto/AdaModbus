--  Ada_Modbus.Master.Async - Async extensions for Modbus Master
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Non-blocking async API using callbacks and polling.
--  ZFP-compatible: no tasking, no dynamic allocation.
--
--  Usage:
--    1. Call async functions to start requests (returns immediately)
--    2. Call Process_Pending regularly to check for responses
--    3. Callbacks are invoked when responses arrive or timeout occurs

with Interfaces;

generic
   --  Maximum number of concurrent pending requests
   Max_Pending_Requests : Positive := 4;
package Ada_Modbus.Master.Async is

   --  Handle for tracking async requests
   type Request_Handle is new Positive range 1 .. Max_Pending_Requests;

   --  Response status passed to callbacks
   type Response_Status is
     (Response_Success,        --  Response received successfully
      Response_Timeout,        --  No response within timeout
      Response_Error,          --  Communication or frame error
      Response_Exception);     --  Modbus exception received

   --  Convert library Status to Response_Status
   function To_Response_Status (S : Status) return Response_Status;

   --------------------------------
   --  Callback Types            --
   --------------------------------

   --  Callback for register read operations (FC 03, 04)
   type Read_Registers_Callback is access procedure
     (Handle         : Request_Handle;
      Resp_Status    : Response_Status;
      Slave          : Unit_Id;
      Values         : Register_Array;
      Exception_Code : Byte);

   --  Callback for coil/discrete read operations (FC 01, 02)
   type Read_Bits_Callback is access procedure
     (Handle         : Request_Handle;
      Resp_Status    : Response_Status;
      Slave          : Unit_Id;
      Values         : Coil_Array;
      Exception_Code : Byte);

   --  Callback for write operations (FC 05, 06, 15, 16)
   type Write_Callback is access procedure
     (Handle         : Request_Handle;
      Resp_Status    : Response_Status;
      Slave          : Unit_Id;
      Exception_Code : Byte);

   --  Generic completion callback (for all operations)
   type Completion_Callback is access procedure
     (Handle         : Request_Handle;
      Resp_Status    : Response_Status;
      Slave          : Unit_Id;
      Exception_Code : Byte);

   --------------------------------
   --  Async Context             --
   --------------------------------

   --  Async-enabled master context
   type Async_Context is private;

   --  Initialize async context with base master context
   procedure Initialize
     (Ctx    : out Async_Context;
      Master : Master_Context);

   --  Check if a slot is available for new request
   function Has_Free_Slot (Ctx : Async_Context) return Boolean;

   --  Get number of pending requests
   function Pending_Count (Ctx : Async_Context) return Natural;

   --------------------------------
   --  Async Read Functions      --
   --------------------------------

   --  Read Holding Registers Async (FC 03)
   --  Returns True if request was queued, False if no free slot
   function Read_Holding_Registers_Async
     (Ctx           : in out Async_Context;
      Slave         : Unit_Id;
      Start_Address : Register_Address;
      Quantity      : Register_Count;
      On_Response   : Read_Registers_Callback;
      Handle        : out Request_Handle;
      Timeout_Ms    : Natural := 0) return Boolean;

   --  Read Input Registers Async (FC 04)
   function Read_Input_Registers_Async
     (Ctx           : in out Async_Context;
      Slave         : Unit_Id;
      Start_Address : Register_Address;
      Quantity      : Register_Count;
      On_Response   : Read_Registers_Callback;
      Handle        : out Request_Handle;
      Timeout_Ms    : Natural := 0) return Boolean;

   --  Read Coils Async (FC 01)
   function Read_Coils_Async
     (Ctx           : in out Async_Context;
      Slave         : Unit_Id;
      Start_Address : Coil_Address;
      Quantity      : Coil_Count;
      On_Response   : Read_Bits_Callback;
      Handle        : out Request_Handle;
      Timeout_Ms    : Natural := 0) return Boolean;

   --  Read Discrete Inputs Async (FC 02)
   function Read_Discrete_Inputs_Async
     (Ctx           : in out Async_Context;
      Slave         : Unit_Id;
      Start_Address : Coil_Address;
      Quantity      : Coil_Count;
      On_Response   : Read_Bits_Callback;
      Handle        : out Request_Handle;
      Timeout_Ms    : Natural := 0) return Boolean;

   --------------------------------
   --  Async Write Functions     --
   --------------------------------

   --  Write Single Coil Async (FC 05)
   function Write_Single_Coil_Async
     (Ctx        : in out Async_Context;
      Slave      : Unit_Id;
      Address    : Coil_Address;
      Value      : Coil_Value;
      On_Response : Write_Callback;
      Handle     : out Request_Handle;
      Timeout_Ms : Natural := 0) return Boolean;

   --  Write Single Register Async (FC 06)
   function Write_Single_Register_Async
     (Ctx        : in out Async_Context;
      Slave      : Unit_Id;
      Address    : Register_Address;
      Value      : Register_Value;
      On_Response : Write_Callback;
      Handle     : out Request_Handle;
      Timeout_Ms : Natural := 0) return Boolean;

   --  Write Multiple Coils Async (FC 15)
   function Write_Multiple_Coils_Async
     (Ctx           : in out Async_Context;
      Slave         : Unit_Id;
      Start_Address : Coil_Address;
      Values        : Coil_Array;
      On_Response   : Write_Callback;
      Handle        : out Request_Handle;
      Timeout_Ms    : Natural := 0) return Boolean;

   --  Write Multiple Registers Async (FC 16)
   function Write_Multiple_Registers_Async
     (Ctx           : in out Async_Context;
      Slave         : Unit_Id;
      Start_Address : Register_Address;
      Values        : Register_Array;
      On_Response   : Write_Callback;
      Handle        : out Request_Handle;
      Timeout_Ms    : Natural := 0) return Boolean;

   --------------------------------
   --  Polling                   --
   --------------------------------

   --  Process pending requests - check for responses and invoke callbacks
   --  Call this regularly in your main loop
   procedure Process_Pending (Ctx : in out Async_Context);

   --  Cancel a pending request
   procedure Cancel_Request
     (Ctx    : in out Async_Context;
      Handle : Request_Handle);

   --  Cancel all pending requests
   procedure Cancel_All (Ctx : in out Async_Context);

private

   use type Interfaces.Unsigned_32;

   --  Request types
   type Request_Type is
     (None,
      Read_Holding_Regs,
      Read_Input_Regs,
      Read_Coils_Type,
      Read_Discrete_Type,
      Write_Single_Coil_Type,
      Write_Single_Reg,
      Write_Multi_Coils,
      Write_Multi_Regs);

   --  Pending request state
   type Pending_State is (Empty, Awaiting_Response);

   --  Single pending request entry
   type Pending_Request is record
      State            : Pending_State := Empty;
      Req_Type         : Request_Type := None;
      Slave            : Unit_Id := 0;
      Start_Time_Ms    : Interfaces.Unsigned_32 := 0;
      Timeout_Ms       : Natural := 0;
      --  Callbacks (only one is used per request)
      On_Registers     : Read_Registers_Callback := null;
      On_Bits          : Read_Bits_Callback := null;
      On_Write         : Write_Callback := null;
      --  Request parameters for response validation
      Expected_Qty     : Natural := 0;
   end record;

   type Pending_Array is array (Request_Handle) of Pending_Request;

   --  Receive buffer for responses
   subtype Receive_Buffer is Byte_Array (0 .. 259);

   --  Async context
   type Async_Context is record
      Master           : Master_Context;
      Pending          : Pending_Array;
      Recv_Buffer      : Receive_Buffer := [others => 0];
      Recv_Index       : Natural := 0;  --  For partial receives
   end record;

end Ada_Modbus.Master.Async;
