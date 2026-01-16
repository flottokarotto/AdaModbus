--  Ada_Modbus.Master - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with Ada_Modbus.Protocol.RTU;
with Ada_Modbus.Protocol.ASCII;

package body Ada_Modbus.Master is

   use type Interfaces.Unsigned_32;
   use type Protocol.TCP.Transaction_Id;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Ctx       : out Master_Context;
      Config    : Master_Config;
      Transport : Transport_Context)
   is
   begin
      Ctx.Config := Config;
      Ctx.Transaction_Id := 0;
      Ctx.Transport := Transport;
   end Initialize;

   ---------------------------
   -- Execute_Transaction --
   ---------------------------

   function Execute_Transaction
     (Ctx        : in out Master_Context;
      Slave      : Unit_Id;
      Request    : Protocol.PDU_Buffer;
      Req_Length : Natural;
      Response   : out Protocol.PDU_Buffer;
      Resp_Len   : out Natural;
      Timeout_Ms : Natural) return Status
   is
      Actual_Timeout : constant Natural :=
        (if Timeout_Ms > 0 then Timeout_Ms else Ctx.Config.Default_Timeout);
      Start_Time     : constant Interfaces.Unsigned_32 := Get_Tick_Ms;
      Bytes_Sent     : Natural;
      Bytes_Received : Natural;
      Result         : Status;
   begin
      Response := [others => 0];
      Resp_Len := 0;

      case Ctx.Config.Mode is
         when RTU =>
            declare
               ADU : Protocol.RTU.ADU_Buffer;
               ADU_Len : Natural;
               Recv_Buf : Protocol.RTU.ADU_Buffer;
               Recv_Slave : Unit_Id;
            begin
               --  Build RTU frame
               Protocol.RTU.Build_Frame (ADU, ADU_Len, Slave, Request, Req_Length);

               --  Send frame
               Bytes_Sent := Send (Ctx.Transport, ADU (0 .. ADU_Len - 1));
               if Bytes_Sent /= ADU_Len then
                  return Frame_Error;
               end if;

               --  Skip response for broadcast
               if Slave = Broadcast_Address then
                  return Success;
               end if;

               --  Receive response
               --  For RTU, we need to receive until silence (inter-frame gap)
               --  Simplified: receive with timeout and verify CRC
               Bytes_Received := Receive (Ctx.Transport, Recv_Buf, Protocol.RTU.Max_ADU_Size, Actual_Timeout);

               if Bytes_Received = 0 then
                  return Timeout;
               end if;

               --  Parse RTU frame
               Protocol.RTU.Parse_Frame
                 (Recv_Buf, Bytes_Received, Recv_Slave, Response, Resp_Len, Result);

               if Result /= Success then
                  return Result;
               end if;

               --  Verify slave address matches
               if Recv_Slave /= Slave then
                  return Invalid_Response;
               end if;

               return Success;
            end;

         when ASCII =>
            declare
               Frame : Protocol.ASCII.Frame_Buffer;
               Frame_Len : Natural;
               Recv_Buf : Protocol.ASCII.Frame_Buffer;
               Recv_Slave : Unit_Id;
            begin
               --  Build ASCII frame
               Protocol.ASCII.Build_Frame (Frame, Frame_Len, Slave, Request, Req_Length);

               --  Send frame
               Bytes_Sent := Send (Ctx.Transport, Frame (0 .. Frame_Len - 1));
               if Bytes_Sent /= Frame_Len then
                  return Frame_Error;
               end if;

               --  Skip response for broadcast
               if Slave = Broadcast_Address then
                  return Success;
               end if;

               --  Receive response (until CRLF)
               Bytes_Received := Receive (Ctx.Transport, Recv_Buf, Protocol.ASCII.Max_Frame_Size, Actual_Timeout);

               if Bytes_Received = 0 then
                  return Timeout;
               end if;

               --  Parse ASCII frame
               Protocol.ASCII.Parse_Frame
                 (Recv_Buf, Bytes_Received, Recv_Slave, Response, Resp_Len, Result);

               if Result /= Success then
                  return Result;
               end if;

               --  Verify slave address matches
               if Recv_Slave /= Slave then
                  return Invalid_Response;
               end if;

               return Success;
            end;

         when TCP =>
            declare
               ADU : Protocol.TCP.ADU_Buffer;
               ADU_Len : Natural;
               Recv_Buf : Protocol.TCP.ADU_Buffer;
               Recv_Trans : Protocol.TCP.Transaction_Id;
               Recv_Unit : Unit_Id;
            begin
               --  Increment transaction ID
               Ctx.Transaction_Id := Ctx.Transaction_Id + 1;

               --  Build TCP frame
               Protocol.TCP.Build_Frame
                 (ADU, ADU_Len, Ctx.Transaction_Id, Slave, Request, Req_Length);

               --  Send frame
               Bytes_Sent := Send (Ctx.Transport, ADU (0 .. ADU_Len - 1));
               if Bytes_Sent /= ADU_Len then
                  return Frame_Error;
               end if;

               --  Skip response for broadcast (Unit ID 0 in TCP)
               if Slave = Broadcast_Address then
                  return Success;
               end if;

               --  Receive response
               --  First receive MBAP header (7 bytes) to get length
               Bytes_Received := Receive (Ctx.Transport, Recv_Buf, 7, Actual_Timeout);

               if Bytes_Received < 7 then
                  return Timeout;
               end if;

               --  Get expected total length and receive remaining
               declare
                  Expected_Len : constant Natural := Protocol.TCP.Get_Expected_Length (Recv_Buf);
                  Remaining : constant Natural := Expected_Len - 7;
               begin
                  if Remaining > 0 then
                     declare
                        More_Buf : Byte_Array (0 .. Remaining - 1);
                        More_Received : Natural;
                        Elapsed : Interfaces.Unsigned_32;
                        Remaining_Timeout : Natural;
                     begin
                        Elapsed := Get_Tick_Ms - Start_Time;
                        if Elapsed >= Interfaces.Unsigned_32 (Actual_Timeout) then
                           return Timeout;
                        end if;
                        Remaining_Timeout := Actual_Timeout - Natural (Elapsed);

                        More_Received := Receive (Ctx.Transport, More_Buf, Remaining, Remaining_Timeout);
                        if More_Received /= Remaining then
                           return Timeout;
                        end if;

                        --  Append to receive buffer
                        for I in 0 .. Remaining - 1 loop
                           Recv_Buf (7 + I) := More_Buf (I);
                        end loop;
                        Bytes_Received := Expected_Len;
                     end;
                  end if;
               end;

               --  Parse TCP frame
               Protocol.TCP.Parse_Frame
                 (Recv_Buf, Bytes_Received, Recv_Trans, Recv_Unit, Response, Resp_Len, Result);

               if Result /= Success then
                  return Result;
               end if;

               --  Verify transaction ID matches
               if Recv_Trans /= Ctx.Transaction_Id then
                  return Invalid_Response;
               end if;

               return Success;
            end;
      end case;
   end Execute_Transaction;

   ----------------
   -- Read_Coils --
   ----------------

   function Read_Coils
     (Ctx           : in out Master_Context;
      Slave         : Unit_Id;
      Start_Address : Coil_Address;
      Quantity      : Coil_Count;
      Values        : out Coil_Array;
      Timeout_Ms    : Natural := 0) return Status
   is
      Request  : Protocol.PDU_Buffer;
      Req_Len  : Natural;
      Response : Protocol.PDU_Buffer;
      Resp_Len : Natural;
      Count    : Natural;
      Result   : Status;
   begin
      Values := [others => False];

      --  Build request PDU
      Protocol.Encode_Read_Bits_Request
        (Request, Req_Len, FC_Read_Coils, Start_Address, Quantity);

      --  Execute transaction
      Result := Execute_Transaction
        (Ctx, Slave, Request, Req_Len, Response, Resp_Len, Timeout_Ms);

      if Result /= Success then
         return Result;
      end if;

      --  Decode response
      Protocol.Decode_Read_Bits_Response (Response, Resp_Len, Values, Count, Result);

      return Result;
   end Read_Coils;

   ---------------------------
   -- Read_Discrete_Inputs --
   ---------------------------

   function Read_Discrete_Inputs
     (Ctx           : in out Master_Context;
      Slave         : Unit_Id;
      Start_Address : Coil_Address;
      Quantity      : Coil_Count;
      Values        : out Coil_Array;
      Timeout_Ms    : Natural := 0) return Status
   is
      Request  : Protocol.PDU_Buffer;
      Req_Len  : Natural;
      Response : Protocol.PDU_Buffer;
      Resp_Len : Natural;
      Count    : Natural;
      Result   : Status;
   begin
      Values := [others => False];

      Protocol.Encode_Read_Bits_Request
        (Request, Req_Len, FC_Read_Discrete_Inputs, Start_Address, Quantity);

      Result := Execute_Transaction
        (Ctx, Slave, Request, Req_Len, Response, Resp_Len, Timeout_Ms);

      if Result /= Success then
         return Result;
      end if;

      Protocol.Decode_Read_Bits_Response (Response, Resp_Len, Values, Count, Result);

      return Result;
   end Read_Discrete_Inputs;

   ----------------------------
   -- Read_Holding_Registers --
   ----------------------------

   function Read_Holding_Registers
     (Ctx           : in out Master_Context;
      Slave         : Unit_Id;
      Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array;
      Timeout_Ms    : Natural := 0) return Status
   is
      Request  : Protocol.PDU_Buffer;
      Req_Len  : Natural;
      Response : Protocol.PDU_Buffer;
      Resp_Len : Natural;
      Count    : Natural;
      Result   : Status;
   begin
      Values := [others => 0];

      Protocol.Encode_Read_Registers_Request
        (Request, Req_Len, FC_Read_Holding_Registers, Start_Address, Quantity);

      Result := Execute_Transaction
        (Ctx, Slave, Request, Req_Len, Response, Resp_Len, Timeout_Ms);

      if Result /= Success then
         return Result;
      end if;

      Protocol.Decode_Read_Registers_Response (Response, Resp_Len, Values, Count, Result);

      return Result;
   end Read_Holding_Registers;

   --------------------------
   -- Read_Input_Registers --
   --------------------------

   function Read_Input_Registers
     (Ctx           : in out Master_Context;
      Slave         : Unit_Id;
      Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array;
      Timeout_Ms    : Natural := 0) return Status
   is
      Request  : Protocol.PDU_Buffer;
      Req_Len  : Natural;
      Response : Protocol.PDU_Buffer;
      Resp_Len : Natural;
      Count    : Natural;
      Result   : Status;
   begin
      Values := [others => 0];

      Protocol.Encode_Read_Registers_Request
        (Request, Req_Len, FC_Read_Input_Registers, Start_Address, Quantity);

      Result := Execute_Transaction
        (Ctx, Slave, Request, Req_Len, Response, Resp_Len, Timeout_Ms);

      if Result /= Success then
         return Result;
      end if;

      Protocol.Decode_Read_Registers_Response (Response, Resp_Len, Values, Count, Result);

      return Result;
   end Read_Input_Registers;

   -----------------------
   -- Write_Single_Coil --
   -----------------------

   function Write_Single_Coil
     (Ctx        : in out Master_Context;
      Slave      : Unit_Id;
      Address    : Coil_Address;
      Value      : Coil_Value;
      Timeout_Ms : Natural := 0) return Status
   is
      Request  : Protocol.PDU_Buffer;
      Req_Len  : Natural;
      Response : Protocol.PDU_Buffer;
      Resp_Len : Natural;
      Resp_Addr : Register_Address;
      Resp_Val : Register_Value;
      Result   : Status;
   begin
      Protocol.Encode_Write_Single_Coil_Request (Request, Req_Len, Address, Value);

      Result := Execute_Transaction
        (Ctx, Slave, Request, Req_Len, Response, Resp_Len, Timeout_Ms);

      if Result /= Success then
         return Result;
      end if;

      --  For broadcast, no response expected
      if Slave = Broadcast_Address then
         return Success;
      end if;

      Protocol.Decode_Write_Single_Response (Response, Resp_Len, Resp_Addr, Resp_Val, Result);

      return Result;
   end Write_Single_Coil;

   ---------------------------
   -- Write_Single_Register --
   ---------------------------

   function Write_Single_Register
     (Ctx        : in out Master_Context;
      Slave      : Unit_Id;
      Address    : Register_Address;
      Value      : Register_Value;
      Timeout_Ms : Natural := 0) return Status
   is
      Request  : Protocol.PDU_Buffer;
      Req_Len  : Natural;
      Response : Protocol.PDU_Buffer;
      Resp_Len : Natural;
      Resp_Addr : Register_Address;
      Resp_Val : Register_Value;
      Result   : Status;
   begin
      Protocol.Encode_Write_Single_Register_Request (Request, Req_Len, Address, Value);

      Result := Execute_Transaction
        (Ctx, Slave, Request, Req_Len, Response, Resp_Len, Timeout_Ms);

      if Result /= Success then
         return Result;
      end if;

      if Slave = Broadcast_Address then
         return Success;
      end if;

      Protocol.Decode_Write_Single_Response (Response, Resp_Len, Resp_Addr, Resp_Val, Result);

      return Result;
   end Write_Single_Register;

   --------------------------
   -- Write_Multiple_Coils --
   --------------------------

   function Write_Multiple_Coils
     (Ctx           : in out Master_Context;
      Slave         : Unit_Id;
      Start_Address : Coil_Address;
      Values        : Coil_Array;
      Timeout_Ms    : Natural := 0) return Status
   is
      Request  : Protocol.PDU_Buffer;
      Req_Len  : Natural;
      Response : Protocol.PDU_Buffer;
      Resp_Len : Natural;
      Resp_Addr : Register_Address;
      Resp_Qty : Natural;
      Result   : Status;
   begin
      Protocol.Encode_Write_Multiple_Coils_Request (Request, Req_Len, Start_Address, Values);

      Result := Execute_Transaction
        (Ctx, Slave, Request, Req_Len, Response, Resp_Len, Timeout_Ms);

      if Result /= Success then
         return Result;
      end if;

      if Slave = Broadcast_Address then
         return Success;
      end if;

      Protocol.Decode_Write_Multiple_Response (Response, Resp_Len, Resp_Addr, Resp_Qty, Result);

      return Result;
   end Write_Multiple_Coils;

   ------------------------------
   -- Write_Multiple_Registers --
   ------------------------------

   function Write_Multiple_Registers
     (Ctx           : in out Master_Context;
      Slave         : Unit_Id;
      Start_Address : Register_Address;
      Values        : Register_Array;
      Timeout_Ms    : Natural := 0) return Status
   is
      Request  : Protocol.PDU_Buffer;
      Req_Len  : Natural;
      Response : Protocol.PDU_Buffer;
      Resp_Len : Natural;
      Resp_Addr : Register_Address;
      Resp_Qty : Natural;
      Result   : Status;
   begin
      Protocol.Encode_Write_Multiple_Registers_Request (Request, Req_Len, Start_Address, Values);

      Result := Execute_Transaction
        (Ctx, Slave, Request, Req_Len, Response, Resp_Len, Timeout_Ms);

      if Result /= Success then
         return Result;
      end if;

      if Slave = Broadcast_Address then
         return Success;
      end if;

      Protocol.Decode_Write_Multiple_Response (Response, Resp_Len, Resp_Addr, Resp_Qty, Result);

      return Result;
   end Write_Multiple_Registers;

   ---------------------------
   -- Read_Exception_Status --
   ---------------------------

   function Read_Exception_Status
     (Ctx              : in out Master_Context;
      Slave            : Unit_Id;
      Exception_Status : out Byte;
      Timeout_Ms       : Natural := 0) return Status
   is
      Request  : Protocol.PDU_Buffer;
      Req_Len  : Natural;
      Response : Protocol.PDU_Buffer;
      Resp_Len : Natural;
      Result   : Status;
   begin
      Exception_Status := 0;

      Protocol.Encode_Read_Exception_Status_Request (Request, Req_Len);

      Result := Execute_Transaction
        (Ctx, Slave, Request, Req_Len, Response, Resp_Len, Timeout_Ms);

      if Result /= Success then
         return Result;
      end if;

      Protocol.Decode_Read_Exception_Status_Response
        (Response, Resp_Len, Exception_Status, Result);

      return Result;
   end Read_Exception_Status;

   -----------------
   -- Diagnostics --
   -----------------

   function Diagnostics
     (Ctx          : in out Master_Context;
      Slave        : Unit_Id;
      Sub_Function : Interfaces.Unsigned_16;
      Data_In      : Interfaces.Unsigned_16;
      Data_Out     : out Interfaces.Unsigned_16;
      Timeout_Ms   : Natural := 0) return Status
   is
      Request  : Protocol.PDU_Buffer;
      Req_Len  : Natural;
      Response : Protocol.PDU_Buffer;
      Resp_Len : Natural;
      Resp_Sub : Interfaces.Unsigned_16;
      Result   : Status;
   begin
      Data_Out := 0;

      Protocol.Encode_Diagnostics_Request (Request, Req_Len, Sub_Function, Data_In);

      Result := Execute_Transaction
        (Ctx, Slave, Request, Req_Len, Response, Resp_Len, Timeout_Ms);

      if Result /= Success then
         return Result;
      end if;

      Protocol.Decode_Diagnostics_Response (Response, Resp_Len, Resp_Sub, Data_Out, Result);

      return Result;
   end Diagnostics;

   ----------------------
   -- Report_Server_Id --
   ----------------------

   function Report_Server_Id
     (Ctx           : in out Master_Context;
      Slave         : Unit_Id;
      Server_Id     : out Byte;
      Run_Indicator : out Boolean;
      Add_Data      : out Byte_Array;
      Add_Data_Len  : out Natural;
      Timeout_Ms    : Natural := 0) return Status
   is
      Request  : Protocol.PDU_Buffer;
      Req_Len  : Natural;
      Response : Protocol.PDU_Buffer;
      Resp_Len : Natural;
      Result   : Status;
   begin
      Server_Id := 0;
      Run_Indicator := False;
      Add_Data := [others => 0];
      Add_Data_Len := 0;

      Protocol.Encode_Report_Server_Id_Request (Request, Req_Len);

      Result := Execute_Transaction
        (Ctx, Slave, Request, Req_Len, Response, Resp_Len, Timeout_Ms);

      if Result /= Success then
         return Result;
      end if;

      Protocol.Decode_Report_Server_Id_Response
        (Response, Resp_Len, Server_Id, Run_Indicator, Add_Data, Add_Data_Len, Result);

      return Result;
   end Report_Server_Id;

   -------------------------
   -- Mask_Write_Register --
   -------------------------

   function Mask_Write_Register
     (Ctx        : in out Master_Context;
      Slave      : Unit_Id;
      Address    : Register_Address;
      And_Mask   : Register_Value;
      Or_Mask    : Register_Value;
      Timeout_Ms : Natural := 0) return Status
   is
      Request  : Protocol.PDU_Buffer;
      Req_Len  : Natural;
      Response : Protocol.PDU_Buffer;
      Resp_Len : Natural;
      Resp_Addr : Register_Address;
      Resp_And : Register_Value;
      Resp_Or : Register_Value;
      Result   : Status;
   begin
      Protocol.Encode_Mask_Write_Register_Request
        (Request, Req_Len, Address, And_Mask, Or_Mask);

      Result := Execute_Transaction
        (Ctx, Slave, Request, Req_Len, Response, Resp_Len, Timeout_Ms);

      if Result /= Success then
         return Result;
      end if;

      if Slave = Broadcast_Address then
         return Success;
      end if;

      Protocol.Decode_Mask_Write_Register_Response
        (Response, Resp_Len, Resp_Addr, Resp_And, Resp_Or, Result);

      return Result;
   end Mask_Write_Register;

   ----------------------------------
   -- Read_Write_Multiple_Registers --
   ----------------------------------

   function Read_Write_Multiple_Registers
     (Ctx            : in out Master_Context;
      Slave          : Unit_Id;
      Read_Start     : Register_Address;
      Read_Quantity  : Register_Count;
      Read_Values    : out Register_Array;
      Write_Start    : Register_Address;
      Write_Values   : Register_Array;
      Timeout_Ms     : Natural := 0) return Status
   is
      Request  : Protocol.PDU_Buffer;
      Req_Len  : Natural;
      Response : Protocol.PDU_Buffer;
      Resp_Len : Natural;
      Count    : Natural;
      Result   : Status;
   begin
      Read_Values := [others => 0];

      Protocol.Encode_Read_Write_Registers_Request
        (Request, Req_Len, Read_Start, Read_Quantity, Write_Start, Write_Values);

      Result := Execute_Transaction
        (Ctx, Slave, Request, Req_Len, Response, Resp_Len, Timeout_Ms);

      if Result /= Success then
         return Result;
      end if;

      Protocol.Decode_Read_Write_Registers_Response
        (Response, Resp_Len, Read_Values, Count, Result);

      return Result;
   end Read_Write_Multiple_Registers;

end Ada_Modbus.Master;
