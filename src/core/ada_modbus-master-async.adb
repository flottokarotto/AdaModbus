--  Ada_Modbus.Master.Async - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with Ada_Modbus.Protocol;
with Ada_Modbus.Protocol.RTU;
with Ada_Modbus.Protocol.ASCII;
with Ada_Modbus.Protocol.TCP;

package body Ada_Modbus.Master.Async is

   use type Protocol.TCP.Transaction_Id;

   --  Forward declarations
   procedure Process_Response
     (Ctx      : in out Async_Context;
      Handle   : Request_Handle;
      PDU      : Protocol.PDU_Buffer;
      PDU_Len  : Natural;
      Result   : in out Status);

   ------------------------
   -- To_Response_Status --
   ------------------------

   function To_Response_Status (S : Status) return Response_Status is
   begin
      case S is
         when Success =>
            return Response_Success;
         when Timeout =>
            return Response_Timeout;
         when Exception_Illegal_Function |
              Exception_Illegal_Address |
              Exception_Illegal_Value |
              Exception_Slave_Failure |
              Exception_Acknowledge |
              Exception_Slave_Busy |
              Exception_Gateway_Path |
              Exception_Gateway_Target =>
            return Response_Exception;
         when others =>
            return Response_Error;
      end case;
   end To_Response_Status;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Ctx    : out Async_Context;
      Master : Master_Context)
   is
   begin
      Ctx.Master := Master;
      Ctx.Pending := [others => (State => Empty, others => <>)];
      Ctx.Recv_Buffer := [others => 0];
      Ctx.Recv_Index := 0;
   end Initialize;

   -------------------
   -- Has_Free_Slot --
   -------------------

   function Has_Free_Slot (Ctx : Async_Context) return Boolean is
   begin
      for I in Ctx.Pending'Range loop
         if Ctx.Pending (I).State = Empty then
            return True;
         end if;
      end loop;
      return False;
   end Has_Free_Slot;

   -------------------
   -- Pending_Count --
   -------------------

   function Pending_Count (Ctx : Async_Context) return Natural is
      Count : Natural := 0;
   begin
      for I in Ctx.Pending'Range loop
         if Ctx.Pending (I).State /= Empty then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Pending_Count;

   ----------------------
   -- Find_Free_Slot   --
   ----------------------

   function Find_Free_Slot (Ctx : Async_Context) return Request_Handle is
   begin
      for I in Ctx.Pending'Range loop
         if Ctx.Pending (I).State = Empty then
            return I;
         end if;
      end loop;
      --  No free slot - return first (caller should check Has_Free_Slot)
      return Ctx.Pending'First;
   end Find_Free_Slot;

   --------------------------------
   -- Send_Request (internal)   --
   --------------------------------

   function Send_Request
     (Ctx     : in out Async_Context;
      Slave   : Unit_Id;
      Request : Protocol.PDU_Buffer;
      Req_Len : Natural) return Boolean
   is
      Bytes_Sent : Natural;
   begin
      case Ctx.Master.Config.Mode is
         when RTU =>
            declare
               ADU : Protocol.RTU.ADU_Buffer;
               ADU_Len : Natural;
            begin
               Protocol.RTU.Build_Frame (ADU, ADU_Len, Slave, Request, Req_Len);
               Bytes_Sent := Send (Ctx.Master.Transport, ADU (0 .. ADU_Len - 1));
               return Bytes_Sent = ADU_Len;
            end;

         when ASCII =>
            declare
               Frame : Protocol.ASCII.Frame_Buffer;
               Frame_Len : Natural;
            begin
               Protocol.ASCII.Build_Frame (Frame, Frame_Len, Slave, Request, Req_Len);
               Bytes_Sent := Send (Ctx.Master.Transport, Frame (0 .. Frame_Len - 1));
               return Bytes_Sent = Frame_Len;
            end;

         when TCP =>
            declare
               ADU : Protocol.TCP.ADU_Buffer;
               ADU_Len : Natural;
            begin
               Ctx.Master.Transaction_Id := Ctx.Master.Transaction_Id + 1;
               Protocol.TCP.Build_Frame
                 (ADU, ADU_Len, Ctx.Master.Transaction_Id, Slave, Request, Req_Len);
               Bytes_Sent := Send (Ctx.Master.Transport, ADU (0 .. ADU_Len - 1));
               return Bytes_Sent = ADU_Len;
            end;
      end case;
   end Send_Request;

   ------------------------------------
   -- Read_Holding_Registers_Async   --
   ------------------------------------

   function Read_Holding_Registers_Async
     (Ctx           : in out Async_Context;
      Slave         : Unit_Id;
      Start_Address : Register_Address;
      Quantity      : Register_Count;
      On_Response   : Read_Registers_Callback;
      Handle        : out Request_Handle;
      Timeout_Ms    : Natural := 0) return Boolean
   is
      Request : Protocol.PDU_Buffer;
      Req_Len : Natural;
      Actual_Timeout : constant Natural :=
        (if Timeout_Ms > 0 then Timeout_Ms else Ctx.Master.Config.Default_Timeout);
   begin
      Handle := Request_Handle'First;

      if not Has_Free_Slot (Ctx) then
         return False;
      end if;

      Handle := Find_Free_Slot (Ctx);

      --  Build request PDU
      Protocol.Encode_Read_Registers_Request
        (Request, Req_Len, FC_Read_Holding_Registers, Start_Address, Quantity);

      --  Send request
      if not Send_Request (Ctx, Slave, Request, Req_Len) then
         return False;
      end if;

      --  Store pending state
      Ctx.Pending (Handle) := (
         State         => Awaiting_Response,
         Req_Type      => Read_Holding_Regs,
         Slave         => Slave,
         Start_Time_Ms => Get_Tick_Ms,
         Timeout_Ms    => Actual_Timeout,
         On_Registers  => On_Response,
         On_Bits       => null,
         On_Write      => null,
         Expected_Qty  => Natural (Quantity)
      );

      return True;
   end Read_Holding_Registers_Async;

   ----------------------------------
   -- Read_Input_Registers_Async   --
   ----------------------------------

   function Read_Input_Registers_Async
     (Ctx           : in out Async_Context;
      Slave         : Unit_Id;
      Start_Address : Register_Address;
      Quantity      : Register_Count;
      On_Response   : Read_Registers_Callback;
      Handle        : out Request_Handle;
      Timeout_Ms    : Natural := 0) return Boolean
   is
      Request : Protocol.PDU_Buffer;
      Req_Len : Natural;
      Actual_Timeout : constant Natural :=
        (if Timeout_Ms > 0 then Timeout_Ms else Ctx.Master.Config.Default_Timeout);
   begin
      Handle := Request_Handle'First;

      if not Has_Free_Slot (Ctx) then
         return False;
      end if;

      Handle := Find_Free_Slot (Ctx);

      Protocol.Encode_Read_Registers_Request
        (Request, Req_Len, FC_Read_Input_Registers, Start_Address, Quantity);

      if not Send_Request (Ctx, Slave, Request, Req_Len) then
         return False;
      end if;

      Ctx.Pending (Handle) := (
         State         => Awaiting_Response,
         Req_Type      => Read_Input_Regs,
         Slave         => Slave,
         Start_Time_Ms => Get_Tick_Ms,
         Timeout_Ms    => Actual_Timeout,
         On_Registers  => On_Response,
         On_Bits       => null,
         On_Write      => null,
         Expected_Qty  => Natural (Quantity)
      );

      return True;
   end Read_Input_Registers_Async;

   -----------------------
   -- Read_Coils_Async --
   -----------------------

   function Read_Coils_Async
     (Ctx           : in out Async_Context;
      Slave         : Unit_Id;
      Start_Address : Coil_Address;
      Quantity      : Coil_Count;
      On_Response   : Read_Bits_Callback;
      Handle        : out Request_Handle;
      Timeout_Ms    : Natural := 0) return Boolean
   is
      Request : Protocol.PDU_Buffer;
      Req_Len : Natural;
      Actual_Timeout : constant Natural :=
        (if Timeout_Ms > 0 then Timeout_Ms else Ctx.Master.Config.Default_Timeout);
   begin
      Handle := Request_Handle'First;

      if not Has_Free_Slot (Ctx) then
         return False;
      end if;

      Handle := Find_Free_Slot (Ctx);

      Protocol.Encode_Read_Bits_Request
        (Request, Req_Len, FC_Read_Coils, Start_Address, Quantity);

      if not Send_Request (Ctx, Slave, Request, Req_Len) then
         return False;
      end if;

      Ctx.Pending (Handle) := (
         State         => Awaiting_Response,
         Req_Type      => Read_Coils_Type,
         Slave         => Slave,
         Start_Time_Ms => Get_Tick_Ms,
         Timeout_Ms    => Actual_Timeout,
         On_Registers  => null,
         On_Bits       => On_Response,
         On_Write      => null,
         Expected_Qty  => Natural (Quantity)
      );

      return True;
   end Read_Coils_Async;

   ---------------------------------
   -- Read_Discrete_Inputs_Async --
   ---------------------------------

   function Read_Discrete_Inputs_Async
     (Ctx           : in out Async_Context;
      Slave         : Unit_Id;
      Start_Address : Coil_Address;
      Quantity      : Coil_Count;
      On_Response   : Read_Bits_Callback;
      Handle        : out Request_Handle;
      Timeout_Ms    : Natural := 0) return Boolean
   is
      Request : Protocol.PDU_Buffer;
      Req_Len : Natural;
      Actual_Timeout : constant Natural :=
        (if Timeout_Ms > 0 then Timeout_Ms else Ctx.Master.Config.Default_Timeout);
   begin
      Handle := Request_Handle'First;

      if not Has_Free_Slot (Ctx) then
         return False;
      end if;

      Handle := Find_Free_Slot (Ctx);

      Protocol.Encode_Read_Bits_Request
        (Request, Req_Len, FC_Read_Discrete_Inputs, Start_Address, Quantity);

      if not Send_Request (Ctx, Slave, Request, Req_Len) then
         return False;
      end if;

      Ctx.Pending (Handle) := (
         State         => Awaiting_Response,
         Req_Type      => Read_Discrete_Type,
         Slave         => Slave,
         Start_Time_Ms => Get_Tick_Ms,
         Timeout_Ms    => Actual_Timeout,
         On_Registers  => null,
         On_Bits       => On_Response,
         On_Write      => null,
         Expected_Qty  => Natural (Quantity)
      );

      return True;
   end Read_Discrete_Inputs_Async;

   ------------------------------
   -- Write_Single_Coil_Async --
   ------------------------------

   function Write_Single_Coil_Async
     (Ctx        : in out Async_Context;
      Slave      : Unit_Id;
      Address    : Coil_Address;
      Value      : Coil_Value;
      On_Response : Write_Callback;
      Handle     : out Request_Handle;
      Timeout_Ms : Natural := 0) return Boolean
   is
      Request : Protocol.PDU_Buffer;
      Req_Len : Natural;
      Actual_Timeout : constant Natural :=
        (if Timeout_Ms > 0 then Timeout_Ms else Ctx.Master.Config.Default_Timeout);
   begin
      Handle := Request_Handle'First;

      if not Has_Free_Slot (Ctx) then
         return False;
      end if;

      Handle := Find_Free_Slot (Ctx);

      Protocol.Encode_Write_Single_Coil_Request (Request, Req_Len, Address, Value);

      if not Send_Request (Ctx, Slave, Request, Req_Len) then
         return False;
      end if;

      Ctx.Pending (Handle) := (
         State         => Awaiting_Response,
         Req_Type      => Write_Single_Coil_Type,
         Slave         => Slave,
         Start_Time_Ms => Get_Tick_Ms,
         Timeout_Ms    => Actual_Timeout,
         On_Registers  => null,
         On_Bits       => null,
         On_Write      => On_Response,
         Expected_Qty  => 0
      );

      return True;
   end Write_Single_Coil_Async;

   ----------------------------------
   -- Write_Single_Register_Async --
   ----------------------------------

   function Write_Single_Register_Async
     (Ctx        : in out Async_Context;
      Slave      : Unit_Id;
      Address    : Register_Address;
      Value      : Register_Value;
      On_Response : Write_Callback;
      Handle     : out Request_Handle;
      Timeout_Ms : Natural := 0) return Boolean
   is
      Request : Protocol.PDU_Buffer;
      Req_Len : Natural;
      Actual_Timeout : constant Natural :=
        (if Timeout_Ms > 0 then Timeout_Ms else Ctx.Master.Config.Default_Timeout);
   begin
      Handle := Request_Handle'First;

      if not Has_Free_Slot (Ctx) then
         return False;
      end if;

      Handle := Find_Free_Slot (Ctx);

      Protocol.Encode_Write_Single_Register_Request (Request, Req_Len, Address, Value);

      if not Send_Request (Ctx, Slave, Request, Req_Len) then
         return False;
      end if;

      Ctx.Pending (Handle) := (
         State         => Awaiting_Response,
         Req_Type      => Write_Single_Reg,
         Slave         => Slave,
         Start_Time_Ms => Get_Tick_Ms,
         Timeout_Ms    => Actual_Timeout,
         On_Registers  => null,
         On_Bits       => null,
         On_Write      => On_Response,
         Expected_Qty  => 0
      );

      return True;
   end Write_Single_Register_Async;

   --------------------------------
   -- Write_Multiple_Coils_Async --
   --------------------------------

   function Write_Multiple_Coils_Async
     (Ctx           : in out Async_Context;
      Slave         : Unit_Id;
      Start_Address : Coil_Address;
      Values        : Coil_Array;
      On_Response   : Write_Callback;
      Handle        : out Request_Handle;
      Timeout_Ms    : Natural := 0) return Boolean
   is
      Request : Protocol.PDU_Buffer;
      Req_Len : Natural;
      Actual_Timeout : constant Natural :=
        (if Timeout_Ms > 0 then Timeout_Ms else Ctx.Master.Config.Default_Timeout);
   begin
      Handle := Request_Handle'First;

      if not Has_Free_Slot (Ctx) then
         return False;
      end if;

      Handle := Find_Free_Slot (Ctx);

      Protocol.Encode_Write_Multiple_Coils_Request (Request, Req_Len, Start_Address, Values);

      if not Send_Request (Ctx, Slave, Request, Req_Len) then
         return False;
      end if;

      Ctx.Pending (Handle) := (
         State         => Awaiting_Response,
         Req_Type      => Write_Multi_Coils,
         Slave         => Slave,
         Start_Time_Ms => Get_Tick_Ms,
         Timeout_Ms    => Actual_Timeout,
         On_Registers  => null,
         On_Bits       => null,
         On_Write      => On_Response,
         Expected_Qty  => Values'Length
      );

      return True;
   end Write_Multiple_Coils_Async;

   ------------------------------------
   -- Write_Multiple_Registers_Async --
   ------------------------------------

   function Write_Multiple_Registers_Async
     (Ctx           : in out Async_Context;
      Slave         : Unit_Id;
      Start_Address : Register_Address;
      Values        : Register_Array;
      On_Response   : Write_Callback;
      Handle        : out Request_Handle;
      Timeout_Ms    : Natural := 0) return Boolean
   is
      Request : Protocol.PDU_Buffer;
      Req_Len : Natural;
      Actual_Timeout : constant Natural :=
        (if Timeout_Ms > 0 then Timeout_Ms else Ctx.Master.Config.Default_Timeout);
   begin
      Handle := Request_Handle'First;

      if not Has_Free_Slot (Ctx) then
         return False;
      end if;

      Handle := Find_Free_Slot (Ctx);

      Protocol.Encode_Write_Multiple_Registers_Request
        (Request, Req_Len, Start_Address, Values);

      if not Send_Request (Ctx, Slave, Request, Req_Len) then
         return False;
      end if;

      Ctx.Pending (Handle) := (
         State         => Awaiting_Response,
         Req_Type      => Write_Multi_Regs,
         Slave         => Slave,
         Start_Time_Ms => Get_Tick_Ms,
         Timeout_Ms    => Actual_Timeout,
         On_Registers  => null,
         On_Bits       => null,
         On_Write      => On_Response,
         Expected_Qty  => Values'Length
      );

      return True;
   end Write_Multiple_Registers_Async;

   ---------------------
   -- Process_Pending --
   ---------------------

   procedure Process_Pending (Ctx : in out Async_Context) is
      Current_Time : constant Interfaces.Unsigned_32 := Get_Tick_Ms;
      Elapsed      : Interfaces.Unsigned_32;
      Bytes_Rcvd   : Natural;
      Response_PDU : Protocol.PDU_Buffer;
      Resp_Len     : Natural;
      Parse_Result : Status;
   begin
      --  Check each pending request
      for H in Ctx.Pending'Range loop
         if Ctx.Pending (H).State = Awaiting_Response then
            declare
               Req : Pending_Request renames Ctx.Pending (H);
            begin
               --  Check for timeout
               Elapsed := Current_Time - Req.Start_Time_Ms;
               if Elapsed >= Interfaces.Unsigned_32 (Req.Timeout_Ms) then
                  --  Timeout - invoke callback and clear slot
                  case Req.Req_Type is
                     when Read_Holding_Regs | Read_Input_Regs =>
                        if Req.On_Registers /= null then
                           declare
                              Empty_Values : Register_Array (0 .. -1);
                           begin
                              Req.On_Registers
                                (H, Response_Timeout, Req.Slave, Empty_Values, 0);
                           end;
                        end if;

                     when Read_Coils_Type | Read_Discrete_Type =>
                        if Req.On_Bits /= null then
                           declare
                              Empty_Values : Coil_Array (0 .. -1);
                           begin
                              Req.On_Bits
                                (H, Response_Timeout, Req.Slave, Empty_Values, 0);
                           end;
                        end if;

                     when Write_Single_Coil_Type | Write_Single_Reg |
                          Write_Multi_Coils | Write_Multi_Regs =>
                        if Req.On_Write /= null then
                           Req.On_Write (H, Response_Timeout, Req.Slave, 0);
                        end if;

                     when None =>
                        null;
                  end case;

                  Ctx.Pending (H).State := Empty;

               else
                  --  Try non-blocking receive (timeout = 0 or very short)
                  --  Note: For proper non-blocking, transport should support
                  --  timeout=0 to check for available data
                  declare
                     Remaining_Timeout : constant Natural :=
                       Req.Timeout_Ms - Natural (Elapsed);
                     Poll_Timeout : constant Natural :=
                       (if Remaining_Timeout > 10 then 10 else Remaining_Timeout);
                  begin
                     --  Use short poll timeout to check for data
                     case Ctx.Master.Config.Mode is
                        when RTU =>
                           declare
                              Recv_Buf : Protocol.RTU.ADU_Buffer;
                              Recv_Slave : Unit_Id;
                           begin
                              Bytes_Rcvd := Receive
                                (Ctx.Master.Transport, Recv_Buf,
                                 Protocol.RTU.Max_ADU_Size, Poll_Timeout);

                              if Bytes_Rcvd > 0 then
                                 Protocol.RTU.Parse_Frame
                                   (Recv_Buf, Bytes_Rcvd, Recv_Slave,
                                    Response_PDU, Resp_Len, Parse_Result);

                                 if Parse_Result = Success and Recv_Slave = Req.Slave then
                                    --  Valid response - process it
                                    Process_Response
                                      (Ctx, H, Response_PDU, Resp_Len, Parse_Result);
                                 end if;
                              end if;
                           end;

                        when ASCII =>
                           declare
                              Recv_Buf : Protocol.ASCII.Frame_Buffer;
                              Recv_Slave : Unit_Id;
                           begin
                              Bytes_Rcvd := Receive
                                (Ctx.Master.Transport, Recv_Buf,
                                 Protocol.ASCII.Max_Frame_Size, Poll_Timeout);

                              if Bytes_Rcvd > 0 then
                                 Protocol.ASCII.Parse_Frame
                                   (Recv_Buf, Bytes_Rcvd, Recv_Slave,
                                    Response_PDU, Resp_Len, Parse_Result);

                                 if Parse_Result = Success and Recv_Slave = Req.Slave then
                                    Process_Response
                                      (Ctx, H, Response_PDU, Resp_Len, Parse_Result);
                                 end if;
                              end if;
                           end;

                        when TCP =>
                           declare
                              Recv_Buf : Protocol.TCP.ADU_Buffer;
                              Recv_Trans : Protocol.TCP.Transaction_Id;
                              Recv_Unit : Unit_Id;
                           begin
                              --  Try to receive MBAP header (7 bytes)
                              Bytes_Rcvd := Receive
                                (Ctx.Master.Transport, Recv_Buf, 7, Poll_Timeout);

                              if Bytes_Rcvd >= 7 then
                                 --  Got header, receive rest
                                 declare
                                    Expected_Len : constant Natural :=
                                      Protocol.TCP.Get_Expected_Length (Recv_Buf);
                                    Remaining : constant Natural := Expected_Len - 7;
                                 begin
                                    if Remaining > 0 then
                                       declare
                                          More_Buf : Byte_Array (0 .. Remaining - 1);
                                          More_Rcvd : Natural;
                                       begin
                                          More_Rcvd := Receive
                                            (Ctx.Master.Transport, More_Buf,
                                             Remaining, Poll_Timeout);
                                          if More_Rcvd = Remaining then
                                             for I in 0 .. Remaining - 1 loop
                                                Recv_Buf (7 + I) := More_Buf (I);
                                             end loop;
                                             Bytes_Rcvd := Expected_Len;
                                          else
                                             Bytes_Rcvd := 0;  --  Incomplete
                                          end if;
                                       end;
                                    end if;
                                 end;

                                 if Bytes_Rcvd > 0 then
                                    Protocol.TCP.Parse_Frame
                                      (Recv_Buf, Bytes_Rcvd, Recv_Trans, Recv_Unit,
                                       Response_PDU, Resp_Len, Parse_Result);

                                    if Parse_Result = Success then
                                       Process_Response
                                         (Ctx, H, Response_PDU, Resp_Len, Parse_Result);
                                    end if;
                                 end if;
                              end if;
                           end;
                     end case;
                  end;
               end if;
            end;
         end if;
      end loop;
   end Process_Pending;

   ----------------------
   -- Process_Response --
   ----------------------

   procedure Process_Response
     (Ctx      : in out Async_Context;
      Handle   : Request_Handle;
      PDU      : Protocol.PDU_Buffer;
      PDU_Len  : Natural;
      Result   : in out Status)
   is
      Req : Pending_Request renames Ctx.Pending (Handle);
   begin
      case Req.Req_Type is
         when Read_Holding_Regs | Read_Input_Regs =>
            declare
               Values : Register_Array (0 .. Req.Expected_Qty - 1) := [others => 0];
               Count  : Natural;
            begin
               Protocol.Decode_Read_Registers_Response
                 (PDU, PDU_Len, Values, Count, Result);

               if Req.On_Registers /= null then
                  Req.On_Registers
                    (Handle, To_Response_Status (Result), Req.Slave,
                     Values (0 .. Count - 1), 0);
               end if;
            end;

         when Read_Coils_Type | Read_Discrete_Type =>
            declare
               Values : Coil_Array (0 .. Req.Expected_Qty - 1) := [others => False];
               Count  : Natural;
            begin
               Protocol.Decode_Read_Bits_Response (PDU, PDU_Len, Values, Count, Result);

               if Req.On_Bits /= null then
                  Req.On_Bits
                    (Handle, To_Response_Status (Result), Req.Slave,
                     Values (0 .. Count - 1), 0);
               end if;
            end;

         when Write_Single_Coil_Type | Write_Single_Reg =>
            declare
               Resp_Addr : Register_Address;
               Resp_Val  : Register_Value;
            begin
               Protocol.Decode_Write_Single_Response
                 (PDU, PDU_Len, Resp_Addr, Resp_Val, Result);

               if Req.On_Write /= null then
                  Req.On_Write
                    (Handle, To_Response_Status (Result), Req.Slave, 0);
               end if;
            end;

         when Write_Multi_Coils | Write_Multi_Regs =>
            declare
               Resp_Addr : Register_Address;
               Resp_Qty  : Natural;
            begin
               Protocol.Decode_Write_Multiple_Response
                 (PDU, PDU_Len, Resp_Addr, Resp_Qty, Result);

               if Req.On_Write /= null then
                  Req.On_Write
                    (Handle, To_Response_Status (Result), Req.Slave, 0);
               end if;
            end;

         when None =>
            null;
      end case;

      --  Clear the slot
      Ctx.Pending (Handle).State := Empty;
   end Process_Response;

   --------------------
   -- Cancel_Request --
   --------------------

   procedure Cancel_Request
     (Ctx    : in out Async_Context;
      Handle : Request_Handle)
   is
   begin
      Ctx.Pending (Handle).State := Empty;
   end Cancel_Request;

   ----------------
   -- Cancel_All --
   ----------------

   procedure Cancel_All (Ctx : in out Async_Context) is
   begin
      for H in Ctx.Pending'Range loop
         Ctx.Pending (H).State := Empty;
      end loop;
   end Cancel_All;

end Ada_Modbus.Master.Async;
