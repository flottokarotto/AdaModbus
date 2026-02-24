--  Modbus_TCP_Master - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with Ada_Modbus.Protocol;
with Ada_Modbus.Protocol.TCP;
with TCP_Client;

package body Modbus_TCP_Master is

   use Ada_Modbus.Protocol;
   use Ada_Modbus.Protocol.TCP;

   --  Auto-incrementing transaction ID
   Next_Trans_Id : Transaction_Id := 1;

   function Next_Transaction return Transaction_Id is
      T : constant Transaction_Id := Next_Trans_Id;
   begin
      if Next_Trans_Id = Transaction_Id'Last then
         Next_Trans_Id := 1;
      else
         Next_Trans_Id := Next_Trans_Id + 1;
      end if;
      return T;
   end Next_Transaction;

   --  Internal helper: send request ADU, receive and parse response ADU
   procedure Transact
     (PDU_Buf      : PDU_Buffer;
      PDU_Len      : Natural;
      Unit         : Unit_Id;
      Timeout_Ms   : Unsigned_32;
      Resp_PDU     : out PDU_Buffer;
      Resp_PDU_Len : out Natural;
      Result       : out Status)
   is
      ADU_Buf       : ADU_Buffer;
      ADU_Len       : Natural;
      Resp_Buf      : ADU_Buffer;
      Resp_Len      : Natural;
      Resp_Trans_Id : Transaction_Id;
      Resp_Unit_Id  : Unit_Id;
   begin
      Resp_PDU := [others => 0];
      Resp_PDU_Len := 0;

      if not TCP_Client.Is_Connected then
         Result := Invalid_Request;
         return;
      end if;

      --  Build MBAP frame
      Build_Frame
        (ADU        => ADU_Buf,
         ADU_Length  => ADU_Len,
         Transaction => Next_Transaction,
         Unit       => Unit,
         PDU        => PDU_Buf,
         PDU_Length  => PDU_Len);

      --  Send and receive
      TCP_Client.Transceive
        (TX_Data    => ADU_Buf (0 .. ADU_Len - 1),
         RX_Data    => Resp_Buf,
         RX_Length  => Resp_Len,
         Timeout_Ms => Timeout_Ms,
         Result     => Result);

      if Result /= Success then
         return;
      end if;

      --  Parse MBAP response
      Parse_Frame
        (ADU        => Resp_Buf,
         ADU_Length  => Resp_Len,
         Transaction => Resp_Trans_Id,
         Unit       => Resp_Unit_Id,
         PDU        => Resp_PDU,
         PDU_Length  => Resp_PDU_Len,
         Result     => Result);
   end Transact;

   --  Internal helper for FC 03 / FC 04
   procedure Read_Registers_Internal
     (FC            : Function_Code;
      Unit          : Unit_Id;
      Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array;
      Count         : out Natural;
      Timeout_Ms    : Unsigned_32;
      Result        : out Status)
   is
      PDU_Buf      : PDU_Buffer;
      PDU_Len      : Natural;
      Resp_PDU     : PDU_Buffer;
      Resp_PDU_Len : Natural;
   begin
      Values := [others => 0];
      Count := 0;

      --  Encode request PDU
      Encode_Read_Registers_Request
        (Buffer        => PDU_Buf,
         Length        => PDU_Len,
         FC            => FC,
         Start_Address => Start_Address,
         Quantity      => Quantity);

      --  Transact
      Transact (PDU_Buf, PDU_Len, Unit, Timeout_Ms,
                Resp_PDU, Resp_PDU_Len, Result);

      if Result /= Success then
         return;
      end if;

      --  Decode register values
      Decode_Read_Registers_Response
        (Buffer   => Resp_PDU,
         Length   => Resp_PDU_Len,
         Values   => Values,
         Count    => Count,
         Response => Result);
   end Read_Registers_Internal;

   ----------------------------
   -- Read_Holding_Registers --
   ----------------------------

   procedure Read_Holding_Registers
     (Unit          : Unit_Id;
      Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array;
      Count         : out Natural;
      Timeout_Ms    : Unsigned_32;
      Result        : out Status)
   is
   begin
      Read_Registers_Internal
        (FC            => FC_Read_Holding_Registers,
         Unit          => Unit,
         Start_Address => Start_Address,
         Quantity      => Quantity,
         Values        => Values,
         Count         => Count,
         Timeout_Ms    => Timeout_Ms,
         Result        => Result);
   end Read_Holding_Registers;

   --------------------------
   -- Read_Input_Registers --
   --------------------------

   procedure Read_Input_Registers
     (Unit          : Unit_Id;
      Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array;
      Count         : out Natural;
      Timeout_Ms    : Unsigned_32;
      Result        : out Status)
   is
   begin
      Read_Registers_Internal
        (FC            => FC_Read_Input_Registers,
         Unit          => Unit,
         Start_Address => Start_Address,
         Quantity      => Quantity,
         Values        => Values,
         Count         => Count,
         Timeout_Ms    => Timeout_Ms,
         Result        => Result);
   end Read_Input_Registers;

   ----------------------------
   -- Write_Single_Register --
   ----------------------------

   procedure Write_Single_Register
     (Unit          : Unit_Id;
      Address       : Register_Address;
      Value         : Register_Value;
      Timeout_Ms    : Unsigned_32;
      Result        : out Status)
   is
      PDU_Buf      : PDU_Buffer;
      PDU_Len      : Natural;
      Resp_PDU     : PDU_Buffer;
      Resp_PDU_Len : Natural;
      Resp_Addr    : Register_Address;
      Resp_Value   : Register_Value;
   begin
      Encode_Write_Single_Register_Request
        (Buffer  => PDU_Buf,
         Length  => PDU_Len,
         Address => Address,
         Value   => Value);

      Transact (PDU_Buf, PDU_Len, Unit, Timeout_Ms,
                Resp_PDU, Resp_PDU_Len, Result);

      if Result /= Success then
         return;
      end if;

      Decode_Write_Single_Response
        (Buffer   => Resp_PDU,
         Length   => Resp_PDU_Len,
         Address  => Resp_Addr,
         Value    => Resp_Value,
         Response => Result);
   end Write_Single_Register;

   --------------------------------
   -- Write_Multiple_Registers --
   --------------------------------

   procedure Write_Multiple_Registers
     (Unit          : Unit_Id;
      Start_Address : Register_Address;
      Values        : Register_Array;
      Timeout_Ms    : Unsigned_32;
      Result        : out Status)
   is
      PDU_Buf      : PDU_Buffer;
      PDU_Len      : Natural;
      Resp_PDU     : PDU_Buffer;
      Resp_PDU_Len : Natural;
      Resp_Addr    : Register_Address;
      Resp_Qty     : Natural;
   begin
      Encode_Write_Multiple_Registers_Request
        (Buffer        => PDU_Buf,
         Length        => PDU_Len,
         Start_Address => Start_Address,
         Values        => Values);

      Transact (PDU_Buf, PDU_Len, Unit, Timeout_Ms,
                Resp_PDU, Resp_PDU_Len, Result);

      if Result /= Success then
         return;
      end if;

      Decode_Write_Multiple_Response
        (Buffer        => Resp_PDU,
         Length        => Resp_PDU_Len,
         Start_Address => Resp_Addr,
         Quantity      => Resp_Qty,
         Response      => Result);
   end Write_Multiple_Registers;

end Modbus_TCP_Master;
