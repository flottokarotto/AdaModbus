--  Delta_Client - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with Config;
with HAL_Stubs;
with Ada_Modbus.Protocol;
with Ada_Modbus.Protocol.RTU;

package body Delta_Client is

   use Ada_Modbus.Protocol;
   use Ada_Modbus.Protocol.RTU;

   --  Internal state
   Last_Status      : EVSE_Status;
   Current_Power_W  : Unsigned_32 := 0;
   Is_Initialized   : Boolean := False;

   --  Communication buffers
   PDU_Buf  : PDU_Buffer;
   ADU_Buf  : ADU_Buffer;
   Resp_Buf : ADU_Buffer;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      --  Initialize USART3 for RS485 (115200 8N1)
      HAL_Stubs.USART3_Initialize (Config.Delta_Baud_Rate);

      --  Initialize state
      Last_Status := (State            => Unavailable,
                      Charge           => Not_Charging,
                      Voltage_Raw      => [0, 0],
                      Power_W          => 0,
                      Current_Raw      => [0, 0],
                      Output_Power_Raw => [0, 0],
                      SOC_Percent_x10  => 0,
                      Charging_Time_S  => 0,
                      Energy_Wh        => 0);
      Current_Power_W := 0;
      Is_Initialized := True;
   end Initialize;

   -----------------
   -- Read_Status --
   -----------------

   procedure Read_Status
     (Status : out EVSE_Status;
      Result : out Ada_Modbus.Status)
   is
      PDU_Len, ADU_Len : Natural;
      Resp_Len         : Natural;
      Resp_PDU         : PDU_Buffer;
      Resp_PDU_Len     : Natural;
      Slave_Id         : Unit_Id;
   begin
      Status := Last_Status;

      if not Is_Initialized then
         Result := Invalid_Request;
         return;
      end if;

      --  Encode request PDU
      Encode_Read_EVSE_Status_Request (PDU_Buf, PDU_Len);

      --  Build RTU frame
      Build_Frame
        (Slave      => Config.Delta_Device_Id,
         PDU        => PDU_Buf,
         PDU_Length => PDU_Len,
         ADU        => ADU_Buf,
         ADU_Length => ADU_Len);

      --  Send/Receive via RS485
      HAL_Stubs.RS485_Transceive
        (TX_Data    => ADU_Buf (0 .. ADU_Len - 1),
         RX_Data    => Resp_Buf,
         RX_Length  => Resp_Len,
         Timeout_Ms => Config.Modbus_Timeout_Ms,
         Result     => Result);

      if Result /= Success then
         return;
      end if;

      --  Parse RTU response
      Parse_Frame
        (ADU        => Resp_Buf,
         ADU_Length => Resp_Len,
         Slave      => Slave_Id,
         PDU        => Resp_PDU,
         PDU_Length => Resp_PDU_Len,
         Result     => Result);

      if Result /= Success then
         return;
      end if;

      --  Decode response
      Decode_EVSE_Status_Response
        (Buffer => Resp_PDU,
         Length => Resp_PDU_Len,
         Status => Status,
         Result => Result);

      if Result = Success then
         Last_Status := Status;
      end if;
   end Read_Status;

   ---------------
   -- Read_Info --
   ---------------

   procedure Read_Info
     (Info   : out Charger_Info;
      Result : out Ada_Modbus.Status)
   is
      PDU_Len, ADU_Len : Natural;
      Resp_Len         : Natural;
      Resp_PDU         : PDU_Buffer;
      Resp_PDU_Len     : Natural;
      Slave_Id         : Unit_Id;
   begin
      Info := (State => Not_Ready, EVSE_Count => 0);

      if not Is_Initialized then
         Result := Invalid_Request;
         return;
      end if;

      --  Encode request PDU
      Encode_Read_Charger_Info_Request (PDU_Buf, PDU_Len);

      --  Build RTU frame
      Build_Frame
        (Slave      => Config.Delta_Device_Id,
         PDU        => PDU_Buf,
         PDU_Length => PDU_Len,
         ADU        => ADU_Buf,
         ADU_Length => ADU_Len);

      --  Send/Receive via RS485
      HAL_Stubs.RS485_Transceive
        (TX_Data    => ADU_Buf (0 .. ADU_Len - 1),
         RX_Data    => Resp_Buf,
         RX_Length  => Resp_Len,
         Timeout_Ms => Config.Modbus_Timeout_Ms,
         Result     => Result);

      if Result /= Success then
         return;
      end if;

      --  Parse RTU response
      Parse_Frame
        (ADU        => Resp_Buf,
         ADU_Length => Resp_Len,
         Slave      => Slave_Id,
         PDU        => Resp_PDU,
         PDU_Length => Resp_PDU_Len,
         Result     => Result);

      if Result /= Success then
         return;
      end if;

      --  Decode response
      Decode_Charger_Info_Response
        (Buffer => Resp_PDU,
         Length => Resp_PDU_Len,
         Info   => Info,
         Result => Result);
   end Read_Info;

   ---------------------
   -- Set_Power_Limit --
   ---------------------

   procedure Set_Power_Limit
     (Power_W : Unsigned_32;
      Result  : out Ada_Modbus.Status)
   is
      PDU_Len, ADU_Len : Natural;
      Resp_Len         : Natural;
   begin
      if not Is_Initialized then
         Result := Invalid_Request;
         return;
      end if;

      --  Encode request PDU
      Encode_Set_Power_Limit_Request (Power_W, PDU_Buf, PDU_Len);

      --  Build RTU frame
      Build_Frame
        (Slave      => Config.Delta_Device_Id,
         PDU        => PDU_Buf,
         PDU_Length => PDU_Len,
         ADU        => ADU_Buf,
         ADU_Length => ADU_Len);

      --  Send/Receive via RS485
      HAL_Stubs.RS485_Transceive
        (TX_Data    => ADU_Buf (0 .. ADU_Len - 1),
         RX_Data    => Resp_Buf,
         RX_Length  => Resp_Len,
         Timeout_Ms => Config.Modbus_Timeout_Ms,
         Result     => Result);

      if Result = Success then
         Current_Power_W := Power_W;
      end if;
   end Set_Power_Limit;

   -----------------
   -- Set_Suspend --
   -----------------

   procedure Set_Suspend
     (Suspend : Boolean;
      Result  : out Ada_Modbus.Status)
   is
      PDU_Len, ADU_Len : Natural;
      Resp_Len         : Natural;
   begin
      if not Is_Initialized then
         Result := Invalid_Request;
         return;
      end if;

      --  Encode request PDU
      Encode_Set_Suspend_Request (Suspend, PDU_Buf, PDU_Len);

      --  Build RTU frame
      Build_Frame
        (Slave      => Config.Delta_Device_Id,
         PDU        => PDU_Buf,
         PDU_Length => PDU_Len,
         ADU        => ADU_Buf,
         ADU_Length => ADU_Len);

      --  Send/Receive via RS485
      HAL_Stubs.RS485_Transceive
        (TX_Data    => ADU_Buf (0 .. ADU_Len - 1),
         RX_Data    => Resp_Buf,
         RX_Length  => Resp_Len,
         Timeout_Ms => Config.Modbus_Timeout_Ms,
         Result     => Result);
   end Set_Suspend;

   ---------------------
   -- Get_Last_Status --
   ---------------------

   function Get_Last_Status return EVSE_Status is
   begin
      return Last_Status;
   end Get_Last_Status;

   -----------------------------
   -- Get_Current_Power_Limit --
   -----------------------------

   function Get_Current_Power_Limit return Unsigned_32 is
   begin
      return Current_Power_W;
   end Get_Current_Power_Limit;

   -----------------
   -- Is_Charging --
   -----------------

   function Is_Charging return Boolean is
   begin
      return Last_Status.State = Charging;
   end Is_Charging;

   ------------------
   -- Is_Available --
   ------------------

   function Is_Available return Boolean is
   begin
      return Last_Status.State = Available
        or else Last_Status.State = Occupied
        or else Last_Status.State = Preparing
        or else Last_Status.State = Charging
        or else Last_Status.State = Suspended_EVSE;
   end Is_Available;

end Delta_Client;
