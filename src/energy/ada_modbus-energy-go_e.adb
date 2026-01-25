--  Ada_Modbus.Energy.Go_E - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

package body Ada_Modbus.Energy.Go_E
  with SPARK_Mode => On
is

   -----------------------------------
   -- Encode_Read_Status_Request   --
   -----------------------------------

   procedure Encode_Read_Status_Request
     (Buffer : out Protocol.PDU_Buffer;
      Length : out Natural)
   is
   begin
      --  Read Input Registers (FC 04): 30101-30160 = 60 registers
      Protocol.Encode_Read_Registers_Request
        (Buffer        => Buffer,
         Length        => Length,
         FC            => FC_Read_Input_Registers,
         Start_Address => Reg_Car_State,
         Quantity      => 60);
   end Encode_Read_Status_Request;

   ------------------------------------
   -- Encode_Read_Car_State_Request --
   ------------------------------------

   procedure Encode_Read_Car_State_Request
     (Buffer : out Protocol.PDU_Buffer;
      Length : out Natural)
   is
   begin
      Protocol.Encode_Read_Registers_Request
        (Buffer        => Buffer,
         Length        => Length,
         FC            => FC_Read_Input_Registers,
         Start_Address => Reg_Car_State,
         Quantity      => 1);
   end Encode_Read_Car_State_Request;

   ---------------------------------
   -- Encode_Read_Power_Request  --
   ---------------------------------

   procedure Encode_Read_Power_Request
     (Buffer : out Protocol.PDU_Buffer;
      Length : out Natural)
   is
   begin
      --  Read power registers: 30121-30122 (total) + L1-L3
      --  We read 8 registers: power total (2) + skip (6) to get power L1-L3
      Protocol.Encode_Read_Registers_Request
        (Buffer        => Buffer,
         Length        => Length,
         FC            => FC_Read_Input_Registers,
         Start_Address => Reg_Power_Total,
         Quantity      => 32);  --  30121 to 30152
   end Encode_Read_Power_Request;

   ------------------------------------
   -- Encode_Read_Settings_Request  --
   ------------------------------------

   procedure Encode_Read_Settings_Request
     (Buffer : out Protocol.PDU_Buffer;
      Length : out Natural)
   is
   begin
      --  Read holding registers 40201-40340
      Protocol.Encode_Read_Registers_Request
        (Buffer        => Buffer,
         Length        => Length,
         FC            => FC_Read_Holding_Registers,
         Start_Address => Reg_Allow,
         Quantity      => 125);  --  Max registers per request
   end Encode_Read_Settings_Request;

   --------------------------------
   -- Encode_Set_Ampere_Request --
   --------------------------------

   procedure Encode_Set_Ampere_Request
     (Ampere : Charging_Current;
      Buffer : out Protocol.PDU_Buffer;
      Length : out Natural)
   is
   begin
      Protocol.Encode_Write_Single_Register_Request
        (Buffer  => Buffer,
         Length  => Length,
         Address => Reg_Ampere_Volatile,
         Value   => Register_Value (Ampere));
   end Encode_Set_Ampere_Request;

   -------------------------------------
   -- Encode_Set_Force_State_Request --
   -------------------------------------

   procedure Encode_Set_Force_State_Request
     (State  : Force_State;
      Buffer : out Protocol.PDU_Buffer;
      Length : out Natural)
   is
      Value : constant Register_Value :=
        (case State is
            when Neutral   => 0,
            when Force_Off => 1,
            when Force_On  => 2);
   begin
      Protocol.Encode_Write_Single_Register_Request
        (Buffer  => Buffer,
         Length  => Length,
         Address => Reg_Force_State,
         Value   => Value);
   end Encode_Set_Force_State_Request;

   -------------------------------
   -- Encode_Set_Allow_Request --
   -------------------------------

   procedure Encode_Set_Allow_Request
     (Allow  : Boolean;
      Buffer : out Protocol.PDU_Buffer;
      Length : out Natural)
   is
   begin
      Protocol.Encode_Write_Single_Register_Request
        (Buffer  => Buffer,
         Length  => Length,
         Address => Reg_Allow,
         Value   => (if Allow then 1 else 0));
   end Encode_Set_Allow_Request;

   ------------------------------------
   -- Encode_Set_Cable_Lock_Request --
   ------------------------------------

   procedure Encode_Set_Cable_Lock_Request
     (Mode   : Cable_Lock_Mode;
      Buffer : out Protocol.PDU_Buffer;
      Length : out Natural)
   is
      Value : constant Register_Value :=
        (case Mode is
            when Unlock_Always  => 0,
            when Lock_While_Car => 1,
            when Lock_Always    => 2);
   begin
      Protocol.Encode_Write_Single_Register_Request
        (Buffer  => Buffer,
         Length  => Length,
         Address => Reg_Cable_Lock,
         Value   => Value);
   end Encode_Set_Cable_Lock_Request;

   ------------------------------------
   -- Encode_Set_Phase_Mode_Request --
   ------------------------------------

   procedure Encode_Set_Phase_Mode_Request
     (Mode   : Phase_Switch_Mode;
      Buffer : out Protocol.PDU_Buffer;
      Length : out Natural)
   is
      Value : constant Register_Value :=
        (case Mode is
            when Auto         => 0,
            when Single_Phase => 1,
            when Three_Phase  => 2);
   begin
      Protocol.Encode_Write_Single_Register_Request
        (Buffer  => Buffer,
         Length  => Length,
         Address => Reg_Phase_Switch,
         Value   => Value);
   end Encode_Set_Phase_Mode_Request;

   -----------------------------------------
   -- Encode_Set_LED_Brightness_Request  --
   -----------------------------------------

   procedure Encode_Set_LED_Brightness_Request
     (Brightness : LED_Brightness;
      Buffer     : out Protocol.PDU_Buffer;
      Length     : out Natural)
   is
   begin
      Protocol.Encode_Write_Single_Register_Request
        (Buffer  => Buffer,
         Length  => Length,
         Address => Reg_LED_Brightness,
         Value   => Register_Value (Brightness));
   end Encode_Set_LED_Brightness_Request;

   ----------------------------------
   -- Decode_Car_State_Response   --
   ----------------------------------

   procedure Decode_Car_State_Response
     (Buffer : Protocol.PDU_Buffer;
      Length : Natural;
      State  : out Car_State;
      Result : out Status)
   is
      Values : Register_Array (0 .. 0);
      Count  : Natural;
   begin
      State := Unknown;

      Protocol.Decode_Read_Registers_Response
        (Buffer   => Buffer,
         Length   => Length,
         Values   => Values,
         Count    => Count,
         Response => Result);

      if Result = Success and then Count >= 1 then
         case Values (0) is
            when 1 => State := Idle;
            when 2 => State := Charging;
            when 3 => State := Waiting_For_Car;
            when 4 => State := Charge_Complete;
            when others => State := Unknown;
         end case;
      end if;
   end Decode_Car_State_Response;

   ----------------------------
   -- Decode_Power_Response --
   ----------------------------

   procedure Decode_Power_Response
     (Buffer : Protocol.PDU_Buffer;
      Length : Natural;
      Power  : out Power_Data;
      Result : out Status)
   is
      Values : Register_Array (0 .. 31);
      Count  : Natural;
   begin
      Power := (Total | L1 | L2 | L3 => 0);

      Protocol.Decode_Read_Registers_Response
        (Buffer   => Buffer,
         Length   => Length,
         Values   => Values,
         Count    => Count,
         Response => Result);

      if Result = Success and then Count >= 32 then
         --  Reg 30121-30122: Power Total (offset 0-1)
         Power.Total := Decode_Uint32 (Values (0), Values (1));
         --  Reg 30147-30152: Power L1-L3 (offset 26-31)
         Power.L1 := Decode_Uint32 (Values (26), Values (27));
         Power.L2 := Decode_Uint32 (Values (28), Values (29));
         Power.L3 := Decode_Uint32 (Values (30), Values (31));
      end if;
   end Decode_Power_Response;

   ---------------------
   -- Decode_Uint32  --
   ---------------------

   function Decode_Uint32
     (High_Word : Register_Value;
      Low_Word  : Register_Value) return Interfaces.Unsigned_32
   is
      use Interfaces;
   begin
      --  go-e uses Big Endian (Word Swap): High word first
      return Unsigned_32 (High_Word) * 65536 + Unsigned_32 (Low_Word);
   end Decode_Uint32;

end Ada_Modbus.Energy.Go_E;
