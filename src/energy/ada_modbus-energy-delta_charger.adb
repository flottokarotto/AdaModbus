--  Ada_Modbus.Energy.Delta_Charger - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

package body Ada_Modbus.Energy.Delta_Charger
  with SPARK_Mode => On
is

   -------------------------------------
   -- Encode_Read_EVSE_Status_Request --
   -------------------------------------

   procedure Encode_Read_EVSE_Status_Request
     (Buffer : out Protocol.PDU_Buffer;
      Length : out Natural)
   is
   begin
      --  Read Input Registers (FC 04): 1000-1020 = 21 registers
      --  Covers: State, Charge State, Voltage, Power, Current,
      --          Output Power, SOC, Charging Time, Energy
      Protocol.Encode_Read_Registers_Request
        (Buffer        => Buffer,
         Length        => Length,
         FC            => FC_Read_Input_Registers,
         Start_Address => EVSE_Base + Register_Address (EVSE_State_Offset),
         Quantity      => 21);
   end Encode_Read_EVSE_Status_Request;

   --------------------------------------
   -- Encode_Read_Charger_Info_Request --
   --------------------------------------

   procedure Encode_Read_Charger_Info_Request
     (Buffer : out Protocol.PDU_Buffer;
      Length : out Natural)
   is
   begin
      --  Read Input Registers (FC 04): 100-102 = 3 registers
      Protocol.Encode_Read_Registers_Request
        (Buffer        => Buffer,
         Length        => Length,
         FC            => FC_Read_Input_Registers,
         Start_Address => Reg_Charger_State,
         Quantity      => 3);
   end Encode_Read_Charger_Info_Request;

   -----------------------------------------
   -- Encode_Read_Comm_Settings_Request --
   -----------------------------------------

   procedure Encode_Read_Comm_Settings_Request
     (Buffer : out Protocol.PDU_Buffer;
      Length : out Natural)
   is
   begin
      --  Read Holding Registers (FC 03): 201-204 = 4 registers
      Protocol.Encode_Read_Registers_Request
        (Buffer        => Buffer,
         Length        => Length,
         FC            => FC_Read_Holding_Registers,
         Start_Address => Reg_Comm_Timeout_Enabled,
         Quantity      => 4);
   end Encode_Read_Comm_Settings_Request;

   -----------------------------------
   -- Encode_Set_Power_Limit_Request --
   -----------------------------------

   procedure Encode_Set_Power_Limit_Request
     (Power_W : Interfaces.Unsigned_32;
      Buffer  : out Protocol.PDU_Buffer;
      Length  : out Natural)
   is
      High_Word, Low_Word : Register_Value;
   begin
      Encode_Uint32 (Power_W, High_Word, Low_Word);

      --  Write Multiple Registers (FC 16): 1600-1601
      Protocol.Encode_Write_Multiple_Registers_Request
        (Buffer        => Buffer,
         Length        => Length,
         Start_Address => EVSE_Base + Register_Address (Power_Limit_Offset),
         Values        => [High_Word, Low_Word]);
   end Encode_Set_Power_Limit_Request;

   ---------------------------------
   -- Encode_Set_Suspend_Request --
   ---------------------------------

   procedure Encode_Set_Suspend_Request
     (Suspend : Boolean;
      Buffer  : out Protocol.PDU_Buffer;
      Length  : out Natural)
   is
   begin
      --  Write Single Register (FC 06): 1602
      --  0 = active (charging), 1 = suspended
      Protocol.Encode_Write_Single_Register_Request
        (Buffer  => Buffer,
         Length  => Length,
         Address => EVSE_Base + Register_Address (Suspend_Charging_Offset),
         Value   => (if Suspend then 1 else 0));
   end Encode_Set_Suspend_Request;

   ----------------------------------------
   -- Encode_Set_Comm_Timeout_Request --
   ----------------------------------------

   procedure Encode_Set_Comm_Timeout_Request
     (Timeout_Seconds : Natural;
      Buffer          : out Protocol.PDU_Buffer;
      Length          : out Natural)
   is
   begin
      Protocol.Encode_Write_Single_Register_Request
        (Buffer  => Buffer,
         Length  => Length,
         Address => Reg_Comm_Timeout,
         Value   => Register_Value (Timeout_Seconds));
   end Encode_Set_Comm_Timeout_Request;

   -----------------------------------------------
   -- Encode_Set_Comm_Timeout_Enabled_Request --
   -----------------------------------------------

   procedure Encode_Set_Comm_Timeout_Enabled_Request
     (Enabled : Boolean;
      Buffer  : out Protocol.PDU_Buffer;
      Length  : out Natural)
   is
   begin
      Protocol.Encode_Write_Single_Register_Request
        (Buffer  => Buffer,
         Length  => Length,
         Address => Reg_Comm_Timeout_Enabled,
         Value   => (if Enabled then 1 else 0));
   end Encode_Set_Comm_Timeout_Enabled_Request;

   --------------------------------------
   -- Encode_Set_Fallback_Power_Request --
   --------------------------------------

   procedure Encode_Set_Fallback_Power_Request
     (Power_W : Interfaces.Unsigned_32;
      Buffer  : out Protocol.PDU_Buffer;
      Length  : out Natural)
   is
      High_Word, Low_Word : Register_Value;
   begin
      Encode_Uint32 (Power_W, High_Word, Low_Word);

      Protocol.Encode_Write_Multiple_Registers_Request
        (Buffer        => Buffer,
         Length        => Length,
         Start_Address => Reg_Fallback_Power,
         Values        => [High_Word, Low_Word]);
   end Encode_Set_Fallback_Power_Request;

   ----------------------------------
   -- Decode_EVSE_Status_Response --
   ----------------------------------

   procedure Decode_EVSE_Status_Response
     (Buffer : Protocol.PDU_Buffer;
      Length : Natural;
      Status : out EVSE_Status;
      Result : out Ada_Modbus.Status)
   is
      Values : Register_Array (0 .. 20);
      Count  : Natural;
   begin
      --  Initialize output
      Status := (State            => Unavailable,
                 Charge           => Not_Charging,
                 Voltage_Raw      => [0, 0],
                 Power_W          => 0,
                 Current_Raw      => [0, 0],
                 Output_Power_Raw => [0, 0],
                 SOC_Percent_x10  => 0,
                 Charging_Time_S  => 0,
                 Energy_Wh        => 0);

      Protocol.Decode_Read_Registers_Response
        (Buffer   => Buffer,
         Length   => Length,
         Values   => Values,
         Count    => Count,
         Response => Result);

      if Result = Success and then Count >= 21 then
         --  Offset 0: EVSE State
         Status.State := To_EVSE_State (Values (EVSE_State_Offset));

         --  Offset 1: Charge State
         Status.Charge := To_Charge_State (Values (Charge_State_Offset));

         --  Offset 3-4: Output Voltage (FLOAT32 raw)
         Status.Voltage_Raw (0) := Values (Output_Voltage_Offset);
         Status.Voltage_Raw (1) := Values (Output_Voltage_Offset + 1);

         --  Offset 5-6: Charging Power (UINT32)
         Status.Power_W := Decode_Uint32
           (Values (Charging_Power_Offset),
            Values (Charging_Power_Offset + 1));

         --  Offset 7-8: Charging Current (FLOAT32 raw)
         Status.Current_Raw (0) := Values (Charging_Current_Offset);
         Status.Current_Raw (1) := Values (Charging_Current_Offset + 1);

         --  Offset 9-10: Output Power (FLOAT32 raw)
         Status.Output_Power_Raw (0) := Values (Output_Power_Offset);
         Status.Output_Power_Raw (1) := Values (Output_Power_Offset + 1);

         --  Offset 11: SOC (% * 10)
         declare
            Raw_SOC : constant Natural := Natural (Values (SOC_Offset));
         begin
            if Raw_SOC <= 1000 then
               Status.SOC_Percent_x10 := Raw_SOC;
            else
               Status.SOC_Percent_x10 := 0;  --  Invalid value
            end if;
         end;

         --  Offset 17-18: Charging Time (UINT32, seconds)
         Status.Charging_Time_S := Decode_Uint32
           (Values (Charging_Time_Offset),
            Values (Charging_Time_Offset + 1));

         --  Offset 19-20: Charged Energy (UINT32, Wh)
         Status.Energy_Wh := Decode_Uint32
           (Values (Charged_Energy_Offset),
            Values (Charged_Energy_Offset + 1));
      end if;
   end Decode_EVSE_Status_Response;

   -----------------------------------
   -- Decode_Charger_Info_Response --
   -----------------------------------

   procedure Decode_Charger_Info_Response
     (Buffer : Protocol.PDU_Buffer;
      Length : Natural;
      Info   : out Charger_Info;
      Result : out Ada_Modbus.Status)
   is
      Values : Register_Array (0 .. 2);
      Count  : Natural;
   begin
      Info := (State => Not_Ready, EVSE_Count => 0);

      Protocol.Decode_Read_Registers_Response
        (Buffer   => Buffer,
         Length   => Length,
         Values   => Values,
         Count    => Count,
         Response => Result);

      if Result = Success and then Count >= 3 then
         --  Register 100: Charger State
         Info.State := To_Charger_State (Values (0));

         --  Register 102: EVSE Count
         if Natural (Values (2)) <= 255 then
            Info.EVSE_Count := Natural (Values (2));
         end if;
      end if;
   end Decode_Charger_Info_Response;

   -------------------------------------
   -- Decode_Comm_Settings_Response --
   -------------------------------------

   procedure Decode_Comm_Settings_Response
     (Buffer   : Protocol.PDU_Buffer;
      Length   : Natural;
      Settings : out Comm_Settings;
      Result   : out Ada_Modbus.Status)
   is
      Values : Register_Array (0 .. 3);
      Count  : Natural;
   begin
      Settings := (Timeout_Enabled => False,
                   Timeout_Seconds => 0,
                   Fallback_Power  => 0);

      Protocol.Decode_Read_Registers_Response
        (Buffer   => Buffer,
         Length   => Length,
         Values   => Values,
         Count    => Count,
         Response => Result);

      if Result = Success and then Count >= 4 then
         --  Register 201: Timeout Enabled
         Settings.Timeout_Enabled := Values (0) /= 0;

         --  Register 202: Timeout Seconds
         Settings.Timeout_Seconds := Natural (Values (1));

         --  Register 203-204: Fallback Power (UINT32)
         Settings.Fallback_Power := Decode_Uint32 (Values (2), Values (3));
      end if;
   end Decode_Comm_Settings_Response;

   -------------------
   -- Decode_Uint32 --
   -------------------

   function Decode_Uint32
     (High_Word : Register_Value;
      Low_Word  : Register_Value) return Interfaces.Unsigned_32
   is
      use Interfaces;
   begin
      --  Big Endian: High word first
      return Unsigned_32 (High_Word) * 65536 + Unsigned_32 (Low_Word);
   end Decode_Uint32;

   -------------------
   -- Encode_Uint32 --
   -------------------

   procedure Encode_Uint32
     (Value     : Interfaces.Unsigned_32;
      High_Word : out Register_Value;
      Low_Word  : out Register_Value)
   is
      use Interfaces;
   begin
      High_Word := Register_Value (Value / 65536);
      Low_Word  := Register_Value (Value mod 65536);
   end Encode_Uint32;

   -------------------
   -- To_EVSE_State --
   -------------------

   function To_EVSE_State (Raw : Register_Value) return EVSE_State is
   begin
      case Raw is
         when 0 => return Unavailable;
         when 1 => return Available;
         when 2 => return Occupied;
         when 3 => return Preparing;
         when 4 => return Charging;
         when 5 => return Finishing;
         when 6 => return Suspended_EV;
         when 7 => return Suspended_EVSE;
         when 8 => return Not_Ready;
         when 9 => return Faulted;
         when others => return Unavailable;  --  Unknown state
      end case;
   end To_EVSE_State;

   ----------------------
   -- To_Charger_State --
   ----------------------

   function To_Charger_State (Raw : Register_Value) return Charger_State is
   begin
      case Raw is
         when 0   => return Not_Ready;
         when 1   => return Operational;
         when 10  => return Faulted;
         when 255 => return Not_Responding;
         when others => return Not_Ready;  --  Unknown state
      end case;
   end To_Charger_State;

   ---------------------
   -- To_Charge_State --
   ---------------------

   function To_Charge_State (Raw : Register_Value) return Charge_State is
   begin
      case Raw is
         when 0 => return Not_Charging;
         when 1 => return Charging_L1;
         when 2 => return Charging_L2;
         when 3 => return Charging_L3;
         when 4 => return Charging_L12;
         when 5 => return Charging_L23;
         when 6 => return Charging_L13;
         when 7 => return Charging_L123;
         when others => return Not_Charging;  --  Unknown state
      end case;
   end To_Charge_State;

end Ada_Modbus.Energy.Delta_Charger;
