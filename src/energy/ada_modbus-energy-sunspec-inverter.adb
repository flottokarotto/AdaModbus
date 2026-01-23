--  Ada_Modbus.Energy.SunSpec.Inverter - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

package body Ada_Modbus.Energy.SunSpec.Inverter
  with SPARK_Mode => On
is

   --  Helper: Extract signed scale factor from register
   function To_Scale_Factor (Value : Register_Value) return Scale_Factor is
      Raw : constant Integer := Integer (Value);
   begin
      if Raw > 32767 then
         --  Negative (two's complement)
         if Raw - 65536 >= -10 then
            return Scale_Factor (Raw - 65536);
         else
            return -10;
         end if;
      elsif Raw <= 10 then
         return Scale_Factor (Raw);
      else
         return 0;
      end if;
   end To_Scale_Factor;

   ----------------------------------------
   -- Encode_Read_AC_Measurements_Request --
   ----------------------------------------

   procedure Encode_Read_AC_Measurements_Request
     (Base_Address  : Register_Address;
      Model_Offset  : Register_Address;
      Buffer        : out Protocol.PDU_Buffer;
      Length        : out Natural)
   is
   begin
      Protocol.Encode_Read_Registers_Request
        (FC            => FC_Read_Holding_Registers,
         Start_Address => Base_Address + Model_Offset + Reg_AC_Current,
         Quantity      => 16,  --  Registers 2-17
         Buffer        => Buffer,
         Length        => Length);
   end Encode_Read_AC_Measurements_Request;

   ----------------------------------------
   -- Decode_AC_Measurements_Response --
   ----------------------------------------

   procedure Decode_AC_Measurements_Response
     (Buffer       : Protocol.PDU_Buffer;
      Length       : Natural;
      Current_A    : out Float;
      Voltage_V    : out Float;
      Power_W      : out Float;
      Frequency_Hz : out Float;
      Result       : out Status)
   is
      Values    : Register_Array (0 .. 15);
      Reg_Count : Natural;
   begin
      Current_A    := 0.0;
      Voltage_V    := 0.0;
      Power_W      := 0.0;
      Frequency_Hz := 0.0;

      Protocol.Decode_Read_Registers_Response
        (Buffer   => Buffer,
         Length   => Length,
         Values   => Values,
         Count    => Reg_Count,
         Response => Result);

      if Result = Success and then Reg_Count >= 16 then
         --  Offsets relative to our read (started at Reg_AC_Current = 2)
         --  Values(0) = AC Current total
         --  Values(4) = Current SF
         --  Values(8) = Voltage AN (single phase)
         --  Values(11) = Voltage SF
         --  Values(12) = AC Power
         --  Values(13) = Power SF
         --  Values(14) = Frequency
         --  Values(15) = Frequency SF

         declare
            I_SF : constant Scale_Factor := To_Scale_Factor (Values (4));
            V_SF : constant Scale_Factor := To_Scale_Factor (Values (11));
            P_SF : constant Scale_Factor := To_Scale_Factor (Values (13));
            F_SF : constant Scale_Factor := To_Scale_Factor (Values (15));
         begin
            Current_A    := Apply_Scale (Values (0), I_SF);
            Voltage_V    := Apply_Scale (Values (8), V_SF);  --  Voltage AN
            Power_W      := Apply_Scale (Values (12), P_SF);
            Frequency_Hz := Apply_Scale (Values (14), F_SF);
         end;
      end if;
   end Decode_AC_Measurements_Response;

   ----------------------------------------
   -- Encode_Read_DC_Measurements_Request --
   ----------------------------------------

   procedure Encode_Read_DC_Measurements_Request
     (Base_Address  : Register_Address;
      Model_Offset  : Register_Address;
      Buffer        : out Protocol.PDU_Buffer;
      Length        : out Natural)
   is
   begin
      Protocol.Encode_Read_Registers_Request
        (FC            => FC_Read_Holding_Registers,
         Start_Address => Base_Address + Model_Offset + Reg_DC_Current,
         Quantity      => 6,  --  Registers 27-32
         Buffer        => Buffer,
         Length        => Length);
   end Encode_Read_DC_Measurements_Request;

   ----------------------------------------
   -- Decode_DC_Measurements_Response --
   ----------------------------------------

   procedure Decode_DC_Measurements_Response
     (Buffer    : Protocol.PDU_Buffer;
      Length    : Natural;
      Current_A : out Float;
      Voltage_V : out Float;
      Power_W   : out Float;
      Result    : out Status)
   is
      Values    : Register_Array (0 .. 5);
      Reg_Count : Natural;
   begin
      Current_A := 0.0;
      Voltage_V := 0.0;
      Power_W   := 0.0;

      Protocol.Decode_Read_Registers_Response
        (Buffer   => Buffer,
         Length   => Length,
         Values   => Values,
         Count    => Reg_Count,
         Response => Result);

      if Result = Success and then Reg_Count >= 6 then
         --  Values(0) = DC Current, Values(1) = Current SF
         --  Values(2) = DC Voltage, Values(3) = Voltage SF
         --  Values(4) = DC Power, Values(5) = Power SF
         declare
            I_SF : constant Scale_Factor := To_Scale_Factor (Values (1));
            V_SF : constant Scale_Factor := To_Scale_Factor (Values (3));
            P_SF : constant Scale_Factor := To_Scale_Factor (Values (5));
         begin
            Current_A := Apply_Scale (Values (0), I_SF);
            Voltage_V := Apply_Scale (Values (2), V_SF);
            Power_W   := Apply_Scale (Values (4), P_SF);
         end;
      end if;
   end Decode_DC_Measurements_Response;

   --------------------------------
   -- Encode_Read_Energy_Request --
   --------------------------------

   procedure Encode_Read_Energy_Request
     (Base_Address  : Register_Address;
      Model_Offset  : Register_Address;
      Buffer        : out Protocol.PDU_Buffer;
      Length        : out Natural)
   is
   begin
      Protocol.Encode_Read_Registers_Request
        (FC            => FC_Read_Holding_Registers,
         Start_Address => Base_Address + Model_Offset + Reg_AC_Energy,
         Quantity      => 3,  --  Registers 24-26 (32-bit value + SF)
         Buffer        => Buffer,
         Length        => Length);
   end Encode_Read_Energy_Request;

   -----------------------------
   -- Decode_Energy_Response --
   -----------------------------

   procedure Decode_Energy_Response
     (Buffer    : Protocol.PDU_Buffer;
      Length    : Natural;
      Energy_Wh : out Float;
      Result    : out Status)
   is
      Values    : Register_Array (0 .. 2);
      Reg_Count : Natural;
   begin
      Energy_Wh := 0.0;

      Protocol.Decode_Read_Registers_Response
        (Buffer   => Buffer,
         Length   => Length,
         Values   => Values,
         Count    => Reg_Count,
         Response => Result);

      if Result = Success and then Reg_Count >= 3 then
         --  32-bit value in registers 0-1 (big-endian)
         --  Combine words first, then scale to avoid float overflow
         declare
            use type Interfaces.Unsigned_32;
            High_Word : constant Interfaces.Unsigned_32 :=
              Interfaces.Unsigned_32 (Values (0));
            Low_Word  : constant Interfaces.Unsigned_32 :=
              Interfaces.Unsigned_32 (Values (1));
            Raw_Value : constant Interfaces.Unsigned_32 :=
              Interfaces.Shift_Left (High_Word, 16) or Low_Word;
            SF : constant Scale_Factor := To_Scale_Factor (Values (2));
         begin
            --  Convert to float then scale - Apply_Scale uses lookup table
            Energy_Wh := Float (Raw_Value) * Scale_Multipliers (SF);
         end;
      end if;
   end Decode_Energy_Response;

   -------------------------------
   -- Encode_Read_State_Request --
   -------------------------------

   procedure Encode_Read_State_Request
     (Base_Address  : Register_Address;
      Model_Offset  : Register_Address;
      Buffer        : out Protocol.PDU_Buffer;
      Length        : out Natural)
   is
   begin
      Protocol.Encode_Read_Registers_Request
        (FC            => FC_Read_Holding_Registers,
         Start_Address => Base_Address + Model_Offset + Reg_Operating_State,
         Quantity      => 2,
         Buffer        => Buffer,
         Length        => Length);
   end Encode_Read_State_Request;

   ----------------------------
   -- Decode_State_Response --
   ----------------------------

   procedure Decode_State_Response
     (Buffer : Protocol.PDU_Buffer;
      Length : Natural;
      State  : out Inverter_State;
      Result : out Status)
   is
      Values    : Register_Array (0 .. 1);
      Reg_Count : Natural;
   begin
      State := Off;

      Protocol.Decode_Read_Registers_Response
        (Buffer   => Buffer,
         Length   => Length,
         Values   => Values,
         Count    => Reg_Count,
         Response => Result);

      if Result = Success and then Reg_Count >= 1 then
         case Values (0) is
            when 1 => State := Off;
            when 2 => State := Sleeping;
            when 3 => State := Starting;
            when 4 => State := Running;
            when 5 => State := Throttled;
            when 6 => State := Shutting_Down;
            when 7 => State := Fault;
            when 8 => State := Standby;
            when others => State := Off;
         end case;
      end if;
   end Decode_State_Response;

end Ada_Modbus.Energy.SunSpec.Inverter;
