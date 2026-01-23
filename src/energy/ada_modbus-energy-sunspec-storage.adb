--  Ada_Modbus.Energy.SunSpec.Storage - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

package body Ada_Modbus.Energy.SunSpec.Storage
  with SPARK_Mode => On
is

   --  Helper: Extract signed scale factor from register
   function To_Scale_Factor (Value : Register_Value) return Scale_Factor is
      Raw : constant Integer := Integer (Value);
   begin
      if Raw > 32767 then
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

   --------------------------------
   -- Encode_Read_Status_Request --
   --------------------------------

   procedure Encode_Read_Status_Request
     (Base_Address  : Register_Address;
      Model_Offset  : Register_Address;
      Buffer        : out Protocol.PDU_Buffer;
      Length        : out Natural)
   is
   begin
      Protocol.Encode_Read_Registers_Request
        (FC            => FC_Read_Holding_Registers,
         Start_Address => Base_Address + Model_Offset + Reg_SOC,
         Quantity      => 6,  --  Registers 10-15
         Buffer        => Buffer,
         Length        => Length);
   end Encode_Read_Status_Request;

   -----------------------------
   -- Decode_Status_Response --
   -----------------------------

   procedure Decode_Status_Response
     (Buffer      : Protocol.PDU_Buffer;
      Length      : Natural;
      SOC_Percent : out Float;
      SOH_Percent : out Float;
      Status      : out Storage_Status;
      Result      : out Ada_Modbus.Status)
   is
      Values    : Register_Array (0 .. 5);
      Reg_Count : Natural;
   begin
      SOC_Percent := 0.0;
      SOH_Percent := 0.0;
      Status      := Off;

      Protocol.Decode_Read_Registers_Response
        (Buffer   => Buffer,
         Length   => Length,
         Values   => Values,
         Count    => Reg_Count,
         Response => Result);

      if Result = Success and then Reg_Count >= 6 then
         --  Values(0) = SOC, Values(1) = SOC SF
         --  Values(2) = SOH, Values(3) = SOH SF
         --  Values(4) = Status, Values(5) = Charge Status
         declare
            SOC_SF : constant Scale_Factor := To_Scale_Factor (Values (1));
            SOH_SF : constant Scale_Factor := To_Scale_Factor (Values (3));
         begin
            SOC_Percent := Apply_Scale (Values (0), SOC_SF);
            SOH_Percent := Apply_Scale (Values (2), SOH_SF);

            case Values (4) is
               when 1 => Status := Off;
               when 2 => Status := Empty;
               when 3 => Status := Discharging;
               when 4 => Status := Charging;
               when 5 => Status := Full;
               when 6 => Status := Holding;
               when 7 => Status := Testing;
               when others => Status := Off;
            end case;
         end;
      end if;
   end Decode_Status_Response;

   ----------------------------------
   -- Encode_Read_Capacity_Request --
   ----------------------------------

   procedure Encode_Read_Capacity_Request
     (Base_Address  : Register_Address;
      Model_Offset  : Register_Address;
      Buffer        : out Protocol.PDU_Buffer;
      Length        : out Natural)
   is
   begin
      Protocol.Encode_Read_Registers_Request
        (FC            => FC_Read_Holding_Registers,
         Start_Address => Base_Address + Model_Offset + Reg_Max_Charge_Rate,
         Quantity      => 8,  --  Registers 2-9
         Buffer        => Buffer,
         Length        => Length);
   end Encode_Read_Capacity_Request;

   -------------------------------
   -- Decode_Capacity_Response --
   -------------------------------

   procedure Decode_Capacity_Response
     (Buffer          : Protocol.PDU_Buffer;
      Length          : Natural;
      Max_Charge_W    : out Float;
      Max_Discharge_W : out Float;
      Stored_Wh       : out Float;
      Capacity_Wh     : out Float;
      Result          : out Ada_Modbus.Status)
   is
      Values    : Register_Array (0 .. 7);
      Reg_Count : Natural;
   begin
      Max_Charge_W    := 0.0;
      Max_Discharge_W := 0.0;
      Stored_Wh       := 0.0;
      Capacity_Wh     := 0.0;

      Protocol.Decode_Read_Registers_Response
        (Buffer   => Buffer,
         Length   => Length,
         Values   => Values,
         Count    => Reg_Count,
         Response => Result);

      if Result = Success and then Reg_Count >= 8 then
         declare
            Charge_SF   : constant Scale_Factor := To_Scale_Factor (Values (1));
            Disch_SF    : constant Scale_Factor := To_Scale_Factor (Values (3));
            Stored_SF   : constant Scale_Factor := To_Scale_Factor (Values (5));
            Capacity_SF : constant Scale_Factor := To_Scale_Factor (Values (7));
         begin
            Max_Charge_W    := Apply_Scale (Values (0), Charge_SF);
            Max_Discharge_W := Apply_Scale (Values (2), Disch_SF);
            Stored_Wh       := Apply_Scale (Values (4), Stored_SF);
            Capacity_Wh     := Apply_Scale (Values (6), Capacity_SF);
         end;
      end if;
   end Decode_Capacity_Response;

   ---------------------------------
   -- Encode_Set_Control_Request --
   ---------------------------------

   procedure Encode_Set_Control_Request
     (Mode   : Charge_Control;
      Buffer : out Protocol.PDU_Buffer;
      Length : out Natural)
   is
      Value : Register_Value;
   begin
      case Mode is
         when Off       => Value := 0;
         when Charge    => Value := 1;
         when Discharge => Value := 2;
      end case;

      --  Note: This encodes the function code and value only
      --  The actual register address depends on the model offset
      Buffer := [others => 0];
      Buffer (0) := Byte (FC_Write_Single_Register);
      Buffer (1) := 0;  --  Placeholder for address high
      Buffer (2) := 0;  --  Placeholder for address low
      Buffer (3) := Byte (Value / 256);
      Buffer (4) := Byte (Value mod 256);
      Length := 5;
   end Encode_Set_Control_Request;

   -------------------------------
   -- Encode_Set_Power_Request --
   -------------------------------

   procedure Encode_Set_Power_Request
     (Base_Address  : Register_Address;
      Model_Offset  : Register_Address;
      Power_W       : Integer;
      Buffer        : out Protocol.PDU_Buffer;
      Length        : out Natural)
   is
      Reg_Addr : constant Register_Address :=
        Base_Address + Model_Offset + Reg_InOut_Power;
      Value    : Register_Value;
   begin
      --  Convert signed integer to unsigned register value
      if Power_W >= 0 then
         if Power_W <= 32767 then
            Value := Register_Value (Power_W);
         else
            Value := 32767;  --  Clamp to max positive
         end if;
      else
         --  Two's complement for negative values
         if Power_W >= -32768 then
            Value := Register_Value (65536 + Power_W);
         else
            Value := 32768;  --  Clamp to max negative
         end if;
      end if;

      Protocol.Encode_Write_Single_Register_Request
        (Address => Reg_Addr,
         Value   => Value,
         Buffer  => Buffer,
         Length  => Length);
   end Encode_Set_Power_Request;

end Ada_Modbus.Energy.SunSpec.Storage;
