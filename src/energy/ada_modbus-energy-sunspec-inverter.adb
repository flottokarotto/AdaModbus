--  Ada_Modbus.Energy.SunSpec.Inverter - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

package body Ada_Modbus.Energy.SunSpec.Inverter
  with SPARK_Mode => On
is
   --  Note: To_Scale_Factor is now exported from parent package SunSpec

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

   -------------------------
   -- MPPT Model 160      --
   -------------------------

   ------------------------
   -- Decode_MPPT_Header --
   ------------------------

   procedure Decode_MPPT_Header
     (Regs   : Register_Array;
      Header : out MPPT_Header)
   is
   begin
      Header.DCA_SF := To_Scale_Factor (Regs (Regs'First));
      Header.DCV_SF := To_Scale_Factor (Regs (Regs'First + 1));
      Header.DCW_SF := To_Scale_Factor (Regs (Regs'First + 2));
      Header.Num_Modules := Natural (Regs (Regs'First + 6));
   end Decode_MPPT_Header;

   ------------------------
   -- Decode_MPPT_Module --
   ------------------------

   procedure Decode_MPPT_Module
     (Regs   : Register_Array;
      Header : MPPT_Header;
      Module : out MPPT_Module_Data)
   is
      Base      : constant Natural := Regs'First;
      DCA_Reg   : constant Register_Value := Regs (Base + MPPT_Mod_DCA);
      DCV_Reg   : constant Register_Value := Regs (Base + MPPT_Mod_DCV);
      DCW_Reg   : constant Register_Value := Regs (Base + MPPT_Mod_DCW);
      State_Reg : constant Register_Value := Regs (Base + MPPT_Mod_DCSt);
   begin
      Module.Module_ID := Natural (Regs (Base + MPPT_Mod_ID));

      --  Check if values are implemented
      Module.Is_Valid := Is_Implemented (DCA_Reg) or else
                         Is_Implemented (DCV_Reg) or else
                         Is_Implemented (DCW_Reg);

      if Is_Implemented (DCA_Reg) then
         Module.Current_A := Apply_Scale (DCA_Reg, Header.DCA_SF);
      else
         Module.Current_A := 0.0;
      end if;

      if Is_Implemented (DCV_Reg) then
         Module.Voltage_V := Apply_Scale (DCV_Reg, Header.DCV_SF);
      else
         Module.Voltage_V := 0.0;
      end if;

      if Is_Implemented (DCW_Reg) then
         Module.Power_W := Apply_Scale (DCW_Reg, Header.DCW_SF);
      else
         Module.Power_W := 0.0;
      end if;

      --  Decode operating state
      case State_Reg is
         when 1 => Module.State := MPPT_Off;
         when 2 => Module.State := MPPT_Sleeping;
         when 3 => Module.State := MPPT_Starting;
         when 4 => Module.State := MPPT_Running;
         when 5 => Module.State := MPPT_Throttled;
         when 6 => Module.State := MPPT_Shutting_Down;
         when 7 => Module.State := MPPT_Fault;
         when 8 => Module.State := MPPT_Standby;
         when others => Module.State := MPPT_Unknown;
      end case;
   end Decode_MPPT_Module;

end Ada_Modbus.Energy.SunSpec.Inverter;
