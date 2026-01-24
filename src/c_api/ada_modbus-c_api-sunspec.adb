--  Ada_Modbus.C_API.SunSpec - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with Interfaces; use Interfaces;

with Ada_Modbus.Energy.SunSpec;
with Ada_Modbus.Energy.SunSpec.Meter;

package body Ada_Modbus.C_API.SunSpec is

   use Ada_Modbus.Energy.SunSpec;

   --  Helper: Read registers via master handle
   function Read_Regs
     (Handle   : C_Master_Handle;
      Slave    : unsigned_char;
      Address  : unsigned_short;
      Quantity : Natural;
      Regs     : out Register_Array) return C_Status
   is
      --  Import the internal read function
      function Internal_Read
        (H : C_Master_Handle;
         S : unsigned_char;
         A : unsigned_short;
         Q : unsigned_short;
         V : access unsigned_short;
         T : int) return C_Status
        with Import, Convention => C,
             External_Name => "modbus_read_holding_registers";

      Temp : aliased array (0 .. Quantity - 1) of aliased unsigned_short;
      Result : C_Status;
   begin
      Result := Internal_Read
        (Handle, Slave, Address, unsigned_short (Quantity),
         Temp (0)'Access, 3000);

      if Result = C_Success then
         for I in 0 .. Quantity - 1 loop
            Regs (Regs'First + I) := Register_Value (Temp (I));
         end loop;
      end if;

      return Result;
   end Read_Regs;

   --  Helper: Copy string to C char_array
   procedure Copy_String
     (Source : String;
      Target : out char_array;
      Length : Natural)
   is
      Copy_Len : constant Natural := Natural'Min (Length, Target'Length - 1);
   begin
      Target := [others => nul];
      for I in 0 .. Copy_Len - 1 loop
         if I < Source'Length then
            Target (size_t (I)) := char (Source (Source'First + I));
         end if;
      end loop;
   end Copy_String;

   -------------------
   -- SunSpec_Check --
   -------------------

   function SunSpec_Check
     (Master_Handle : C_Master_Handle;
      Slave         : unsigned_char;
      Base_Address  : unsigned_short) return C_Status
   is
      Regs   : Register_Array (0 .. 1);
      Result : C_Status;
   begin
      Result := Read_Regs (Master_Handle, Slave, Base_Address, 2, Regs);
      if Result /= C_Success then
         return Result;
      end if;

      if Regs (0) = SunS_ID_High and Regs (1) = SunS_ID_Low then
         return C_Success;
      else
         return C_Invalid_Response;
      end if;
   end SunSpec_Check;

   ------------------------------
   -- SunSpec_Read_Model_Header --
   ------------------------------

   function SunSpec_Read_Model_Header
     (Master_Handle : C_Master_Handle;
      Slave         : unsigned_char;
      Address       : unsigned_short;
      Header        : access C_SunSpec_Model_Header) return C_Status
   is
      Regs   : Register_Array (0 .. 1);
      Result : C_Status;
   begin
      Result := Read_Regs (Master_Handle, Slave, Address, 2, Regs);
      if Result /= C_Success then
         return Result;
      end if;

      Header.Model_ID := unsigned_short (Regs (0));
      Header.Length   := unsigned_short (Regs (1));
      Header.Address  := Address;

      return C_Success;
   end SunSpec_Read_Model_Header;

   -----------------------
   -- SunSpec_Find_Model --
   -----------------------

   function SunSpec_Find_Model
     (Master_Handle : C_Master_Handle;
      Slave         : unsigned_char;
      Start_Address : unsigned_short;
      Model_ID      : unsigned_short;
      Header        : access C_SunSpec_Model_Header) return C_Status
   is
      Addr   : unsigned_short := Start_Address;
      Result : C_Status;
      Max_Iterations : constant := 50;
   begin
      for I in 1 .. Max_Iterations loop
         Result := SunSpec_Read_Model_Header (Master_Handle, Slave, Addr, Header);
         if Result /= C_Success then
            return Result;
         end if;

         --  End of model list
         if Header.Model_ID = unsigned_short (C_Model_End) then
            return C_Not_Implemented;
         end if;

         --  Found requested model
         if Header.Model_ID = Model_ID then
            return C_Success;
         end if;

         --  Move to next model
         Addr := Addr + Header.Length + 2;
      end loop;

      return C_Not_Implemented;
   end SunSpec_Find_Model;

   ------------------------
   -- SunSpec_Read_Common --
   ------------------------

   function SunSpec_Read_Common
     (Master_Handle : C_Master_Handle;
      Slave         : unsigned_char;
      Model_Address : unsigned_short;
      Data          : access C_SunSpec_Common) return C_Status
   is
      Regs    : Register_Array (0 .. 65);
      Result  : C_Status;
      Str_Val : SunSpec_String;
      Str_Len : Natural;
   begin
      --  Read all common model registers (66 data registers after header)
      Result := Read_Regs (Master_Handle, Slave,
                           Model_Address + 2, 66, Regs);
      if Result /= C_Success then
         return Result;
      end if;

      --  Decode strings
      declare
         Mfr_Regs : Register_Array (0 .. 15);
         Mdl_Regs : Register_Array (0 .. 15);
         Ser_Regs : Register_Array (0 .. 15);
         Ver_Regs : Register_Array (0 .. 7);
      begin
         --  Copy register slices
         for I in 0 .. 15 loop
            Mfr_Regs (I) := Regs (I);                    --  Offset 0-15
            Mdl_Regs (I) := Regs (16 + I);               --  Offset 16-31
            Ser_Regs (I) := Regs (32 + I);               --  Offset 32-47
         end loop;
         for I in 0 .. 7 loop
            Ver_Regs (I) := Regs (48 + I);               --  Offset 48-55
         end loop;

         Decode_String (Mfr_Regs, Str_Val, Str_Len);
         Copy_String (Str_Val (1 .. Str_Len), Data.Manufacturer, Str_Len);

         Decode_String (Mdl_Regs, Str_Val, Str_Len);
         Copy_String (Str_Val (1 .. Str_Len), Data.Model, Str_Len);

         Decode_String (Ser_Regs, Str_Val, Str_Len);
         Copy_String (Str_Val (1 .. Str_Len), Data.Serial, Str_Len);

         Decode_String (Ver_Regs, Str_Val, Str_Len);
         Copy_String (Str_Val (1 .. Str_Len), Data.Version, Str_Len);
      end;

      return C_Success;
   end SunSpec_Read_Common;

   --------------------------
   -- SunSpec_Read_Inverter --
   --------------------------

   function SunSpec_Read_Inverter
     (Master_Handle : C_Master_Handle;
      Slave         : unsigned_char;
      Model_Address : unsigned_short;
      Data          : access C_SunSpec_Inverter) return C_Status
   is
      Regs   : Register_Array (0 .. 49);
      Result : C_Status;
   begin
      --  Read inverter model registers
      Result := Read_Regs (Master_Handle, Slave,
                           Model_Address + 2, 50, Regs);
      if Result /= C_Success then
         return Result;
      end if;

      --  AC measurements
      declare
         I_SF : constant Scale_Factor := To_Scale_Factor (Regs (4));
         V_SF : constant Scale_Factor := To_Scale_Factor (Regs (11));
         P_SF : constant Scale_Factor := To_Scale_Factor (Regs (13));
         F_SF : constant Scale_Factor := To_Scale_Factor (Regs (15));
      begin
         Data.AC_Current_A    := C_float (Apply_Scale (Regs (0), I_SF));
         Data.AC_Voltage_V    := C_float (Apply_Scale (Regs (8), V_SF));
         Data.AC_Power_W      := C_float (Apply_Scale (Regs (12), P_SF));
         Data.AC_Frequency_Hz := C_float (Apply_Scale (Regs (14), F_SF));
      end;

      --  Energy (32-bit)
      declare
         Energy_Raw : constant Unsigned_32 :=
           Unsigned_32 (Regs (22)) * 65536 + Unsigned_32 (Regs (23));
         Energy_SF  : constant Scale_Factor := To_Scale_Factor (Regs (24));
      begin
         Data.AC_Energy_Wh := C_float (Float (Energy_Raw) * Scale_Multipliers (Energy_SF));
      end;

      --  DC measurements
      declare
         DCI_SF : constant Scale_Factor := To_Scale_Factor (Regs (26));
         DCV_SF : constant Scale_Factor := To_Scale_Factor (Regs (28));
         DCW_SF : constant Scale_Factor := To_Scale_Factor (Regs (30));
      begin
         if Is_Implemented (Regs (25)) then
            Data.DC_Current_A := C_float (Apply_Scale (Regs (25), DCI_SF));
         else
            Data.DC_Current_A := 0.0;
         end if;

         if Is_Implemented (Regs (27)) then
            Data.DC_Voltage_V := C_float (Apply_Scale (Regs (27), DCV_SF));
         else
            Data.DC_Voltage_V := 0.0;
         end if;

         if Is_Implemented (Regs (29)) then
            Data.DC_Power_W := C_float (Apply_Scale (Regs (29), DCW_SF));
         else
            Data.DC_Power_W := 0.0;
         end if;
      end;

      --  Temperature
      declare
         Temp_SF : constant Scale_Factor := To_Scale_Factor (Regs (35));
      begin
         if Is_Implemented (Regs (31)) then
            Data.Cabinet_Temp_C := C_float (Apply_Scale_Signed (Regs (31), Temp_SF));
         else
            Data.Cabinet_Temp_C := 0.0;
         end if;
      end;

      --  State
      Data.Operating_State := int (Regs (36));

      return C_Success;
   end SunSpec_Read_Inverter;

   -----------------------
   -- SunSpec_Read_Meter --
   -----------------------

   function SunSpec_Read_Meter
     (Master_Handle : C_Master_Handle;
      Slave         : unsigned_char;
      Model_Address : unsigned_short;
      Data          : access C_SunSpec_Meter) return C_Status
   is
      use Ada_Modbus.Energy.SunSpec.Meter;
      Regs   : Register_Array (0 .. 54);
      SF     : Meter_Scale_Factors;
      Result : C_Status;
   begin
      Result := Read_Regs (Master_Handle, Slave,
                           Model_Address + 2, 55, Regs);
      if Result /= C_Success then
         return Result;
      end if;

      Decode_Meter_Scale_Factors (Regs, SF);

      --  Totals
      Data.Total_Current_A := C_float (Apply_Scale (Regs (0), SF.A_SF));
      Data.Total_Voltage_V := C_float (Apply_Scale (Regs (5), SF.V_SF));
      Data.Frequency_Hz    := C_float (Apply_Scale (Regs (14), SF.Hz_SF));
      Data.Total_Power_W   := C_float (Apply_Scale_Signed (Regs (16), SF.W_SF));
      Data.Total_VA        := C_float (Apply_Scale_Signed (Regs (21), SF.VA_SF));
      Data.Total_VAR       := C_float (Apply_Scale_Signed (Regs (26), SF.VAR_SF));
      Data.Power_Factor    := C_float (Apply_Scale_Signed (Regs (31), SF.PF_SF));

      --  Energy (32-bit values)
      declare
         Exp_Raw : constant Unsigned_32 :=
           Unsigned_32 (Regs (36)) * 65536 + Unsigned_32 (Regs (37));
         Imp_Raw : constant Unsigned_32 :=
           Unsigned_32 (Regs (44)) * 65536 + Unsigned_32 (Regs (45));
      begin
         Data.Export_Wh := C_float (Float (Exp_Raw) * Scale_Multipliers (SF.Wh_SF));
         Data.Import_Wh := C_float (Float (Imp_Raw) * Scale_Multipliers (SF.Wh_SF));
      end;

      --  Per-phase
      Data.L1_Current_A := C_float (Apply_Scale (Regs (1), SF.A_SF));
      Data.L2_Current_A := C_float (Apply_Scale (Regs (2), SF.A_SF));
      Data.L3_Current_A := C_float (Apply_Scale (Regs (3), SF.A_SF));

      Data.L1_Voltage_V := C_float (Apply_Scale (Regs (6), SF.V_SF));
      Data.L2_Voltage_V := C_float (Apply_Scale (Regs (7), SF.V_SF));
      Data.L3_Voltage_V := C_float (Apply_Scale (Regs (8), SF.V_SF));

      Data.L1_Power_W := C_float (Apply_Scale_Signed (Regs (17), SF.W_SF));
      Data.L2_Power_W := C_float (Apply_Scale_Signed (Regs (18), SF.W_SF));
      Data.L3_Power_W := C_float (Apply_Scale_Signed (Regs (19), SF.W_SF));

      return C_Success;
   end SunSpec_Read_Meter;

   -------------------------
   -- SunSpec_Read_Battery --
   -------------------------

   function SunSpec_Read_Battery
     (Master_Handle : C_Master_Handle;
      Slave         : unsigned_char;
      Model_Address : unsigned_short;
      Data          : access C_SunSpec_Battery) return C_Status
   is
      Regs   : Register_Array (0 .. 17);
      Result : C_Status;
   begin
      --  Try Model 124 first (Basic Storage)
      Result := Read_Regs (Master_Handle, Slave,
                           Model_Address + 2, 18, Regs);
      if Result /= C_Success then
         return Result;
      end if;

      --  Decode Model 124
      declare
         ChgRate_SF : constant Scale_Factor := To_Scale_Factor (Regs (1));
         DisRate_SF : constant Scale_Factor := To_Scale_Factor (Regs (3));
         Cap_SF     : constant Scale_Factor := To_Scale_Factor (Regs (7));
         SOC_SF     : constant Scale_Factor := To_Scale_Factor (Regs (9));
         SOH_SF     : constant Scale_Factor := To_Scale_Factor (Regs (11));
      begin
         Data.Max_Charge_W    := C_float (Apply_Scale (Regs (0), ChgRate_SF));
         Data.Max_Discharge_W := C_float (Apply_Scale (Regs (2), DisRate_SF));
         Data.Capacity_Wh     := C_float (Apply_Scale (Regs (6), Cap_SF));
         Data.SOC_Percent     := C_float (Apply_Scale (Regs (8), SOC_SF));
         Data.SOH_Percent     := C_float (Apply_Scale (Regs (10), SOH_SF));
         Data.State           := int (Regs (13));
      end;

      --  Fields not in Model 124
      Data.Voltage_V    := 0.0;
      Data.Current_A    := 0.0;
      Data.Power_W      := 0.0;
      Data.Cycle_Count  := 0;
      Data.Cell_V_Max   := 0.0;
      Data.Cell_V_Min   := 0.0;

      return C_Success;
   end SunSpec_Read_Battery;

end Ada_Modbus.C_API.SunSpec;
