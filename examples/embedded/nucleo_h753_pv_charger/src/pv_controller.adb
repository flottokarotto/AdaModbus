--  PV_Controller - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with Config;
with KSEM_Client;
with Delta_Client;
with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.Energy.Delta_Charger; use Ada_Modbus.Energy.Delta_Charger;

package body PV_Controller is

   --  Internal state
   Current_State     : Controller_State := Idle;
   Target_Power      : Unsigned_32 := 0;
   Actual_Power      : Unsigned_32 := 0;
   Grid_Power        : Integer_32 := 0;
   Surplus_Power     : Integer_32 := 0;
   Manual_Mode       : Boolean := False;
   Manual_Power      : Unsigned_32 := 0;
   Stats             : Statistics := (0, 0, 0, 0, 0);
   Last_Session_Wh   : Unsigned_32 := 0;

   --  Ramp rate (W per update cycle)
   Ramp_Rate_W : constant := 500;

   --  Error counter for connection issues
   Error_Count : Natural := 0;
   Max_Errors  : constant := 5;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      KSEM_Client.Initialize;
      Delta_Client.Initialize;
      Current_State := Idle;
      Target_Power := 0;
      Error_Count := 0;
      Manual_Mode := False;
   end Initialize;

   ----------------------
   -- Calculate_Target --
   ----------------------

   function Calculate_Target return Unsigned_32 is
      Available : Integer_32;
   begin
      if Manual_Mode then
         return Manual_Power;
      end if;

      --  Available power = current charging + grid surplus
      Available := Integer_32 (Actual_Power) + Grid_Power;

      --  Account for import threshold (allow small import)
      Available := Available + Integer_32 (Config.Grid_Import_Threshold_W);

      --  Clamp to valid range
      if Available < Integer_32 (Config.Min_Charging_Power_W) then
         return 0;  --  Not enough surplus
      elsif Available > Integer_32 (Config.Max_Charging_Power_W) then
         return Unsigned_32 (Config.Max_Charging_Power_W);
      else
         return Unsigned_32 (Available);
      end if;
   end Calculate_Target;

   --------------------
   -- Apply_Hysteresis --
   --------------------

   function Should_Start_Charging return Boolean is
   begin
      --  Start only if surplus is above minimum + hysteresis
      return Surplus_Power >= Integer_32 (Config.Min_Charging_Power_W +
                                          Config.Power_Hysteresis_W);
   end Should_Start_Charging;

   function Should_Stop_Charging return Boolean is
   begin
      --  Stop only if surplus is below minimum - hysteresis
      return Surplus_Power < Integer_32 (Config.Min_Charging_Power_W) -
                             Integer_32 (Config.Power_Hysteresis_W);
   end Should_Stop_Charging;

   -------------------
   -- Update_Status --
   -------------------

   procedure Update_Status is
      KSEM_Result  : Status;
      Delta_Result : Status;
      Power_Data   : KSEM_Client.Power_Data;
      EVSE         : EVSE_Status;
   begin
      --  Read KSEM power
      KSEM_Client.Read_Power (Power_Data, KSEM_Result);
      if KSEM_Result = Success and then Power_Data.Valid then
         Grid_Power := Power_Data.Total_Power_W;
         Error_Count := 0;
      else
         Error_Count := Error_Count + 1;
      end if;

      --  Read Delta status
      Delta_Client.Read_Status (EVSE, Delta_Result);
      if Delta_Result = Success then
         Actual_Power := EVSE.Power_W;
         Error_Count := 0;

         --  Update session energy
         if EVSE.Energy_Wh > Last_Session_Wh then
            Stats.Session_Energy_Wh := EVSE.Energy_Wh - Last_Session_Wh;
         end if;
         Stats.Session_Time_S := EVSE.Charging_Time_S;

         --  Track peak power
         if EVSE.Power_W > Stats.Peak_Power_W then
            Stats.Peak_Power_W := EVSE.Power_W;
         end if;
      else
         Error_Count := Error_Count + 1;
      end if;

      --  Calculate surplus (positive = available for charging)
      Surplus_Power := Grid_Power + Integer_32 (Actual_Power);

      --  Check for persistent errors
      if Error_Count >= Max_Errors then
         Current_State := Error;
      end if;
   end Update_Status;

   ---------------
   -- Apply_Power --
   ---------------

   procedure Apply_Power (New_Power : Unsigned_32) is
      Result : Status;
      Ramped_Power : Unsigned_32;
   begin
      --  Apply ramping to avoid sudden changes
      if New_Power > Target_Power then
         --  Ramping up
         if New_Power - Target_Power > Unsigned_32 (Ramp_Rate_W) then
            Ramped_Power := Target_Power + Unsigned_32 (Ramp_Rate_W);
         else
            Ramped_Power := New_Power;
         end if;
      elsif New_Power < Target_Power then
         --  Ramping down
         if Target_Power - New_Power > Unsigned_32 (Ramp_Rate_W) then
            Ramped_Power := Target_Power - Unsigned_32 (Ramp_Rate_W);
         else
            Ramped_Power := New_Power;
         end if;
      else
         Ramped_Power := New_Power;
      end if;

      --  Apply power limit to wallbox
      if Ramped_Power /= Target_Power then
         Delta_Client.Set_Power_Limit (Ramped_Power, Result);
         if Result = Success then
            Target_Power := Ramped_Power;
            Stats.Adjustments_Count := Stats.Adjustments_Count + 1;
         end if;
      end if;
   end Apply_Power;

   ------------
   -- Update --
   ------------

   procedure Update is
      EVSE       : EVSE_Status;
      New_Target : Unsigned_32;
      Result     : Status;
   begin
      --  Read current status
      Update_Status;

      --  Get current EVSE state
      EVSE := Delta_Client.Get_Last_Status;

      --  State machine
      case Current_State is
         when Idle =>
            --  Check if car is connected and ready
            if Delta_Client.Is_Available then
               if Should_Start_Charging then
                  Current_State := Starting;
                  Last_Session_Wh := EVSE.Energy_Wh;  --  Track session start
                  Stats.Session_Energy_Wh := 0;
               end if;
            end if;

         when Starting =>
            --  Ramp up to target power
            New_Target := Calculate_Target;
            if New_Target >= Unsigned_32 (Config.Min_Charging_Power_W) then
               Apply_Power (New_Target);
               --  Resume charging if suspended
               Delta_Client.Set_Suspend (False, Result);
               if Delta_Client.Is_Charging then
                  Current_State := Charging;
               end if;
            else
               Current_State := Suspending;
            end if;

         when Charging =>
            --  Adjust power based on surplus
            New_Target := Calculate_Target;
            if Should_Stop_Charging then
               Current_State := Suspending;
            else
               Apply_Power (New_Target);
            end if;

            --  Check if car stopped charging
            if not Delta_Client.Is_Available then
               Current_State := Idle;
               Stats.Total_Energy_Wh := Stats.Total_Energy_Wh +
                                        Stats.Session_Energy_Wh;
            end if;

         when Suspending =>
            --  Ramp down and suspend
            Apply_Power (0);
            if Target_Power = 0 then
               Delta_Client.Set_Suspend (True, Result);
               Current_State := Suspended;
            end if;

         when Suspended =>
            --  Wait for surplus to return
            if Should_Start_Charging then
               Current_State := Starting;
            end if;

            --  Check if car disconnected
            if not Delta_Client.Is_Available then
               Current_State := Idle;
            end if;

         when Error =>
            --  Try to recover
            Error_Count := 0;
            Current_State := Idle;
      end case;
   end Update;

   ---------------
   -- Get_State --
   ---------------

   function Get_State return Controller_State is
   begin
      return Current_State;
   end Get_State;

   ------------------------
   -- Get_Target_Power_W --
   ------------------------

   function Get_Target_Power_W return Unsigned_32 is
   begin
      return Target_Power;
   end Get_Target_Power_W;

   ------------------------
   -- Get_Actual_Power_W --
   ------------------------

   function Get_Actual_Power_W return Unsigned_32 is
   begin
      return Actual_Power;
   end Get_Actual_Power_W;

   ----------------------
   -- Get_Grid_Power_W --
   ----------------------

   function Get_Grid_Power_W return Integer_32 is
   begin
      return Grid_Power;
   end Get_Grid_Power_W;

   -------------------------
   -- Get_Surplus_Power_W --
   -------------------------

   function Get_Surplus_Power_W return Integer_32 is
   begin
      return Surplus_Power;
   end Get_Surplus_Power_W;

   --------------------
   -- Get_Statistics --
   --------------------

   function Get_Statistics return Statistics is
   begin
      return Stats;
   end Get_Statistics;

   ----------------------
   -- Reset_Statistics --
   ----------------------

   procedure Reset_Statistics is
   begin
      Stats := (0, 0, 0, 0, 0);
   end Reset_Statistics;

   -------------------
   -- Force_Suspend --
   -------------------

   procedure Force_Suspend is
   begin
      Current_State := Suspending;
      Manual_Mode := False;
   end Force_Suspend;

   ------------------
   -- Force_Resume --
   ------------------

   procedure Force_Resume is
   begin
      if Delta_Client.Is_Available then
         Current_State := Starting;
      end if;
      Manual_Mode := False;
   end Force_Resume;

   ----------------------
   -- Set_Manual_Power --
   ----------------------

   procedure Set_Manual_Power (Power_W : Unsigned_32) is
   begin
      if Power_W = 0 then
         Manual_Mode := False;
      else
         Manual_Mode := True;
         Manual_Power := Power_W;
         Current_State := Starting;
      end if;
   end Set_Manual_Power;

   --------------------
   -- Is_Manual_Mode --
   --------------------

   function Is_Manual_Mode return Boolean is
   begin
      return Manual_Mode;
   end Is_Manual_Mode;

end PV_Controller;
