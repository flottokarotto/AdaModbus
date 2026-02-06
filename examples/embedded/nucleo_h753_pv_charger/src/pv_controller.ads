--  PV_Controller - PV Surplus Charging Control Logic
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Implements PV surplus charging strategy:
--    1. Read grid power from KSEM
--    2. Calculate available PV surplus
--    3. Adjust wallbox power limit
--    4. Suspend charging if surplus too low
--
--  Strategy:
--    - Positive grid power = export to grid = PV surplus
--    - Negative grid power = import from grid = no surplus
--    - Target: minimize grid exchange while maximizing EV charging
--
--  Hysteresis to prevent oscillation:
--    - Start charging only if surplus > Min_Power + Hysteresis
--    - Stop charging only if surplus < Min_Power - Hysteresis

with Interfaces; use Interfaces;

package PV_Controller is

   --  Controller state
   type Controller_State is
     (Idle,           --  No car connected or waiting
      Starting,       --  Starting charge (ramp up)
      Charging,       --  Actively charging
      Suspending,     --  Stopping charge (surplus too low)
      Suspended,      --  Charge suspended, waiting for surplus
      Error);         --  Communication error

   --  Controller statistics
   type Statistics is record
      Total_Energy_Wh    : Unsigned_32;  --  Total energy charged
      Session_Energy_Wh  : Unsigned_32;  --  Current session energy
      Peak_Power_W       : Unsigned_32;  --  Peak charging power
      Session_Time_S     : Unsigned_32;  --  Current session time
      Adjustments_Count  : Natural;      --  Number of power adjustments
   end record;

   --  Initialize the PV controller
   procedure Initialize;

   --  Main control loop iteration
   --  Call this periodically (e.g., every 2 seconds)
   procedure Update;

   --  Get current controller state
   function Get_State return Controller_State;

   --  Get current target power (what we're trying to achieve)
   function Get_Target_Power_W return Unsigned_32;

   --  Get current actual power (what wallbox is delivering)
   function Get_Actual_Power_W return Unsigned_32;

   --  Get current grid power (positive = export, negative = import)
   function Get_Grid_Power_W return Integer_32;

   --  Get current PV surplus (available for charging)
   function Get_Surplus_Power_W return Integer_32;

   --  Get statistics
   function Get_Statistics return Statistics;

   --  Reset statistics
   procedure Reset_Statistics;

   --  Manual override: force suspend
   procedure Force_Suspend;

   --  Manual override: force resume
   procedure Force_Resume;

   --  Manual override: set fixed power (0 = auto mode)
   procedure Set_Manual_Power (Power_W : Unsigned_32);

   --  Check if in manual mode
   function Is_Manual_Mode return Boolean;

end PV_Controller;
