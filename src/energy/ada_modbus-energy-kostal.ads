--  Ada_Modbus.Energy.Kostal - Kostal Inverter Device Profile
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Device profile for Kostal solar inverters (PLENTICORE plus, PIKO IQ/CI).
--  Uses standard SunSpec protocol - this package provides Kostal-specific
--  defaults and supported model information.
--
--  Kostal Configuration:
--    Port: 1502 (Kostal default, not standard 502)
--    Unit ID: 71 (default)
--    SunSpec Base: 40000 (standard)
--    Enable: Inverter Settings -> Modbus/SunSpec (TCP) -> Activate
--
--  Supported SunSpec Models:
--    1       Common (device information)
--    101-103 Inverter (AC/DC measurements, state)
--    120     Nameplate (ratings, DER type)
--    121     Basic Settings (operational limits)
--    124     Storage (battery SOC, capacity)
--    160     MPPT (per-string DC measurements)
--    701     DER AC Measurements
--    704     DER AC Controls (power limiting)
--    802     Battery Extended (cell data, alarms)

with Ada_Modbus.Energy.SunSpec;

package Ada_Modbus.Energy.Kostal
  with SPARK_Mode => On
is


   --  Kostal connection defaults
   Default_Port    : constant := 1502;
   Default_Unit    : constant Unit_Id := 71;
   Default_Base    : constant Register_Address :=
     Ada_Modbus.Energy.SunSpec.Default_Base_Address;

   --  Supported SunSpec model IDs for Kostal inverters
   --  Use these to filter during model discovery
   function Is_Supported_Model
     (ID : Ada_Modbus.Energy.SunSpec.Model_ID) return Boolean
     with Inline;

end Ada_Modbus.Energy.Kostal;
