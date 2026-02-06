--  Ada_Modbus.Energy.KSEM - Kostal Smart Energy Meter Device Profile
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Device profile for Kostal Smart Energy Meter (KSEM G1/G2).
--  Uses standard SunSpec protocol - this package provides KSEM-specific
--  defaults and supported model information.
--
--  KSEM Configuration:
--    Port: 502 (standard Modbus TCP)
--    Unit ID: 1 (default)
--    SunSpec Base: 40000 (standard)
--    Enable: Modbus TCP Slave must be enabled in KSEM settings
--
--  Supported SunSpec Models:
--    1       Common (device information)
--    201-204 Meter (energy measurements, 1P/SP/3P-Wye/3P-Delta)
--
--  Typical KSEM delivers Model 203 (Three Phase Wye Meter)

with Ada_Modbus.Energy.SunSpec;

package Ada_Modbus.Energy.KSEM
  with SPARK_Mode => On
is


   --  KSEM connection defaults
   Default_Port    : constant := 502;
   Default_Unit    : constant Unit_Id := 1;
   Default_Base    : constant Register_Address :=
     Ada_Modbus.Energy.SunSpec.Default_Base_Address;

   --  Supported SunSpec model IDs for KSEM
   --  Use these to filter during model discovery
   function Is_Supported_Model
     (ID : Ada_Modbus.Energy.SunSpec.Model_ID) return Boolean
     with Inline;

end Ada_Modbus.Energy.KSEM;
