--  Ada_Modbus.Energy.Kostal - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

package body Ada_Modbus.Energy.Kostal
  with SPARK_Mode => On
is

   use Ada_Modbus.Energy.SunSpec;

   ------------------------
   -- Is_Supported_Model --
   ------------------------

   function Is_Supported_Model
     (ID : Ada_Modbus.Energy.SunSpec.Model_ID) return Boolean
   is
   begin
      return Natural (ID) in
        Model_Common |
        Model_Inverter_1P | Model_Inverter_SP | Model_Inverter_3P |
        Model_Nameplate | Model_Basic_Settings |
        Model_Storage | Model_MPPT |
        701 | 704 | 802;
   end Is_Supported_Model;

end Ada_Modbus.Energy.Kostal;
