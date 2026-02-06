--  Ada_Modbus.Energy.KSEM - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

package body Ada_Modbus.Energy.KSEM
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
        Model_Meter_1P | Model_Meter_SP |
        Model_Meter_3P_WYE | Model_Meter_3P_Delta;
   end Is_Supported_Model;

end Ada_Modbus.Energy.KSEM;
