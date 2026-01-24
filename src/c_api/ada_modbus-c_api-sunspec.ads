--  Ada_Modbus.C_API.SunSpec - SunSpec C bindings
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  High-level SunSpec API for C developers.
--  Provides decoded data structures without manual register handling.

package Ada_Modbus.C_API.SunSpec is

   --  SunSpec base address (standard: 40000)
   Default_SunSpec_Base : constant := 40000;

   ---------------------
   --  Common Types
   ---------------------

   --  Model IDs
   C_Model_Common       : constant := 1;
   C_Model_Inverter_1P  : constant := 101;
   C_Model_Inverter_SP  : constant := 102;
   C_Model_Inverter_3P  : constant := 103;
   C_Model_Storage      : constant := 124;
   C_Model_MPPT         : constant := 160;
   C_Model_Meter_1P     : constant := 201;
   C_Model_Meter_SP     : constant := 202;
   C_Model_Meter_3P_Wye : constant := 203;
   C_Model_Meter_3P_Delta : constant := 204;
   C_Model_DER_AC       : constant := 701;
   C_Model_DER_Ctrl     : constant := 704;
   C_Model_Battery      : constant := 802;
   C_Model_End          : constant := 16#FFFF#;

   --  Operating states
   C_State_Off          : constant := 1;
   C_State_Sleeping     : constant := 2;
   C_State_Starting     : constant := 3;
   C_State_Running      : constant := 4;
   C_State_Throttled    : constant := 5;
   C_State_Shutting_Down : constant := 6;
   C_State_Fault        : constant := 7;
   C_State_Standby      : constant := 8;

   ---------------------
   --  Data Structures
   ---------------------

   --  Device information (Model 1)
   type C_SunSpec_Common is record
      Manufacturer : char_array (0 .. 31);
      Model        : char_array (0 .. 31);
      Serial       : char_array (0 .. 31);
      Version      : char_array (0 .. 15);
   end record
     with Convention => C;

   --  Inverter data (Models 101-103)
   type C_SunSpec_Inverter is record
      AC_Power_W      : C_float;
      AC_Voltage_V    : C_float;
      AC_Current_A    : C_float;
      AC_Frequency_Hz : C_float;
      AC_Energy_Wh    : C_float;
      DC_Power_W      : C_float;
      DC_Voltage_V    : C_float;
      DC_Current_A    : C_float;
      Cabinet_Temp_C  : C_float;
      Operating_State : int;
   end record
     with Convention => C;

   --  Meter data (Models 201-204)
   type C_SunSpec_Meter is record
      Total_Power_W   : C_float;
      Total_Voltage_V : C_float;
      Total_Current_A : C_float;
      Frequency_Hz    : C_float;
      Power_Factor    : C_float;
      Total_VA        : C_float;
      Total_VAR       : C_float;
      Export_Wh       : C_float;
      Import_Wh       : C_float;
      --  Per-phase (3-phase meters)
      L1_Power_W      : C_float;
      L2_Power_W      : C_float;
      L3_Power_W      : C_float;
      L1_Voltage_V    : C_float;
      L2_Voltage_V    : C_float;
      L3_Voltage_V    : C_float;
      L1_Current_A    : C_float;
      L2_Current_A    : C_float;
      L3_Current_A    : C_float;
   end record
     with Convention => C;

   --  Battery/Storage data (Models 124, 802)
   type C_SunSpec_Battery is record
      SOC_Percent     : C_float;
      SOH_Percent     : C_float;
      Voltage_V       : C_float;
      Current_A       : C_float;
      Power_W         : C_float;
      Capacity_Wh     : C_float;
      Max_Charge_W    : C_float;
      Max_Discharge_W : C_float;
      Cycle_Count     : int;
      State           : int;      --  0=Off, 3=Discharging, 4=Charging, 5=Full
      Cell_V_Max      : C_float;
      Cell_V_Min      : C_float;
   end record
     with Convention => C;

   --  Model header (for iteration)
   type C_SunSpec_Model_Header is record
      Model_ID : unsigned_short;
      Length   : unsigned_short;
      Address  : unsigned_short;
   end record
     with Convention => C;

   ---------------------
   --  Discovery API
   ---------------------

   --  Check if device supports SunSpec (looks for "SunS" at base address)
   --  Returns: C_Success if SunSpec found, C_Invalid_Response otherwise
   function SunSpec_Check
     (Master_Handle : C_Master_Handle;
      Slave         : unsigned_char;
      Base_Address  : unsigned_short) return C_Status
     with Export, Convention => C, External_Name => "sunspec_check";

   --  Read model header at given address
   --  Returns: C_Success and fills Header, or error status
   function SunSpec_Read_Model_Header
     (Master_Handle : C_Master_Handle;
      Slave         : unsigned_char;
      Address       : unsigned_short;
      Header        : access C_SunSpec_Model_Header) return C_Status
     with Export, Convention => C, External_Name => "sunspec_read_model_header";

   --  Find next model in device (for iteration)
   --  Start with Address = Base + 2, then use returned Header.Address + Header.Length + 2
   --  Returns: C_Success until End model (ID=0xFFFF)
   function SunSpec_Find_Model
     (Master_Handle : C_Master_Handle;
      Slave         : unsigned_char;
      Start_Address : unsigned_short;
      Model_ID      : unsigned_short;
      Header        : access C_SunSpec_Model_Header) return C_Status
     with Export, Convention => C, External_Name => "sunspec_find_model";

   ---------------------
   --  High-Level Read API
   ---------------------

   --  Read device information (Model 1)
   function SunSpec_Read_Common
     (Master_Handle : C_Master_Handle;
      Slave         : unsigned_char;
      Model_Address : unsigned_short;
      Data          : access C_SunSpec_Common) return C_Status
     with Export, Convention => C, External_Name => "sunspec_read_common";

   --  Read inverter data (Models 101-103)
   function SunSpec_Read_Inverter
     (Master_Handle : C_Master_Handle;
      Slave         : unsigned_char;
      Model_Address : unsigned_short;
      Data          : access C_SunSpec_Inverter) return C_Status
     with Export, Convention => C, External_Name => "sunspec_read_inverter";

   --  Read meter data (Models 201-204)
   function SunSpec_Read_Meter
     (Master_Handle : C_Master_Handle;
      Slave         : unsigned_char;
      Model_Address : unsigned_short;
      Data          : access C_SunSpec_Meter) return C_Status
     with Export, Convention => C, External_Name => "sunspec_read_meter";

   --  Read battery data (Model 124 or 802)
   function SunSpec_Read_Battery
     (Master_Handle : C_Master_Handle;
      Slave         : unsigned_char;
      Model_Address : unsigned_short;
      Data          : access C_SunSpec_Battery) return C_Status
     with Export, Convention => C, External_Name => "sunspec_read_battery";

end Ada_Modbus.C_API.SunSpec;
