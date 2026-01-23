--  Ada_Modbus.Energy.SunSpec.Inverter - SunSpec Inverter Models (101-103)
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Inverter models for AC power production:
--  - Model 101: Single Phase Inverter
--  - Model 102: Split Phase Inverter
--  - Model 103: Three Phase Inverter
--
--  All models share the same register structure for basic measurements.

with Ada_Modbus.Protocol;

package Ada_Modbus.Energy.SunSpec.Inverter
  with SPARK_Mode => On
is

   --  Inverter model register offsets (after 2-reg header)
   Reg_AC_Current       : constant := 2;   --  Total AC Current (A)
   Reg_AC_Current_A     : constant := 3;   --  Phase A Current
   Reg_AC_Current_B     : constant := 4;   --  Phase B Current
   Reg_AC_Current_C     : constant := 5;   --  Phase C Current
   Reg_AC_Current_SF    : constant := 6;   --  Current Scale Factor

   Reg_AC_Voltage_AB    : constant := 7;   --  Line-Line Voltage AB
   Reg_AC_Voltage_BC    : constant := 8;   --  Line-Line Voltage BC
   Reg_AC_Voltage_CA    : constant := 9;   --  Line-Line Voltage CA
   Reg_AC_Voltage_AN    : constant := 10;  --  Line-Neutral Voltage A
   Reg_AC_Voltage_BN    : constant := 11;  --  Line-Neutral Voltage B
   Reg_AC_Voltage_CN    : constant := 12;  --  Line-Neutral Voltage C
   Reg_AC_Voltage_SF    : constant := 13;  --  Voltage Scale Factor

   Reg_AC_Power         : constant := 14;  --  AC Power (W)
   Reg_AC_Power_SF      : constant := 15;  --  Power Scale Factor

   Reg_AC_Frequency     : constant := 16;  --  AC Frequency (Hz)
   Reg_AC_Frequency_SF  : constant := 17;  --  Frequency Scale Factor

   Reg_AC_VA            : constant := 18;  --  Apparent Power (VA)
   Reg_AC_VA_SF         : constant := 19;  --  VA Scale Factor

   Reg_AC_VAR           : constant := 20;  --  Reactive Power (var)
   Reg_AC_VAR_SF        : constant := 21;  --  var Scale Factor

   Reg_AC_PF            : constant := 22;  --  Power Factor (%)
   Reg_AC_PF_SF         : constant := 23;  --  PF Scale Factor

   Reg_AC_Energy        : constant := 24;  --  AC Energy (Wh) - 32-bit
   Reg_AC_Energy_SF     : constant := 26;  --  Energy Scale Factor

   Reg_DC_Current       : constant := 27;  --  DC Current (A)
   Reg_DC_Current_SF    : constant := 28;  --  DC Current Scale Factor

   Reg_DC_Voltage       : constant := 29;  --  DC Voltage (V)
   Reg_DC_Voltage_SF    : constant := 30;  --  DC Voltage Scale Factor

   Reg_DC_Power         : constant := 31;  --  DC Power (W)
   Reg_DC_Power_SF      : constant := 32;  --  DC Power Scale Factor

   Reg_Cabinet_Temp     : constant := 33;  --  Cabinet Temperature (C)
   Reg_Heat_Sink_Temp   : constant := 34;  --  Heat Sink Temperature (C)
   Reg_Transformer_Temp : constant := 35;  --  Transformer Temperature (C)
   Reg_Other_Temp       : constant := 36;  --  Other Temperature (C)
   Reg_Temp_SF          : constant := 37;  --  Temperature Scale Factor

   Reg_Operating_State  : constant := 38;  --  Operating State
   Reg_Vendor_State     : constant := 39;  --  Vendor-specific state

   Reg_Event_1          : constant := 40;  --  Event Flags 1 (32-bit)
   Reg_Event_2          : constant := 42;  --  Event Flags 2 (32-bit)
   Reg_Vendor_Event_1   : constant := 44;  --  Vendor Event 1 (32-bit)
   Reg_Vendor_Event_2   : constant := 46;  --  Vendor Event 2 (32-bit)

   --  Operating states
   type Inverter_State is
     (Off,
      Sleeping,
      Starting,
      Running,         --  MPPT
      Throttled,       --  Power limited
      Shutting_Down,
      Fault,
      Standby);

   --  Inverter measurements
   type Inverter_Data is record
      AC_Current_A   : Float;   --  Amps
      AC_Voltage_V   : Float;   --  Volts (phase-neutral or line-line)
      AC_Power_W     : Float;   --  Watts
      AC_Frequency   : Float;   --  Hz
      AC_Energy_Wh   : Float;   --  Watt-hours (total production)
      DC_Voltage_V   : Float;   --  DC input voltage
      DC_Current_A   : Float;   --  DC input current
      DC_Power_W     : Float;   --  DC input power
      Temperature_C  : Float;   --  Cabinet/heatsink temp
      State          : Inverter_State;
   end record;

   --  Read main AC measurements (current, voltage, power, frequency)
   --  Reads registers 2-17 (16 registers)
   procedure Encode_Read_AC_Measurements_Request
     (Base_Address  : Register_Address;
      Model_Offset  : Register_Address;
      Buffer        : out Protocol.PDU_Buffer;
      Length        : out Natural)
     with Post => Length <= Protocol.Max_PDU_Size;

   --  Decode AC measurements
   procedure Decode_AC_Measurements_Response
     (Buffer       : Protocol.PDU_Buffer;
      Length       : Natural;
      Current_A    : out Float;
      Voltage_V    : out Float;
      Power_W      : out Float;
      Frequency_Hz : out Float;
      Result       : out Status)
     with Pre => Length <= Protocol.Max_PDU_Size;

   --  Read DC measurements (current, voltage, power)
   --  Reads registers 27-32 (6 registers)
   procedure Encode_Read_DC_Measurements_Request
     (Base_Address  : Register_Address;
      Model_Offset  : Register_Address;
      Buffer        : out Protocol.PDU_Buffer;
      Length        : out Natural)
     with Post => Length <= Protocol.Max_PDU_Size;

   --  Decode DC measurements
   procedure Decode_DC_Measurements_Response
     (Buffer    : Protocol.PDU_Buffer;
      Length    : Natural;
      Current_A : out Float;
      Voltage_V : out Float;
      Power_W   : out Float;
      Result    : out Status)
     with Pre => Length <= Protocol.Max_PDU_Size;

   --  Read energy production (32-bit Wh counter)
   --  Reads registers 24-26 (3 registers: 32-bit value + SF)
   procedure Encode_Read_Energy_Request
     (Base_Address  : Register_Address;
      Model_Offset  : Register_Address;
      Buffer        : out Protocol.PDU_Buffer;
      Length        : out Natural)
     with Post => Length <= Protocol.Max_PDU_Size;

   --  Decode energy response
   procedure Decode_Energy_Response
     (Buffer    : Protocol.PDU_Buffer;
      Length    : Natural;
      Energy_Wh : out Float;
      Result    : out Status)
     with Pre => Length <= Protocol.Max_PDU_Size;

   --  Read operating state
   --  Reads registers 38-39 (2 registers)
   procedure Encode_Read_State_Request
     (Base_Address  : Register_Address;
      Model_Offset  : Register_Address;
      Buffer        : out Protocol.PDU_Buffer;
      Length        : out Natural)
     with Post => Length <= Protocol.Max_PDU_Size;

   --  Decode operating state
   procedure Decode_State_Response
     (Buffer : Protocol.PDU_Buffer;
      Length : Natural;
      State  : out Inverter_State;
      Result : out Status)
     with Pre => Length <= Protocol.Max_PDU_Size;

end Ada_Modbus.Energy.SunSpec.Inverter;
