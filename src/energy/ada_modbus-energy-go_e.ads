--  Ada_Modbus.Energy.Go_E - go-e Charger Wallbox Interface
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  go-e Charger Modbus TCP Interface (Firmware 040+)
--  Default: Port 502, Device ID 1, Big Endian (Word Swap)
--
--  Input Registers (Read-Only):  30xxx (FC 04)
--  Holding Registers (R/W):      40xxx (FC 03/06/16)
--
--  Register addresses use Modbus convention (0-indexed internally)

with Interfaces;
with Ada_Modbus.Protocol;

package Ada_Modbus.Energy.Go_E
  with SPARK_Mode => On
is

   --  go-e Charger Device ID (default)
   Default_Device_Id : constant Unit_Id := 1;

   ---------------------
   --  Register Map   --
   ---------------------

   --  Input Registers (30xxx series, read with FC 04)
   Reg_Car_State        : constant Register_Address := 100;  --  30101
   Reg_PP_Cable         : constant Register_Address := 101;  --  30102
   Reg_Firmware         : constant Register_Address := 105;  --  30106-30107 (2 regs)
   Reg_Error            : constant Register_Address := 107;  --  30108
   Reg_Voltage_L1       : constant Register_Address := 108;  --  30109-30110 (2 regs)
   Reg_Voltage_L2       : constant Register_Address := 110;  --  30111-30112 (2 regs)
   Reg_Voltage_L3       : constant Register_Address := 112;  --  30113-30114 (2 regs)
   Reg_Current_L1       : constant Register_Address := 114;  --  30115-30116 (2 regs)
   Reg_Current_L2       : constant Register_Address := 116;  --  30117-30118 (2 regs)
   Reg_Current_L3       : constant Register_Address := 118;  --  30119-30120 (2 regs)
   Reg_Power_Total      : constant Register_Address := 120;  --  30121-30122 (2 regs)
   Reg_Energy_Total     : constant Register_Address := 128;  --  30129-30130 (2 regs)
   Reg_Energy_Session   : constant Register_Address := 132;  --  30133-30134 (2 regs)
   Reg_Voltage_N        : constant Register_Address := 144;  --  30145-30146 (2 regs)
   Reg_Power_L1         : constant Register_Address := 146;  --  30147-30148 (2 regs)
   Reg_Power_L2         : constant Register_Address := 148;  --  30149-30150 (2 regs)
   Reg_Power_L3         : constant Register_Address := 150;  --  30151-30152 (2 regs)
   Reg_Power_Factor_L1  : constant Register_Address := 152;  --  30153-30154 (2 regs)
   Reg_Power_Factor_L2  : constant Register_Address := 154;  --  30155-30156 (2 regs)
   Reg_Power_Factor_L3  : constant Register_Address := 156;  --  30157-30158 (2 regs)
   Reg_Phases           : constant Register_Address := 204;  --  30205
   Reg_Serial_Number    : constant Register_Address := 303;  --  30304-30309 (6 regs)

   --  Holding Registers (40xxx series, read/write with FC 03/06/16)
   Reg_Allow            : constant Register_Address := 200;  --  40201
   Reg_Access_State     : constant Register_Address := 201;  --  40202
   Reg_Cable_Lock       : constant Register_Address := 204;  --  40205
   Reg_LED_Brightness   : constant Register_Address := 206;  --  40207
   Reg_Ampere_Max       : constant Register_Address := 211;  --  40212
   Reg_Ampere_Volatile  : constant Register_Address := 299;  --  40300
   Reg_Ampere_EEPROM    : constant Register_Address := 300;  --  40301
   Reg_Phase_Switch     : constant Register_Address := 332;  --  40333
   Reg_Force_State      : constant Register_Address := 337;  --  40338

   -----------------------
   --  State Types      --
   -----------------------

   --  Car/PWM State (Reg 30101)
   type Car_State is
     (Unknown,           --  0: Unknown/Error
      Idle,              --  1: Idle (no car)
      Charging,          --  2: Charging
      Waiting_For_Car,   --  3: Waiting for car
      Charge_Complete);  --  4: Charge finished
   for Car_State use
     (Unknown         => 0,
      Idle            => 1,
      Charging        => 2,
      Waiting_For_Car => 3,
      Charge_Complete => 4);

   --  Error Codes (Reg 30108)
   type Error_Code is
     (No_Error,          --  0: No error
      RCCB,              --  1: FI protection triggered
      Phase_Error,       --  3: Phase error
      No_Ground);        --  8: No ground / PE error
   for Error_Code use
     (No_Error    => 0,
      RCCB        => 1,
      Phase_Error => 3,
      No_Ground   => 8);

   --  Access Control State (Reg 40202)
   type Access_State is
     (Open,              --  0: Open access
      RFID_App,          --  1: RFID or App required
      Price_Based,       --  2: Electricity price based
      Scheduler);        --  3: Scheduler based
   for Access_State use
     (Open        => 0,
      RFID_App    => 1,
      Price_Based => 2,
      Scheduler   => 3);

   --  Force State (Reg 40338)
   type Force_State is
     (Neutral,           --  0: Charger uses own logic
      Force_Off,         --  1: Force charging stop
      Force_On);         --  2: Force charging on
   for Force_State use
     (Neutral   => 0,
      Force_Off => 1,
      Force_On  => 2);

   --  Cable Lock Mode (Reg 40205)
   type Cable_Lock_Mode is
     (Unlock_Always,     --  0: Always unlocked
      Lock_While_Car,    --  1: Lock while car connected
      Lock_Always);      --  2: Always locked
   for Cable_Lock_Mode use
     (Unlock_Always  => 0,
      Lock_While_Car => 1,
      Lock_Always    => 2);

   --  Phase Switch Mode (Reg 40333)
   type Phase_Switch_Mode is
     (Auto,              --  0: Automatic
      Single_Phase,      --  1: Force 1 phase
      Three_Phase);      --  2: Force 3 phases
   for Phase_Switch_Mode use
     (Auto         => 0,
      Single_Phase => 1,
      Three_Phase  => 2);

   --  Charging current limits (go-e supports 6-32A)
   subtype Charging_Current is Natural range 6 .. 32;

   --  LED brightness (0-255)
   subtype LED_Brightness is Natural range 0 .. 255;

   --  Cable Ampere Encoding (Reg 30102)
   type Cable_Ampere is
     (No_Cable,          --  0: No cable detected
      Cable_13A,         --  13: 13A cable
      Cable_20A,         --  20: 20A cable
      Cable_32A);        --  32: 32A cable

   ----------------------------
   --  Measurement Records   --
   ----------------------------

   --  Voltage measurements per phase (in Volts)
   type Voltage_Data is record
      L1 : Natural;      --  Phase 1 voltage
      L2 : Natural;      --  Phase 2 voltage
      L3 : Natural;      --  Phase 3 voltage
      N  : Natural;      --  Neutral voltage
   end record;

   --  Current measurements per phase (in 0.1A, divide by 10)
   type Current_Data is record
      L1 : Natural;      --  Phase 1 current (x0.1A)
      L2 : Natural;      --  Phase 2 current (x0.1A)
      L3 : Natural;      --  Phase 3 current (x0.1A)
   end record;

   --  Power measurements (in 0.01W for total, 0.1kW for phases)
   --  Using Unsigned_32 for full 32-bit register range
   type Power_Data is record
      Total : Interfaces.Unsigned_32;  --  Total power (x0.01W)
      L1    : Interfaces.Unsigned_32;  --  Phase 1 power (x0.1kW)
      L2    : Interfaces.Unsigned_32;  --  Phase 2 power (x0.1kW)
      L3    : Interfaces.Unsigned_32;  --  Phase 3 power (x0.1kW)
   end record;

   --  Energy data (in 0.1kWh for total, Deka-Watt-Seconds for session)
   --  Using Unsigned_32 for full 32-bit register range
   type Energy_Data is record
      Total   : Interfaces.Unsigned_32;  --  Total energy ever (x0.1kWh)
      Session : Interfaces.Unsigned_32;  --  Current session (Deka-Watt-Seconds)
   end record;

   --  Power factor per phase (in %)
   type Power_Factor_Data is record
      L1 : Natural;      --  Phase 1 power factor (%)
      L2 : Natural;      --  Phase 2 power factor (%)
      L3 : Natural;      --  Phase 3 power factor (%)
   end record;

   --  Complete charger status
   type Charger_Status is record
      Car          : Car_State;
      Error        : Error_Code;
      Voltage      : Voltage_Data;
      Current      : Current_Data;
      Power        : Power_Data;
      Energy       : Energy_Data;
      Power_Factor : Power_Factor_Data;
      Cable_Amps   : Natural;         --  Cable rating (0, 13, 20, 32)
      Phases_Used  : Natural range 0 .. 3;  --  Active phases
   end record;

   --  Charger settings
   type Charger_Settings is record
      Allow        : Boolean;         --  PWM signal allowed
      Access_Mode  : Access_State;
      Force        : Force_State;
      Cable_Lock   : Cable_Lock_Mode;
      Phase_Mode   : Phase_Switch_Mode;
      Ampere_Max   : Charging_Current; --  Max ampere (6-32)
      Ampere_Set   : Charging_Current; --  Current limit (6-32)
      LED_Bright   : LED_Brightness;   --  LED brightness (0-255)
   end record;

   --------------------------
   --  Request Encoding    --
   --------------------------

   --  Read all input registers for status (FC 04)
   --  Reads 30101-30160 (60 registers)
   procedure Encode_Read_Status_Request
     (Buffer : out Protocol.PDU_Buffer;
      Length : out Natural)
     with Post => Length <= Protocol.Max_PDU_Size;

   --  Read car state only (FC 04, 1 register)
   procedure Encode_Read_Car_State_Request
     (Buffer : out Protocol.PDU_Buffer;
      Length : out Natural)
     with Post => Length = 5;

   --  Read power and energy (FC 04)
   procedure Encode_Read_Power_Request
     (Buffer : out Protocol.PDU_Buffer;
      Length : out Natural)
     with Post => Length = 5;

   --  Read settings (FC 03, Holding Registers)
   procedure Encode_Read_Settings_Request
     (Buffer : out Protocol.PDU_Buffer;
      Length : out Natural)
     with Post => Length = 5;

   --  Set charging current (FC 06, volatile - not saved to EEPROM)
   procedure Encode_Set_Ampere_Request
     (Ampere : Charging_Current;
      Buffer : out Protocol.PDU_Buffer;
      Length : out Natural)
     with Post => Length = 5;

   --  Set force state (FC 06)
   procedure Encode_Set_Force_State_Request
     (State  : Force_State;
      Buffer : out Protocol.PDU_Buffer;
      Length : out Natural)
     with Post => Length = 5;

   --  Set allow charging (FC 06)
   procedure Encode_Set_Allow_Request
     (Allow  : Boolean;
      Buffer : out Protocol.PDU_Buffer;
      Length : out Natural)
     with Post => Length = 5;

   --  Set cable lock mode (FC 06)
   procedure Encode_Set_Cable_Lock_Request
     (Mode   : Cable_Lock_Mode;
      Buffer : out Protocol.PDU_Buffer;
      Length : out Natural)
     with Post => Length = 5;

   --  Set phase switch mode (FC 06)
   procedure Encode_Set_Phase_Mode_Request
     (Mode   : Phase_Switch_Mode;
      Buffer : out Protocol.PDU_Buffer;
      Length : out Natural)
     with Post => Length = 5;

   --  Set LED brightness (FC 06)
   procedure Encode_Set_LED_Brightness_Request
     (Brightness : LED_Brightness;
      Buffer     : out Protocol.PDU_Buffer;
      Length     : out Natural)
     with Post => Length = 5;

   ---------------------------
   --  Response Decoding    --
   ---------------------------

   --  Decode car state response
   procedure Decode_Car_State_Response
     (Buffer : Protocol.PDU_Buffer;
      Length : Natural;
      State  : out Car_State;
      Result : out Status)
     with Pre => Length <= Protocol.Max_PDU_Size;

   --  Decode power data response (4 registers: total + L1-L3)
   procedure Decode_Power_Response
     (Buffer : Protocol.PDU_Buffer;
      Length : Natural;
      Power  : out Power_Data;
      Result : out Status)
     with Pre => Length <= Protocol.Max_PDU_Size;

   --  Decode 32-bit value from two consecutive registers (Big Endian)
   --  Returns Unsigned_32 to avoid overflow (max 4_294_967_295)
   function Decode_Uint32
     (High_Word : Register_Value;
      Low_Word  : Register_Value) return Interfaces.Unsigned_32;

   --------------------------
   --  Conversion Helpers  --
   --------------------------

   --  Raw value limits to prevent overflow in conversions
   subtype Raw_Power_Phase is Natural range 0 .. 21_474_836;   --  * 100 safe
   subtype Raw_Energy_Value is Natural range 0 .. 21_474_836;  --  * 100 safe
   subtype Raw_Session_Value is Natural range 0 .. 214_748_364; --  * 10 safe

   --  Convert raw power total (0.01W) to Watts
   function To_Watts (Raw_Power : Natural) return Natural is
     (Raw_Power / 100);

   --  Convert raw phase power (0.1kW) to Watts
   function To_Watts_Phase (Raw_Power : Raw_Power_Phase) return Natural is
     (Raw_Power * 100);

   --  Convert raw current (0.1A) to Amps (x10 for precision)
   function To_Deciamps (Raw_Current : Natural) return Natural is
     (Raw_Current);

   --  Convert raw energy total (0.1kWh) to Wh
   function To_Wh (Raw_Energy : Raw_Energy_Value) return Natural is
     (Raw_Energy * 100);

   --  Convert session energy (Deka-Watt-Seconds) to Wh
   function Session_To_Wh (Raw_Session : Raw_Session_Value) return Natural is
     (Raw_Session * 10 / 3600);

end Ada_Modbus.Energy.Go_E;
