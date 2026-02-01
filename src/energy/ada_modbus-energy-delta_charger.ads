--  Ada_Modbus.Energy.Delta_Charger - Delta AC Max Basic Wallbox Interface
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Delta AC Max Basic EV-Charger Modbus Interface
--  Modbus RTU: RS485, 115200 Baud, 8N1
--  Modbus TCP: Port 502
--  Default Device ID: 1
--
--  Input Registers (Read-Only):   FC 04
--  Holding Registers (R/W):       FC 03/06/16
--
--  EVSE-specific registers use base address = Connector * 1000
--  This implementation supports Connector 1 (Base = 1000)
--
--  Note: AC Max Basic has Register 1001 (Charge State), Smart version doesn't

with Interfaces;
with Ada_Modbus.Protocol;

package Ada_Modbus.Energy.Delta_Charger
  with SPARK_Mode => On
is

   --  Delta Charger Device ID (default)
   Default_Device_Id : constant Unit_Id := 1;

   --  EVSE Base Address (Connector 1 = 1000)
   EVSE_Base : constant Register_Address := 1000;

   ---------------------
   --  Register Map   --
   ---------------------

   --  Charger-Level Input Registers (FC 04)
   Reg_Charger_State  : constant Register_Address := 100;   --  Charger state
   Reg_EVSE_Count     : constant Register_Address := 102;   --  Number of EVSEs
   Reg_Serial_Number  : constant Register_Address := 110;   --  Serial (10 regs)
   Reg_Model          : constant Register_Address := 130;   --  Model (10 regs)

   --  Charger-Level Holding Registers (FC 03/06/16)
   Reg_Comm_Timeout_Enabled : constant Register_Address := 201;  --  0/1
   Reg_Comm_Timeout         : constant Register_Address := 202;  --  Seconds
   Reg_Fallback_Power       : constant Register_Address := 203;  --  UINT32 (W)

   --  EVSE-Specific Registers (Offsets from EVSE_Base)
   --  For Connector 1: Base = 1000
   EVSE_State_Offset       : constant := 0;     --  EVSE state (0-9)
   Charge_State_Offset     : constant := 1;     --  Charge state (0-7)
   Output_Voltage_Offset   : constant := 3;     --  FLOAT32 (V)
   Charging_Power_Offset   : constant := 5;     --  UINT32 (W)
   Charging_Current_Offset : constant := 7;     --  FLOAT32 (A)
   Output_Power_Offset     : constant := 9;     --  FLOAT32 (W)
   SOC_Offset              : constant := 11;    --  UINT16 (%/10)
   Charging_Time_Offset    : constant := 17;    --  UINT32 (seconds)
   Charged_Energy_Offset   : constant := 19;    --  UINT32 (Wh)
   RFID_UID_Offset         : constant := 100;   --  STRING (7 regs)
   Power_Limit_Offset      : constant := 600;   --  UINT32 (W)
   Suspend_Charging_Offset : constant := 602;   --  0=active, 1=suspended

   -----------------------
   --  State Types      --
   -----------------------

   --  Charger State (Reg 100)
   type Charger_State is
     (Not_Ready,       --  0: Charger not ready
      Operational,     --  1: Charger operational
      Faulted,         --  10: Charger faulted
      Not_Responding)  --  255: Not responding
     with Size => 16;
   for Charger_State use
     (Not_Ready      => 0,
      Operational    => 1,
      Faulted        => 10,
      Not_Responding => 255);

   --  EVSE State (Reg 1000)
   type EVSE_State is
     (Unavailable,     --  0: EVSE unavailable
      Available,       --  1: EVSE available (ready to charge)
      Occupied,        --  2: Occupied (cable connected)
      Preparing,       --  3: Preparing to charge
      Charging,        --  4: Charging in progress
      Finishing,       --  5: Finishing charge
      Suspended_EV,    --  6: Suspended by EV
      Suspended_EVSE,  --  7: Suspended by EVSE
      Not_Ready,       --  8: EVSE not ready
      Faulted)         --  9: EVSE faulted
     with Size => 16;
   for EVSE_State use
     (Unavailable    => 0,
      Available      => 1,
      Occupied       => 2,
      Preparing      => 3,
      Charging       => 4,
      Finishing      => 5,
      Suspended_EV   => 6,
      Suspended_EVSE => 7,
      Not_Ready      => 8,
      Faulted        => 9);

   --  Charge State (Reg 1001, AC Max Basic only)
   type Charge_State is
     (Not_Charging,    --  0: Not charging
      Charging_L1,     --  1: Charging on L1
      Charging_L2,     --  2: Charging on L2
      Charging_L3,     --  3: Charging on L3
      Charging_L12,    --  4: Charging on L1+L2
      Charging_L23,    --  5: Charging on L2+L3
      Charging_L13,    --  6: Charging on L1+L3
      Charging_L123)   --  7: Charging on all phases
     with Size => 16;
   for Charge_State use
     (Not_Charging  => 0,
      Charging_L1   => 1,
      Charging_L2   => 2,
      Charging_L3   => 3,
      Charging_L12  => 4,
      Charging_L23  => 5,
      Charging_L13  => 6,
      Charging_L123 => 7);

   ----------------------------
   --  Measurement Records   --
   ----------------------------

   --  FLOAT32 raw data (two UINT16 registers, Big Endian)
   --  Use external conversion to float if needed
   subtype Float32_Raw is Register_Array (0 .. 1);

   --  EVSE Status Record
   type EVSE_Status is record
      State            : EVSE_State;                     --  Current EVSE state
      Charge           : Charge_State;                   --  Current charge state
      Voltage_Raw      : Float32_Raw;                    --  Output voltage (V)
      Power_W          : Interfaces.Unsigned_32;         --  Charging power (W)
      Current_Raw      : Float32_Raw;                    --  Charging current (A)
      Output_Power_Raw : Float32_Raw;                    --  Output power (W)
      SOC_Percent_x10  : Natural range 0 .. 1000;        --  SOC (% * 10)
      Charging_Time_S  : Interfaces.Unsigned_32;         --  Charging time (s)
      Energy_Wh        : Interfaces.Unsigned_32;         --  Charged energy (Wh)
   end record;

   --  Charger Info Record
   type Charger_Info is record
      State      : Charger_State;                        --  Charger state
      EVSE_Count : Natural range 0 .. 255;               --  Number of EVSEs
   end record;

   --  Communication Settings Record
   type Comm_Settings is record
      Timeout_Enabled : Boolean;                         --  Timeout enabled
      Timeout_Seconds : Natural range 0 .. 65535;        --  Timeout value
      Fallback_Power  : Interfaces.Unsigned_32;          --  Fallback power (W)
   end record;

   --------------------------
   --  Request Encoding    --
   --------------------------

   --  Read EVSE Status (FC 04, registers 1000-1020)
   --  Reads: State, Charge State, Voltage, Power, Current, SOC, Time, Energy
   procedure Encode_Read_EVSE_Status_Request
     (Buffer : out Protocol.PDU_Buffer;
      Length : out Natural)
     with Post => Length = 5;

   --  Read Charger Info (FC 04, registers 100-102)
   --  Reads: Charger State, EVSE Count
   procedure Encode_Read_Charger_Info_Request
     (Buffer : out Protocol.PDU_Buffer;
      Length : out Natural)
     with Post => Length = 5;

   --  Read Communication Settings (FC 03, registers 201-204)
   --  Reads: Timeout Enabled, Timeout, Fallback Power
   procedure Encode_Read_Comm_Settings_Request
     (Buffer : out Protocol.PDU_Buffer;
      Length : out Natural)
     with Post => Length = 5;

   --  Set Power Limit (FC 16, registers 1600-1601)
   --  Writes UINT32 power limit in Watts
   procedure Encode_Set_Power_Limit_Request
     (Power_W : Interfaces.Unsigned_32;
      Buffer  : out Protocol.PDU_Buffer;
      Length  : out Natural)
     with Post => Length <= Protocol.Max_PDU_Size;

   --  Suspend/Resume Charging (FC 06, register 1602)
   --  0 = active (charging), 1 = suspended (paused)
   procedure Encode_Set_Suspend_Request
     (Suspend : Boolean;
      Buffer  : out Protocol.PDU_Buffer;
      Length  : out Natural)
     with Post => Length = 5;

   --  Set Communication Timeout (FC 06, register 202)
   --  Timeout in seconds
   procedure Encode_Set_Comm_Timeout_Request
     (Timeout_Seconds : Natural;
      Buffer          : out Protocol.PDU_Buffer;
      Length          : out Natural)
     with Pre  => Timeout_Seconds <= 65535,
          Post => Length = 5;

   --  Enable/Disable Communication Timeout (FC 06, register 201)
   procedure Encode_Set_Comm_Timeout_Enabled_Request
     (Enabled : Boolean;
      Buffer  : out Protocol.PDU_Buffer;
      Length  : out Natural)
     with Post => Length = 5;

   --  Set Fallback Power (FC 16, registers 203-204)
   --  Power in Watts when communication timeout occurs
   procedure Encode_Set_Fallback_Power_Request
     (Power_W : Interfaces.Unsigned_32;
      Buffer  : out Protocol.PDU_Buffer;
      Length  : out Natural)
     with Post => Length <= Protocol.Max_PDU_Size;

   ---------------------------
   --  Response Decoding    --
   ---------------------------

   --  Decode EVSE Status Response
   procedure Decode_EVSE_Status_Response
     (Buffer : Protocol.PDU_Buffer;
      Length : Natural;
      Status : out EVSE_Status;
      Result : out Ada_Modbus.Status)
     with Pre => Length <= Protocol.Max_PDU_Size;

   --  Decode Charger Info Response
   procedure Decode_Charger_Info_Response
     (Buffer : Protocol.PDU_Buffer;
      Length : Natural;
      Info   : out Charger_Info;
      Result : out Ada_Modbus.Status)
     with Pre => Length <= Protocol.Max_PDU_Size;

   --  Decode Communication Settings Response
   procedure Decode_Comm_Settings_Response
     (Buffer   : Protocol.PDU_Buffer;
      Length   : Natural;
      Settings : out Comm_Settings;
      Result   : out Ada_Modbus.Status)
     with Pre => Length <= Protocol.Max_PDU_Size;

   --------------------------
   --  Conversion Helpers  --
   --------------------------

   --  Decode 32-bit unsigned value from two consecutive registers (Big Endian)
   function Decode_Uint32
     (High_Word : Register_Value;
      Low_Word  : Register_Value) return Interfaces.Unsigned_32;

   --  Encode 32-bit unsigned value to two consecutive registers (Big Endian)
   procedure Encode_Uint32
     (Value     : Interfaces.Unsigned_32;
      High_Word : out Register_Value;
      Low_Word  : out Register_Value);

   --  Convert SOC raw value (% * 10) to percentage
   function To_SOC_Percent (Raw_SOC : Natural) return Natural is
     (Raw_SOC / 10)
     with Pre => Raw_SOC <= 1000;

   --  Convert raw value to EVSE_State (with bounds checking)
   function To_EVSE_State (Raw : Register_Value) return EVSE_State;

   --  Convert raw value to Charger_State (with bounds checking)
   function To_Charger_State (Raw : Register_Value) return Charger_State;

   --  Convert raw value to Charge_State (with bounds checking)
   function To_Charge_State (Raw : Register_Value) return Charge_State;

end Ada_Modbus.Energy.Delta_Charger;
