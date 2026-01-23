--  Ada_Modbus.Energy.SunSpec.Storage - SunSpec Storage Model (Model 124)
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Model 124: Basic Storage Controls
--  Controls battery storage system charge/discharge operations.

with Ada_Modbus.Protocol;

package Ada_Modbus.Energy.SunSpec.Storage
  with SPARK_Mode => On
is

   --  Model 124 register offsets (after 2-reg header)
   Reg_Max_Charge_Rate    : constant := 2;   --  Max charge rate (W)
   Reg_Max_Charge_Rate_SF : constant := 3;   --  Scale factor
   Reg_Max_Discharge_Rate : constant := 4;   --  Max discharge rate (W)
   Reg_Max_Discharge_SF   : constant := 5;   --  Scale factor
   Reg_Stored_Energy      : constant := 6;   --  Stored energy (Wh)
   Reg_Stored_Energy_SF   : constant := 7;   --  Scale factor
   Reg_Capacity           : constant := 8;   --  Usable capacity (Wh)
   Reg_Capacity_SF        : constant := 9;   --  Scale factor
   Reg_SOC                : constant := 10;  --  State of Charge (%)
   Reg_SOC_SF             : constant := 11;  --  Scale factor
   Reg_SOH                : constant := 12;  --  State of Health (%)
   Reg_SOH_SF             : constant := 13;  --  Scale factor
   Reg_Status             : constant := 14;  --  Storage status
   Reg_Charge_Status      : constant := 15;  --  Charge status

   --  Writable control registers
   Reg_InOut_Ctrl         : constant := 16;  --  Charge/Discharge control
   Reg_InOut_Power        : constant := 17;  --  Target power (W)
   Reg_InOut_Power_SF     : constant := 18;  --  Scale factor

   --  Storage status values
   type Storage_Status is
     (Off,
      Empty,
      Discharging,
      Charging,
      Full,
      Holding,
      Testing);

   --  Charge control modes
   type Charge_Control is
     (Off,
      Charge,
      Discharge);

   --  Storage measurements
   type Storage_Data is record
      Max_Charge_W     : Float;   --  Maximum charge power (W)
      Max_Discharge_W  : Float;   --  Maximum discharge power (W)
      Stored_Wh        : Float;   --  Current stored energy (Wh)
      Capacity_Wh      : Float;   --  Total usable capacity (Wh)
      SOC_Percent      : Float;   --  State of charge (%)
      SOH_Percent      : Float;   --  State of health (%)
      Status           : Storage_Status;
   end record;

   --  Read storage status (SOC, SOH, status)
   --  Reads registers 10-15 (6 registers)
   procedure Encode_Read_Status_Request
     (Base_Address  : Register_Address;
      Model_Offset  : Register_Address;
      Buffer        : out Protocol.PDU_Buffer;
      Length        : out Natural)
     with Post => Length <= Protocol.Max_PDU_Size;

   --  Decode storage status
   procedure Decode_Status_Response
     (Buffer      : Protocol.PDU_Buffer;
      Length      : Natural;
      SOC_Percent : out Float;
      SOH_Percent : out Float;
      Status      : out Storage_Status;
      Result      : out Ada_Modbus.Status)
     with Pre => Length <= Protocol.Max_PDU_Size;

   --  Read capacity info (max rates, capacity)
   --  Reads registers 2-9 (8 registers)
   procedure Encode_Read_Capacity_Request
     (Base_Address  : Register_Address;
      Model_Offset  : Register_Address;
      Buffer        : out Protocol.PDU_Buffer;
      Length        : out Natural)
     with Post => Length <= Protocol.Max_PDU_Size;

   --  Decode capacity info
   procedure Decode_Capacity_Response
     (Buffer          : Protocol.PDU_Buffer;
      Length          : Natural;
      Max_Charge_W    : out Float;
      Max_Discharge_W : out Float;
      Stored_Wh       : out Float;
      Capacity_Wh     : out Float;
      Result          : out Ada_Modbus.Status)
     with Pre => Length <= Protocol.Max_PDU_Size;

   --  Set charge/discharge mode
   procedure Encode_Set_Control_Request
     (Mode   : Charge_Control;
      Buffer : out Protocol.PDU_Buffer;
      Length : out Natural)
     with Post => Length <= Protocol.Max_PDU_Size;

   --  Set charge/discharge power (positive = charge, negative = discharge)
   procedure Encode_Set_Power_Request
     (Base_Address  : Register_Address;
      Model_Offset  : Register_Address;
      Power_W       : Integer;
      Buffer        : out Protocol.PDU_Buffer;
      Length        : out Natural)
     with Post => Length <= Protocol.Max_PDU_Size;

end Ada_Modbus.Energy.SunSpec.Storage;
