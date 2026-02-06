--  Delta_Client - Delta Wallbox Modbus RTU Client
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Controls Delta AC Max Basic Wallbox via Modbus RTU (RS485).
--  Uses Ada_Modbus.Energy.Delta_Charger for protocol encoding.
--
--  Hardware:
--    - USART3: TX=PD8, RX=PD9
--    - RS485 DE/RE: PD12 (directly controlled)
--    - 115200 Baud, 8N1

with Interfaces; use Interfaces;
with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.Energy.Delta_Charger; use Ada_Modbus.Energy.Delta_Charger;

package Delta_Client is

   --  Initialize Delta client (USART3, RS485)
   procedure Initialize;

   --  Read current EVSE status
   procedure Read_Status
     (Status : out EVSE_Status;
      Result : out Ada_Modbus.Status);

   --  Read charger info (state, EVSE count)
   procedure Read_Info
     (Info   : out Charger_Info;
      Result : out Ada_Modbus.Status);

   --  Set charging power limit (Watts)
   procedure Set_Power_Limit
     (Power_W : Unsigned_32;
      Result  : out Ada_Modbus.Status);

   --  Suspend or resume charging
   procedure Set_Suspend
     (Suspend : Boolean;
      Result  : out Ada_Modbus.Status);

   --  Get last known status (cached)
   function Get_Last_Status return EVSE_Status;

   --  Get last known power limit (cached)
   function Get_Current_Power_Limit return Unsigned_32;

   --  Check if charger is in charging state
   function Is_Charging return Boolean;

   --  Check if charger is available for charging
   function Is_Available return Boolean;

end Delta_Client;
