--  Config - PV Charger Configuration
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Central configuration for all system parameters.
--  Modify this file to match your installation.

with Interfaces; use Interfaces;
with Ada_Modbus; use Ada_Modbus;

package Config is

   -------------------------
   --  Network Settings   --
   -------------------------

   --  NUCLEO-H753ZI IP Configuration (static)
   Local_IP_A : constant Unsigned_8 := 192;
   Local_IP_B : constant Unsigned_8 := 168;
   Local_IP_C : constant Unsigned_8 := 1;
   Local_IP_D : constant Unsigned_8 := 100;

   Local_Netmask_A : constant Unsigned_8 := 255;
   Local_Netmask_B : constant Unsigned_8 := 255;
   Local_Netmask_C : constant Unsigned_8 := 255;
   Local_Netmask_D : constant Unsigned_8 := 0;

   Local_Gateway_A : constant Unsigned_8 := 192;
   Local_Gateway_B : constant Unsigned_8 := 168;
   Local_Gateway_C : constant Unsigned_8 := 1;
   Local_Gateway_D : constant Unsigned_8 := 1;

   --  KSEM (Kostal Smart Energy Meter) IP Address
   KSEM_IP_A : constant Unsigned_8 := 192;
   KSEM_IP_B : constant Unsigned_8 := 168;
   KSEM_IP_C : constant Unsigned_8 := 1;
   KSEM_IP_D : constant Unsigned_8 := 50;
   KSEM_Port : constant Unsigned_16 := 502;  --  Modbus TCP standard port

   ---------------------------
   --  Modbus RTU Settings  --
   ---------------------------

   --  Delta Wallbox Modbus RTU (RS485)
   Delta_Device_Id : constant Unit_Id := 1;
   Delta_Baud_Rate : constant := 115200;  --  Baud
   --  USART3: TX=PD8, RX=PD9, DE/RE=PD12 (directly controlled)

   --------------------------
   --  Charging Settings   --
   --------------------------

   --  Minimum charging power (Watts)
   --  Below this, suspend charging to avoid inefficiency
   Min_Charging_Power_W : constant := 1380;  --  ~6A @ 230V

   --  Maximum charging power (Watts)
   --  Delta AC Max Basic: 11 kW (3-phase, 16A)
   Max_Charging_Power_W : constant := 11000;

   --  Hysteresis to prevent oscillation (Watts)
   Power_Hysteresis_W : constant := 200;

   --  Grid import threshold (Watts)
   --  Allow small grid import to maximize PV usage
   Grid_Import_Threshold_W : constant := 100;

   --  Update interval (milliseconds)
   Update_Interval_Ms : constant := 2000;  --  2 seconds

   --  Communication timeout (milliseconds)
   Modbus_Timeout_Ms : constant := 1000;

   -------------------
   --  I2C / OLED   --
   -------------------

   --  I2C1 for SSD1306 OLED (PB8=SCL, PB9=SDA)
   OLED_I2C_Address : constant Unsigned_8 := 16#3C#;  --  0x3C or 0x3D

   --  Display dimensions
   OLED_Width  : constant := 128;
   OLED_Height : constant := 64;

   -----------------
   --  GPIO Pins  --
   -----------------

   --  User Button B1 (active low)
   Button_Pin : constant := 13;  --  PC13

   --  User LEDs
   LED_Green_Pin  : constant := 0;   --  PB0 (LD1)
   LED_Blue_Pin   : constant := 7;   --  PB7 (LD2)
   LED_Red_Pin    : constant := 14;  --  PB14 (LD3)

   --  RS485 Direction Control (directly controlled via GPIO)
   RS485_DE_Pin : constant := 12;  --  PD12

end Config;
