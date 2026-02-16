--  Config - KSEM Semihosting Test Configuration
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Network configuration for KSEM semihosting test.
--  Modify this file to match your installation.

with Interfaces; use Interfaces;

package Config is

   -------------------------
   --  Network Settings   --
   -------------------------

   --  NUCLEO-H753ZI IP Configuration (static)
   Local_IP_A : constant Unsigned_8 := 192;
   Local_IP_B : constant Unsigned_8 := 168;
   Local_IP_C : constant Unsigned_8 := 42;
   Local_IP_D : constant Unsigned_8 := 5;

   Local_Netmask_A : constant Unsigned_8 := 255;
   Local_Netmask_B : constant Unsigned_8 := 255;
   Local_Netmask_C : constant Unsigned_8 := 255;
   Local_Netmask_D : constant Unsigned_8 := 0;

   Local_Gateway_A : constant Unsigned_8 := 192;
   Local_Gateway_B : constant Unsigned_8 := 168;
   Local_Gateway_C : constant Unsigned_8 := 42;
   Local_Gateway_D : constant Unsigned_8 := 1;

   --  KSEM (Kostal Smart Energy Meter) IP Address
   KSEM_IP_A : constant Unsigned_8 := 192;
   KSEM_IP_B : constant Unsigned_8 := 168;
   KSEM_IP_C : constant Unsigned_8 := 42;
   KSEM_IP_D : constant Unsigned_8 := 105;
   KSEM_Port : constant Unsigned_16 := 502;  --  Modbus TCP standard port

   --  Communication timeout (milliseconds)
   Modbus_Timeout_Ms : constant := 3000;

end Config;
