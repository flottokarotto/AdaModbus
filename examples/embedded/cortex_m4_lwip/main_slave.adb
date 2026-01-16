--  Main_Slave - Vollständiges Modbus TCP Slave Beispiel für STM32F4
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Target: STM32F4-Discovery oder QEMU (netduino2)
--
--  QEMU-Ausführung:
--    qemu-system-arm -M netduino2 -nographic -kernel main_slave
--
--  Hinweis: Für echtes Netzwerk benötigt QEMU TAP-Interface:
--    qemu-system-arm -M netduino2 -nographic -kernel main_slave \
--      -netdev tap,id=net0,ifname=tap0 -device virtio-net-device,netdev=net0

with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);
with Interfaces; use Interfaces;
with Ada_Modbus; use Ada_Modbus;
with Modbus_TCP_Slave;

procedure Main_Slave is

   Result : Status;

   --  Simulated ADC values
   ADC_Temperature : Register_Value := 250;  --  25.0°C
   ADC_Humidity    : Register_Value := 450;  --  45.0%

   --  Simulated GPIO state
   LED_States : array (0 .. 3) of Boolean := [others => False];

   --  Simple delay (busy wait - replace with proper delay in real app)
   procedure Delay_Ms (Ms : Natural) is
      Iterations : constant Natural := Ms * 10_000;
   begin
      for I in 1 .. Iterations loop
         null;
      end loop;
   end Delay_Ms;

begin
   --  Initialize Modbus TCP Slave on port 502
   Modbus_TCP_Slave.Modbus_Init (502, Result);

   if Result /= Success then
      --  Initialization failed - blink error LED
      loop
         Delay_Ms (100);
      end loop;
   end if;

   --  Main application loop
   loop
      --  Update input registers with sensor values
      --  In real application: read from ADC
      Modbus_TCP_Slave.Set_Input_Register (0, ADC_Temperature);
      Modbus_TCP_Slave.Set_Input_Register (1, ADC_Humidity);

      --  Simulate sensor drift
      ADC_Temperature := ADC_Temperature + 1;
      if ADC_Temperature > 350 then
         ADC_Temperature := 200;
      end if;

      --  Update discrete inputs (e.g., button states)
      --  In real application: read from GPIO
      Modbus_TCP_Slave.Set_Discrete_Input (0, True);   --  Button 1 pressed
      Modbus_TCP_Slave.Set_Discrete_Input (1, False);  --  Button 2 released

      --  Process Modbus requests
      Modbus_TCP_Slave.Modbus_Poll;

      --  Read coil outputs and update LEDs
      --  In real application: write to GPIO
      LED_States (0) := Modbus_TCP_Slave.Get_Coil (0);
      LED_States (1) := Modbus_TCP_Slave.Get_Coil (1);
      LED_States (2) := Modbus_TCP_Slave.Get_Coil (2);
      LED_States (3) := Modbus_TCP_Slave.Get_Coil (3);

      --  Read holding registers for configuration
      --  Register 0: Blink interval in ms
      declare
         Blink_Interval : constant Register_Value :=
           Modbus_TCP_Slave.Get_Holding_Register (0);
      begin
         if Blink_Interval > 0 then
            Delay_Ms (Natural (Blink_Interval));
         else
            Delay_Ms (100);  --  Default 100ms
         end if;
      end;
   end loop;
end Main_Slave;
