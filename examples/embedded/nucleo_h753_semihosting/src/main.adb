--  Main - KSEM UART Test
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Minimal test application for NUCLEO-H753ZI:
--  Connects to KSEM via Modbus TCP and outputs power readings
--  via USART3 (ST-Link VCP, 115200 8N1).
--
--  Flash:
--    ./flash.sh

with Interfaces; use Interfaces;
with Ada_Modbus; use Ada_Modbus;
with STM32H7_HAL;
with HAL_Stubs;
with TCP_Client;
with Config;
with KSEM_Client;
with UART_Console;

procedure Main is

   Last_Read : Unsigned_32 := 0;
   Read_Interval_Ms : constant := 2000;

begin
   --  Hardware init (clock, SysTick, GPIO, LEDs)
   STM32H7_HAL.System_Init;
   STM32H7_HAL.SysTick_Init;
   STM32H7_HAL.GPIO_Init;

   --  Initialize USART3 for serial console (ST-Link VCP)
   STM32H7_HAL.USART3_Init (115_200);

   UART_Console.Put_Line ("NUCLEO-H753ZI KSEM Test");
   UART_Console.Put_Line ("Initializing Ethernet...");

   --  Initialize Ethernet/TCP
   HAL_Stubs.Ethernet_Initialize;

   UART_Console.Put_Line ("Ethernet OK, waiting for link...");

   --  Wait for Ethernet link (with timeout)
   declare
      Start : constant Unsigned_32 := HAL_Stubs.Get_Tick_Ms;
   begin
      while not TCP_Client.Link_Is_Up loop
         HAL_Stubs.Ethernet_Poll;
         if HAL_Stubs.Get_Tick_Ms - Start > 10_000 then
            UART_Console.Put_Line ("ERROR: No Ethernet link after 10s");
            HAL_Stubs.Set_LED (HAL_Stubs.LED_Red, True);
            loop
               null;  --  Halt, keep heartbeat blinking
            end loop;
         end if;
      end loop;
   end;

   UART_Console.Put_Line ("Link up!");

   --  Print network configuration
   UART_Console.Put ("Local IP: 192.168.");
   UART_Console.Put_Int (Integer_32 (Config.Local_IP_C));
   UART_Console.Put (".");
   UART_Console.Put_Int (Integer_32 (Config.Local_IP_D));
   UART_Console.Put_Line ("");
   UART_Console.Put ("KSEM IP:  192.168.");
   UART_Console.Put_Int (Integer_32 (Config.KSEM_IP_C));
   UART_Console.Put (".");
   UART_Console.Put_Int (Integer_32 (Config.KSEM_IP_D));
   UART_Console.Put (":");
   UART_Console.Put_Int (Integer_32 (Config.KSEM_Port));
   UART_Console.Put_Line ("");

   --  Wait 2 seconds for switch to learn MAC address
   UART_Console.Put_Line ("Waiting for network settle...");
   declare
      Start : constant Unsigned_32 := HAL_Stubs.Get_Tick_Ms;
   begin
      while HAL_Stubs.Get_Tick_Ms - Start < 2000 loop
         HAL_Stubs.Ethernet_Poll;
      end loop;
   end;

   --  Initialize and connect to KSEM
   KSEM_Client.Initialize;

   UART_Console.Put_Line ("Connecting to KSEM...");

   declare
      Result : Status;
      Retry  : Natural := 0;
   begin
      loop
         UART_Console.Put ("TCP connect attempt ");
         UART_Console.Put_Int (Integer_32 (Retry + 1));
         UART_Console.Put_Line ("...");

         KSEM_Client.Connect (Result);

         UART_Console.Put ("Result: ");
         UART_Console.Put_Int (Integer_32 (Status'Pos (Result)));
         UART_Console.Put_Line ("");

         exit when Result = Success;

         Retry := Retry + 1;
         if Retry >= 1 then
            UART_Console.Put ("ERROR: KSEM connect failed after 1 attempts");
            UART_Console.Put_Int (Integer_32 (Status'Pos (Result)));
            UART_Console.Put_Line (")");
            HAL_Stubs.Set_LED (HAL_Stubs.LED_Red, True);
            loop
               HAL_Stubs.Ethernet_Poll;
            end loop;
         end if;

         --  Wait 2s before retry, keep polling
         declare
            Wait_Start : constant Unsigned_32 := HAL_Stubs.Get_Tick_Ms;
         begin
            while HAL_Stubs.Get_Tick_Ms - Wait_Start < 2000 loop
               HAL_Stubs.Ethernet_Poll;
            end loop;
         end;
      end loop;
   end;

   UART_Console.Put_Line ("KSEM connected!");
   HAL_Stubs.Set_LED (HAL_Stubs.LED_Blue, True);

   --  Main loop: read power every 2 seconds
   loop
      HAL_Stubs.Ethernet_Poll;

      if HAL_Stubs.Get_Tick_Ms - Last_Read >= Read_Interval_Ms then
         Last_Read := HAL_Stubs.Get_Tick_Ms;

         declare
            Data   : KSEM_Client.Power_Data;
            Result : Status;
         begin
            KSEM_Client.Read_Power (Data, Result);

            if Result = Success and then Data.Valid then
               UART_Console.Put ("Grid: ");
               UART_Console.Put_Int (Data.Total_Power_W);
               UART_Console.Put ("W  L1:");
               UART_Console.Put_Int (Data.Phase_L1_W);
               UART_Console.Put ("  L2:");
               UART_Console.Put_Int (Data.Phase_L2_W);
               UART_Console.Put ("  L3:");
               UART_Console.Put_Int (Data.Phase_L3_W);
               UART_Console.Put_Line ("W");
            else
               UART_Console.Put_Line ("KSEM read failed");
               HAL_Stubs.Toggle_LED (HAL_Stubs.LED_Red);
            end if;
         end;
      end if;
   end loop;
end Main;
