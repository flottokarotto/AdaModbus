--  Main - PV Surplus Charger Application
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Main application for NUCLEO-H753ZI PV surplus charging controller.
--
--  Hardware connections:
--    - Ethernet (onboard LAN8742A) -> KSEM (Modbus TCP)
--    - USART3/RS485 (Waveshare Shield) -> Delta Wallbox (Modbus RTU)
--    - I2C1 (PB8/PB9) -> SSD1306 OLED Display
--    - User Button B1 (PC13) -> Manual control
--    - User LEDs (PB0/PB7/PB14) -> Status indication
--
--  LED Indicators:
--    - Green (LD1): System running
--    - Blue (LD2): Charging active
--    - Red (LD3): Error condition
--
--  Button Functions:
--    - Short press: Toggle display mode
--    - Long press (3s): Force resume/suspend

with Interfaces;
with Ada_Modbus;
with Ada_Modbus.Energy.Delta_Charger;
with STM32H7_HAL;
with Config;
with HAL_Stubs;
with KSEM_Client;
with Delta_Client;
with PV_Controller;
with OLED_Display;

procedure Main is

   use type PV_Controller.Controller_State;

   --  State machine
   type App_State is
     (State_Init,        --  System initialization
      State_Connecting,  --  Connecting to devices
      State_Running,     --  Normal operation
      State_Error);      --  Error state

   Current_State : App_State := State_Init;
   Last_Update   : Interfaces.Unsigned_32 := 0;
   Error_Message : String (1 .. 21) := (others => ' ');

   --  Button handling
   Button_Down_Time : Interfaces.Unsigned_32 := 0;
   Button_Was_Down  : Boolean := False;
   Long_Press_Ms    : constant := 3000;

   --  Display mode
   type Display_Mode is (Mode_Status, Mode_Statistics, Mode_Debug);
   Current_Display_Mode : Display_Mode := Mode_Status;

   -----------------------
   -- Initialize_System --
   -----------------------

   procedure Initialize_System is
   begin
      --  Early hardware init (clock, SysTick, GPIO)
      STM32H7_HAL.System_Init;
      STM32H7_HAL.SysTick_Init;
      STM32H7_HAL.GPIO_Init;

      --  Initialize Ethernet/TCP
      HAL_Stubs.Ethernet_Initialize;

      --  Show splash screen (initializes I2C1 internally)
      OLED_Display.Initialize;
      OLED_Display.Show_Splash;
      HAL_Stubs.Set_LED (HAL_Stubs.LED_Green, True);

      --  Brief delay for splash
      HAL_Stubs.Delay_Ms (2000);

      --  Initialize PV controller (initializes KSEM and Delta clients internally)
      PV_Controller.Initialize;
   end Initialize_System;

   ----------------------
   -- Connect_Devices --
   ----------------------

   procedure Connect_Devices is
      use Ada_Modbus;
      Result : Status;
   begin
      OLED_Display.Put_Line (1, "Connecting KSEM...");

      --  Connect to KSEM
      KSEM_Client.Connect (Result);
      if Result /= Success then
         Error_Message := "KSEM connect failed  ";
         Current_State := State_Error;
         return;
      end if;

      OLED_Display.Put_Line (1, "KSEM OK");
      OLED_Display.Put_Line (2, "Checking Delta...");

      --  Test Delta connection (read charger info)
      declare
         use Ada_Modbus.Energy.Delta_Charger;
         Info : Charger_Info;
      begin
         Delta_Client.Read_Info (Info, Result);
         if Result /= Success then
            Error_Message := "Delta connect failed ";
            Current_State := State_Error;
            return;
         end if;

         OLED_Display.Put_Line (2, "Delta OK - EVSEs: " &
                                Natural'Image (Info.EVSE_Count));
      end;

      --  All connected
      OLED_Display.Put_Line (3, "System ready!");
      HAL_Stubs.Delay_Ms (1000);
      Current_State := State_Running;
   end Connect_Devices;

   -----------------------
   -- Handle_Button --
   -----------------------

   procedure Handle_Button is
      use Interfaces;
      Now : constant Unsigned_32 := HAL_Stubs.Get_Tick_Ms;
      Pressed : constant Boolean := HAL_Stubs.Button_Pressed;
   begin
      if Pressed then
         if not Button_Was_Down then
            --  Button just pressed
            Button_Down_Time := Now;
            Button_Was_Down := True;
         elsif Now - Button_Down_Time >= Long_Press_Ms then
            --  Long press detected
            if PV_Controller.Get_State = PV_Controller.Charging then
               PV_Controller.Force_Suspend;
               OLED_Display.Put_Line (3, "Manual: SUSPENDED");
            else
               PV_Controller.Force_Resume;
               OLED_Display.Put_Line (3, "Manual: RESUMING");
            end if;
            --  Reset to prevent repeat
            Button_Down_Time := Now;
         end if;
      else
         if Button_Was_Down then
            --  Button released
            if Now - Button_Down_Time < Long_Press_Ms then
               --  Short press - cycle display mode
               case Current_Display_Mode is
                  when Mode_Status =>
                     Current_Display_Mode := Mode_Statistics;
                  when Mode_Statistics =>
                     Current_Display_Mode := Mode_Debug;
                  when Mode_Debug =>
                     Current_Display_Mode := Mode_Status;
               end case;
            end if;
            Button_Was_Down := False;
         end if;
      end if;
   end Handle_Button;

   --------------------
   -- Update_Display --
   --------------------

   procedure Update_Display is
      use PV_Controller;
      State : constant Controller_State := Get_State;
      Stats : Statistics;
      State_Name : String (1 .. 8) := "        ";
   begin
      --  Get state name
      case State is
         when Idle       => State_Name := "IDLE    ";
         when Starting   => State_Name := "STARTING";
         when Charging   => State_Name := "CHARGING";
         when Suspending => State_Name := "STOPPING";
         when Suspended  => State_Name := "SUSPEND ";
         when Error      => State_Name := "ERROR   ";
      end case;

      case Current_Display_Mode is
         when Mode_Status =>
            OLED_Display.Update_Display
              (State_Name   => State_Name,
               Grid_Power_W => Get_Grid_Power_W,
               Charge_W     => Get_Actual_Power_W,
               SOC_Percent  => Delta_Client.Get_Last_Status.SOC_Percent_x10 / 10,
               Session_Wh   => Get_Statistics.Session_Energy_Wh);

         when Mode_Statistics =>
            Stats := Get_Statistics;
            OLED_Display.Put_Line (0, "=== STATISTICS ===");
            OLED_Display.Put_Line (1, "Total:" &
                                   Interfaces.Unsigned_32'Image (Stats.Total_Energy_Wh) & "Wh");
            OLED_Display.Put_Line (2, "Peak:" &
                                   Interfaces.Unsigned_32'Image (Stats.Peak_Power_W) & "W");
            OLED_Display.Put_Line (3, "Adj:" &
                                   Natural'Image (Stats.Adjustments_Count));

         when Mode_Debug =>
            OLED_Display.Put_Line (0, "=== DEBUG ===");
            OLED_Display.Put_Line (1, "Grid:" &
                                   Interfaces.Integer_32'Image (Get_Grid_Power_W));
            OLED_Display.Put_Line (2, "Target:" &
                                   Interfaces.Unsigned_32'Image (Get_Target_Power_W));
            OLED_Display.Put_Line (3, "Actual:" &
                                   Interfaces.Unsigned_32'Image (Get_Actual_Power_W));
      end case;

      --  Update LEDs
      HAL_Stubs.Toggle_LED (HAL_Stubs.LED_Green);  --  Heartbeat
      HAL_Stubs.Set_LED (HAL_Stubs.LED_Blue, State = Charging);
      HAL_Stubs.Set_LED (HAL_Stubs.LED_Red, State = Error);
   end Update_Display;

   ------------------
   -- Handle_Error --
   ------------------

   procedure Handle_Error is
   begin
      OLED_Display.Show_Error (Error_Message);
      HAL_Stubs.Set_LED (HAL_Stubs.LED_Red, True);
      HAL_Stubs.Set_LED (HAL_Stubs.LED_Blue, False);

      --  Wait for button press to retry
      while not HAL_Stubs.Button_Pressed loop
         HAL_Stubs.Toggle_LED (HAL_Stubs.LED_Red);
         HAL_Stubs.Delay_Ms (500);
      end loop;

      --  Debounce
      HAL_Stubs.Delay_Ms (200);
      while HAL_Stubs.Button_Pressed loop
         null;
      end loop;

      --  Retry connection
      Current_State := State_Connecting;
   end Handle_Error;

   ---------------
   -- Main Loop --
   ---------------

   use type Interfaces.Unsigned_32;

begin
   --  System initialization
   Initialize_System;
   Current_State := State_Connecting;

   --  Main loop
   loop
      --  Process Ethernet packets
      HAL_Stubs.Ethernet_Poll;

      --  Handle button input
      Handle_Button;

      case Current_State is
         when State_Init =>
            Initialize_System;
            Current_State := State_Connecting;

         when State_Connecting =>
            Connect_Devices;

         when State_Running =>
            --  Check if it's time for an update
            if HAL_Stubs.Get_Tick_Ms - Last_Update >= Config.Update_Interval_Ms then
               --  Run PV controller
               PV_Controller.Update;

               --  Update display
               Update_Display;

               Last_Update := HAL_Stubs.Get_Tick_Ms;

               --  Check for errors
               if PV_Controller.Get_State = PV_Controller.Error then
                  Error_Message := "Communication error  ";
                  Current_State := State_Error;
               end if;
            end if;

         when State_Error =>
            Handle_Error;
      end case;
   end loop;
end Main;
