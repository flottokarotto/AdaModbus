--  HAL_Stubs - Hardware Abstraction Layer Stubs
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Placeholder implementations for hardware peripherals.
--  Replace with actual STM32H7 HAL implementations.
--
--  Required peripherals:
--    - USART3 (RS485 via Waveshare Shield)
--    - I2C1 (OLED Display)
--    - LAN8742A Ethernet (onboard)
--    - GPIO (LEDs, Button, RS485 DE)
--    - SysTick (timing)

with Interfaces; use Interfaces;
with Ada_Modbus; use Ada_Modbus;

package HAL_Stubs is

   -----------------------
   --  System Timing    --
   -----------------------

   --  Get system tick in milliseconds
   function Get_Tick_Ms return Unsigned_32;

   --  Delay for specified milliseconds
   procedure Delay_Ms (Ms : Natural);

   ----------------------
   --  USART3 / RS485  --
   ----------------------

   --  Initialize USART3 for RS485 communication
   procedure USART3_Initialize (Baud_Rate : Natural);

   --  Transmit and receive via RS485 (half-duplex)
   --  Controls DE/RE pin automatically
   procedure RS485_Transceive
     (TX_Data    : Byte_Array;
      RX_Data    : out Byte_Array;
      RX_Length  : out Natural;
      Timeout_Ms : Natural;
      Result     : out Status);

   ----------------------
   --  I2C1 / OLED     --
   ----------------------

   --  Initialize I2C1
   procedure I2C1_Initialize;

   --  Write data to I2C device
   procedure I2C_Write
     (Address : Unsigned_8;
      Data    : Byte_Array);

   --  Read data from I2C device
   procedure I2C_Read
     (Address : Unsigned_8;
      Data    : out Byte_Array;
      Length  : Natural);

   -------------------------
   --  Ethernet / TCP     --
   -------------------------

   --  Initialize Ethernet peripheral and LwIP stack
   procedure Ethernet_Initialize;

   --  Process Ethernet packets (call periodically)
   procedure Ethernet_Poll;

   --  Connect to TCP server
   procedure TCP_Connect
     (IP_A, IP_B, IP_C, IP_D : Unsigned_8;
      Port                   : Unsigned_16;
      Timeout_Ms             : Natural;
      Result                 : out Status);

   --  Disconnect TCP connection
   procedure TCP_Disconnect;

   --  Check if TCP connected
   function TCP_Is_Connected return Boolean;

   --  Send/Receive TCP data (for Modbus TCP)
   procedure TCP_Transceive
     (TX_Data    : Byte_Array;
      RX_Data    : out Byte_Array;
      RX_Length  : out Natural;
      Timeout_Ms : Natural;
      Result     : out Status);

   --  Modbus TCP Read Holding Registers (FC 03)
   procedure Modbus_TCP_Read_Holding_Registers
     (Unit_Id       : Ada_Modbus.Unit_Id;
      Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array;
      Count         : out Natural;
      Result        : out Status);

   --  Modbus TCP Read Input Registers (FC 04)
   procedure Modbus_TCP_Read_Input_Registers
     (Unit_Id       : Ada_Modbus.Unit_Id;
      Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array;
      Count         : out Natural;
      Result        : out Status);

   ------------------
   --  GPIO / LEDs --
   ------------------

   --  Initialize GPIO pins
   procedure GPIO_Initialize;

   --  Set LED state
   type LED_Color is (LED_Green, LED_Blue, LED_Red);
   procedure Set_LED (LED : LED_Color; On : Boolean);

   --  Read button state (returns True when pressed)
   function Button_Pressed return Boolean;

   --  Toggle LED
   procedure Toggle_LED (LED : LED_Color);

end HAL_Stubs;
