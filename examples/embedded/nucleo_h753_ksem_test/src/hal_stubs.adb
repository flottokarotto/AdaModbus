--  HAL_Stubs - Implementation using STM32H7 HAL and LwIP
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Bridges the application layer to:
--  - STM32H7 HAL (GPIO, USART, I2C)
--  - TCP_Client (LwIP-based TCP)

with STM32H7_HAL;
with TCP_Client;
with Modbus_TCP_Master;
with Time_Exports;  --  Ensure time export is elaborated
pragma Unreferenced (Time_Exports);
with Config;

package body HAL_Stubs is

   --  Network initialized flag
   Network_Initialized : Boolean := False;

   -----------------
   -- Get_Tick_Ms --
   -----------------

   function Get_Tick_Ms return Unsigned_32 is
   begin
      return STM32H7_HAL.Get_Tick;
   end Get_Tick_Ms;

   --------------
   -- Delay_Ms --
   --------------

   procedure Delay_Ms (Ms : Natural) is
   begin
      STM32H7_HAL.Delay_Ms (Unsigned_32 (Ms));
   end Delay_Ms;

   -----------------------
   -- USART3_Initialize --
   -----------------------

   procedure USART3_Initialize (Baud_Rate : Natural) is
   begin
      STM32H7_HAL.USART3_Init (Unsigned_32 (Baud_Rate));
   end USART3_Initialize;

   ----------------------
   -- RS485_Transceive --
   ----------------------

   procedure RS485_Transceive
     (TX_Data    : Byte_Array;
      RX_Data    : out Byte_Array;
      RX_Length  : out Natural;
      Timeout_Ms : Natural;
      Result     : out Status)
   is
   begin
      STM32H7_HAL.RS485_Transceive
        (TX_Data    => TX_Data,
         RX_Data    => RX_Data,
         RX_Length  => RX_Length,
         Timeout_Ms => Unsigned_32 (Timeout_Ms),
         Result     => Result);
   end RS485_Transceive;

   --------------------
   -- I2C1_Initialize --
   --------------------

   procedure I2C1_Initialize is
   begin
      STM32H7_HAL.I2C1_Init;
   end I2C1_Initialize;

   ---------------
   -- I2C_Write --
   ---------------

   procedure I2C_Write
     (Address : Unsigned_8;
      Data    : Byte_Array)
   is
      Success : Boolean;
   begin
      STM32H7_HAL.I2C1_Write (Address * 2, Data, Success);
   end I2C_Write;

   --------------
   -- I2C_Read --
   --------------

   procedure I2C_Read
     (Address : Unsigned_8;
      Data    : out Byte_Array;
      Length  : Natural)
   is
      Success : Boolean;
   begin
      STM32H7_HAL.I2C1_Read (Address * 2 + 1, Data, Length, Success);
   end I2C_Read;

   -------------------------
   -- Ethernet_Initialize --
   -------------------------

   procedure Ethernet_Initialize is
      use Config;

      Local_IP : constant Unsigned_32 :=
        Unsigned_32 (Local_IP_A) or
        Unsigned_32 (Local_IP_B) * 256 or
        Unsigned_32 (Local_IP_C) * 65536 or
        Unsigned_32 (Local_IP_D) * 16777216;

      Netmask : constant Unsigned_32 :=
        Unsigned_32 (Local_Netmask_A) or
        Unsigned_32 (Local_Netmask_B) * 256 or
        Unsigned_32 (Local_Netmask_C) * 65536 or
        Unsigned_32 (Local_Netmask_D) * 16777216;

      Gateway : constant Unsigned_32 :=
        Unsigned_32 (Local_Gateway_A) or
        Unsigned_32 (Local_Gateway_B) * 256 or
        Unsigned_32 (Local_Gateway_C) * 65536 or
        Unsigned_32 (Local_Gateway_D) * 16777216;
   begin
      --  Initialize TCP/IP stack (Ethernet GPIO configured by LwIP ethernetif.c)
      TCP_Client.Initialize (Local_IP, Netmask, Gateway);

      Network_Initialized := True;
   end Ethernet_Initialize;

   -------------------
   -- Ethernet_Poll --
   -------------------

   procedure Ethernet_Poll is
   begin
      if Network_Initialized then
         TCP_Client.Poll;
      end if;
   end Ethernet_Poll;

   -----------------
   -- TCP_Connect --
   -----------------

   procedure TCP_Connect
     (IP_A, IP_B, IP_C, IP_D : Unsigned_8;
      Port                   : Unsigned_16;
      Timeout_Ms             : Natural;
      Result                 : out Status)
   is
      Remote_IP : constant Unsigned_32 :=
        Unsigned_32 (IP_A) or
        Unsigned_32 (IP_B) * 256 or
        Unsigned_32 (IP_C) * 65536 or
        Unsigned_32 (IP_D) * 16777216;
   begin
      if not Network_Initialized then
         Result := Invalid_Request;
         return;
      end if;

      TCP_Client.Connect (Remote_IP, Port, Unsigned_32 (Timeout_Ms), Result);
   end TCP_Connect;

   --------------------
   -- TCP_Disconnect --
   --------------------

   procedure TCP_Disconnect is
   begin
      TCP_Client.Disconnect;
   end TCP_Disconnect;

   ----------------------
   -- TCP_Is_Connected --
   ----------------------

   function TCP_Is_Connected return Boolean is
   begin
      return TCP_Client.Is_Connected;
   end TCP_Is_Connected;

   --------------------
   -- TCP_Transceive --
   --------------------

   procedure TCP_Transceive
     (TX_Data    : Byte_Array;
      RX_Data    : out Byte_Array;
      RX_Length  : out Natural;
      Timeout_Ms : Natural;
      Result     : out Status)
   is
   begin
      if not TCP_Client.Is_Connected then
         RX_Data := [others => 0];
         RX_Length := 0;
         Result := Invalid_Request;
         return;
      end if;

      TCP_Client.Transceive
        (TX_Data    => TX_Data,
         RX_Data    => RX_Data,
         RX_Length  => RX_Length,
         Timeout_Ms => Unsigned_32 (Timeout_Ms),
         Result     => Result);
   end TCP_Transceive;

   --------------------------------------
   -- Modbus_TCP_Read_Holding_Registers --
   --------------------------------------

   procedure Modbus_TCP_Read_Holding_Registers
     (Unit_Id       : Ada_Modbus.Unit_Id;
      Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array;
      Count         : out Natural;
      Result        : out Status)
   is
   begin
      Modbus_TCP_Master.Read_Holding_Registers
        (Unit          => Unit_Id,
         Start_Address => Start_Address,
         Quantity      => Quantity,
         Values        => Values,
         Count         => Count,
         Timeout_Ms    => Unsigned_32 (Config.Modbus_Timeout_Ms),
         Result        => Result);
   end Modbus_TCP_Read_Holding_Registers;

   ------------------------------------
   -- Modbus_TCP_Read_Input_Registers --
   ------------------------------------

   procedure Modbus_TCP_Read_Input_Registers
     (Unit_Id       : Ada_Modbus.Unit_Id;
      Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array;
      Count         : out Natural;
      Result        : out Status)
   is
   begin
      Modbus_TCP_Master.Read_Input_Registers
        (Unit          => Unit_Id,
         Start_Address => Start_Address,
         Quantity      => Quantity,
         Values        => Values,
         Count         => Count,
         Timeout_Ms    => Unsigned_32 (Config.Modbus_Timeout_Ms),
         Result        => Result);
   end Modbus_TCP_Read_Input_Registers;

   ---------------------
   -- GPIO_Initialize --
   ---------------------

   procedure GPIO_Initialize is
   begin
      STM32H7_HAL.GPIO_Init;
   end GPIO_Initialize;

   -------------
   -- Set_LED --
   -------------

   procedure Set_LED (LED : LED_Color; On : Boolean) is
   begin
      case LED is
         when LED_Green  => STM32H7_HAL.GPIO_Write (STM32H7_HAL.Port_B, 0, On);
         when LED_Yellow => STM32H7_HAL.GPIO_Write (STM32H7_HAL.Port_E, 1, On);
         when LED_Red    => STM32H7_HAL.GPIO_Write (STM32H7_HAL.Port_B, 14, On);
      end case;
   end Set_LED;

   --------------------
   -- Button_Pressed --
   --------------------

   function Button_Pressed return Boolean is
   begin
      --  Button B1 on PC13 is active low
      return not STM32H7_HAL.GPIO_Read (STM32H7_HAL.Port_C, 13);
   end Button_Pressed;

   ----------------
   -- Toggle_LED --
   ----------------

   procedure Toggle_LED (LED : LED_Color) is
   begin
      case LED is
         when LED_Green  => STM32H7_HAL.GPIO_Toggle (STM32H7_HAL.Port_B, 0);
         when LED_Yellow => STM32H7_HAL.GPIO_Toggle (STM32H7_HAL.Port_E, 1);
         when LED_Red    => STM32H7_HAL.GPIO_Toggle (STM32H7_HAL.Port_B, 14);
      end case;
   end Toggle_LED;

end HAL_Stubs;
