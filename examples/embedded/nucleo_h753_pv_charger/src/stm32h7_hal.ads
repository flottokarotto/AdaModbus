--  STM32H7_HAL - Hardware Abstraction Layer for STM32H753
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Low-level driver implementations for STM32H753ZI peripherals.

with Interfaces; use Interfaces;
with Ada_Modbus; use Ada_Modbus;

package STM32H7_HAL is

   pragma Preelaborate;

   -----------------------
   --  System Timing    --
   -----------------------

   --  Initialize system clock (HSI 64MHz for simplicity)
   procedure System_Init;

   --  Initialize SysTick for 1ms ticks
   procedure SysTick_Init;

   --  Get system tick in milliseconds
   function Get_Tick return Unsigned_32;

   --  Delay for specified milliseconds
   procedure Delay_Ms (Ms : Unsigned_32);

   ----------------------
   --  GPIO            --
   ----------------------

   type GPIO_Port is (Port_B, Port_C, Port_D);
   type GPIO_Pin is range 0 .. 15;
   type GPIO_Mode is (Mode_Input, Mode_Output, Mode_AF, Mode_Analog);
   type GPIO_AF is range 0 .. 15;

   procedure GPIO_Init;

   procedure GPIO_Set_Mode
     (Port : GPIO_Port;
      Pin  : GPIO_Pin;
      Mode : GPIO_Mode);

   procedure GPIO_Set_AF
     (Port : GPIO_Port;
      Pin  : GPIO_Pin;
      AF   : GPIO_AF);

   procedure GPIO_Write
     (Port  : GPIO_Port;
      Pin   : GPIO_Pin;
      State : Boolean);

   function GPIO_Read
     (Port : GPIO_Port;
      Pin  : GPIO_Pin) return Boolean;

   procedure GPIO_Toggle
     (Port : GPIO_Port;
      Pin  : GPIO_Pin);

   ----------------------
   --  USART3 / RS485  --
   ----------------------

   --  Initialize USART3 (PD8=TX, PD9=RX)
   procedure USART3_Init (Baud_Rate : Unsigned_32);

   --  Send single byte
   procedure USART3_Send_Byte (Data : Unsigned_8);

   --  Send byte array
   procedure USART3_Send (Data : Byte_Array);

   --  Receive single byte with timeout
   procedure USART3_Receive_Byte
     (Data    : out Unsigned_8;
      Timeout : Unsigned_32;
      Success : out Boolean);

   --  Receive byte array with timeout
   procedure USART3_Receive
     (Data    : out Byte_Array;
      Length  : out Natural;
      Timeout : Unsigned_32);

   --  RS485 Direction Control (PD12)
   procedure RS485_Set_TX_Mode;
   procedure RS485_Set_RX_Mode;

   --  RS485 Transceive (half-duplex)
   procedure RS485_Transceive
     (TX_Data    : Byte_Array;
      RX_Data    : out Byte_Array;
      RX_Length  : out Natural;
      Timeout_Ms : Unsigned_32;
      Result     : out Status);

   ----------------------
   --  I2C1            --
   ----------------------

   --  Initialize I2C1 (PB8=SCL, PB9=SDA)
   procedure I2C1_Init;

   --  Write to I2C device
   procedure I2C1_Write
     (Address : Unsigned_8;
      Data    : Byte_Array;
      Success : out Boolean);

   --  Read from I2C device
   procedure I2C1_Read
     (Address : Unsigned_8;
      Data    : out Byte_Array;
      Length  : Natural;
      Success : out Boolean);

end STM32H7_HAL;
