--  LM3S_UART - UART Driver for LM3S6965 (QEMU lm3s6965evb)
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Minimal UART driver for Stellaris LM3S6965, designed for QEMU emulation.

with Interfaces; use Interfaces;

package LM3S_UART is

   --  UART peripherals available on LM3S6965
   type UART_Peripheral is (UART0, UART1, UART2);

   --  Supported baud rates (for 50 MHz system clock in QEMU)
   type Baud_Rate is (B9600, B19200, B38400, B57600, B115200);

   --  Byte array for data transfer
   type Byte_Array is array (Natural range <>) of Unsigned_8;

   --  Initialize UART peripheral
   procedure Initialize (Port : UART_Peripheral; Rate : Baud_Rate);

   --  Send data (blocking)
   function Send (Port : UART_Peripheral; Data : Byte_Array) return Natural;

   --  Receive data with timeout (returns number of bytes received)
   function Receive
     (Port       : UART_Peripheral;
      Buffer     : out Byte_Array;
      Max_Length : Natural;
      Timeout_Ms : Natural) return Natural;

   --  Check if TX is ready
   function TX_Ready (Port : UART_Peripheral) return Boolean;

   --  Check if RX data is available
   function RX_Available (Port : UART_Peripheral) return Boolean;

end LM3S_UART;
