--  UART_Console - Serial output via USART3 (ST-Link VCP)
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  NUCLEO-H753ZI has USART3 (PD8=TX, PD9=RX) routed to ST-Link USB
--  as Virtual COM Port. Connect at 115200 8N1.

with Interfaces; use Interfaces;

package UART_Console is

   procedure Put (Msg : String);
   procedure Put_Line (Msg : String);
   procedure Put_Int (Value : Integer_32);

end UART_Console;
