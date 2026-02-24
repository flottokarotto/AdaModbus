--  UART_Console - Serial output via configurable callback
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Board-independent serial console.  Call Set_Output with a
--  board-specific byte-send procedure before using Put/Put_Line.

with Interfaces; use Interfaces;

package UART_Console is

   type Put_Byte_Callback is access procedure (B : Unsigned_8);

   --  Register output callback (must be called before any Put)
   procedure Set_Output (Callback : Put_Byte_Callback);

   procedure Put (Msg : String);
   procedure Put_Line (Msg : String);
   procedure Put_Int (Value : Integer_32);

end UART_Console;
