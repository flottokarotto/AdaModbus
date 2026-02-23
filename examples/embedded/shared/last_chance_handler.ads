--  Last_Chance_Handler - Exception handler for light runtime
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Generic output via Put_Byte callback. Set_Output must be called
--  before any exception can occur. Works with UART, semihosting, ITM, etc.

with System;
with Interfaces;

package Last_Chance_Handler is

   type Put_Byte_Callback is access procedure (B : Interfaces.Unsigned_8);
   type On_Exception_Callback is access procedure;

   --  Register output callback (call early, e.g. before any allocation)
   procedure Set_Output (Callback : Put_Byte_Callback);

   --  Register callback invoked once before output (e.g. switch LEDs)
   procedure Set_On_Exception (Callback : On_Exception_Callback);

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer);
   pragma Export (C, Last_Chance_Handler, "__gnat_last_chance_handler");

end Last_Chance_Handler;
