--  Last_Chance_Handler - Exception handler for light runtime
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with System;

package Last_Chance_Handler is

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer);
   pragma Export (C, Last_Chance_Handler, "__gnat_last_chance_handler");

end Last_Chance_Handler;
