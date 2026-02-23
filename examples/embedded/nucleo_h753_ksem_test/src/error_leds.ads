--  Error_LEDs - LED callback for Last_Chance_Handler
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

package Error_LEDs is

   --  Red LED on, green heartbeat LED off
   procedure On_Exception;

end Error_LEDs;
