--  System_Init - Early System Initialization
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Called before main() to initialize hardware.

package System_Init is

   --  Initialize system clock, SysTick, and GPIO
   procedure Initialize;

end System_Init;
