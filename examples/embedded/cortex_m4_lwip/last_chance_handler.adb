--  Last_Chance_Handler - Exception handler for light runtime
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Required for light/zfp runtime when exceptions might occur

package body Last_Chance_Handler is

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer) is
      pragma Unreferenced (Msg, Line);
   begin
      --  Endless loop on exception
      --  In real application: log error, reset watchdog, etc.
      loop
         null;
      end loop;
   end Last_Chance_Handler;

end Last_Chance_Handler;
