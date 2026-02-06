--  OLED_Display - SSD1306 OLED Display Driver
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Simple text-mode driver for SSD1306 0.96" OLED (128x64).
--  Uses I2C1 on STM32H7 (PB8=SCL, PB9=SDA).
--
--  Display layout (4 lines, 21 chars each):
--    Line 0: "PV CHARGER  [STATE]"
--    Line 1: "Grid: +1234W  PV: 5678W"
--    Line 2: "Charge: 4321W  SOC: 80%"
--    Line 3: "Session: 12.3 kWh"

with Interfaces; use Interfaces;

package OLED_Display is

   --  Display dimensions
   Width  : constant := 128;
   Height : constant := 64;
   Chars_Per_Line : constant := 21;
   Num_Lines      : constant := 4;

   --  Initialize display (I2C, SSD1306 init sequence)
   procedure Initialize;

   --  Clear entire display
   procedure Clear;

   --  Set cursor position (0-based)
   procedure Set_Cursor (Line : Natural; Column : Natural);

   --  Write a single character at cursor
   procedure Put_Char (C : Character);

   --  Write a string at cursor
   procedure Put_String (S : String);

   --  Write a string at specific position
   procedure Put_At (Line : Natural; Column : Natural; S : String);

   --  Clear a line and write string
   procedure Put_Line (Line : Natural; S : String);

   --  Convenience procedures for PV charger display
   procedure Show_State (State_Name : String);
   procedure Show_Grid_Power (Power_W : Integer_32);
   procedure Show_Charge_Power (Power_W : Unsigned_32; SOC_Percent : Natural);
   procedure Show_Session_Energy (Energy_Wh : Unsigned_32);

   --  Update entire display with current values
   procedure Update_Display
     (State_Name   : String;
      Grid_Power_W : Integer_32;
      Charge_W     : Unsigned_32;
      SOC_Percent  : Natural;
      Session_Wh   : Unsigned_32);

   --  Show error message
   procedure Show_Error (Message : String);

   --  Show startup splash
   procedure Show_Splash;

   --  Invert display colors
   procedure Set_Inverted (Inverted : Boolean);

   --  Set display contrast (0-255)
   procedure Set_Contrast (Level : Unsigned_8);

end OLED_Display;
