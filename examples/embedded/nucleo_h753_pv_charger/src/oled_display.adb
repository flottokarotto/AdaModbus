--  OLED_Display - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with Ada_Modbus;
with Config;
with HAL_Stubs;

package body OLED_Display is

   --  SSD1306 Commands
   CMD_DISPLAY_OFF     : constant Unsigned_8 := 16#AE#;
   CMD_DISPLAY_ON      : constant Unsigned_8 := 16#AF#;
   CMD_SET_CONTRAST    : constant Unsigned_8 := 16#81#;
   CMD_NORMAL_DISPLAY  : constant Unsigned_8 := 16#A6#;
   CMD_INVERT_DISPLAY  : constant Unsigned_8 := 16#A7#;
   CMD_SET_MUX_RATIO   : constant Unsigned_8 := 16#A8#;
   CMD_SET_DISPLAY_OFFSET : constant Unsigned_8 := 16#D3#;
   CMD_SET_START_LINE  : constant Unsigned_8 := 16#40#;
   CMD_SET_SEG_REMAP   : constant Unsigned_8 := 16#A1#;
   CMD_SET_COM_SCAN    : constant Unsigned_8 := 16#C8#;
   CMD_SET_COM_PINS    : constant Unsigned_8 := 16#DA#;
   CMD_SET_CLOCK_DIV   : constant Unsigned_8 := 16#D5#;  --  Not used (default OK)
   pragma Unreferenced (CMD_SET_CLOCK_DIV);
   CMD_SET_PRECHARGE   : constant Unsigned_8 := 16#D9#;
   CMD_SET_VCOM_DETECT : constant Unsigned_8 := 16#DB#;
   CMD_CHARGE_PUMP     : constant Unsigned_8 := 16#8D#;
   CMD_MEMORY_MODE     : constant Unsigned_8 := 16#20#;
   CMD_COLUMN_ADDR     : constant Unsigned_8 := 16#21#;
   CMD_PAGE_ADDR       : constant Unsigned_8 := 16#22#;

   --  Current cursor position
   Cursor_Line   : Natural := 0;
   Cursor_Column : Natural := 0;

   --  Display initialized flag
   Is_Initialized : Boolean := False;

   --  Simple 5x7 font (ASCII 32-127)
   --  Each character is 6 pixels wide (5 + 1 space)
   type Font_Char is array (0 .. 4) of Unsigned_8;
   type Font_Table is array (Character range ' ' .. '~') of Font_Char;

   --  Simplified font data (subset)
   Font : constant Font_Table :=
     [' ' => [16#00#, 16#00#, 16#00#, 16#00#, 16#00#],
      '!' => [16#00#, 16#00#, 16#5F#, 16#00#, 16#00#],
      '"' => [16#00#, 16#07#, 16#00#, 16#07#, 16#00#],
      '#' => [16#14#, 16#7F#, 16#14#, 16#7F#, 16#14#],
      '$' => [16#24#, 16#2A#, 16#7F#, 16#2A#, 16#12#],
      '%' => [16#23#, 16#13#, 16#08#, 16#64#, 16#62#],
      '&' => [16#36#, 16#49#, 16#55#, 16#22#, 16#50#],
      ''' => [16#00#, 16#05#, 16#03#, 16#00#, 16#00#],
      '(' => [16#00#, 16#1C#, 16#22#, 16#41#, 16#00#],
      ')' => [16#00#, 16#41#, 16#22#, 16#1C#, 16#00#],
      '*' => [16#14#, 16#08#, 16#3E#, 16#08#, 16#14#],
      '+' => [16#08#, 16#08#, 16#3E#, 16#08#, 16#08#],
      ',' => [16#00#, 16#50#, 16#30#, 16#00#, 16#00#],
      '-' => [16#08#, 16#08#, 16#08#, 16#08#, 16#08#],
      '.' => [16#00#, 16#60#, 16#60#, 16#00#, 16#00#],
      '/' => [16#20#, 16#10#, 16#08#, 16#04#, 16#02#],
      '0' => [16#3E#, 16#51#, 16#49#, 16#45#, 16#3E#],
      '1' => [16#00#, 16#42#, 16#7F#, 16#40#, 16#00#],
      '2' => [16#42#, 16#61#, 16#51#, 16#49#, 16#46#],
      '3' => [16#21#, 16#41#, 16#45#, 16#4B#, 16#31#],
      '4' => [16#18#, 16#14#, 16#12#, 16#7F#, 16#10#],
      '5' => [16#27#, 16#45#, 16#45#, 16#45#, 16#39#],
      '6' => [16#3C#, 16#4A#, 16#49#, 16#49#, 16#30#],
      '7' => [16#01#, 16#71#, 16#09#, 16#05#, 16#03#],
      '8' => [16#36#, 16#49#, 16#49#, 16#49#, 16#36#],
      '9' => [16#06#, 16#49#, 16#49#, 16#29#, 16#1E#],
      ':' => [16#00#, 16#36#, 16#36#, 16#00#, 16#00#],
      ';' => [16#00#, 16#56#, 16#36#, 16#00#, 16#00#],
      '<' => [16#08#, 16#14#, 16#22#, 16#41#, 16#00#],
      '=' => [16#14#, 16#14#, 16#14#, 16#14#, 16#14#],
      '>' => [16#00#, 16#41#, 16#22#, 16#14#, 16#08#],
      '?' => [16#02#, 16#01#, 16#51#, 16#09#, 16#06#],
      '@' => [16#32#, 16#49#, 16#79#, 16#41#, 16#3E#],
      'A' => [16#7E#, 16#11#, 16#11#, 16#11#, 16#7E#],
      'B' => [16#7F#, 16#49#, 16#49#, 16#49#, 16#36#],
      'C' => [16#3E#, 16#41#, 16#41#, 16#41#, 16#22#],
      'D' => [16#7F#, 16#41#, 16#41#, 16#22#, 16#1C#],
      'E' => [16#7F#, 16#49#, 16#49#, 16#49#, 16#41#],
      'F' => [16#7F#, 16#09#, 16#09#, 16#09#, 16#01#],
      'G' => [16#3E#, 16#41#, 16#49#, 16#49#, 16#7A#],
      'H' => [16#7F#, 16#08#, 16#08#, 16#08#, 16#7F#],
      'I' => [16#00#, 16#41#, 16#7F#, 16#41#, 16#00#],
      'J' => [16#20#, 16#40#, 16#41#, 16#3F#, 16#01#],
      'K' => [16#7F#, 16#08#, 16#14#, 16#22#, 16#41#],
      'L' => [16#7F#, 16#40#, 16#40#, 16#40#, 16#40#],
      'M' => [16#7F#, 16#02#, 16#0C#, 16#02#, 16#7F#],
      'N' => [16#7F#, 16#04#, 16#08#, 16#10#, 16#7F#],
      'O' => [16#3E#, 16#41#, 16#41#, 16#41#, 16#3E#],
      'P' => [16#7F#, 16#09#, 16#09#, 16#09#, 16#06#],
      'Q' => [16#3E#, 16#41#, 16#51#, 16#21#, 16#5E#],
      'R' => [16#7F#, 16#09#, 16#19#, 16#29#, 16#46#],
      'S' => [16#46#, 16#49#, 16#49#, 16#49#, 16#31#],
      'T' => [16#01#, 16#01#, 16#7F#, 16#01#, 16#01#],
      'U' => [16#3F#, 16#40#, 16#40#, 16#40#, 16#3F#],
      'V' => [16#1F#, 16#20#, 16#40#, 16#20#, 16#1F#],
      'W' => [16#3F#, 16#40#, 16#38#, 16#40#, 16#3F#],
      'X' => [16#63#, 16#14#, 16#08#, 16#14#, 16#63#],
      'Y' => [16#07#, 16#08#, 16#70#, 16#08#, 16#07#],
      'Z' => [16#61#, 16#51#, 16#49#, 16#45#, 16#43#],
      '[' => [16#00#, 16#7F#, 16#41#, 16#41#, 16#00#],
      '\' => [16#02#, 16#04#, 16#08#, 16#10#, 16#20#],
      ']' => [16#00#, 16#41#, 16#41#, 16#7F#, 16#00#],
      '^' => [16#04#, 16#02#, 16#01#, 16#02#, 16#04#],
      '_' => [16#40#, 16#40#, 16#40#, 16#40#, 16#40#],
      '`' => [16#00#, 16#01#, 16#02#, 16#04#, 16#00#],
      'a' => [16#20#, 16#54#, 16#54#, 16#54#, 16#78#],
      'b' => [16#7F#, 16#48#, 16#44#, 16#44#, 16#38#],
      'c' => [16#38#, 16#44#, 16#44#, 16#44#, 16#20#],
      'd' => [16#38#, 16#44#, 16#44#, 16#48#, 16#7F#],
      'e' => [16#38#, 16#54#, 16#54#, 16#54#, 16#18#],
      'f' => [16#08#, 16#7E#, 16#09#, 16#01#, 16#02#],
      'g' => [16#0C#, 16#52#, 16#52#, 16#52#, 16#3E#],
      'h' => [16#7F#, 16#08#, 16#04#, 16#04#, 16#78#],
      'i' => [16#00#, 16#44#, 16#7D#, 16#40#, 16#00#],
      'j' => [16#20#, 16#40#, 16#44#, 16#3D#, 16#00#],
      'k' => [16#7F#, 16#10#, 16#28#, 16#44#, 16#00#],
      'l' => [16#00#, 16#41#, 16#7F#, 16#40#, 16#00#],
      'm' => [16#7C#, 16#04#, 16#18#, 16#04#, 16#78#],
      'n' => [16#7C#, 16#08#, 16#04#, 16#04#, 16#78#],
      'o' => [16#38#, 16#44#, 16#44#, 16#44#, 16#38#],
      'p' => [16#7C#, 16#14#, 16#14#, 16#14#, 16#08#],
      'q' => [16#08#, 16#14#, 16#14#, 16#18#, 16#7C#],
      'r' => [16#7C#, 16#08#, 16#04#, 16#04#, 16#08#],
      's' => [16#48#, 16#54#, 16#54#, 16#54#, 16#20#],
      't' => [16#04#, 16#3F#, 16#44#, 16#40#, 16#20#],
      'u' => [16#3C#, 16#40#, 16#40#, 16#20#, 16#7C#],
      'v' => [16#1C#, 16#20#, 16#40#, 16#20#, 16#1C#],
      'w' => [16#3C#, 16#40#, 16#30#, 16#40#, 16#3C#],
      'x' => [16#44#, 16#28#, 16#10#, 16#28#, 16#44#],
      'y' => [16#0C#, 16#50#, 16#50#, 16#50#, 16#3C#],
      'z' => [16#44#, 16#64#, 16#54#, 16#4C#, 16#44#],
      '{' => [16#00#, 16#08#, 16#36#, 16#41#, 16#00#],
      '|' => [16#00#, 16#00#, 16#7F#, 16#00#, 16#00#],
      '}' => [16#00#, 16#41#, 16#36#, 16#08#, 16#00#],
      '~' => [16#10#, 16#08#, 16#08#, 16#10#, 16#08#]];

   ------------------
   -- Send_Command --
   ------------------

   procedure Send_Command (Cmd : Unsigned_8) is
      Data : constant Ada_Modbus.Byte_Array (0 .. 1) :=
        [0 => 16#00#, 1 => Ada_Modbus.Byte (Cmd)];  --  Co=0, D/C#=0 (command)
   begin
      HAL_Stubs.I2C_Write (Config.OLED_I2C_Address, Data);
   end Send_Command;

   ---------------
   -- Send_Data --
   ---------------

   procedure Send_Data (D : Unsigned_8) is
      Data : constant Ada_Modbus.Byte_Array (0 .. 1) :=
        [0 => 16#40#, 1 => Ada_Modbus.Byte (D)];  --  Co=0, D/C#=1 (data)
   begin
      HAL_Stubs.I2C_Write (Config.OLED_I2C_Address, Data);
   end Send_Data;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      --  Initialize I2C1
      HAL_Stubs.I2C1_Initialize;

      --  SSD1306 initialization sequence
      Send_Command (CMD_DISPLAY_OFF);
      Send_Command (CMD_SET_MUX_RATIO);
      Send_Command (16#3F#);  --  64 lines
      Send_Command (CMD_SET_DISPLAY_OFFSET);
      Send_Command (16#00#);
      Send_Command (CMD_SET_START_LINE or 16#00#);
      Send_Command (CMD_SET_SEG_REMAP);  --  Segment remap
      Send_Command (CMD_SET_COM_SCAN);   --  COM scan direction
      Send_Command (CMD_SET_COM_PINS);
      Send_Command (16#12#);
      Send_Command (CMD_SET_CONTRAST);
      Send_Command (16#CF#);
      Send_Command (CMD_SET_PRECHARGE);
      Send_Command (16#F1#);
      Send_Command (CMD_SET_VCOM_DETECT);
      Send_Command (16#40#);
      Send_Command (CMD_MEMORY_MODE);
      Send_Command (16#00#);  --  Horizontal addressing
      Send_Command (CMD_CHARGE_PUMP);
      Send_Command (16#14#);  --  Enable charge pump
      Send_Command (CMD_NORMAL_DISPLAY);
      Send_Command (CMD_DISPLAY_ON);

      Is_Initialized := True;

      --  Clear display
      Clear;
   end Initialize;

   -----------
   -- Clear --
   -----------

   procedure Clear is
   begin
      if not Is_Initialized then
         return;
      end if;

      --  Set column address range 0-127
      Send_Command (CMD_COLUMN_ADDR);
      Send_Command (0);
      Send_Command (127);

      --  Set page address range 0-7
      Send_Command (CMD_PAGE_ADDR);
      Send_Command (0);
      Send_Command (7);

      --  Write zeros to entire display (128x64 = 8192 bits = 1024 bytes)
      for I in 1 .. 1024 loop
         Send_Data (0);
      end loop;

      Cursor_Line := 0;
      Cursor_Column := 0;
   end Clear;

   ----------------
   -- Set_Cursor --
   ----------------

   procedure Set_Cursor (Line : Natural; Column : Natural) is
      Page : Unsigned_8;
      Col  : Unsigned_8;
   begin
      if Line >= Num_Lines or else Column >= Chars_Per_Line then
         return;
      end if;

      Cursor_Line := Line;
      Cursor_Column := Column;

      --  Each text line uses 2 pages (16 pixels high)
      Page := Unsigned_8 (Line * 2);
      Col := Unsigned_8 (Column * 6);

      Send_Command (CMD_COLUMN_ADDR);
      Send_Command (Col);
      Send_Command (127);

      Send_Command (CMD_PAGE_ADDR);
      Send_Command (Page);
      Send_Command (Page + 1);
   end Set_Cursor;

   --------------
   -- Put_Char --
   --------------

   procedure Put_Char (C : Character) is
      Ch   : Character := C;
      Glyph : Font_Char;
   begin
      if not Is_Initialized then
         return;
      end if;

      --  Clamp to printable range
      if Ch < ' ' or else Ch > '~' then
         Ch := '?';
      end if;

      Glyph := Font (Ch);

      --  Write 5 columns + 1 space
      for I in Glyph'Range loop
         Send_Data (Glyph (I));
      end loop;
      Send_Data (0);  --  Space between characters

      --  Update cursor
      Cursor_Column := Cursor_Column + 1;
      if Cursor_Column >= Chars_Per_Line then
         Cursor_Column := 0;
         Cursor_Line := Cursor_Line + 1;
         if Cursor_Line >= Num_Lines then
            Cursor_Line := 0;
         end if;
      end if;
   end Put_Char;

   ----------------
   -- Put_String --
   ----------------

   procedure Put_String (S : String) is
   begin
      for I in S'Range loop
         Put_Char (S (I));
      end loop;
   end Put_String;

   ------------
   -- Put_At --
   ------------

   procedure Put_At (Line : Natural; Column : Natural; S : String) is
   begin
      Set_Cursor (Line, Column);
      Put_String (S);
   end Put_At;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Line : Natural; S : String) is
      Padded : String (1 .. Chars_Per_Line) := (others => ' ');
   begin
      --  Copy string and pad with spaces
      for I in S'Range loop
         if I - S'First + 1 <= Padded'Length then
            Padded (I - S'First + 1) := S (I);
         end if;
      end loop;

      Put_At (Line, 0, Padded);
   end Put_Line;

   ----------------
   -- Show_State --
   ----------------

   procedure Show_State (State_Name : String) is
      Line : String (1 .. Chars_Per_Line) := "PV CHARGER           ";
   begin
      --  Put state at end of line
      if State_Name'Length <= 8 then
         for I in State_Name'Range loop
            Line (Chars_Per_Line - State_Name'Length + I - State_Name'First + 1) :=
              State_Name (I);
         end loop;
      end if;
      Put_Line (0, Line);
   end Show_State;

   ---------------------
   -- Show_Grid_Power --
   ---------------------

   procedure Show_Grid_Power (Power_W : Integer_32) is
      Line : String (1 .. Chars_Per_Line) := "Grid:                ";
      Sign : Character;
      Abs_Power : Unsigned_32;
      Temp : Unsigned_32;
      Num_Digits : Natural := 0;
      Num_Str : String (1 .. 6);
   begin
      if Power_W >= 0 then
         Sign := '+';
         Abs_Power := Unsigned_32 (Power_W);
      else
         Sign := '-';
         Abs_Power := Unsigned_32 (-Power_W);
      end if;

      --  Convert to string
      Temp := Abs_Power;
      loop
         Num_Digits := Num_Digits + 1;
         Num_Str (7 - Num_Digits) := Character'Val (48 + Natural (Temp mod 10));
         Temp := Temp / 10;
         exit when Temp = 0;
      end loop;

      --  Format: "Grid: +12345W"
      Line (7) := Sign;
      for I in 1 .. Num_Digits loop
         Line (7 + I) := Num_Str (7 - Num_Digits + I - 1);
      end loop;
      Line (8 + Num_Digits) := 'W';

      Put_Line (1, Line);
   end Show_Grid_Power;

   -----------------------
   -- Show_Charge_Power --
   -----------------------

   procedure Show_Charge_Power (Power_W : Unsigned_32; SOC_Percent : Natural) is
      Line : String (1 .. Chars_Per_Line) := "Charge:              ";
      Temp : Unsigned_32;
      Num_Digits : Natural := 0;
      Num_Str : String (1 .. 6);
      Pos : Natural := 8;
   begin
      --  Power
      Temp := Power_W;
      if Temp = 0 then
         Line (Pos) := '0';
         Pos := Pos + 1;
      else
         loop
            Num_Digits := Num_Digits + 1;
            Num_Str (7 - Num_Digits) := Character'Val (48 + Natural (Temp mod 10));
            Temp := Temp / 10;
            exit when Temp = 0;
         end loop;
         for I in 1 .. Num_Digits loop
            Line (Pos) := Num_Str (7 - Num_Digits + I - 1);
            Pos := Pos + 1;
         end loop;
      end if;
      Line (Pos) := 'W';

      --  SOC at end, right-aligned
      if SOC_Percent >= 100 then
         Line (17) := '1';
         Line (18) := '0';
         Line (19) := '0';
         Line (20) := '%';
      elsif SOC_Percent >= 10 then
         Line (18) := Character'Val (48 + SOC_Percent / 10);
         Line (19) := Character'Val (48 + SOC_Percent mod 10);
         Line (20) := '%';
      else
         Line (19) := Character'Val (48 + SOC_Percent);
         Line (20) := '%';
      end if;

      Put_Line (2, Line);
   end Show_Charge_Power;

   -------------------------
   -- Show_Session_Energy --
   -------------------------

   procedure Show_Session_Energy (Energy_Wh : Unsigned_32) is
      Line : String (1 .. Chars_Per_Line) := "Session:             ";
      Kwh  : constant Unsigned_32 := Energy_Wh / 1000;
      Frac : constant Unsigned_32 := (Energy_Wh mod 1000) / 100;  --  First decimal
      Pos : Natural := 9;
      Temp : Unsigned_32;
      Num_Digits : Natural := 0;
      Num_Str : String (1 .. 6);
   begin
      --  Format: "Session: XX.X kWh"
      Temp := Kwh;
      if Temp = 0 then
         Line (Pos) := '0';
         Pos := Pos + 1;
      else
         loop
            Num_Digits := Num_Digits + 1;
            Num_Str (7 - Num_Digits) := Character'Val (48 + Natural (Temp mod 10));
            Temp := Temp / 10;
            exit when Temp = 0;
         end loop;
         for I in 1 .. Num_Digits loop
            Line (Pos) := Num_Str (7 - Num_Digits + I - 1);
            Pos := Pos + 1;
         end loop;
      end if;

      Line (Pos) := '.';
      Line (Pos + 1) := Character'Val (48 + Natural (Frac));
      Line (Pos + 2) := ' ';
      Line (Pos + 3) := 'k';
      Line (Pos + 4) := 'W';
      Line (Pos + 5) := 'h';

      Put_Line (3, Line);
   end Show_Session_Energy;

   --------------------
   -- Update_Display --
   --------------------

   procedure Update_Display
     (State_Name   : String;
      Grid_Power_W : Integer_32;
      Charge_W     : Unsigned_32;
      SOC_Percent  : Natural;
      Session_Wh   : Unsigned_32)
   is
   begin
      Show_State (State_Name);
      Show_Grid_Power (Grid_Power_W);
      Show_Charge_Power (Charge_W, SOC_Percent);
      Show_Session_Energy (Session_Wh);
   end Update_Display;

   ----------------
   -- Show_Error --
   ----------------

   procedure Show_Error (Message : String) is
   begin
      Put_Line (0, "*** ERROR ***");
      Put_Line (1, Message);
      Put_Line (2, "");
      Put_Line (3, "Press BTN to retry");
   end Show_Error;

   -----------------
   -- Show_Splash --
   -----------------

   procedure Show_Splash is
   begin
      Clear;
      Put_Line (0, "   PV CHARGER");
      Put_Line (1, "   Delta AC Max");
      Put_Line (2, "   NUCLEO-H753ZI");
      Put_Line (3, "   AdaModbus v1.0");
   end Show_Splash;

   ------------------
   -- Set_Inverted --
   ------------------

   procedure Set_Inverted (Inverted : Boolean) is
   begin
      if Inverted then
         Send_Command (CMD_INVERT_DISPLAY);
      else
         Send_Command (CMD_NORMAL_DISPLAY);
      end if;
   end Set_Inverted;

   ------------------
   -- Set_Contrast --
   ------------------

   procedure Set_Contrast (Level : Unsigned_8) is
   begin
      Send_Command (CMD_SET_CONTRAST);
      Send_Command (Level);
   end Set_Contrast;

end OLED_Display;
