--  UART_Console - Implementation via configurable callback

package body UART_Console is

   Output : Put_Byte_Callback := null;

   ----------------
   -- Set_Output --
   ----------------

   procedure Set_Output (Callback : Put_Byte_Callback) is
   begin
      Output := Callback;
   end Set_Output;

   ---------
   -- Put --
   ---------

   procedure Put (Msg : String) is
   begin
      if Output /= null then
         for I in Msg'Range loop
            Output (Character'Pos (Msg (I)));
         end loop;
      end if;
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Msg : String) is
   begin
      if Output /= null then
         Put (Msg);
         Output (16#0D#);  -- CR
         Output (16#0A#);  -- LF
      end if;
   end Put_Line;

   -------------
   -- Put_Int --
   -------------

   procedure Put_Int (Value : Integer_32) is
      Buf   : String (1 .. 12) := (others => ' ');
      V     : Unsigned_32;
      Neg   : constant Boolean := Value < 0;
      Pos   : Natural := Buf'Last;
   begin
      if Neg then
         V := Unsigned_32 (-Integer_64'(Integer_64 (Value)));
      else
         V := Unsigned_32 (Value);
      end if;

      if V = 0 then
         Buf (Pos) := '0';
         Pos := Pos - 1;
      else
         while V > 0 loop
            Buf (Pos) := Character'Val (48 + Natural (V mod 10));
            V := V / 10;
            Pos := Pos - 1;
         end loop;
      end if;

      if Neg then
         Buf (Pos) := '-';
         Pos := Pos - 1;
      end if;

      Put (Buf (Pos + 1 .. Buf'Last));
   end Put_Int;

end UART_Console;
