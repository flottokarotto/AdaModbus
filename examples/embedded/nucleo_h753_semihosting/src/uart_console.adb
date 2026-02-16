--  UART_Console - Implementation via USART3

with STM32H7_HAL;

package body UART_Console is

   ---------
   -- Put --
   ---------

   procedure Put (Msg : String) is
   begin
      for I in Msg'Range loop
         STM32H7_HAL.USART3_Send_Byte (Character'Pos (Msg (I)));
      end loop;
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Msg : String) is
   begin
      Put (Msg);
      STM32H7_HAL.USART3_Send_Byte (16#0D#);  -- CR
      STM32H7_HAL.USART3_Send_Byte (16#0A#);  -- LF
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
