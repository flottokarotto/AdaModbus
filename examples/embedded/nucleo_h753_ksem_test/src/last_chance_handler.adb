--  Last_Chance_Handler - Exception handler with UART output
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Outputs exception message and line number via USART3 (ST-Link VCP),
--  turns on red LED, then halts.

with Interfaces; use Interfaces;
with STM32H7_HAL;

package body Last_Chance_Handler is

   procedure Put_Byte (B : Unsigned_8) is
   begin
      STM32H7_HAL.USART3_Send_Byte (B);
   end Put_Byte;

   procedure Put_Char (C : Character) is
   begin
      Put_Byte (Character'Pos (C));
   end Put_Char;

   procedure Put_String (S : String) is
   begin
      for C of S loop
         Put_Char (C);
      end loop;
   end Put_String;

   procedure Put_Newline is
   begin
      Put_Char (ASCII.CR);
      Put_Char (ASCII.LF);
   end Put_Newline;

   procedure Put_Int (V : Integer) is
      Buf : String (1 .. 11);
      Pos : Natural := Buf'Last;
      N   : Natural;
   begin
      if V < 0 then
         Put_Char ('-');
         N := Natural (-(V + 1)) + 1;
      elsif V = 0 then
         Put_Char ('0');
         return;
      else
         N := Natural (V);
      end if;

      while N > 0 loop
         Buf (Pos) := Character'Val (Character'Pos ('0') + (N mod 10));
         N := N / 10;
         Pos := Pos - 1;
      end loop;

      Put_String (Buf (Pos + 1 .. Buf'Last));
   end Put_Int;

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer) is
      Max_Msg_Len : constant := 256;
   begin
      --  Turn on red LED (PB14)
      STM32H7_HAL.GPIO_Write (STM32H7_HAL.Port_B, 14, True);
      --  Turn off green heartbeat LED (PB0)
      STM32H7_HAL.GPIO_Write (STM32H7_HAL.Port_B, 0, False);

      Put_Newline;
      Put_String ("*** EXCEPTION: ");

      --  Print null-terminated message from Msg address
      if Msg /= System.Null_Address then
         declare
            Bytes : array (0 .. Max_Msg_Len - 1) of Unsigned_8
              with Address => Msg, Import;
         begin
            for I in Bytes'Range loop
               exit when Bytes (I) = 0;
               Put_Byte (Bytes (I));
            end loop;
         end;
      else
         Put_String ("(no message)");
      end if;

      Put_String (" at line ");
      Put_Int (Line);
      Put_Newline;

      --  Halt
      loop
         null;
      end loop;
   end Last_Chance_Handler;

end Last_Chance_Handler;
