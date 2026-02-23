--  Last_Chance_Handler - Exception handler for light runtime
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Outputs exception message and source line via registered callback,
--  then halts in an infinite loop.

with Interfaces; use Interfaces;

package body Last_Chance_Handler is

   Output       : Put_Byte_Callback := null;
   On_Exception : On_Exception_Callback := null;

   ----------------
   -- Set_Output --
   ----------------

   procedure Set_Output (Callback : Put_Byte_Callback) is
   begin
      Output := Callback;
   end Set_Output;

   ----------------------
   -- Set_On_Exception --
   ----------------------

   procedure Set_On_Exception (Callback : On_Exception_Callback) is
   begin
      On_Exception := Callback;
   end Set_On_Exception;

   ---------------
   -- Put_Char  --
   ---------------

   procedure Put_Char (C : Character) is
   begin
      if Output /= null then
         Output (Character'Pos (C));
      end if;
   end Put_Char;

   ----------------
   -- Put_String --
   ----------------

   procedure Put_String (S : String) is
   begin
      for C of S loop
         Put_Char (C);
      end loop;
   end Put_String;

   -----------------
   -- Put_Newline --
   -----------------

   procedure Put_Newline is
   begin
      Put_Char (ASCII.CR);
      Put_Char (ASCII.LF);
   end Put_Newline;

   -------------
   -- Put_Int --
   -------------

   procedure Put_Int (V : Integer) is
      Buf : String (1 .. 11);
      Pos : Natural := Buf'Last;
      N   : Unsigned_32;
   begin
      if V < 0 then
         Put_Char ('-');
         N := Unsigned_32 (-(V + 1)) + 1;
      elsif V = 0 then
         Put_Char ('0');
         return;
      else
         N := Unsigned_32 (V);
      end if;

      while N > 0 loop
         Buf (Pos) := Character'Val (Character'Pos ('0') + Natural (N mod 10));
         N := N / 10;
         Pos := Pos - 1;
      end loop;

      Put_String (Buf (Pos + 1 .. Buf'Last));
   end Put_Int;

   --------------------------
   -- Last_Chance_Handler  --
   --------------------------

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer) is
      use type System.Address;
      Max_Msg_Len : constant := 256;
   begin
      if On_Exception /= null then
         On_Exception.all;
      end if;

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
               Put_Char (Character'Val (Bytes (I)));
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
