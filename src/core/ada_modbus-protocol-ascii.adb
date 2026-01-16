--  Ada_Modbus.Protocol.ASCII - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with Ada_Modbus.LRC;

package body Ada_Modbus.Protocol.ASCII
  with SPARK_Mode => On
is

   --  Hex character lookup table
   Hex_Chars : constant array (0 .. 15) of Byte :=
     [Character'Pos ('0'), Character'Pos ('1'), Character'Pos ('2'), Character'Pos ('3'),
      Character'Pos ('4'), Character'Pos ('5'), Character'Pos ('6'), Character'Pos ('7'),
      Character'Pos ('8'), Character'Pos ('9'), Character'Pos ('A'), Character'Pos ('B'),
      Character'Pos ('C'), Character'Pos ('D'), Character'Pos ('E'), Character'Pos ('F')];

   -----------------
   -- Byte_To_Hex --
   -----------------

   procedure Byte_To_Hex (Value : Byte; High, Low : out Byte) is
   begin
      High := Hex_Chars (Natural (Value / 16));
      Low := Hex_Chars (Natural (Value mod 16));
   end Byte_To_Hex;

   ------------------
   -- Is_Hex_Digit --
   ------------------

   function Is_Hex_Digit (C : Byte) return Boolean is
   begin
      return C in Character'Pos ('0') .. Character'Pos ('9')
        or else C in Character'Pos ('A') .. Character'Pos ('F')
        or else C in Character'Pos ('a') .. Character'Pos ('f');
   end Is_Hex_Digit;

   -----------------
   -- Hex_To_Byte --
   -----------------

   function Hex_Nibble (C : Byte) return Byte;
   --  Convert single hex character to 4-bit value

   function Hex_Nibble (C : Byte) return Byte is
   begin
      if C in Character'Pos ('0') .. Character'Pos ('9') then
         return C - Character'Pos ('0');
      elsif C in Character'Pos ('A') .. Character'Pos ('F') then
         return C - Character'Pos ('A') + 10;
      elsif C in Character'Pos ('a') .. Character'Pos ('f') then
         return C - Character'Pos ('a') + 10;
      else
         return 0;
      end if;
   end Hex_Nibble;

   function Hex_To_Byte (High, Low : Byte) return Byte is
   begin
      return Hex_Nibble (High) * 16 + Hex_Nibble (Low);
   end Hex_To_Byte;

   -----------------
   -- Build_Frame --
   -----------------

   procedure Build_Frame
     (Frame        : out Frame_Buffer;
      Frame_Length : out Natural;
      Slave        : Unit_Id;
      PDU          : PDU_Buffer;
      PDU_Length   : Natural)
   is
      Idx       : Natural := 0;
      LRC_Data  : Byte_Array (0 .. PDU_Length) := [others => 0];
      LRC_Val   : LRC.LRC_Value;
      H, L      : Byte;
   begin
      Frame := [others => 0];

      --  Start character
      Frame (Idx) := Frame_Start;
      Idx := Idx + 1;

      --  Prepare data for LRC calculation (address + PDU)
      LRC_Data (0) := Byte (Slave);
      for I in 0 .. PDU_Length - 1 loop
         pragma Loop_Invariant (1 + I <= LRC_Data'Last);
         LRC_Data (1 + I) := PDU (I);
      end loop;

      --  Calculate LRC
      LRC_Val := LRC.Calculate (LRC_Data);

      --  Slave address as hex
      Byte_To_Hex (Byte (Slave), H, L);
      Frame (Idx) := H;
      Frame (Idx + 1) := L;
      Idx := Idx + 2;

      --  PDU as hex
      for I in 0 .. PDU_Length - 1 loop
         pragma Loop_Invariant (Idx = 3 + 2 * I);
         pragma Loop_Invariant (Idx + 1 < Max_Frame_Size);
         Byte_To_Hex (PDU (I), H, L);
         Frame (Idx) := H;
         Frame (Idx + 1) := L;
         Idx := Idx + 2;
      end loop;

      --  At this point: Idx = 3 + 2 * PDU_Length
      --  LRC as hex
      pragma Assert (Idx + 1 < Max_Frame_Size);
      Byte_To_Hex (LRC_Val, H, L);
      Frame (Idx) := H;
      Frame (Idx + 1) := L;
      Idx := Idx + 2;

      --  End characters
      pragma Assert (Idx + 1 < Max_Frame_Size);
      Frame (Idx) := Frame_CR;
      Frame (Idx + 1) := Frame_LF;
      Idx := Idx + 2;

      Frame_Length := Idx;
   end Build_Frame;

   -----------------
   -- Parse_Frame --
   -----------------

   procedure Parse_Frame
     (Frame        : Frame_Buffer;
      Frame_Length : Natural;
      Slave        : out Unit_Id;
      PDU          : out PDU_Buffer;
      PDU_Length   : out Natural;
      Result       : out Status)
   is
      Idx          : Natural;
      Binary_Data  : Byte_Array (0 .. 254) := [others => 0];
      Binary_Len   : Natural := 0;
   begin
      --  Initialize outputs
      Slave := 0;
      PDU := [others => 0];
      PDU_Length := 0;

      --  Minimum frame: ':' + 2 addr + 2 FC + 2 LRC + CR + LF = 9
      if Frame_Length < 9 then
         Result := Frame_Error;
         return;
      end if;

      --  Check start character
      if Frame (0) /= Frame_Start then
         Result := Frame_Error;
         return;
      end if;

      --  Check end characters
      if Frame (Frame_Length - 2) /= Frame_CR
        or else Frame (Frame_Length - 1) /= Frame_LF
      then
         Result := Frame_Error;
         return;
      end if;

      --  Calculate number of hex characters (excluding ':' and CRLF)
      --  Includes LRC at the end
      declare
         Hex_Chars_Count : constant Natural := Frame_Length - 3;  --  -1 ':' -2 CRLF
      begin
         --  Must be even number of hex characters
         if Hex_Chars_Count mod 2 /= 0 then
            Result := Frame_Error;
            return;
         end if;

         --  Convert hex to binary
         Idx := 1;  --  Skip ':'
         while Idx < Frame_Length - 2 loop
            pragma Loop_Invariant (Idx >= 1 and Idx < Frame_Length - 1);
            pragma Loop_Invariant (Binary_Len = (Idx - 1) / 2);
            pragma Loop_Invariant (Binary_Len <= 254);
            if not Is_Hex_Digit (Frame (Idx))
              or else not Is_Hex_Digit (Frame (Idx + 1))
            then
               Result := Frame_Error;
               return;
            end if;
            Binary_Data (Binary_Len) := Hex_To_Byte (Frame (Idx), Frame (Idx + 1));
            Binary_Len := Binary_Len + 1;
            Idx := Idx + 2;
         end loop;
      end;

      --  Verify LRC (binary data includes LRC byte)
      --  Binary_Len >= 2 is guaranteed since Hex_Chars_Count >= 6 (for 9-char frame)
      pragma Assert (Binary_Len >= 2);
      if not LRC.Verify (Binary_Data (0 .. Binary_Len - 1)) then
         Result := LRC_Error;
         return;
      end if;

      --  Extract slave address (first byte of binary data)
      --  Binary_Data(0) might be > 247, so we need to validate
      if Binary_Data (0) > 247 then
         Result := Frame_Error;
         return;
      end if;
      Slave := Unit_Id (Binary_Data (0));

      --  Extract PDU (everything except address and LRC)
      PDU_Length := Binary_Len - 2;  --  -1 addr -1 LRC
      for I in 0 .. PDU_Length - 1 loop
         pragma Loop_Invariant (1 + I < Binary_Len);
         pragma Loop_Invariant (I < PDU_Length);
         PDU (I) := Binary_Data (1 + I);
      end loop;

      Result := Success;
   end Parse_Frame;

end Ada_Modbus.Protocol.ASCII;
