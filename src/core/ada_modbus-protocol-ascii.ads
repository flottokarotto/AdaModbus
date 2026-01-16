--  Ada_Modbus.Protocol.ASCII - ASCII framing
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  ASCII ADU format:
--  - Start: ':' (0x3A)
--  - Address (2 hex characters)
--  - Function Code (2 hex characters)
--  - Data (2 hex characters per byte)
--  - LRC (2 hex characters)
--  - End: CR LF (0x0D 0x0A)
--
--  Maximum ASCII frame: 1 + 2 + 2 + 2*252 + 2 + 2 = 513 characters

package Ada_Modbus.Protocol.ASCII
  with SPARK_Mode => On
is

   pragma Pure;

   --  Frame delimiters
   Frame_Start : constant Byte := 16#3A#;  --  ':'
   Frame_CR    : constant Byte := 16#0D#;  --  Carriage Return
   Frame_LF    : constant Byte := 16#0A#;  --  Line Feed

   --  Maximum ASCII frame size
   --  1 ':' + 2*254 (addr+PDU) + 2 LRC + 2 CRLF = 513
   Max_Frame_Size : constant := 513;

   subtype Frame_Buffer is Byte_Array (0 .. Max_Frame_Size - 1);

   --  Build ASCII frame from PDU
   --  Converts binary data to hex ASCII and adds framing
   --  Frame size = 1 ':' + 2*(1 addr + PDU_Length) + 2 LRC + 2 CRLF
   procedure Build_Frame
     (Frame        : out Frame_Buffer;
      Frame_Length : out Natural;
      Slave        : Unit_Id;
      PDU          : PDU_Buffer;
      PDU_Length   : Natural)
     with Pre => PDU_Length <= Max_PDU_Size
                 and then 1 + 2 * (1 + PDU_Length) + 4 <= Max_Frame_Size;

   --  Parse ASCII frame, extract PDU
   --  Verifies LRC and extracts slave address and PDU
   procedure Parse_Frame
     (Frame        : Frame_Buffer;
      Frame_Length : Natural;
      Slave        : out Unit_Id;
      PDU          : out PDU_Buffer;
      PDU_Length   : out Natural;
      Result       : out Status)
     with Pre => Frame_Length <= Max_Frame_Size;

   --  Convert byte to two hex ASCII characters
   procedure Byte_To_Hex (Value : Byte; High, Low : out Byte);

   --  Convert two hex ASCII characters to byte
   function Hex_To_Byte (High, Low : Byte) return Byte;

   --  Check if character is valid hex digit
   function Is_Hex_Digit (C : Byte) return Boolean;

end Ada_Modbus.Protocol.ASCII;
