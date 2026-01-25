--  Ada_Modbus.Protocol.RTU - RTU framing
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  RTU ADU (Application Data Unit) format:
--  - Slave Address (1 byte)
--  - Function Code (1 byte)
--  - Data (n bytes)
--  - CRC-16 (2 bytes, LSB first)
--
--  Maximum ADU size: 256 bytes (1 + 253 PDU + 2 CRC)

with Ada_Modbus.CRC16;

package Ada_Modbus.Protocol.RTU
  with SPARK_Mode => On
is

   pragma Pure;

   --  Maximum RTU ADU size
   Max_ADU_Size : constant := 256;  --  1 addr + 253 PDU + 2 CRC

   --  ADU buffer type
   subtype ADU_Index is Natural range 0 .. Max_ADU_Size - 1;
   subtype ADU_Buffer is Byte_Array (0 .. Max_ADU_Size - 1);

   --  ADU data length (for parameters, not indices)
   subtype ADU_Data_Length is Natural range 0 .. Max_ADU_Size;

   --  Build RTU frame from PDU
   --  Adds slave address at front and CRC at end
   procedure Build_Frame
     (ADU         : out ADU_Buffer;
      ADU_Length  : out Natural;
      Slave       : Unit_Id;
      PDU         : PDU_Buffer;
      PDU_Length  : PDU_Data_Length)
     with Pre  => PDU_Length + 3 <= Max_ADU_Size,
          Post => ADU_Length = PDU_Length + 3;

   --  Parse RTU frame, extract PDU
   --  Verifies CRC and extracts slave address and PDU
   procedure Parse_Frame
     (ADU        : ADU_Buffer;
      ADU_Length : ADU_Data_Length;
      Slave      : out Unit_Id;
      PDU        : out PDU_Buffer;
      PDU_Length : out Natural;
      Result     : out Status);

   --  Verify CRC of a complete RTU frame
   function Verify_CRC (ADU : Byte_Array) return Boolean
     renames CRC16.Verify;

   --  Calculate CRC for data
   function Calculate_CRC (Data : Byte_Array) return CRC16.CRC_Value
     renames CRC16.Calculate;

end Ada_Modbus.Protocol.RTU;
