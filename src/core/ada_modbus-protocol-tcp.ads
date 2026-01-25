--  Ada_Modbus.Protocol.TCP - TCP/MBAP framing
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Modbus TCP uses MBAP (Modbus Application Protocol) Header:
--  - Transaction ID (2 bytes) - For request/response matching
--  - Protocol ID (2 bytes) - Always 0x0000 for Modbus
--  - Length (2 bytes) - Number of following bytes (Unit ID + PDU)
--  - Unit ID (1 byte) - Slave address (or 0xFF for TCP-only devices)
--  - PDU follows
--
--  Maximum ADU: 7 MBAP + 253 PDU = 260 bytes

with Interfaces;

package Ada_Modbus.Protocol.TCP
  with SPARK_Mode => On
is

   pragma Pure;

   --  MBAP Header size
   MBAP_Header_Size : constant := 7;

   --  Maximum TCP ADU size
   Max_ADU_Size : constant := 260;  --  7 MBAP + 253 PDU

   --  Protocol identifier (always 0 for Modbus)
   Modbus_Protocol_ID : constant := 0;

   --  ADU buffer type
   subtype ADU_Index is Natural range 0 .. Max_ADU_Size - 1;
   subtype ADU_Buffer is Byte_Array (0 .. Max_ADU_Size - 1);

   --  ADU data length (for parameters, not indices)
   subtype ADU_Data_Length is Natural range 0 .. Max_ADU_Size;

   --  Transaction ID type
   type Transaction_Id is new Interfaces.Unsigned_16;

   --  Build TCP frame (MBAP header + PDU)
   procedure Build_Frame
     (ADU           : out ADU_Buffer;
      ADU_Length    : out Natural;
      Transaction   : Transaction_Id;
      Unit          : Unit_Id;
      PDU           : PDU_Buffer;
      PDU_Length    : PDU_Data_Length)
     with Pre  => PDU_Length + MBAP_Header_Size <= Max_ADU_Size,
          Post => ADU_Length = PDU_Length + MBAP_Header_Size;

   --  Parse TCP frame, extract PDU
   --  Verifies protocol ID and extracts transaction ID, unit ID, and PDU
   procedure Parse_Frame
     (ADU           : ADU_Buffer;
      ADU_Length    : ADU_Data_Length;
      Transaction   : out Transaction_Id;
      Unit          : out Unit_Id;
      PDU           : out PDU_Buffer;
      PDU_Length    : out Natural;
      Result        : out Status);

   --  Extract transaction ID from frame (for response matching)
   function Get_Transaction_Id (ADU : ADU_Buffer) return Transaction_Id;

   --  Extract expected length from MBAP header
   function Get_Expected_Length (ADU : ADU_Buffer) return Natural
     with Pre => ADU'Length >= 6;

end Ada_Modbus.Protocol.TCP;
