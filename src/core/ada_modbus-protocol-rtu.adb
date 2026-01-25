--  Ada_Modbus.Protocol.RTU - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with Interfaces; use Interfaces;

package body Ada_Modbus.Protocol.RTU
  with SPARK_Mode => On
is

   -----------------
   -- Build_Frame --
   -----------------

   procedure Build_Frame
     (ADU         : out ADU_Buffer;
      ADU_Length  : out Natural;
      Slave       : Unit_Id;
      PDU         : PDU_Buffer;
      PDU_Length  : PDU_Data_Length)
   is
      CRC : CRC16.CRC_Value;
   begin
      ADU := [others => 0];
      --  Slave address
      ADU (0) := Byte (Slave);

      --  Copy PDU
      for I in 0 .. PDU_Length - 1 loop
         ADU (1 + I) := PDU (I);
      end loop;

      --  Calculate and append CRC
      CRC := CRC16.Calculate (ADU (0 .. PDU_Length));

      --  CRC is appended LSB first
      ADU (1 + PDU_Length) := Byte (Unsigned_16 (CRC) and 16#FF#);
      ADU (2 + PDU_Length) := Byte (Shift_Right (Unsigned_16 (CRC), 8));

      ADU_Length := PDU_Length + 3;  --  1 addr + PDU + 2 CRC
   end Build_Frame;

   -----------------
   -- Parse_Frame --
   -----------------

   procedure Parse_Frame
     (ADU        : ADU_Buffer;
      ADU_Length : ADU_Data_Length;
      Slave      : out Unit_Id;
      PDU        : out PDU_Buffer;
      PDU_Length : out Natural;
      Result     : out Status)
   is
   begin
      --  Initialize outputs
      Slave := 0;
      PDU := [others => 0];
      PDU_Length := 0;

      --  Minimum frame: 1 addr + 1 FC + 2 CRC = 4 bytes
      if ADU_Length < 4 then
         Result := Frame_Error;
         return;
      end if;

      --  Verify CRC
      if not CRC16.Verify (ADU (0 .. ADU_Length - 1)) then
         Result := CRC_Error;
         return;
      end if;

      --  Extract slave address (validate range for Unit_Id)
      if ADU (0) > 247 then
         Result := Frame_Error;
         return;
      end if;
      Slave := Unit_Id (ADU (0));

      --  Extract PDU (everything except address and CRC)
      PDU_Length := ADU_Length - 3;
      for I in 0 .. PDU_Length - 1 loop
         pragma Loop_Invariant (1 + I < ADU_Length - 2);
         pragma Loop_Invariant (I < PDU_Length);
         PDU (I) := ADU (1 + I);
      end loop;

      Result := Success;
   end Parse_Frame;

end Ada_Modbus.Protocol.RTU;
