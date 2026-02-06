--  Ada_Modbus.Gateway - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

package body Ada_Modbus.Gateway
  with SPARK_Mode => On
is

   ----------------
   -- TCP_To_RTU --
   ----------------

   procedure TCP_To_RTU
     (TCP_ADU        : Protocol.TCP.ADU_Buffer;
      TCP_Length      : Protocol.TCP.ADU_Data_Length;
      RTU_ADU        : out Protocol.RTU.ADU_Buffer;
      RTU_Length      : out Natural;
      Transaction     : out Protocol.TCP.Transaction_Id;
      Result          : out Status)
   is
      Unit    : Unit_Id;
      PDU     : Protocol.PDU_Buffer;
      PDU_Len : Natural;
   begin
      RTU_ADU := [others => 0];
      RTU_Length := 0;
      Transaction := 0;

      --  Parse TCP frame to extract PDU
      Protocol.TCP.Parse_Frame
        (ADU        => TCP_ADU,
         ADU_Length => TCP_Length,
         Transaction => Transaction,
         Unit       => Unit,
         PDU        => PDU,
         PDU_Length => PDU_Len,
         Result     => Result);

      if Result /= Success then
         return;
      end if;

      --  Validate PDU length (Parse_Frame has no postcondition on PDU_Length)
      if PDU_Len > Protocol.Max_PDU_Size then
         Result := Frame_Error;
         return;
      end if;

      --  Check that PDU fits in RTU frame
      if PDU_Len + 3 > Protocol.RTU.Max_ADU_Size then
         Result := Buffer_Too_Small;
         return;
      end if;

      --  Build RTU frame from extracted PDU
      Protocol.RTU.Build_Frame
        (ADU        => RTU_ADU,
         ADU_Length => RTU_Length,
         Slave      => Unit,
         PDU        => PDU,
         PDU_Length => PDU_Len);

      Result := Success;
   end TCP_To_RTU;

   ----------------
   -- RTU_To_TCP --
   ----------------

   procedure RTU_To_TCP
     (RTU_ADU        : Protocol.RTU.ADU_Buffer;
      RTU_Length      : Protocol.RTU.ADU_Data_Length;
      Transaction     : Protocol.TCP.Transaction_Id;
      TCP_ADU        : out Protocol.TCP.ADU_Buffer;
      TCP_Length      : out Natural;
      Result          : out Status)
   is
      Slave   : Unit_Id;
      PDU     : Protocol.PDU_Buffer;
      PDU_Len : Natural;
   begin
      TCP_ADU := [others => 0];
      TCP_Length := 0;

      --  Parse RTU frame (verifies CRC)
      Protocol.RTU.Parse_Frame
        (ADU        => RTU_ADU,
         ADU_Length => RTU_Length,
         Slave      => Slave,
         PDU        => PDU,
         PDU_Length => PDU_Len,
         Result     => Result);

      if Result /= Success then
         return;
      end if;

      --  Validate PDU length (Parse_Frame has no postcondition on PDU_Length)
      if PDU_Len > Protocol.Max_PDU_Size then
         Result := Frame_Error;
         return;
      end if;

      --  Check that PDU fits in TCP frame
      if PDU_Len + Protocol.TCP.MBAP_Header_Size > Protocol.TCP.Max_ADU_Size then
         Result := Buffer_Too_Small;
         return;
      end if;

      --  Build TCP frame with original transaction ID
      Protocol.TCP.Build_Frame
        (ADU        => TCP_ADU,
         ADU_Length => TCP_Length,
         Transaction => Transaction,
         Unit       => Slave,
         PDU        => PDU,
         PDU_Length => PDU_Len);

      Result := Success;
   end RTU_To_TCP;

end Ada_Modbus.Gateway;
