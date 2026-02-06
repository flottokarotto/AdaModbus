--  Ada_Modbus.Gateway - TCP to RTU Protocol Translation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Pure protocol translation between Modbus TCP (MBAP) and Modbus RTU frames.
--  No transport involved - just converts frame formats.
--
--  TCP->RTU: Parse MBAP header, extract PDU, build RTU frame with CRC
--  RTU->TCP: Parse RTU frame, verify CRC, build MBAP response

with Ada_Modbus.Protocol;
with Ada_Modbus.Protocol.TCP;
with Ada_Modbus.Protocol.RTU;

package Ada_Modbus.Gateway
  with SPARK_Mode => On
is

   pragma Pure;

   --  Convert a Modbus TCP request to a Modbus RTU request.
   --  Extracts the PDU from the MBAP frame and wraps it in an RTU frame.
   --  The Transaction_Id is returned so it can be used in the response.
   procedure TCP_To_RTU
     (TCP_ADU        : Protocol.TCP.ADU_Buffer;
      TCP_Length      : Protocol.TCP.ADU_Data_Length;
      RTU_ADU        : out Protocol.RTU.ADU_Buffer;
      RTU_Length      : out Natural;
      Transaction     : out Protocol.TCP.Transaction_Id;
      Result          : out Status);

   --  Convert a Modbus RTU response to a Modbus TCP response.
   --  Extracts the PDU from the RTU frame and wraps it in an MBAP frame
   --  using the provided Transaction_Id from the original request.
   procedure RTU_To_TCP
     (RTU_ADU        : Protocol.RTU.ADU_Buffer;
      RTU_Length      : Protocol.RTU.ADU_Data_Length;
      Transaction     : Protocol.TCP.Transaction_Id;
      TCP_ADU        : out Protocol.TCP.ADU_Buffer;
      TCP_Length      : out Natural;
      Result          : out Status);

end Ada_Modbus.Gateway;
