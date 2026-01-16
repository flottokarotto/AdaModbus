--  Ada_Modbus.Transport - Transport abstraction layer
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  This package provides transport backend abstractions for Modbus.
--  Child packages implement specific transports (TCP, Serial, etc.)

package Ada_Modbus.Transport is

   pragma Pure;

   --  Common transport constants
   Default_TCP_Port : constant := 502;
   Default_Timeout_Ms : constant := 1000;

end Ada_Modbus.Transport;
