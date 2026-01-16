--  LwIP_Bindings - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

package body LwIP_Bindings is

   -------------------
   -- Make_IP4_Addr --
   -------------------

   function Make_IP4_Addr (A, B, C, D : Unsigned_8) return IP4_Addr is
   begin
      --  LwIP uses network byte order (big-endian) internally
      --  On little-endian ARM, we construct it so LwIP sees it correctly
      return (Addr => Unsigned_32 (A)
                   or Shift_Left (Unsigned_32 (B), 8)
                   or Shift_Left (Unsigned_32 (C), 16)
                   or Shift_Left (Unsigned_32 (D), 24));
   end Make_IP4_Addr;

end LwIP_Bindings;
