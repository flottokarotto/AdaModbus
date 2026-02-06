--  LwIP_Bindings - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

package body LwIP_Bindings is

   --------------------
   -- Make_IP4_Addr --
   --------------------

   function Make_IP4_Addr (A, B, C, D : Unsigned_8) return IP4_Addr_T is
   begin
      --  Little endian: A is LSB
      return (Addr => Unsigned_32 (A) or
                      Unsigned_32 (B) * 256 or
                      Unsigned_32 (C) * 65536 or
                      Unsigned_32 (D) * 16777216);
   end Make_IP4_Addr;

   ----------------------
   -- Pbuf_Get_Payload --
   ----------------------

   function Pbuf_Get_Payload (P : Pbuf_Ptr) return System.Address is
   begin
      if P = null then
         return System.Null_Address;
      end if;
      return P.Payload;
   end Pbuf_Get_Payload;

   ------------------
   -- Pbuf_Get_Len --
   ------------------

   function Pbuf_Get_Len (P : Pbuf_Ptr) return Unsigned_16 is
   begin
      if P = null then
         return 0;
      end if;
      return P.Len;
   end Pbuf_Get_Len;

   ----------------------
   -- Pbuf_Get_Tot_Len --
   ----------------------

   function Pbuf_Get_Tot_Len (P : Pbuf_Ptr) return Unsigned_16 is
   begin
      if P = null then
         return 0;
      end if;
      return P.Tot_Len;
   end Pbuf_Get_Tot_Len;

end LwIP_Bindings;
