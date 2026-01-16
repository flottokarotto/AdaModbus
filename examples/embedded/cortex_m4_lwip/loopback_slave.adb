--  Loopback_Slave - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

package body Loopback_Slave is

   --------------------------------
   -- Read_Holding_Registers_CB --
   --------------------------------

   function Read_Holding_Registers_CB
     (Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array) return Status
   is
      Start : constant Natural := Natural (Start_Address);
   begin
      if Start + Natural (Quantity) > Holding_Registers'Length then
         return Exception_Illegal_Address;
      end if;
      for I in 0 .. Natural (Quantity) - 1 loop
         Values (Values'First + I) := Holding_Registers (Start + I);
      end loop;
      return Success;
   end Read_Holding_Registers_CB;

   ------------------------------
   -- Read_Input_Registers_CB --
   ------------------------------

   function Read_Input_Registers_CB
     (Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array) return Status
   is
      Start : constant Natural := Natural (Start_Address);
   begin
      if Start + Natural (Quantity) > Input_Registers'Length then
         return Exception_Illegal_Address;
      end if;
      for I in 0 .. Natural (Quantity) - 1 loop
         Values (Values'First + I) := Input_Registers (Start + I);
      end loop;
      return Success;
   end Read_Input_Registers_CB;

   -----------------------------
   -- Write_Single_Register_CB --
   -----------------------------

   function Write_Single_Register_CB
     (Address : Register_Address;
      Value   : Register_Value) return Status
   is
      Addr : constant Natural := Natural (Address);
   begin
      if Addr >= Holding_Registers'Length then
         return Exception_Illegal_Address;
      end if;
      Holding_Registers (Addr) := Value;
      return Success;
   end Write_Single_Register_CB;

   --------------------------------
   -- Write_Multiple_Registers_CB --
   --------------------------------

   function Write_Multiple_Registers_CB
     (Start_Address : Register_Address;
      Values        : Register_Array) return Status
   is
      Start : constant Natural := Natural (Start_Address);
   begin
      if Start + Values'Length > Holding_Registers'Length then
         return Exception_Illegal_Address;
      end if;
      for I in Values'Range loop
         Holding_Registers (Start + I - Values'First) := Values (I);
      end loop;
      return Success;
   end Write_Multiple_Registers_CB;

   -------------------
   -- Read_Coils_CB --
   -------------------

   function Read_Coils_CB
     (Start_Address : Coil_Address;
      Quantity      : Coil_Count;
      Values        : out Coil_Array) return Status
   is
      Start : constant Natural := Natural (Start_Address);
   begin
      if Start + Natural (Quantity) > Coils'Length then
         return Exception_Illegal_Address;
      end if;
      for I in 0 .. Natural (Quantity) - 1 loop
         Values (Values'First + I) := Coils (Start + I);
      end loop;
      return Success;
   end Read_Coils_CB;

   -------------------------
   -- Write_Single_Coil_CB --
   -------------------------

   function Write_Single_Coil_CB
     (Address : Coil_Address;
      Value   : Coil_Value) return Status
   is
      Addr : constant Natural := Natural (Address);
   begin
      if Addr >= Coils'Length then
         return Exception_Illegal_Address;
      end if;
      Coils (Addr) := Value;
      return Success;
   end Write_Single_Coil_CB;

end Loopback_Slave;
