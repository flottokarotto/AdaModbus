--  Ada_Modbus.Scaled_IO - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with Ada.Unchecked_Conversion;
with Ada_Modbus.Scaling;

package body Ada_Modbus.Scaled_IO
  with SPARK_Mode => On
is

   package body Map
     with SPARK_Mode => On
   is

      --  Float array matching the record layout
      type Float_Fields is array (1 .. Field_Count) of Float
        with Pack;

      function To_Record is new Ada.Unchecked_Conversion
        (Source => Float_Fields, Target => Scaled_Record);

      ----------------------
      -- From_Registers  --
      ----------------------

      function From_Registers
        (Regs  : Register_Array;
         Order : Utilities.Word_Order :=
           Utilities.Big_Endian) return Scaled_Record
      is
         use Scaling;
         Result : Float_Fields;
      begin
         for I in Fields'Range loop
            declare
               F   : Field_Descriptor renames Fields (I);
               Reg : constant Natural := F.Reg;
               Idx : constant Positive := I - Fields'First + 1;
            begin
               case F.Kind is
                  when Raw_U16 =>
                     Result (Idx) := Float (Regs (Reg));

                  when Raw_S16 =>
                     Result (Idx) := Float (To_Signed (Regs (Reg)));

                  when Raw_U32 =>
                     Result (Idx) := Float
                       (Utilities.To_Unsigned_32
                          (Regs (Reg), Regs (Reg + 1), Order));

                  when SF_U16 =>
                     Result (Idx) := Apply
                       (Regs (Reg), To_SF (Regs (F.SF_Reg)));

                  when SF_S16 =>
                     Result (Idx) := Apply_Signed
                       (Regs (Reg), To_SF (Regs (F.SF_Reg)));

                  when SF_U32 =>
                     Result (Idx) := Apply_U32
                       (Regs (Reg), Regs (Reg + 1),
                        To_SF (Regs (F.SF_Reg)), Order);

                  when Factor_U16 =>
                     Result (Idx) := Scale (Regs (Reg), F.Factor);

                  when Factor_S16 =>
                     Result (Idx) := Scale_Signed (Regs (Reg), F.Factor);

                  when Factor_U32 =>
                     Result (Idx) := Scale_U32
                       (Regs (Reg), Regs (Reg + 1), F.Factor, Order);

                  when Affine_U16 =>
                     Result (Idx) := Affine (Regs (Reg), F.Factor, F.Offset);

                  when Affine_S16 =>
                     Result (Idx) := Affine_Signed
                       (Regs (Reg), F.Factor, F.Offset);
               end case;
            end;
         end loop;

         return To_Record (Result);
      end From_Registers;

   end Map;

end Ada_Modbus.Scaled_IO;
