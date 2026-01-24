--  Ada_Modbus.Energy.SunSpec - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

package body Ada_Modbus.Energy.SunSpec
  with SPARK_Mode => On
is

   ---------------------
   -- To_Scale_Factor --
   ---------------------

   function To_Scale_Factor (Value : Register_Value) return Scale_Factor is
      Raw : Integer;
   begin
      --  Convert unsigned to signed (two's complement)
      if Value > 32767 then
         Raw := Integer (Value) - 65536;
      else
         Raw := Integer (Value);
      end if;

      --  Clamp to valid range
      if Raw in -10 .. 10 then
         return Scale_Factor (Raw);
      else
         --  Invalid SF, use 0 (no scaling)
         return 0;
      end if;
   end To_Scale_Factor;

   ------------------
   -- To_Signed_16 --
   ------------------

   function To_Signed_16 (Value : Register_Value) return Integer is
   begin
      if Value > 32767 then
         return Integer (Value) - 65536;
      else
         return Integer (Value);
      end if;
   end To_Signed_16;

   --------------------
   -- Is_Implemented --
   --------------------

   function Is_Implemented (Value : Register_Value) return Boolean is
   begin
      --  Check common "not implemented" markers for uint16
      return Value /= Not_Implemented and then
             Value /= 16#7FFF#;  --  Some devices use 0x7FFF
   end Is_Implemented;

   --------------------------
   -- Is_Implemented_Int16 --
   --------------------------

   function Is_Implemented_Int16 (Value : Register_Value) return Boolean is
   begin
      --  Check "not implemented" markers for int16
      return Value /= Not_Implemented_Int16 and then
             Value /= Not_Implemented;
   end Is_Implemented_Int16;

   -----------------
   -- Apply_Scale --
   -----------------

   function Apply_Scale (Value : Register_Value; SF : Scale_Factor) return Float
   is
   begin
      return Float (Value) * Scale_Multipliers (SF);
   end Apply_Scale;

   ------------------------
   -- Apply_Scale_Signed --
   ------------------------

   function Apply_Scale_Signed
     (Value : Register_Value; SF : Scale_Factor) return Float
   is
      Signed_Value : Integer;
      Result       : Float;
   begin
      --  Convert unsigned 16-bit to signed (two's complement)
      if Value > 32767 then
         Signed_Value := Integer (Value) - 65536;
      else
         Signed_Value := Integer (Value);
      end if;
      --  Signed_Value is bounded to -32768..32767, Scale_Multipliers to 1E-10..1E10
      --  Maximum result: 32767 * 1E10 = 3.2767E14 < Float'Last (3.4E38)
      Result := Float (Signed_Value) * Scale_Multipliers (SF);
      pragma Annotate (GNATprove, Intentional,
                       "float overflow check might fail",
                       "Signed_Value bounded to int16 range, result < Float'Last");
      return Result;
   end Apply_Scale_Signed;

   -------------------
   -- Decode_String --
   -------------------

   procedure Decode_String
     (Registers : Register_Array;
      Result    : out SunSpec_String;
      Length    : out Natural)
   is
      Idx : Natural := 0;
   begin
      Result := (others => ' ');
      Length := 0;

      for R of Registers loop
         --  High byte first (big-endian)
         declare
            Hi : constant Character := Character'Val (Natural (R / 256) mod 256);
            Lo : constant Character := Character'Val (Natural (R) mod 256);
         begin
            if Idx < 32 and then Hi /= ASCII.NUL then
               Idx := Idx + 1;
               Result (Idx) := Hi;
               Length := Idx;
            end if;
            if Idx < 32 and then Lo /= ASCII.NUL then
               Idx := Idx + 1;
               Result (Idx) := Lo;
               Length := Idx;
            end if;
         end;
      end loop;
   end Decode_String;

   ---------------------------------
   -- Encode_Check_SunSpec_Request --
   ---------------------------------

   procedure Encode_Check_SunSpec_Request
     (Base_Address : Register_Address;
      Buffer       : out Protocol.PDU_Buffer;
      Length       : out Natural)
   is
   begin
      Protocol.Encode_Read_Registers_Request
        (FC            => FC_Read_Holding_Registers,
         Start_Address => Base_Address,
         Quantity      => 2,
         Buffer        => Buffer,
         Length        => Length);
   end Encode_Check_SunSpec_Request;

   ----------------------------------
   -- Decode_Check_SunSpec_Response --
   ----------------------------------

   procedure Decode_Check_SunSpec_Response
     (Buffer   : Protocol.PDU_Buffer;
      Length   : Natural;
      Is_Valid : out Boolean;
      Result   : out Status)
   is
      Values    : Register_Array (0 .. 1);
      Reg_Count : Natural;
   begin
      Is_Valid := False;

      Protocol.Decode_Read_Registers_Response
        (Buffer   => Buffer,
         Length   => Length,
         Values   => Values,
         Count    => Reg_Count,
         Response => Result);

      if Result = Success and then Reg_Count = 2 then
         Is_Valid := Values (0) = SunS_ID_High and then
                     Values (1) = SunS_ID_Low;
      end if;
   end Decode_Check_SunSpec_Response;

   -------------------------------------
   -- Encode_Read_Model_Header_Request --
   -------------------------------------

   procedure Encode_Read_Model_Header_Request
     (Base_Address : Register_Address;
      Offset       : Register_Address;
      Buffer       : out Protocol.PDU_Buffer;
      Length       : out Natural)
   is
   begin
      Protocol.Encode_Read_Registers_Request
        (FC            => FC_Read_Holding_Registers,
         Start_Address => Base_Address + Offset,
         Quantity      => 2,
         Buffer        => Buffer,
         Length        => Length);
   end Encode_Read_Model_Header_Request;

   ---------------------------------
   -- Decode_Model_Header_Response --
   ---------------------------------

   procedure Decode_Model_Header_Response
     (Buffer : Protocol.PDU_Buffer;
      Length : Natural;
      Header : out Model_Header;
      Result : out Status)
   is
      Values    : Register_Array (0 .. 1);
      Reg_Count : Natural;
   begin
      Header := (ID => 0, Length => 0);

      Protocol.Decode_Read_Registers_Response
        (Buffer   => Buffer,
         Length   => Length,
         Values   => Values,
         Count    => Reg_Count,
         Response => Result);

      if Result = Success and then Reg_Count = 2 then
         Header.ID     := Model_ID (Values (0));
         Header.Length := Model_Length (Values (1));
      end if;
   end Decode_Model_Header_Response;

   ---------------------
   -- Model Discovery --
   ---------------------

   -------------------------
   -- Init_Model_Iterator --
   -------------------------

   procedure Init_Model_Iterator
     (Iterator     : out Model_Iterator;
      Base_Address : Register_Address := Default_Base_Address;
      Max_Offset   : Register_Address := 1000)
   is
   begin
      Iterator := (Base_Address   => Base_Address,
                   Current_Offset => 2,  --  Skip SunS identifier
                   Max_Offset     => Max_Offset,
                   Is_Valid       => True);
   end Init_Model_Iterator;

   ----------------------------
   -- Advance_Model_Iterator --
   ----------------------------

   procedure Advance_Model_Iterator
     (Iterator : in out Model_Iterator;
      Length   : Model_Length)
   is
      New_Offset : Register_Address;
   begin
      --  Move past header (2) + model data
      New_Offset := Iterator.Current_Offset + 2 + Register_Address (Length);

      if New_Offset >= Iterator.Max_Offset then
         Iterator.Is_Valid := False;
      else
         Iterator.Current_Offset := New_Offset;
      end if;
   end Advance_Model_Iterator;

   ------------------
   -- Is_End_Model --
   ------------------

   function Is_End_Model (Header : Model_Header) return Boolean is
   begin
      return Header.ID = Model_ID (End_Model_ID);
   end Is_End_Model;

   ------------------------
   -- Get_Header_Address --
   ------------------------

   function Get_Header_Address (Iterator : Model_Iterator) return Register_Address is
   begin
      return Iterator.Base_Address + Iterator.Current_Offset;
   end Get_Header_Address;

   ----------------------
   -- Get_Data_Address --
   ----------------------

   function Get_Data_Address
     (Base_Address : Register_Address;
      Model_Offset : Register_Address) return Register_Address
   is
   begin
      return Base_Address + Model_Offset + 2;
   end Get_Data_Address;

   -----------------------
   -- Is_Inverter_Model --
   -----------------------

   function Is_Inverter_Model (ID : Model_ID) return Boolean is
   begin
      return ID in 101 .. 103;
   end Is_Inverter_Model;

   --------------------
   -- Is_Meter_Model --
   --------------------

   function Is_Meter_Model (ID : Model_ID) return Boolean is
   begin
      return ID in 201 .. 204;
   end Is_Meter_Model;

end Ada_Modbus.Energy.SunSpec;
