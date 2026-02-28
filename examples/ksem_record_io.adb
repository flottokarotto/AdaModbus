--  KSEM_Record_IO - Example using Record_IO with Kostal Smart Energy Meter
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Demonstrates Record_IO for mapping KSEM meter registers (SunSpec Model
--  203) directly to an Ada record. Instead of manual register decoding,
--  the complete meter block is read once and converted via Unchecked_Conversion.
--
--  Usage: ksem_record_io <ip-address> [port] [unit-id]
--  Example: ksem_record_io 192.168.1.100

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO;
with Ada.Command_Line;
with Ada.Calendar;
with Ada.Exceptions;
with Interfaces; use Interfaces;

with Ada_Modbus;
with Ada_Modbus.Master;
with Ada_Modbus.Transport.TCP;
with Ada_Modbus.Record_IO;
with Ada_Modbus.Utilities;
with Ada_Modbus.Energy.SunSpec;

procedure KSEM_Record_IO is

   use Ada_Modbus;
   use Ada_Modbus.Transport.TCP;
   use Ada_Modbus.Energy.SunSpec;

   --  KSEM defaults
   Default_Port    : constant := 502;
   Default_Unit_Id : constant := 1;
   SunSpec_Base    : constant Register_Address := Default_Base_Address;

   ---------------------------------------------------------------------------
   --  Record mapping for SunSpec Model 203 (Three Phase Wye Meter)
   --
   --  53 registers starting after the 2-register model header.
   --  16-bit fields use Integer_16 (signed) or Register_Value (unsigned).
   --  Energy counters are Unsigned_32 (two consecutive registers).
   ---------------------------------------------------------------------------

   type Meter_Register_Map is record
      --  Current (int16, offset 0-4)
      Total_Current   : Integer_16;
      Phase_A_Current : Integer_16;
      Phase_B_Current : Integer_16;
      Phase_C_Current : Integer_16;
      Current_SF      : Integer_16;

      --  Voltage (uint16, offset 5-13)
      Voltage_LN      : Register_Value;
      Phase_A_Voltage : Register_Value;
      Phase_B_Voltage : Register_Value;
      Phase_C_Voltage : Register_Value;
      Voltage_LL      : Register_Value;
      Voltage_AB      : Register_Value;
      Voltage_BC      : Register_Value;
      Voltage_CA      : Register_Value;
      Voltage_SF      : Integer_16;

      --  Frequency (uint16, offset 14-15)
      Frequency       : Register_Value;
      Frequency_SF    : Integer_16;

      --  Active Power (int16, offset 16-20)
      Total_Power     : Integer_16;
      Phase_A_Power   : Integer_16;
      Phase_B_Power   : Integer_16;
      Phase_C_Power   : Integer_16;
      Power_SF        : Integer_16;

      --  Apparent Power (int16, offset 21-25)
      Total_VA        : Integer_16;
      Phase_A_VA      : Integer_16;
      Phase_B_VA      : Integer_16;
      Phase_C_VA      : Integer_16;
      VA_SF           : Integer_16;

      --  Reactive Power (int16, offset 26-30)
      Total_VAR       : Integer_16;
      Phase_A_VAR     : Integer_16;
      Phase_B_VAR     : Integer_16;
      Phase_C_VAR     : Integer_16;
      VAR_SF          : Integer_16;

      --  Power Factor (int16, offset 31-35)
      Total_PF        : Integer_16;
      Phase_A_PF      : Integer_16;
      Phase_B_PF      : Integer_16;
      Phase_C_PF      : Integer_16;
      PF_SF           : Integer_16;

      --  Energy Exported (uint32, offset 36-43)
      Total_Wh_Exp    : Unsigned_32;
      Phase_A_Wh_Exp  : Unsigned_32;
      Phase_B_Wh_Exp  : Unsigned_32;
      Phase_C_Wh_Exp  : Unsigned_32;

      --  Energy Imported (uint32, offset 44-51)
      Total_Wh_Imp    : Unsigned_32;
      Phase_A_Wh_Imp  : Unsigned_32;
      Phase_B_Wh_Imp  : Unsigned_32;
      Phase_C_Wh_Imp  : Unsigned_32;

      --  Energy Scale Factor (int16, offset 52)
      Energy_SF       : Integer_16;
   end record with Size => 53 * 16;

   for Meter_Register_Map use record
      Total_Current   at  0 range 0 .. 15;
      Phase_A_Current at  2 range 0 .. 15;
      Phase_B_Current at  4 range 0 .. 15;
      Phase_C_Current at  6 range 0 .. 15;
      Current_SF      at  8 range 0 .. 15;

      Voltage_LN      at 10 range 0 .. 15;
      Phase_A_Voltage at 12 range 0 .. 15;
      Phase_B_Voltage at 14 range 0 .. 15;
      Phase_C_Voltage at 16 range 0 .. 15;
      Voltage_LL      at 18 range 0 .. 15;
      Voltage_AB      at 20 range 0 .. 15;
      Voltage_BC      at 22 range 0 .. 15;
      Voltage_CA      at 24 range 0 .. 15;
      Voltage_SF      at 26 range 0 .. 15;

      Frequency       at 28 range 0 .. 15;
      Frequency_SF    at 30 range 0 .. 15;

      Total_Power     at 32 range 0 .. 15;
      Phase_A_Power   at 34 range 0 .. 15;
      Phase_B_Power   at 36 range 0 .. 15;
      Phase_C_Power   at 38 range 0 .. 15;
      Power_SF        at 40 range 0 .. 15;

      Total_VA        at 42 range 0 .. 15;
      Phase_A_VA      at 44 range 0 .. 15;
      Phase_B_VA      at 46 range 0 .. 15;
      Phase_C_VA      at 48 range 0 .. 15;
      VA_SF           at 50 range 0 .. 15;

      Total_VAR       at 52 range 0 .. 15;
      Phase_A_VAR     at 54 range 0 .. 15;
      Phase_B_VAR     at 56 range 0 .. 15;
      Phase_C_VAR     at 58 range 0 .. 15;
      VAR_SF          at 60 range 0 .. 15;

      Total_PF        at 62 range 0 .. 15;
      Phase_A_PF      at 64 range 0 .. 15;
      Phase_B_PF      at 66 range 0 .. 15;
      Phase_C_PF      at 68 range 0 .. 15;
      PF_SF           at 70 range 0 .. 15;

      Total_Wh_Exp    at 72 range 0 .. 31;
      Phase_A_Wh_Exp  at 76 range 0 .. 31;
      Phase_B_Wh_Exp  at 80 range 0 .. 31;
      Phase_C_Wh_Exp  at 84 range 0 .. 31;

      Total_Wh_Imp    at 88 range 0 .. 31;
      Phase_A_Wh_Imp  at 92 range 0 .. 31;
      Phase_B_Wh_Imp  at 96 range 0 .. 31;
      Phase_C_Wh_Imp  at 100 range 0 .. 31;

      Energy_SF       at 104 range 0 .. 15;
   end record;

   --  Instantiate Record_IO for the meter map
   package Meter_IO is new Ada_Modbus.Record_IO (Meter_Register_Map);

   --  Field layout: describe each field as 16-bit or 32-bit.
   --  Record_IO computes register indices for word order adjustment.
   use Meter_IO;
   Meter_Fields : constant Field_Sizes :=
     [1 .. 36 => Bits_16,   --  Current, Voltage, Freq, Power, VA, VAR, PF + SFs
      37 .. 44 => Bits_32,  --  8 energy counters (2 registers each)
      45       => Bits_16]; --  Energy SF

   ---------------------------------------------------------------------------
   --  Transport setup
   ---------------------------------------------------------------------------

   type Connection_Access is access all TCP_Connection;
   Connection : aliased TCP_Connection;
   Conn_Ptr   : constant Connection_Access := Connection'Access;

   function Send_Data
     (Ctx  : in out Connection_Access;
      Data : Byte_Array) return Natural is
   begin
      return Send (Ctx.all, Data);
   end Send_Data;

   function Receive_Data
     (Ctx        : in out Connection_Access;
      Buffer     : out Byte_Array;
      Max_Length : Natural;
      Timeout_Ms : Natural) return Natural is
   begin
      return Receive (Ctx.all, Buffer, Max_Length, Timeout_Ms);
   end Receive_Data;

   function Get_Tick return Unsigned_32 is
      use Ada.Calendar;
      Seconds : constant Day_Duration := Ada.Calendar.Seconds (Clock);
   begin
      return Unsigned_32 (Seconds * 1000.0) mod Unsigned_32'Last;
   end Get_Tick;

   package Modbus_Master is new Ada_Modbus.Master
     (Transport_Context => Connection_Access,
      Send              => Send_Data,
      Receive           => Receive_Data,
      Get_Tick_Ms       => Get_Tick);

   Ctx : Modbus_Master.Master_Context;

   ---------------------------------------------------------------------------
   --  Helpers
   ---------------------------------------------------------------------------

   function Fmt (Value : Float; Decimals : Natural := 2) return String is
      Result : String (1 .. 20);
   begin
      Ada.Float_Text_IO.Put (Result, Value, Aft => Decimals, Exp => 0);
      for I in Result'Range loop
         if Result (I) /= ' ' then
            return Result (I .. Result'Last);
         end if;
      end loop;
      return Result;
   end Fmt;

   --  Apply SunSpec scale factor: value * 10^SF
   function Scale (Raw : Integer_16; SF : Integer_16) return Float is
   begin
      return Float (Raw) * (10.0 ** Natural (Integer (SF) + 10)) / (10.0 ** 10);
   end Scale;

   function Scale (Raw : Register_Value; SF : Integer_16) return Float is
   begin
      return Float (Raw) * (10.0 ** Natural (Integer (SF) + 10)) / (10.0 ** 10);
   end Scale;

   function Scale (Raw : Unsigned_32; SF : Integer_16) return Float is
   begin
      return Float (Raw) * (10.0 ** Natural (Integer (SF) + 10)) / (10.0 ** 10);
   end Scale;

   function Read_Registers
     (Slave    : Unit_Id;
      Address  : Register_Address;
      Quantity : Natural;
      Values   : out Register_Array) return Boolean
   is
      Result : Status;
   begin
      Result := Modbus_Master.Read_Holding_Registers
        (Ctx, Slave, Address, Register_Count (Quantity), Values);
      if Result /= Success then
         Put_Line ("  Error at " & Address'Image & ": " & Result'Image);
         return False;
      end if;
      return True;
   end Read_Registers;

   --  Display the decoded meter record
   procedure Print_Meter (M : Meter_Register_Map) is
   begin
      Put_Line ("--- Power ---");
      Put_Line ("  Total: " & Fmt (Scale (M.Total_Power, M.Power_SF), 1) & " W");
      Put_Line ("  L1:    " & Fmt (Scale (M.Phase_A_Power, M.Power_SF), 1) & " W");
      Put_Line ("  L2:    " & Fmt (Scale (M.Phase_B_Power, M.Power_SF), 1) & " W");
      Put_Line ("  L3:    " & Fmt (Scale (M.Phase_C_Power, M.Power_SF), 1) & " W");

      New_Line;
      Put_Line ("--- Voltage (L-N) ---");
      Put_Line ("  Avg:   " & Fmt (Scale (M.Voltage_LN, M.Voltage_SF), 1) & " V");
      Put_Line ("  L1:    " & Fmt (Scale (M.Phase_A_Voltage, M.Voltage_SF), 1) & " V");
      Put_Line ("  L2:    " & Fmt (Scale (M.Phase_B_Voltage, M.Voltage_SF), 1) & " V");
      Put_Line ("  L3:    " & Fmt (Scale (M.Phase_C_Voltage, M.Voltage_SF), 1) & " V");

      New_Line;
      Put_Line ("--- Current ---");
      Put_Line ("  Total: " & Fmt (Scale (M.Total_Current, M.Current_SF), 2) & " A");
      Put_Line ("  L1:    " & Fmt (Scale (M.Phase_A_Current, M.Current_SF), 2) & " A");
      Put_Line ("  L2:    " & Fmt (Scale (M.Phase_B_Current, M.Current_SF), 2) & " A");
      Put_Line ("  L3:    " & Fmt (Scale (M.Phase_C_Current, M.Current_SF), 2) & " A");

      New_Line;
      Put_Line ("--- Frequency ---");
      Put_Line ("  " & Fmt (Scale (M.Frequency, M.Frequency_SF), 2) & " Hz");

      New_Line;
      Put_Line ("--- Power Factor ---");
      Put_Line ("  " & Fmt (Scale (M.Total_PF, M.PF_SF), 3));

      New_Line;
      Put_Line ("--- Energy ---");
      Put_Line ("  Exported: " &
        Fmt (Scale (M.Total_Wh_Exp, M.Energy_SF) / 1000.0, 2) & " kWh");
      Put_Line ("  Imported: " &
        Fmt (Scale (M.Total_Wh_Imp, M.Energy_SF) / 1000.0, 2) & " kWh");
   end Print_Meter;

   ---------------------------------------------------------------------------
   --  Main
   ---------------------------------------------------------------------------

   Host     : String (1 .. 64) := [others => ' '];
   Host_Len : Natural := 0;
   Port     : Natural := Default_Port;
   Unit     : Unit_Id := Default_Unit_Id;
   Result   : Status;

begin
   Put_Line ("=== KSEM Record_IO Example ===");
   New_Line;

   if Ada.Command_Line.Argument_Count < 1 then
      Put_Line ("Usage: ksem_record_io <ip-address> [port] [unit-id]");
      Put_Line ("  Default port: 502");
      Put_Line ("  Default unit: 1");
      return;
   end if;

   declare
      Arg1 : constant String := Ada.Command_Line.Argument (1);
   begin
      Host_Len := Natural'Min (Arg1'Length, 64);
      Host (1 .. Host_Len) := Arg1 (1 .. Host_Len);
   end;

   if Ada.Command_Line.Argument_Count >= 2 then
      Port := Natural'Value (Ada.Command_Line.Argument (2));
   end if;

   if Ada.Command_Line.Argument_Count >= 3 then
      Unit := Unit_Id'Value (Ada.Command_Line.Argument (3));
   end if;

   Put_Line ("Connecting to " & Host (1 .. Host_Len) &
             ":" & Port'Image & " (Unit " & Unit'Image & ")...");

   Connect (Connection, Host (1 .. Host_Len), Port, 5.0, Result);
   if Result /= Success then
      Put_Line ("Connection failed: " & Result'Image);
      return;
   end if;
   Put_Line ("Connected.");
   New_Line;

   Modbus_Master.Initialize
     (Ctx,
      (Mode => Modbus_Master.TCP, Default_Slave => Unit, Default_Timeout => 3000),
      Conn_Ptr);

   --  Check SunSpec identifier
   declare
      Id_Regs : Register_Array (0 .. 1);
   begin
      Put ("Checking SunSpec identifier... ");
      if not Read_Registers (Unit, SunSpec_Base, 2, Id_Regs) then
         Disconnect (Connection);
         return;
      end if;
      if Id_Regs (0) /= SunS_ID_High or Id_Regs (1) /= SunS_ID_Low then
         Put_Line ("Not found! Enable Modbus TCP in KSEM settings.");
         Disconnect (Connection);
         return;
      end if;
      Put_Line ("OK");
   end;

   --  Walk SunSpec models to find Model 203 (Three Phase Wye Meter)
   declare
      Iterator    : Model_Iterator;
      Header      : Register_Array (0 .. 1);
      Found       : Boolean := False;
   begin
      Init_Model_Iterator (Iterator, SunSpec_Base);

      while Iterator.Is_Valid and Iterator.Current_Offset < 500 loop
         if not Read_Registers (Unit, Get_Header_Address (Iterator), 2, Header)
         then
            exit;
         end if;

         exit when Header (0) = End_Model_ID;

         declare
            M_ID  : constant Natural := Natural (Header (0));
            M_Len : constant Natural := Natural (Header (1));
            M_Addr : constant Register_Address := Get_Header_Address (Iterator);
         begin
            Put_Line ("  Model " & M_ID'Image & " at " & M_Addr'Image &
                      " (len " & M_Len'Image & ")");

            --  Model 203 = Three Phase Wye Meter (typical KSEM)
            if M_ID = 203 then
               Put_Line ("  -> Reading Model 203 via Record_IO...");
               New_Line;

               --  Read all 53 data registers (after 2-register header)
               declare
                  Raw_Regs : Meter_IO.Map_Registers;
                  Meter    : Meter_Register_Map;
               begin
                  if Read_Registers
                       (Unit, M_Addr + 2, Natural (Meter_IO.Register_Size),
                        Raw_Regs)
                  then
                     --  Convert registers to record with word order
                     --  adjustment (SunSpec = Big_Endian)
                     Meter := Meter_IO.From_Registers
                       (Raw_Regs, Meter_Fields, Ada_Modbus.Utilities.Big_Endian);

                     Print_Meter (Meter);
                     Found := True;
                  end if;
               end;
            end if;

            Advance_Model_Iterator (Iterator, Model_Length (M_Len));
         end;
      end loop;

      if not Found then
         New_Line;
         Put_Line ("No Model 203 (Three Phase Wye Meter) found.");
         Put_Line ("Your KSEM may use a different meter model (201/202/204).");
      end if;
   end;

   New_Line;
   Put_Line ("Done.");
   Disconnect (Connection);

exception
   when E : others =>
      Put_Line ("Error: " & Ada.Exceptions.Exception_Message (E));
      Disconnect (Connection);
end KSEM_Record_IO;
