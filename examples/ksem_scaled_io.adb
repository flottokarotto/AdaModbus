--  KSEM_Scaled_IO - Example using Scaled_IO with Kostal Smart Energy Meter
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Demonstrates Scaled_IO for declarative register-to-Float mapping.
--  Compare with ksem_record_io.adb (Record_IO, manual scaling):
--    - No representation clause needed
--    - No manual Scale() helper functions
--    - Field descriptors declare source register + scaling kind
--    - Output record contains physical Float values directly
--
--  Usage: ksem_scaled_io <ip-address> [port] [unit-id]
--  Example: ksem_scaled_io 192.168.1.100

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO;
with Ada.Command_Line;
with Ada.Calendar;
with Ada.Exceptions;
with Interfaces; use Interfaces;

with Ada_Modbus;
with Ada_Modbus.Master;
with Ada_Modbus.Transport.TCP;
with Ada_Modbus.Scaled_IO;
with Ada_Modbus.Energy.SunSpec;

procedure KSEM_Scaled_IO is

   use Ada_Modbus;
   use Ada_Modbus.Transport.TCP;
   use Ada_Modbus.Energy.SunSpec;

   --  KSEM defaults
   Default_Port    : constant := 502;
   Default_Unit_Id : constant := 1;
   SunSpec_Base    : constant Register_Address := Default_Base_Address;

   --  Number of data registers in SunSpec Model 203 (after 2-reg header)
   Model_203_Regs : constant := 53;

   ---------------------------------------------------------------------------
   --  Scaled record: only the fields we care about, all as Float
   ---------------------------------------------------------------------------

   type Meter_Data is record
      --  Power (W)
      Total_Power   : Float;
      Phase_A_Power : Float;
      Phase_B_Power : Float;
      Phase_C_Power : Float;
      --  Voltage L-N (V)
      Voltage_LN    : Float;
      Phase_A_V     : Float;
      Phase_B_V     : Float;
      Phase_C_V     : Float;
      --  Current (A)
      Total_Current : Float;
      Phase_A_A     : Float;
      Phase_B_A     : Float;
      Phase_C_A     : Float;
      --  Misc
      Frequency     : Float;  --  Hz
      Total_PF      : Float;  --  -
      --  Energy (Wh)
      Total_Wh_Exp  : Float;
      Total_Wh_Imp  : Float;
   end record;

   ---------------------------------------------------------------------------
   --  Field descriptors: register index + scaling kind, that's it.
   --
   --  SunSpec Model 203 register offsets (0-based from model data start):
   --    0-3:   Current (int16), 4: Current_SF
   --    5-12:  Voltage (uint16), 13: Voltage_SF
   --    14:    Frequency (uint16), 15: Frequency_SF
   --    16-19: Power (int16), 20: Power_SF
   --    31:    Total PF (int16), 35: PF_SF
   --    36-37: Total Wh Export (uint32), 44-45: Total Wh Import (uint32)
   --    52:    Energy_SF
   ---------------------------------------------------------------------------

   package Meter_IO is new Ada_Modbus.Scaled_IO (Meter_Data);
   use Meter_IO;

   Meter_Fields : constant Field_Descriptors :=
     [--  Power: signed * 10^SF, SF at register 20
      (Reg => 16, Kind => SF_S16, SF_Reg => 20, others => <>),  --  Total_Power
      (Reg => 17, Kind => SF_S16, SF_Reg => 20, others => <>),  --  Phase_A
      (Reg => 18, Kind => SF_S16, SF_Reg => 20, others => <>),  --  Phase_B
      (Reg => 19, Kind => SF_S16, SF_Reg => 20, others => <>),  --  Phase_C
      --  Voltage L-N: unsigned * 10^SF, SF at register 13
      (Reg =>  5, Kind => SF_U16, SF_Reg => 13, others => <>),  --  Voltage_LN
      (Reg =>  6, Kind => SF_U16, SF_Reg => 13, others => <>),  --  Phase_A
      (Reg =>  7, Kind => SF_U16, SF_Reg => 13, others => <>),  --  Phase_B
      (Reg =>  8, Kind => SF_U16, SF_Reg => 13, others => <>),  --  Phase_C
      --  Current: signed * 10^SF, SF at register 4
      (Reg =>  0, Kind => SF_S16, SF_Reg =>  4, others => <>),  --  Total
      (Reg =>  1, Kind => SF_S16, SF_Reg =>  4, others => <>),  --  Phase_A
      (Reg =>  2, Kind => SF_S16, SF_Reg =>  4, others => <>),  --  Phase_B
      (Reg =>  3, Kind => SF_S16, SF_Reg =>  4, others => <>),  --  Phase_C
      --  Frequency: unsigned * 10^SF, SF at register 15
      (Reg => 14, Kind => SF_U16, SF_Reg => 15, others => <>),  --  Frequency
      --  Power Factor: signed * 10^SF, SF at register 35
      (Reg => 31, Kind => SF_S16, SF_Reg => 35, others => <>),  --  Total_PF
      --  Energy: unsigned 32-bit * 10^SF, SF at register 52
      (Reg => 36, Kind => SF_U32, SF_Reg => 52, others => <>),  --  Wh Export
      (Reg => 44, Kind => SF_U32, SF_Reg => 52, others => <>)]; --  Wh Import

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

   --  Display the scaled meter data (already in physical units)
   procedure Print_Meter (M : Meter_Data) is
   begin
      Put_Line ("--- Power ---");
      Put_Line ("  Total: " & Fmt (M.Total_Power, 1) & " W");
      Put_Line ("  L1:    " & Fmt (M.Phase_A_Power, 1) & " W");
      Put_Line ("  L2:    " & Fmt (M.Phase_B_Power, 1) & " W");
      Put_Line ("  L3:    " & Fmt (M.Phase_C_Power, 1) & " W");

      New_Line;
      Put_Line ("--- Voltage (L-N) ---");
      Put_Line ("  Avg:   " & Fmt (M.Voltage_LN, 1) & " V");
      Put_Line ("  L1:    " & Fmt (M.Phase_A_V, 1) & " V");
      Put_Line ("  L2:    " & Fmt (M.Phase_B_V, 1) & " V");
      Put_Line ("  L3:    " & Fmt (M.Phase_C_V, 1) & " V");

      New_Line;
      Put_Line ("--- Current ---");
      Put_Line ("  Total: " & Fmt (M.Total_Current, 2) & " A");
      Put_Line ("  L1:    " & Fmt (M.Phase_A_A, 2) & " A");
      Put_Line ("  L2:    " & Fmt (M.Phase_B_A, 2) & " A");
      Put_Line ("  L3:    " & Fmt (M.Phase_C_A, 2) & " A");

      New_Line;
      Put_Line ("--- Frequency ---");
      Put_Line ("  " & Fmt (M.Frequency, 2) & " Hz");

      New_Line;
      Put_Line ("--- Power Factor ---");
      Put_Line ("  " & Fmt (M.Total_PF, 3));

      New_Line;
      Put_Line ("--- Energy ---");
      Put_Line ("  Exported: " & Fmt (M.Total_Wh_Exp / 1000.0, 2) & " kWh");
      Put_Line ("  Imported: " & Fmt (M.Total_Wh_Imp / 1000.0, 2) & " kWh");
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
   Put_Line ("=== KSEM Scaled_IO Example ===");
   New_Line;

   if Ada.Command_Line.Argument_Count < 1 then
      Put_Line ("Usage: ksem_scaled_io <ip-address> [port] [unit-id]");
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
      Iterator : Model_Iterator;
      Header   : Register_Array (0 .. 1);
      Found    : Boolean := False;
   begin
      Init_Model_Iterator (Iterator, SunSpec_Base);

      while Iterator.Is_Valid and Iterator.Current_Offset < 500 loop
         if not Read_Registers (Unit, Get_Header_Address (Iterator), 2, Header)
         then
            exit;
         end if;

         exit when Header (0) = End_Model_ID;

         declare
            M_ID   : constant Natural := Natural (Header (0));
            M_Len  : constant Natural := Natural (Header (1));
            M_Addr : constant Register_Address := Get_Header_Address (Iterator);
         begin
            Put_Line ("  Model " & M_ID'Image & " at " & M_Addr'Image &
                      " (len " & M_Len'Image & ")");

            --  Model 203 = Three Phase Wye Meter (typical KSEM)
            if M_ID = 203 then
               Put_Line ("  -> Reading Model 203 via Scaled_IO...");
               New_Line;

               --  Read all 53 data registers (after 2-register header)
               declare
                  Raw_Regs : Register_Array (0 .. Model_203_Regs - 1);
               begin
                  if Read_Registers
                       (Unit, M_Addr + 2, Model_203_Regs, Raw_Regs)
                  then
                     --  One-liner: registers -> physical Float values
                     Print_Meter (Meter_IO.From_Registers (Raw_Regs, Meter_Fields));
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
end KSEM_Scaled_IO;
