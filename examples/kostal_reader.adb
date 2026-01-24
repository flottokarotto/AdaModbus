--  Kostal_Reader - Example for reading Kostal inverter via SunSpec/Modbus TCP
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Tested with: PLENTICORE plus, PIKO IQ, PIKO CI
--
--  Kostal Configuration:
--    Port: 1502 (Kostal default, not standard 502)
--    Unit ID: 71 (default)
--    Enable: Settings -> Modbus/SunSpec (TCP) -> Activate Modbus
--
--  Usage: kostal_reader <ip-address> [port] [unit-id]
--  Example: kostal_reader 192.168.1.50
--           kostal_reader 192.168.1.50 1502 71

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO;
with Ada.Command_Line;
with Ada.Calendar;
with Ada.Exceptions;
with Interfaces; use Interfaces;

with Ada_Modbus;
with Ada_Modbus.Master;
with Ada_Modbus.Transport.TCP;
with Ada_Modbus.Energy.SunSpec;
with Ada_Modbus.Energy.SunSpec.Common;
with Ada_Modbus.Energy.SunSpec.Inverter;

procedure Kostal_Reader is

   use Ada_Modbus;
   use Ada_Modbus.Transport.TCP;
   use Ada_Modbus.Energy.SunSpec;
   use Ada_Modbus.Energy.SunSpec.Inverter;

   --  Kostal default settings
   Default_Port    : constant := 1502;
   Default_Unit_Id : constant := 71;
   SunSpec_Base    : constant Register_Address := Default_Base_Address;

   --  Connection
   type Connection_Access is access all TCP_Connection;
   Connection : aliased TCP_Connection;
   Conn_Ptr   : constant Connection_Access := Connection'Access;

   --  Transport callbacks for Master generic
   function Send_Data
     (Ctx  : in out Connection_Access;
      Data : Byte_Array) return Natural is
   begin
      return Send (Ctx.all, Data);
   end Send_Data;

   function Receive_Data
     (Ctx        : in Out Connection_Access;
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

   --  Instantiate master
   package Modbus_Master is new Ada_Modbus.Master
     (Transport_Context => Connection_Access,
      Send              => Send_Data,
      Receive           => Receive_Data,
      Get_Tick_Ms       => Get_Tick);

   Ctx : Modbus_Master.Master_Context;

   --  Format float without E notation
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

   --  Helper: Read registers with error handling
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

   --  Check SunSpec identifier
   function Check_SunSpec (Slave : Unit_Id) return Boolean is
      Values : Register_Array (0 .. 1);
   begin
      if not Read_Registers (Slave, SunSpec_Base, 2, Values) then
         return False;
      end if;
      return Values (0) = SunS_ID_High and Values (1) = SunS_ID_Low;
   end Check_SunSpec;

   --  Read and display Common Model (Model 1)
   procedure Read_Common_Model (Slave : Unit_Id; Model_Start : Register_Address)
   is
      use Ada_Modbus.Energy.SunSpec.Common;
      Str_Regs : Register_Array (0 .. 15);
      Str_Val  : SunSpec_String;
      Str_Len  : Natural;
   begin
      Put_Line ("--- Device Information (Model 1) ---");

      if Read_Registers (Slave, Model_Start + Reg_Manufacturer, 16, Str_Regs) then
         Decode_String (Str_Regs, Str_Val, Str_Len);
         Put_Line ("  Manufacturer: " & Str_Val (1 .. Str_Len));
      end if;

      if Read_Registers (Slave, Model_Start + Reg_Model, 16, Str_Regs) then
         Decode_String (Str_Regs, Str_Val, Str_Len);
         Put_Line ("  Model:        " & Str_Val (1 .. Str_Len));
      end if;

      if Read_Registers (Slave, Model_Start + Reg_Serial, 16, Str_Regs) then
         Decode_String (Str_Regs, Str_Val, Str_Len);
         Put_Line ("  Serial:       " & Str_Val (1 .. Str_Len));
      end if;

      declare
         Ver_Regs : Register_Array (0 .. 7);
      begin
         if Read_Registers (Slave, Model_Start + Reg_Version, 8, Ver_Regs) then
            Decode_String (Ver_Regs, Str_Val, Str_Len);
            Put_Line ("  Version:      " & Str_Val (1 .. Str_Len));
         end if;
      end;
   end Read_Common_Model;

   --  Read and display Inverter Model (Model 101/102/103)
   procedure Read_Inverter_Model
     (Slave       : Unit_Id;
      Model_Start : Register_Address;
      Model_Id    : Natural)
   is
      AC_Regs     : Register_Array (0 .. 15);
      DC_Regs     : Register_Array (0 .. 5);
      Energy_Regs : Register_Array (0 .. 2);
      State_Regs  : Register_Array (0 .. 1);
      Temp_Regs   : Register_Array (0 .. 4);
   begin
      New_Line;
      Put_Line ("--- Inverter Data (Model " & Model_Id'Image & ") ---");

      --  AC measurements (regs 2-17)
      if Read_Registers (Slave, Model_Start + Reg_AC_Current, 16, AC_Regs) then
         declare
            I_SF : constant Scale_Factor := To_Scale_Factor (AC_Regs (4));
            V_SF : constant Scale_Factor := To_Scale_Factor (AC_Regs (11));
            P_SF : constant Scale_Factor := To_Scale_Factor (AC_Regs (13));
            F_SF : constant Scale_Factor := To_Scale_Factor (AC_Regs (15));
         begin
            Put_Line ("  AC Current:   " &
                      Fmt (Apply_Scale (AC_Regs (0), I_SF)) & " A");
            Put_Line ("  AC Voltage:   " &
                      Fmt (Apply_Scale (AC_Regs (8), V_SF), 1) & " V");
            Put_Line ("  AC Power:     " &
                      Fmt (Apply_Scale (AC_Regs (12), P_SF), 0) & " W");
            Put_Line ("  AC Frequency: " &
                      Fmt (Apply_Scale (AC_Regs (14), F_SF)) & " Hz");
         end;
      end if;

      --  DC measurements (regs 27-32)
      if Read_Registers (Slave, Model_Start + Reg_DC_Current, 6, DC_Regs) then
         declare
            I_SF : constant Scale_Factor := To_Scale_Factor (DC_Regs (1));
            V_SF : constant Scale_Factor := To_Scale_Factor (DC_Regs (3));
            W_SF : constant Scale_Factor := To_Scale_Factor (DC_Regs (5));
         begin
            if Is_Implemented (DC_Regs (0)) then
               Put_Line ("  DC Current:   " &
                         Fmt (Apply_Scale (DC_Regs (0), I_SF)) & " A");
            end if;
            if Is_Implemented (DC_Regs (2)) then
               Put_Line ("  DC Voltage:   " &
                         Fmt (Apply_Scale (DC_Regs (2), V_SF), 1) & " V");
            end if;
            if Is_Implemented (DC_Regs (4)) then
               Put_Line ("  DC Power:     " &
                         Fmt (Apply_Scale (DC_Regs (4), W_SF), 0) & " W");
            end if;
         end;
      end if;

      --  Energy (regs 24-26: 32-bit value + SF)
      if Read_Registers (Slave, Model_Start + Reg_AC_Energy, 3, Energy_Regs) then
         declare
            Energy_Raw : constant Unsigned_32 :=
              Unsigned_32 (Energy_Regs (0)) * 65536 +
              Unsigned_32 (Energy_Regs (1));
            Energy_SF  : constant Scale_Factor :=
              To_Scale_Factor (Energy_Regs (2));
            Energy_Wh  : constant Float :=
              Float (Energy_Raw) * Scale_Multipliers (Energy_SF);
         begin
            Put_Line ("  Total Energy: " & Fmt (Energy_Wh / 1000.0, 1) & " kWh");
         end;
      end if;

      --  Temperature (regs 33-37)
      if Read_Registers (Slave, Model_Start + Reg_Cabinet_Temp, 5, Temp_Regs) then
         declare
            Temp_SF : constant Scale_Factor := To_Scale_Factor (Temp_Regs (4));
         begin
            Put_Line ("  Cabinet Temp: " &
                      Fmt (Apply_Scale_Signed (Temp_Regs (0), Temp_SF), 1) & " C");
         end;
      end if;

      --  Operating State (regs 38-39)
      if Read_Registers (Slave, Model_Start + Reg_Operating_State, 2, State_Regs) then
         declare
            State_Code : constant Natural := Natural (State_Regs (0));
            State_Name : constant String :=
              (case State_Code is
                  when 1 => "Off",
                  when 2 => "Sleeping",
                  when 3 => "Starting",
                  when 4 => "Running (MPPT)",
                  when 5 => "Throttled",
                  when 6 => "Shutting Down",
                  when 7 => "Fault",
                  when 8 => "Standby",
                  when others => "Unknown (" & State_Code'Image & ")");
         begin
            Put_Line ("  State:        " & State_Name);
         end;
      end if;
   end Read_Inverter_Model;

   --  Read and display MPPT Model (Model 160)
   procedure Read_MPPT_Model
     (Slave       : Unit_Id;
      Model_Start : Register_Address;
      Model_Len   : Natural)
   is
      Header_Regs : Register_Array (0 .. 7);
      Module_Regs : Register_Array (0 .. MPPT_Module_Size - 1);
      Header      : MPPT_Header;
      Module      : MPPT_Module_Data;
      Module_Base : Register_Address;
   begin
      New_Line;
      Put_Line ("--- MPPT Data (Model 160) ---");

      if not Read_Registers (Slave, Model_Start + MPPT_Reg_DCA_SF, 8, Header_Regs) then
         return;
      end if;

      Decode_MPPT_Header (Header_Regs, Header);
      Put_Line ("  Modules: " & Header.Num_Modules'Image);

      Module_Base := Model_Start + 10;
      for M in 1 .. Header.Num_Modules loop
         exit when Natural (Module_Base - Model_Start) + MPPT_Module_Size > Model_Len + 2;

         if Read_Registers (Slave, Module_Base, MPPT_Module_Size, Module_Regs) then
            Decode_MPPT_Module (Module_Regs, Header, Module);

            if Module.Is_Valid then
               New_Line;
               Put_Line ("  String " & Module.Module_ID'Image & ":");
               Put_Line ("    Current: " & Fmt (Module.Current_A) & " A");
               Put_Line ("    Voltage: " & Fmt (Module.Voltage_V, 1) & " V");
               Put_Line ("    Power:   " & Fmt (Module.Power_W, 0) & " W");
            end if;
         end if;

         Module_Base := Module_Base + MPPT_Module_Size;
      end loop;
   end Read_MPPT_Model;

   --  Main variables
   Host     : String (1 .. 64) := [others => ' '];
   Host_Len : Natural := 0;
   Port     : Natural := Default_Port;
   Unit     : Unit_Id := Default_Unit_Id;
   Result   : Status;

begin
   Put_Line ("=== Kostal Inverter Reader (SunSpec) ===");
   New_Line;

   if Ada.Command_Line.Argument_Count < 1 then
      Put_Line ("Usage: kostal_reader <ip-address> [port] [unit-id]");
      Put_Line ("  Default port: 1502");
      Put_Line ("  Default unit: 71");
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

   Put ("Checking SunSpec identifier... ");
   if not Check_SunSpec (Unit) then
      Put_Line ("Not found!");
      Put_Line ("Make sure Modbus/SunSpec is enabled in the inverter settings.");
      Disconnect (Connection);
      return;
   end if;
   Put_Line ("OK (SunS found at 40000)");
   New_Line;

   --  Walk through models using library iterator
   declare
      Iterator : Model_Iterator;
      Header   : Register_Array (0 .. 1);
   begin
      Init_Model_Iterator (Iterator, SunSpec_Base);

      while Iterator.Is_Valid and Iterator.Current_Offset < 500 loop
         if not Read_Registers (Unit, Get_Header_Address (Iterator), 2, Header) then
            exit;
         end if;

         exit when Header (0) = End_Model_ID;

         declare
            M_ID  : constant Natural := Natural (Header (0));
            M_Len : constant Natural := Natural (Header (1));
            M_Start : constant Register_Address := Get_Header_Address (Iterator);
         begin
            case M_ID is
               when Model_Common =>
                  Read_Common_Model (Unit, M_Start);
               when Model_Inverter_1P | Model_Inverter_SP | Model_Inverter_3P =>
                  Read_Inverter_Model (Unit, M_Start, M_ID);
               when Model_Storage =>
                  New_Line;
                  Put_Line ("--- Battery Storage (Model 124) found ---");
               when Model_MPPT =>
                  Read_MPPT_Model (Unit, M_Start, M_Len);
               when others =>
                  null;
            end case;

            Advance_Model_Iterator (Iterator, Model_Length (M_Len));
         end;
      end loop;

      New_Line;
      Put_Line ("--- End of Model List ---");
   end;

   New_Line;
   Put_Line ("Done.");
   Disconnect (Connection);

exception
   when E : others =>
      Put_Line ("Error: " & Ada.Exceptions.Exception_Message (E));
      Disconnect (Connection);
end Kostal_Reader;
