--  KSEM_Reader - Example for reading Kostal Smart Energy Meter via Modbus TCP
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Tested with: KSEM G1/G2
--
--  KSEM Configuration:
--    Port: 502 (standard Modbus)
--    Unit ID: 1 (default)
--    Enable: Modbus TCP Slave must be enabled in KSEM settings
--
--  The KSEM uses SunSpec protocol (Models 1, 201-204 for meters)
--
--  Usage: ksem_reader <ip-address> [port] [unit-id]
--  Example: ksem_reader 192.168.1.100
--           ksem_reader 192.168.1.100 502 1

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
with Ada_Modbus.Energy.SunSpec.Meter;

procedure KSEM_Reader is

   use Ada_Modbus;
   use Ada_Modbus.Transport.TCP;
   use Ada_Modbus.Energy.SunSpec;
   use Ada_Modbus.Energy.SunSpec.Meter;

   --  KSEM default settings
   Default_Port    : constant := 502;
   Default_Unit_Id : constant := 1;
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

   --  Format value with "N/A" for not-implemented
   function Format_Float (Value : Float; Decimals : Natural; Unit : String)
      return String is
   begin
      if Value = 0.0 then
         return "N/A";
      else
         return Fmt (Value, Decimals) & " " & Unit;
      end if;
   end Format_Float;

   --  Read and display Meter Model (201-204)
   procedure Read_Meter_Model
     (Slave       : Unit_Id;
      Model_Start : Register_Address;
      M_ID        : Natural)
   is
      --  Read all registers (need at least 55 for full decode)
      Meter_Regs : Register_Array (0 .. 54);
      SF         : Meter_Scale_Factors;
      Data       : Meter_Data;
      M_Type     : constant Meter_Type := To_Meter_Type (Model_ID (M_ID));
   begin
      New_Line;
      Put_Line ("--- Meter Readings (Model " & M_ID'Image & ") ---");

      --  Read registers (offset 2 = after header)
      if not Read_Registers (Slave, Model_Start + 2, 55, Meter_Regs) then
         Put_Line ("  Failed to read meter registers.");
         return;
      end if;

      --  Decode scale factors and data
      Decode_Meter_Scale_Factors (Meter_Regs, SF);
      Decode_Meter_Totals (Meter_Regs, SF, M_Type, Data);
      Decode_Meter_Phases (Meter_Regs, SF, Data);

      --  Display totals
      Put_Line ("--- Power ---");
      Put_Line ("  Total:  " & Format_Float (Data.Total_Power, 1, "W"));

      if M_ID >= 203 then
         Put_Line ("  L1:     " & Format_Float (Data.Phase_A.Power_W, 1, "W"));
         Put_Line ("  L2:     " & Format_Float (Data.Phase_B.Power_W, 1, "W"));
         Put_Line ("  L3:     " & Format_Float (Data.Phase_C.Power_W, 1, "W"));
      end if;

      New_Line;
      Put_Line ("--- Voltage (L-N) ---");
      Put_Line ("  Avg:    " & Format_Float (Data.Total_Voltage, 1, "V"));

      if M_ID >= 203 then
         Put_Line ("  L1:     " & Format_Float (Data.Phase_A.Voltage_V, 1, "V"));
         Put_Line ("  L2:     " & Format_Float (Data.Phase_B.Voltage_V, 1, "V"));
         Put_Line ("  L3:     " & Format_Float (Data.Phase_C.Voltage_V, 1, "V"));
      end if;

      New_Line;
      Put_Line ("--- Current ---");
      Put_Line ("  Total:  " & Format_Float (Data.Total_Current, 2, "A"));

      if M_ID >= 203 then
         Put_Line ("  L1:     " & Format_Float (Data.Phase_A.Current_A, 2, "A"));
         Put_Line ("  L2:     " & Format_Float (Data.Phase_B.Current_A, 2, "A"));
         Put_Line ("  L3:     " & Format_Float (Data.Phase_C.Current_A, 2, "A"));
      end if;

      New_Line;
      Put_Line ("--- Frequency ---");
      Put_Line ("  " & Format_Float (Data.Frequency, 2, "Hz"));

      New_Line;
      Put_Line ("--- Apparent Power ---");
      Put_Line ("  Total:  " & Format_Float (Data.Total_VA, 1, "VA"));

      New_Line;
      Put_Line ("--- Reactive Power ---");
      Put_Line ("  Total:  " & Format_Float (Data.Total_VAR, 1, "var"));

      New_Line;
      Put_Line ("--- Power Factor ---");
      Put_Line ("  " & Format_Float (Data.Total_PF, 3, ""));

      --  Energy counters
      if Data.Total_Exp_Wh > 0.0 or Data.Total_Imp_Wh > 0.0 then
         New_Line;
         Put_Line ("--- Energy ---");
         Put_Line ("  Exported: " & Fmt (Data.Total_Exp_Wh / 1000.0, 2) & " kWh");
         Put_Line ("  Imported: " & Fmt (Data.Total_Imp_Wh / 1000.0, 2) & " kWh");
      end if;
   end Read_Meter_Model;

   --  Main variables
   Host     : String (1 .. 64) := [others => ' '];
   Host_Len : Natural := 0;
   Port     : Natural := Default_Port;
   Unit     : Unit_Id := Default_Unit_Id;
   Result   : Status;

begin
   Put_Line ("=== Kostal Smart Energy Meter (KSEM) Reader ===");
   New_Line;

   if Ada.Command_Line.Argument_Count < 1 then
      Put_Line ("Usage: ksem_reader <ip-address> [port] [unit-id]");
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

   Put ("Checking SunSpec identifier... ");
   if not Check_SunSpec (Unit) then
      Put_Line ("Not found!");
      Put_Line ("Make sure Modbus TCP Slave is enabled in KSEM settings.");
      Disconnect (Connection);
      return;
   end if;
   Put_Line ("OK (SunS found at 40000)");
   New_Line;

   --  Walk through models using library iterator
   declare
      Iterator     : Model_Iterator;
      Header       : Register_Array (0 .. 1);
      Found_Meter  : Boolean := False;
   begin
      Init_Model_Iterator (Iterator, SunSpec_Base);

      while Iterator.Is_Valid and Iterator.Current_Offset < 500 loop
         if not Read_Registers (Unit, Get_Header_Address (Iterator), 2, Header) then
            exit;
         end if;

         exit when Header (0) = End_Model_ID;

         declare
            M_ID    : constant Natural := Natural (Header (0));
            M_Len   : constant Natural := Natural (Header (1));
            M_Start : constant Register_Address := Get_Header_Address (Iterator);
         begin
            case M_ID is
               when Model_Common =>
                  Read_Common_Model (Unit, M_Start);

               when Model_Meter_1P | Model_Meter_SP |
                    Model_Meter_3P_Wye | Model_Meter_3P_Delta =>
                  Read_Meter_Model (Unit, M_Start, M_ID);
                  Found_Meter := True;

               when others =>
                  Put_Line ("  Found Model " & M_ID'Image &
                            " at " & M_Start'Image & " (len " & M_Len'Image & ")");
            end case;

            Advance_Model_Iterator (Iterator, Model_Length (M_Len));
         end;
      end loop;

      if not Found_Meter then
         New_Line;
         Put_Line ("No meter model (201-204) found.");
      end if;

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
end KSEM_Reader;
