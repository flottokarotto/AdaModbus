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
with Ada.Command_Line;
with Ada.Calendar;
with Ada.Exceptions;
with Interfaces; use Interfaces;

with Ada_Modbus;
with Ada_Modbus.Master;
with Ada_Modbus.Transport.TCP;

procedure KSEM_Reader is

   use Ada_Modbus;
   use Ada_Modbus.Transport.TCP;

   --  KSEM default settings
   Default_Port    : constant := 502;
   Default_Unit_Id : constant := 1;

   --  SunSpec base address
   SunSpec_Base : constant Register_Address := 40000;

   --  Connection
   type Connection_Access is access all TCP_Connection;
   Connection : aliased TCP_Connection;
   Conn_Ptr   : constant Connection_Access := Connection'Access;

   --  Transport callbacks
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

   --  Scale factor type for SunSpec
   type Scale_Factor is range -10 .. 10;

   --  Convert register to signed scale factor
   function To_Scale_Factor (Reg : Register_Value) return Scale_Factor is
      Raw : Integer;
   begin
      if Reg > 32767 then
         Raw := Integer (Reg) - 65536;
      else
         Raw := Integer (Reg);
      end if;
      if Raw in -10 .. 10 then
         return Scale_Factor (Raw);
      else
         return 0;
      end if;
   end To_Scale_Factor;

   --  Apply scale factor to value
   function Apply_Scale (Value : Integer; SF : Scale_Factor) return Float is
      Mult : constant Float := 10.0 ** Integer (SF);
   begin
      return Float (Value) * Mult;
   end Apply_Scale;

   --  Decode string from registers (2 chars per register)
   function Decode_String (Regs : Register_Array) return String is
      Result : String (1 .. Regs'Length * 2);
      Idx    : Natural := 1;
      Hi, Lo : Character;
   begin
      for R of Regs loop
         Hi := Character'Val (Natural (R / 256) mod 256);
         Lo := Character'Val (Natural (R mod 256));
         if Hi /= ASCII.NUL then
            Result (Idx) := Hi;
            Idx := Idx + 1;
         end if;
         if Lo /= ASCII.NUL then
            Result (Idx) := Lo;
            Idx := Idx + 1;
         end if;
      end loop;
      return Result (1 .. Idx - 1);
   end Decode_String;

   --  Convert signed 16-bit register to integer
   function To_Signed (Reg : Register_Value) return Integer is
   begin
      if Reg > 32767 then
         return Integer (Reg) - 65536;
      else
         return Integer (Reg);
      end if;
   end To_Signed;

   --  Check if value is "not implemented" (0x8000 or 0x7FFF)
   function Is_Not_Implemented (Reg : Register_Value) return Boolean is
   begin
      return Reg = 16#8000# or Reg = 16#7FFF# or Reg = 16#FFFF#;
   end Is_Not_Implemented;

   --  Format value with scale, or "N/A" if not implemented
   function Format_Value (Reg : Register_Value; SF : Scale_Factor; Unit : String) return String is
   begin
      if Is_Not_Implemented (Reg) then
         return "N/A";
      else
         return Float'Image (Apply_Scale (To_Signed (Reg), SF)) & " " & Unit;
      end if;
   end Format_Value;

   --  Main variables
   Host     : String (1 .. 64) := [others => ' '];
   Host_Len : Natural := 0;
   Port     : Natural := Default_Port;
   Unit     : Unit_Id := Default_Unit_Id;
   Result   : Status;

begin
   Put_Line ("=== Kostal Smart Energy Meter (KSEM) Reader ===");
   New_Line;

   --  Parse command line
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

   --  Connect
   Connect (Connection, Host (1 .. Host_Len), Port, 5.0, Result);
   if Result /= Success then
      Put_Line ("Connection failed: " & Result'Image);
      return;
   end if;
   Put_Line ("Connected.");
   New_Line;

   --  Initialize master
   Modbus_Master.Initialize
     (Ctx,
      (Mode => Modbus_Master.TCP, Default_Slave => Unit, Default_Timeout => 3000),
      Conn_Ptr);

   --  Check for SunSpec marker
   declare
      Regs : Register_Array (0 .. 1);
   begin
      Result := Modbus_Master.Read_Holding_Registers
        (Ctx, Unit, SunSpec_Base, 2, Regs);
      if Result /= Success then
         Put_Line ("Failed to read SunSpec base: " & Result'Image);
         Disconnect (Connection);
         return;
      end if;

      if Regs (0) /= 16#5375# or Regs (1) /= 16#6E53# then
         Put_Line ("SunSpec marker not found!");
         Put_Line ("Got: " & Regs (0)'Image & ", " & Regs (1)'Image);
         Disconnect (Connection);
         return;
      end if;
      Put_Line ("SunSpec protocol detected.");
   end;

   --  Read Common Block (Model 1) at 40002
   Put_Line ("--- Device Information ---");
   declare
      Common_Base : constant Register_Address := SunSpec_Base + 2;
      Regs : Register_Array (0 .. 65);
      Model_Id : Register_Value;
      Block_Len : Register_Value;
   begin
      --  Read model header
      Result := Modbus_Master.Read_Holding_Registers
        (Ctx, Unit, Common_Base, 2, Regs (0 .. 1));
      if Result /= Success then
         Put_Line ("Failed to read Common block header: " & Result'Image);
      else
         Model_Id := Regs (0);
         Block_Len := Regs (1);
         Put_Line ("  Model: " & Model_Id'Image & " (Common)");
         Put_Line ("  Block Length: " & Block_Len'Image);

         --  Read Common block data (manufacturer, model, etc.)
         if Block_Len <= 64 then
            Result := Modbus_Master.Read_Holding_Registers
              (Ctx, Unit, Common_Base + 2, Register_Count (Block_Len), Regs (0 .. Natural (Block_Len) - 1));
            if Result = Success then
               --  Manufacturer: offset 0, length 16
               Put_Line ("  Manufacturer: " & Decode_String (Regs (0 .. 15)));
               --  Model: offset 16, length 16
               Put_Line ("  Model: " & Decode_String (Regs (16 .. 31)));
               --  Serial: offset 48, length 16
               Put_Line ("  Serial: " & Decode_String (Regs (48 .. 63)));
            end if;
         end if;
      end if;
   end;

   --  Find and read Meter model (201-204)
   New_Line;
   Put_Line ("--- Searching for Meter Model ---");
   declare
      Current_Addr : Register_Address := SunSpec_Base + 2;  --  Start after "SunS"
      Regs : Register_Array (0 .. 124);
      Model_Id : Register_Value;
      Block_Len : Register_Value;
      Found_Meter : Boolean := False;
      Max_Models : constant := 20;
   begin
      for Model_Count in 1 .. Max_Models loop
         Result := Modbus_Master.Read_Holding_Registers
           (Ctx, Unit, Current_Addr, 2, Regs (0 .. 1));
         if Result /= Success then
            Put_Line ("Read error at " & Current_Addr'Image);
            exit;
         end if;

         Model_Id := Regs (0);
         Block_Len := Regs (1);

         --  End marker
         if Model_Id = 16#FFFF# then
            Put_Line ("  End of SunSpec models.");
            exit;
         end if;

         Put_Line ("  Found Model " & Model_Id'Image &
                   " at " & Current_Addr'Image &
                   " (len " & Block_Len'Image & ")");

         --  Meter models: 201 (single-phase), 202 (split-phase),
         --                203 (wye 3-phase), 204 (delta 3-phase)
         if Model_Id in 201 .. 204 then
            Found_Meter := True;
            Put_Line ("  -> Meter model found!");

            --  Read meter data
            if Block_Len <= 124 then
               Result := Modbus_Master.Read_Holding_Registers
                 (Ctx, Unit, Current_Addr + 2, Register_Count (Block_Len),
                  Regs (0 .. Natural (Block_Len) - 1));

               if Result = Success then
                  New_Line;
                  Put_Line ("--- Meter Readings (Model " & Model_Id'Image & ") ---");

                  --  SunSpec Meter Model 203 layout:
                  --  Offset 0: A (Total Current) - int16, SF at offset 2
                  --  Offset 1: AphA (Phase A Current)
                  --  Offset 2: AphB
                  --  Offset 3: AphC
                  --  Offset 4: A_SF (Current scale factor)
                  --  Offset 5: PhV (Voltage L-N avg)
                  --  Offset 6: PhVphA
                  --  Offset 7: PhVphB
                  --  Offset 8: PhVphC
                  --  Offset 9: PPV (Voltage L-L avg)
                  --  ...
                  --  Offset 14: V_SF
                  --  Offset 15: Hz
                  --  Offset 16: Hz_SF
                  --  Offset 17: W (Total Active Power)
                  --  Offset 18: WphA
                  --  Offset 19: WphB
                  --  Offset 20: WphC
                  --  Offset 21: W_SF
                  --  Offset 22: VA (Total Apparent Power)
                  --  ...
                  --  Offset 26: VA_SF
                  --  Offset 27: VAR (Total Reactive Power)
                  --  ...
                  --  Offset 31: VAR_SF
                  --  Offset 32: PF
                  --  ...
                  --  Offset 36: PF_SF
                  --  Offset 37: TotWhExp (Total Wh Exported) - uint32
                  --  Offset 39: TotWhImp (Total Wh Imported) - uint32
                  --  ...
                  --  Offset 53: TotWh_SF

                  --  SunSpec Model 203 offsets (corrected):
                  --    0: A (Total Current), 1-3: AphA/B/C, 4: A_SF
                  --    5: PhV (L-N avg), 6-8: PhVphA/B/C
                  --    9: PPV (L-L avg), 10-12: PPVphAB/BC/CA
                  --   13: V_SF
                  --   14: Hz, 15: Hz_SF
                  --   16: W (Total), 17-19: WphA/B/C, 20: W_SF
                  --   21: VA (Total), 22-24: VAphA/B/C, 25: VA_SF
                  --   26: VAR (Total), 27-29: VARphA/B/C, 30: VAR_SF
                  --   31: PF (Total), 32-34: PFphA/B/C, 35: PF_SF
                  --   36-37: TotWhExp (uint32), 38-39: TotWhExpPhA, ...
                  --   52-53: TotWhImp (uint32), 54-55: TotWhImpPhA, ...
                  --   68: TotWh_SF

                  declare
                     A_SF   : constant Scale_Factor := To_Scale_Factor (Regs (4));
                     V_SF   : constant Scale_Factor := To_Scale_Factor (Regs (13));
                     Hz_SF  : constant Scale_Factor := To_Scale_Factor (Regs (15));
                     W_SF   : constant Scale_Factor := To_Scale_Factor (Regs (20));
                     VA_SF  : constant Scale_Factor := To_Scale_Factor (Regs (25));
                     VAR_SF : constant Scale_Factor := To_Scale_Factor (Regs (30));
                     PF_SF  : constant Scale_Factor := To_Scale_Factor (Regs (35));
                  begin

                     Put_Line ("--- Power ---");
                     Put_Line ("  Total:  " & Format_Value (Regs (16), W_SF, "W"));
                     if Model_Id >= 203 then
                        Put_Line ("  L1:     " & Format_Value (Regs (17), W_SF, "W"));
                        Put_Line ("  L2:     " & Format_Value (Regs (18), W_SF, "W"));
                        Put_Line ("  L3:     " & Format_Value (Regs (19), W_SF, "W"));
                     end if;

                     New_Line;
                     Put_Line ("--- Voltage (L-N) ---");
                     Put_Line ("  Avg:    " & Format_Value (Regs (5), V_SF, "V"));
                     if Model_Id >= 203 then
                        Put_Line ("  L1:     " & Format_Value (Regs (6), V_SF, "V"));
                        Put_Line ("  L2:     " & Format_Value (Regs (7), V_SF, "V"));
                        Put_Line ("  L3:     " & Format_Value (Regs (8), V_SF, "V"));
                     end if;

                     New_Line;
                     Put_Line ("--- Current ---");
                     Put_Line ("  Total:  " & Format_Value (Regs (0), A_SF, "A"));
                     if Model_Id >= 203 then
                        Put_Line ("  L1:     " & Format_Value (Regs (1), A_SF, "A"));
                        Put_Line ("  L2:     " & Format_Value (Regs (2), A_SF, "A"));
                        Put_Line ("  L3:     " & Format_Value (Regs (3), A_SF, "A"));
                     end if;

                     New_Line;
                     Put_Line ("--- Frequency ---");
                     Put_Line ("  " & Format_Value (Regs (14), Hz_SF, "Hz"));

                     New_Line;
                     Put_Line ("--- Apparent Power ---");
                     Put_Line ("  Total:  " & Format_Value (Regs (21), VA_SF, "VA"));

                     New_Line;
                     Put_Line ("--- Reactive Power ---");
                     Put_Line ("  Total:  " & Format_Value (Regs (26), VAR_SF, "var"));

                     New_Line;
                     Put_Line ("--- Power Factor ---");
                     Put_Line ("  " & Format_Value (Regs (31), PF_SF, ""));

                     --  Note: Energy counters in SunSpec Model 203 may not be
                     --  implemented correctly by all devices. The KSEM may store
                     --  energy data in different registers. Check device docs.
                  end;
               end if;
            end if;
            exit;  --  Found meter, done
         end if;

         --  Move to next model
         Current_Addr := Current_Addr + 2 + Register_Address (Block_Len);
      end loop;

      if not Found_Meter then
         Put_Line ("No meter model (201-204) found.");
      end if;
   end;

   New_Line;
   Put_Line ("Done.");
   Disconnect (Connection);

exception
   when E : others =>
      Put_Line ("Error: " & Ada.Exceptions.Exception_Message (E));
      Disconnect (Connection);
end KSEM_Reader;
