--  Viessmann_Reader - Example for reading Viessmann heat pumps via Modbus TCP
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Tested with: Vitocal 200-S, Vitocal 250-A (adjust registers as needed)
--
--  Viessmann Configuration:
--    Port: 502 (standard Modbus TCP)
--    Unit ID: 1 (default, configurable in heat pump settings)
--    Enable: Modbus TCP must be enabled in heat pump service menu
--
--  Note: Register addresses vary by model and firmware version!
--  Check your Viessmann Modbus documentation for exact addresses.
--  Values are typically scaled by 10 (e.g., 215 = 21.5 degrees)
--
--  Usage: viessmann_reader <ip-address> [port] [unit-id]
--  Example: viessmann_reader 192.168.1.100
--           viessmann_reader 192.168.1.100 502 1

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Calendar;
with Ada.Exceptions;
with Interfaces; use Interfaces;

with Ada_Modbus;
with Ada_Modbus.Master;
with Ada_Modbus.Transport.TCP;

procedure Viessmann_Reader is

   use Ada_Modbus;
   use Ada_Modbus.Transport.TCP;

   --  Viessmann default settings
   Default_Port    : constant := 502;
   Default_Unit_Id : constant := 1;

   --  Common Viessmann register addresses (Vitocal series)
   --  Note: These may vary by model - check your documentation!
   --
   --  Temperature registers (value / 10 = degrees Celsius)
   Reg_Outside_Temp       : constant Register_Address := 16#0000#;  --  Outside temperature
   Reg_Flow_Temp          : constant Register_Address := 16#0002#;  --  Flow/supply temperature
   Reg_Return_Temp        : constant Register_Address := 16#0003#;  --  Return temperature
   Reg_DHW_Temp           : constant Register_Address := 16#0004#;  --  DHW (hot water) temperature
   Reg_DHW_Setpoint       : constant Register_Address := 16#0005#;  --  DHW setpoint
   Reg_Room_Temp          : constant Register_Address := 16#0006#;  --  Room temperature (if sensor)
   Reg_Room_Setpoint      : constant Register_Address := 16#0007#;  --  Room setpoint

   --  Compressor/system registers
   Reg_Compressor_Status  : constant Register_Address := 16#0100#;  --  Compressor on/off
   Reg_Operating_Mode     : constant Register_Address := 16#0101#;  --  Operating mode
   Reg_Heat_Pump_Status   : constant Register_Address := 16#0102#;  --  Heat pump status
   Reg_Error_Code         : constant Register_Address := 16#0103#;  --  Current error code

   --  Energy/power registers
   Reg_Power_Consumption  : constant Register_Address := 16#0200#;  --  Current power (W)
   Reg_COP                : constant Register_Address := 16#0201#;  --  Coefficient of Performance * 10
   Reg_Heat_Output        : constant Register_Address := 16#0202#;  --  Heat output (W)

   --  Energy counters (kWh)
   Reg_Energy_Heating     : constant Register_Address := 16#0300#;  --  Total heating energy
   Reg_Energy_DHW         : constant Register_Address := 16#0302#;  --  Total DHW energy
   Reg_Energy_Consumed    : constant Register_Address := 16#0304#;  --  Total electricity consumed

   --  Connection
   type Connection_Access is access all TCP_Connection;
   Connection : aliased TCP_Connection;
   Conn_Ptr   : constant Connection_Access := Connection'Access;

   --  Transport callbacks
   function Send_Data
     (Ctx  : in Out Connection_Access;
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

   --  Convert signed 16-bit register to temperature (scaled by 10)
   function To_Temperature (Reg : Register_Value) return Float is
      Raw : Integer;
   begin
      if Reg > 32767 then
         Raw := Integer (Reg) - 65536;  --  Two's complement for negative temps
      else
         Raw := Integer (Reg);
      end if;
      return Float (Raw) / 10.0;
   end To_Temperature;

   --  Convert unsigned register to value scaled by 10
   function To_Scaled (Reg : Register_Value) return Float is
   begin
      return Float (Reg) / 10.0;
   end To_Scaled;

   --  Check if value indicates "not available" (0x7FFF or 0x8000)
   function Is_Not_Available (Reg : Register_Value) return Boolean is
   begin
      return Reg = 16#7FFF# or Reg = 16#8000# or Reg = 16#FFFF#;
   end Is_Not_Available;

   --  Read single register with error handling
   function Read_Register
     (Slave   : Unit_Id;
      Address : Register_Address;
      Value   : out Register_Value) return Boolean
   is
      Regs   : Register_Array (0 .. 0);
      Result : Status;
   begin
      Result := Modbus_Master.Read_Holding_Registers
        (Ctx, Slave, Address, 1, Regs);
      if Result = Success then
         Value := Regs (0);
         return True;
      else
         Value := 16#FFFF#;
         return False;
      end if;
   end Read_Register;

   --  Read 32-bit register pair (for energy counters)
   function Read_Register_32
     (Slave   : Unit_Id;
      Address : Register_Address;
      Value   : out Unsigned_32) return Boolean
   is
      Regs   : Register_Array (0 .. 1);
      Result : Status;
   begin
      Result := Modbus_Master.Read_Holding_Registers
        (Ctx, Slave, Address, 2, Regs);
      if Result = Success then
         --  Viessmann typically uses Big-Endian (ABCD)
         Value := Unsigned_32 (Regs (0)) * 65536 + Unsigned_32 (Regs (1));
         return True;
      else
         Value := 0;
         return False;
      end if;
   end Read_Register_32;

   --  Print temperature with unit
   procedure Print_Temperature
     (Label   : String;
      Slave   : Unit_Id;
      Address : Register_Address)
   is
      Reg : Register_Value;
   begin
      if Read_Register (Slave, Address, Reg) then
         if Is_Not_Available (Reg) then
            Put_Line ("  " & Label & "N/A");
         else
            Put_Line ("  " & Label & Float'Image (To_Temperature (Reg)) & " C");
         end if;
      else
         Put_Line ("  " & Label & "(read error)");
      end if;
   end Print_Temperature;

   --  Decode operating mode
   function Operating_Mode_String (Mode : Register_Value) return String is
   begin
      case Mode is
         when 0 => return "Standby";
         when 1 => return "DHW only";
         when 2 => return "Heating + DHW";
         when 3 => return "Heating only";
         when 4 => return "Cooling";
         when 5 => return "Cooling + DHW";
         when others => return "Unknown (" & Mode'Image & ")";
      end case;
   end Operating_Mode_String;

   --  Decode compressor status
   function Compressor_Status_String (Status : Register_Value) return String is
   begin
      case Status is
         when 0 => return "Off";
         when 1 => return "Running";
         when 2 => return "Starting";
         when 3 => return "Defrost";
         when others => return "Unknown (" & Status'Image & ")";
      end case;
   end Compressor_Status_String;

   --  Decode heat pump status
   function HP_Status_String (Status : Register_Value) return String is
   begin
      case Status is
         when 0 => return "Idle";
         when 1 => return "Heating";
         when 2 => return "Cooling";
         when 3 => return "DHW heating";
         when 4 => return "Defrosting";
         when 5 => return "Error";
         when others => return "Unknown (" & Status'Image & ")";
      end case;
   end HP_Status_String;

   --  Main variables
   Host     : String (1 .. 64) := [others => ' '];
   Host_Len : Natural := 0;
   Port     : Natural := Default_Port;
   Unit     : Unit_Id := Default_Unit_Id;
   Result   : Status;
   Reg      : Register_Value;
   Reg32    : Unsigned_32;

begin
   Put_Line ("=== Viessmann Heat Pump Reader ===");
   New_Line;

   --  Parse command line
   if Ada.Command_Line.Argument_Count < 1 then
      Put_Line ("Usage: viessmann_reader <ip-address> [port] [unit-id]");
      Put_Line ("  Default port: 502");
      Put_Line ("  Default unit: 1");
      New_Line;
      Put_Line ("Note: Register addresses may vary by model!");
      Put_Line ("Check your Viessmann Modbus documentation.");
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

   --  Read temperatures
   Put_Line ("--- Temperatures ---");
   Print_Temperature ("Outside:      ", Unit, Reg_Outside_Temp);
   Print_Temperature ("Flow:         ", Unit, Reg_Flow_Temp);
   Print_Temperature ("Return:       ", Unit, Reg_Return_Temp);
   Print_Temperature ("DHW:          ", Unit, Reg_DHW_Temp);
   Print_Temperature ("DHW Setpoint: ", Unit, Reg_DHW_Setpoint);
   Print_Temperature ("Room:         ", Unit, Reg_Room_Temp);
   Print_Temperature ("Room Setpoint:", Unit, Reg_Room_Setpoint);

   --  Read status
   New_Line;
   Put_Line ("--- Status ---");

   if Read_Register (Unit, Reg_Operating_Mode, Reg) then
      Put_Line ("  Operating Mode: " & Operating_Mode_String (Reg));
   else
      Put_Line ("  Operating Mode: (read error)");
   end if;

   if Read_Register (Unit, Reg_Compressor_Status, Reg) then
      Put_Line ("  Compressor:     " & Compressor_Status_String (Reg));
   else
      Put_Line ("  Compressor:     (read error)");
   end if;

   if Read_Register (Unit, Reg_Heat_Pump_Status, Reg) then
      Put_Line ("  HP Status:      " & HP_Status_String (Reg));
   else
      Put_Line ("  HP Status:      (read error)");
   end if;

   if Read_Register (Unit, Reg_Error_Code, Reg) then
      if Reg = 0 then
         Put_Line ("  Error Code:     None");
      else
         Put_Line ("  Error Code:     " & Reg'Image);
      end if;
   else
      Put_Line ("  Error Code:     (read error)");
   end if;

   --  Read power/performance
   New_Line;
   Put_Line ("--- Power & Performance ---");

   if Read_Register (Unit, Reg_Power_Consumption, Reg) then
      if not Is_Not_Available (Reg) then
         Put_Line ("  Power In:       " & Reg'Image & " W");
      else
         Put_Line ("  Power In:       N/A");
      end if;
   else
      Put_Line ("  Power In:       (read error)");
   end if;

   if Read_Register (Unit, Reg_Heat_Output, Reg) then
      if not Is_Not_Available (Reg) then
         Put_Line ("  Heat Output:    " & Reg'Image & " W");
      else
         Put_Line ("  Heat Output:    N/A");
      end if;
   else
      Put_Line ("  Heat Output:    (read error)");
   end if;

   if Read_Register (Unit, Reg_COP, Reg) then
      if not Is_Not_Available (Reg) and Reg > 0 then
         Put_Line ("  COP:            " & Float'Image (To_Scaled (Reg)));
      else
         Put_Line ("  COP:            N/A");
      end if;
   else
      Put_Line ("  COP:            (read error)");
   end if;

   --  Read energy counters
   New_Line;
   Put_Line ("--- Energy Counters ---");

   if Read_Register_32 (Unit, Reg_Energy_Heating, Reg32) then
      Put_Line ("  Heating Total:  " & Float'Image (Float (Reg32) / 10.0) & " kWh");
   else
      Put_Line ("  Heating Total:  (read error)");
   end if;

   if Read_Register_32 (Unit, Reg_Energy_DHW, Reg32) then
      Put_Line ("  DHW Total:      " & Float'Image (Float (Reg32) / 10.0) & " kWh");
   else
      Put_Line ("  DHW Total:      (read error)");
   end if;

   if Read_Register_32 (Unit, Reg_Energy_Consumed, Reg32) then
      Put_Line ("  Elec. Consumed: " & Float'Image (Float (Reg32) / 10.0) & " kWh");
   else
      Put_Line ("  Elec. Consumed: (read error)");
   end if;

   New_Line;
   Put_Line ("Done.");
   Disconnect (Connection);

exception
   when E : others =>
      Put_Line ("Error: " & Ada.Exceptions.Exception_Message (E));
      Disconnect (Connection);
end Viessmann_Reader;
