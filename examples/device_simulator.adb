--  Device_Simulator - Simulated Modbus Device
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Simulates a realistic Modbus device with:
--  - Counters that increment over time
--  - Simulated analog inputs (sine wave, random)
--  - Configurable coil outputs
--  - Device identification
--
--  Usage: device_simulator [port] [unit_id]
--  Example: device_simulator 1502 1

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Numerics.Elementary_Functions;
with Ada.Calendar; use Ada.Calendar;
with Interfaces; use Interfaces;

with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.Slave; use Ada_Modbus.Slave;
with Ada_Modbus.Transport.TCP; use Ada_Modbus.Transport.TCP;
with Ada_Modbus.Protocol.TCP;

procedure Device_Simulator is

   --  Simulated device data
   --  Register map:
   --    0-9:   Holding registers (read/write)
   --    10-19: Counter registers (read, auto-increment)
   --    20-29: Analog input simulation
   --    30-39: Device info

   Holding_Registers : Register_Array (0 .. 99) := [others => 0];
   Input_Registers   : Register_Array (0 .. 99) := [others => 0];
   Coils             : Coil_Array (0 .. 127) := [others => False];
   Discrete_Inputs   : Coil_Array (0 .. 127) := [others => False];

   --  Device identification
   Device_Id         : constant Byte := 42;
   Device_Running    : constant Boolean := True;

   --  Simulation state
   Start_Time        : Time;
   Request_Count     : Natural := 0;
   Tick_Counter      : Unsigned_32 := 0;

   --  Diagnostics counters (for FC 08)
   Bus_Message_Count      : Unsigned_16 := 0;
   Bus_Comm_Error_Count   : Unsigned_16 := 0;
   Slave_Message_Count    : Unsigned_16 := 0;
   Exception_Error_Count  : Unsigned_16 := 0;

   --  Update simulation values
   procedure Update_Simulation is
      use Ada.Numerics.Elementary_Functions;
      Elapsed : Duration;
      Secs    : Natural;
   begin
      Elapsed := Clock - Start_Time;
      Secs := Natural (Elapsed);
      Tick_Counter := Tick_Counter + 1;

      --  Counter registers (10-14)
      Input_Registers (10) := Register_Value (Tick_Counter mod 65536);
      Input_Registers (11) := Register_Value ((Tick_Counter / 65536) mod 65536);
      Input_Registers (12) := Register_Value (Secs mod 65536);
      Input_Registers (13) := Register_Value (Request_Count mod 65536);
      Input_Registers (14) := Register_Value ((Secs / 60) mod 65536);  --  Minutes

      --  Analog simulation (20-24)
      --  Sine wave (0-1000 range)
      declare
         Angle : constant Float := Float (Tick_Counter) * 0.1;
         Sine_Val : constant Float := (Sin (Angle) + 1.0) * 500.0;
      begin
         Input_Registers (20) := Register_Value (Natural (Sine_Val));
      end;

      --  Cosine wave
      declare
         Angle : constant Float := Float (Tick_Counter) * 0.1;
         Cos_Val : constant Float := (Cos (Angle) + 1.0) * 500.0;
      begin
         Input_Registers (21) := Register_Value (Natural (Cos_Val));
      end;

      --  Triangle wave
      declare
         Phase : constant Natural := Natural (Tick_Counter) mod 200;
         Tri_Val : constant Natural := (if Phase < 100 then Phase * 10 else (200 - Phase) * 10);
      begin
         Input_Registers (22) := Register_Value (Tri_Val);
      end;

      --  Sawtooth wave
      Input_Registers (23) := Register_Value (Natural (Tick_Counter) mod 1000);

      --  Random-ish value (simple LCG)
      declare
         Rand : constant Unsigned_32 := Tick_Counter * 1103515245 + 12345;
      begin
         Input_Registers (24) := Register_Value ((Rand / 65536) mod 1000);
      end;

      --  Device info (30-34)
      Input_Registers (30) := Register_Value (Device_Id);
      Input_Registers (31) := 16#0100#;  --  Version 1.0
      Input_Registers (32) := Register_Value (Natural (Bus_Message_Count));
      Input_Registers (33) := Register_Value (Natural (Exception_Error_Count));
      Input_Registers (34) := Register_Value (Secs mod 65536);  --  Uptime

      --  Update discrete inputs based on analog values
      Discrete_Inputs (0) := Input_Registers (20) > 500;  --  Sine above midpoint
      Discrete_Inputs (1) := Input_Registers (21) > 500;  --  Cos above midpoint
      Discrete_Inputs (2) := Input_Registers (22) > 500;  --  Triangle above midpoint
      Discrete_Inputs (3) := Input_Registers (23) > 500;  --  Sawtooth above midpoint

      --  Alarm simulation: high value alarm
      Discrete_Inputs (8) := Input_Registers (20) > 900;
      Discrete_Inputs (9) := Input_Registers (20) < 100;
   end Update_Simulation;

   --  Slave callbacks

   function Read_Holding_Registers_CB
     (Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array) return Status
   is
      Start : constant Natural := Natural (Start_Address);
   begin
      if Start + Natural (Quantity) - 1 > Holding_Registers'Last then
         Exception_Error_Count := Exception_Error_Count + 1;
         return Exception_Illegal_Address;
      end if;

      for I in 0 .. Natural (Quantity) - 1 loop
         Values (Values'First + I) := Holding_Registers (Start + I);
      end loop;
      return Success;
   end Read_Holding_Registers_CB;

   function Read_Input_Registers_CB
     (Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array) return Status
   is
      Start : constant Natural := Natural (Start_Address);
   begin
      Update_Simulation;

      if Start + Natural (Quantity) - 1 > Input_Registers'Last then
         Exception_Error_Count := Exception_Error_Count + 1;
         return Exception_Illegal_Address;
      end if;

      for I in 0 .. Natural (Quantity) - 1 loop
         Values (Values'First + I) := Input_Registers (Start + I);
      end loop;
      return Success;
   end Read_Input_Registers_CB;

   function Write_Single_Register_CB
     (Address : Register_Address;
      Value   : Register_Value) return Status
   is
      Addr : constant Natural := Natural (Address);
   begin
      if Addr > Holding_Registers'Last then
         Exception_Error_Count := Exception_Error_Count + 1;
         return Exception_Illegal_Address;
      end if;

      Holding_Registers (Addr) := Value;
      return Success;
   end Write_Single_Register_CB;

   function Write_Multiple_Registers_CB
     (Start_Address : Register_Address;
      Values        : Register_Array) return Status
   is
      Start : constant Natural := Natural (Start_Address);
   begin
      if Start + Values'Length - 1 > Holding_Registers'Last then
         Exception_Error_Count := Exception_Error_Count + 1;
         return Exception_Illegal_Address;
      end if;

      for I in Values'Range loop
         Holding_Registers (Start + I - Values'First) := Values (I);
      end loop;
      return Success;
   end Write_Multiple_Registers_CB;

   function Read_Coils_CB
     (Start_Address : Coil_Address;
      Quantity      : Coil_Count;
      Values        : out Coil_Array) return Status
   is
      Start : constant Natural := Natural (Start_Address);
   begin
      if Start + Natural (Quantity) - 1 > Coils'Last then
         Exception_Error_Count := Exception_Error_Count + 1;
         return Exception_Illegal_Address;
      end if;

      for I in 0 .. Natural (Quantity) - 1 loop
         Values (Values'First + I) := Coils (Start + I);
      end loop;
      return Success;
   end Read_Coils_CB;

   function Read_Discrete_Inputs_CB
     (Start_Address : Coil_Address;
      Quantity      : Coil_Count;
      Values        : out Coil_Array) return Status
   is
      Start : constant Natural := Natural (Start_Address);
   begin
      Update_Simulation;

      if Start + Natural (Quantity) - 1 > Discrete_Inputs'Last then
         Exception_Error_Count := Exception_Error_Count + 1;
         return Exception_Illegal_Address;
      end if;

      for I in 0 .. Natural (Quantity) - 1 loop
         Values (Values'First + I) := Discrete_Inputs (Start + I);
      end loop;
      return Success;
   end Read_Discrete_Inputs_CB;

   function Write_Single_Coil_CB
     (Address : Coil_Address;
      Value   : Coil_Value) return Status
   is
      Addr : constant Natural := Natural (Address);
   begin
      if Addr > Coils'Last then
         Exception_Error_Count := Exception_Error_Count + 1;
         return Exception_Illegal_Address;
      end if;

      Coils (Addr) := Value;
      return Success;
   end Write_Single_Coil_CB;

   function Write_Multiple_Coils_CB
     (Start_Address : Coil_Address;
      Values        : Coil_Array) return Status
   is
      Start : constant Natural := Natural (Start_Address);
   begin
      if Start + Values'Length - 1 > Coils'Last then
         Exception_Error_Count := Exception_Error_Count + 1;
         return Exception_Illegal_Address;
      end if;

      for I in Values'Range loop
         Coils (Start + I - Values'First) := Values (I);
      end loop;
      return Success;
   end Write_Multiple_Coils_CB;

   function Read_Exception_Status_CB
     (Exception_Status : out Byte) return Status
   is
   begin
      --  Return a status byte based on simulated conditions
      Exception_Status := 0;
      if Discrete_Inputs (8) then  --  High alarm
         Exception_Status := Exception_Status or 16#01#;
      end if;
      if Discrete_Inputs (9) then  --  Low alarm
         Exception_Status := Exception_Status or 16#02#;
      end if;
      if not Device_Running then
         Exception_Status := Exception_Status or 16#80#;
      end if;
      return Success;
   end Read_Exception_Status_CB;

   function Diagnostics_CB
     (Sub_Function : Unsigned_16;
      Data_In      : Unsigned_16;
      Data_Out     : out Unsigned_16) return Status
   is
   begin
      case Sub_Function is
         when 16#0000# =>  --  Return Query Data (echo)
            Data_Out := Data_In;
         when 16#000A# =>  --  Clear Counters
            Bus_Message_Count := 0;
            Bus_Comm_Error_Count := 0;
            Slave_Message_Count := 0;
            Data_Out := 0;
         when 16#000B# =>  --  Return Bus Message Count
            Data_Out := Bus_Message_Count;
         when 16#000C# =>  --  Return Bus Communication Error Count
            Data_Out := Bus_Comm_Error_Count;
         when 16#000E# =>  --  Return Slave Message Count
            Data_Out := Slave_Message_Count;
         when others =>
            return Exception_Illegal_Function;
      end case;
      return Success;
   end Diagnostics_CB;

   function Report_Server_Id_CB
     (Server_Id     : out Byte;
      Run_Indicator : out Boolean;
      Add_Data      : out Byte_Array;
      Add_Data_Len  : out Natural) return Status
   is
      --  Additional data: "SIMDEV"
      Id_String : constant String := "SIMDEV";
   begin
      Server_Id := Device_Id;
      Run_Indicator := Device_Running;

      Add_Data_Len := Id_String'Length;
      for I in Id_String'Range loop
         Add_Data (I - Id_String'First) := Byte (Character'Pos (Id_String (I)));
      end loop;

      return Success;
   end Report_Server_Id_CB;

   function Mask_Write_Register_CB
     (Address  : Register_Address;
      And_Mask : Register_Value;
      Or_Mask  : Register_Value) return Status
   is
      Addr : constant Natural := Natural (Address);
      Current : Register_Value;
   begin
      if Addr > Holding_Registers'Last then
         Exception_Error_Count := Exception_Error_Count + 1;
         return Exception_Illegal_Address;
      end if;

      Current := Holding_Registers (Addr);
      Holding_Registers (Addr) := (Current and And_Mask) or
                                   (Or_Mask and (not And_Mask));
      return Success;
   end Mask_Write_Register_CB;

   function Read_Write_Registers_CB
     (Read_Start    : Register_Address;
      Read_Quantity : Register_Count;
      Read_Values   : out Register_Array;
      Write_Start   : Register_Address;
      Write_Values  : Register_Array) return Status
   is
      R_Start : constant Natural := Natural (Read_Start);
      Res     : Status;
   begin
      --  Write first
      Res := Write_Multiple_Registers_CB (Write_Start, Write_Values);
      if Res /= Success then
         return Res;
      end if;

      --  Then read
      if R_Start + Natural (Read_Quantity) - 1 > Holding_Registers'Last then
         return Exception_Illegal_Address;
      end if;

      for I in 0 .. Natural (Read_Quantity) - 1 loop
         Read_Values (Read_Values'First + I) := Holding_Registers (R_Start + I);
      end loop;

      return Success;
   end Read_Write_Registers_CB;

   --  Slave configuration
   My_Unit_Id : Unit_Id := 1;

   Config : Slave_Config :=
     (Mode      => TCP,
      Unit_Id   => 1,
      Callbacks =>
        (Read_Holding_Registers   => Read_Holding_Registers_CB'Unrestricted_Access,
         Read_Input_Registers     => Read_Input_Registers_CB'Unrestricted_Access,
         Write_Single_Register    => Write_Single_Register_CB'Unrestricted_Access,
         Write_Multiple_Registers => Write_Multiple_Registers_CB'Unrestricted_Access,
         Read_Coils               => Read_Coils_CB'Unrestricted_Access,
         Read_Discrete_Inputs     => Read_Discrete_Inputs_CB'Unrestricted_Access,
         Write_Single_Coil        => Write_Single_Coil_CB'Unrestricted_Access,
         Write_Multiple_Coils     => Write_Multiple_Coils_CB'Unrestricted_Access,
         Read_Exception_Status    => Read_Exception_Status_CB'Unrestricted_Access,
         Diagnostics              => Diagnostics_CB'Unrestricted_Access,
         Report_Server_Id         => Report_Server_Id_CB'Unrestricted_Access,
         Mask_Write_Register      => Mask_Write_Register_CB'Unrestricted_Access,
         Read_Write_Registers     => Read_Write_Registers_CB'Unrestricted_Access));

   Server    : aliased TCP_Connection;
   Client    : aliased TCP_Connection;
   Port      : Natural := 1502;
   Result    : Status;
   Has_Client : Boolean := False;

   Request_Buffer  : Byte_Array (0 .. Protocol.TCP.Max_ADU_Size - 1) := [others => 0];
   Response_Buffer : Byte_Array (0 .. Protocol.TCP.Max_ADU_Size - 1) := [others => 0];
   Request_Length  : Natural;
   Response_Length : Natural;
   Send_Response   : Boolean;

begin
   Put_Line ("╔══════════════════════════════════════════════════════════════╗");
   Put_Line ("║               Modbus Device Simulator                        ║");
   Put_Line ("╚══════════════════════════════════════════════════════════════╝");
   New_Line;

   --  Parse arguments
   if Ada.Command_Line.Argument_Count >= 1 then
      Port := Natural'Value (Ada.Command_Line.Argument (1));
   end if;

   if Ada.Command_Line.Argument_Count >= 2 then
      My_Unit_Id := Unit_Id (Natural'Value (Ada.Command_Line.Argument (2)));
      Config.Unit_Id := My_Unit_Id;
   end if;

   Put_Line ("Port:    " & Port'Image);
   Put_Line ("Unit ID: " & My_Unit_Id'Image);
   New_Line;

   Put_Line ("Register Map:");
   Put_Line ("  Holding Registers 0-9:   Read/Write data");
   Put_Line ("  Input Registers 10-14:   Counters (tick, time, requests)");
   Put_Line ("  Input Registers 20-24:   Analog sim (sine, cos, tri, saw, rand)");
   Put_Line ("  Input Registers 30-34:   Device info");
   Put_Line ("  Coils 0-127:             Read/Write outputs");
   Put_Line ("  Discrete Inputs 0-9:     Alarm/status flags");
   New_Line;

   Start_Time := Clock;

   --  Start server
   Listen (Server, Port, Result);
   if Result /= Success then
      Put_Line ("ERROR: Failed to start server: " & Result'Image);
      return;
   end if;

   Put_Line ("Server listening on port " & Port'Image);
   Put_Line ("Press Ctrl+C to stop.");
   New_Line;

   --  Main loop
   loop
      Update_Simulation;

      if not Has_Client then
         Accept_Connection (Server, Client, Result);
         if Result = Success then
            Has_Client := True;
            Put_Line ("Client connected.");
         end if;
      end if;

      if Has_Client then
         Request_Length := Receive (Client, Request_Buffer,
                                    Protocol.TCP.Max_ADU_Size, 100);

         if Request_Length > 0 then
            Request_Count := Request_Count + 1;
            Bus_Message_Count := Bus_Message_Count + 1;
            Slave_Message_Count := Slave_Message_Count + 1;

            Process_Request (Config,
                             Request_Buffer, Request_Length,
                             Response_Buffer, Response_Length,
                             Send_Response);

            if Send_Response and Response_Length > 0 then
               declare
                  Sent : constant Natural :=
                    Send (Client, Response_Buffer (0 .. Response_Length - 1));
               begin
                  if Sent /= Response_Length then
                     Bus_Comm_Error_Count := Bus_Comm_Error_Count + 1;
                  end if;
               end;
            end if;
         end if;
      end if;
   end loop;

exception
   when E : others =>
      Put_Line ("Exception: " & Ada.Exceptions.Exception_Message (E));
      Put_Line ("Requests processed: " & Request_Count'Image);
      Disconnect (Client);
      Disconnect (Server);
end Device_Simulator;
