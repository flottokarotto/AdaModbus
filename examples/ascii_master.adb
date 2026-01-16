--  ASCII_Master - Modbus ASCII Master Demo
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Demonstrates Modbus ASCII mode over serial port.
--  Usage: ascii_master <port> [baud] [slave_id]
--  Example: ascii_master COM3 9600 1
--           ascii_master /dev/ttyUSB0 19200 1

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Calendar;
with Interfaces; use Interfaces;

with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.Transport.Serial; use Ada_Modbus.Transport.Serial;
with Ada_Modbus.Master;

procedure ASCII_Master is

   --  Use access type for limited Serial_Connection
   type Connection_Access is access all Serial_Connection;
   Connection : aliased Serial_Connection;
   Conn_Ptr   : constant Connection_Access := Connection'Access;

   function Send
     (Ctx  : in out Connection_Access;
      Data : Byte_Array) return Natural
   is
   begin
      return Ada_Modbus.Transport.Serial.Send (Ctx.all, Data);
   end Send;

   function Receive
     (Ctx        : in Out Connection_Access;
      Buffer     : out Byte_Array;
      Max_Length : Natural;
      Timeout_Ms : Natural) return Natural
   is
   begin
      return Ada_Modbus.Transport.Serial.Receive
        (Ctx.all, Buffer, Max_Length, Timeout_Ms);
   end Receive;

   function Get_Tick_Ms return Unsigned_32 is
      use Ada.Calendar;
      Now     : constant Time := Clock;
      Seconds : constant Duration := Now - Time_Of (1970, 1, 1, 0.0);
   begin
      return Unsigned_32 (Seconds * 1000.0) mod Unsigned_32'Last;
   end Get_Tick_Ms;

   --  Instantiate Master with access type
   package My_Master is new Ada_Modbus.Master
     (Transport_Context => Connection_Access,
      Send              => Send,
      Receive           => Receive,
      Get_Tick_Ms       => Get_Tick_Ms);

   Master_Ctx : My_Master.Master_Context;
   Config     : My_Master.Master_Config;
   Result     : Status;

   Port_Name  : String (1 .. 256) := [others => ' '];
   Port_Len   : Natural := 0;
   Baud       : Baud_Rate := B9600;
   Slave_Id   : Unit_Id := 1;

   Values     : Register_Array (0 .. 9);
   Coils      : Coil_Array (0 .. 7);

   procedure Print_Usage is
   begin
      Put_Line ("Usage: ascii_master <port> [baud] [slave_id]");
      Put_Line ("  port     - Serial port (COM3, /dev/ttyUSB0, etc.)");
      Put_Line ("  baud     - Baud rate: 9600, 19200, 38400, 57600, 115200");
      Put_Line ("  slave_id - Slave address (1-247, default: 1)");
      New_Line;
      Put_Line ("Example: ascii_master COM3 9600 1");
   end Print_Usage;

   function Parse_Baud (S : String) return Baud_Rate is
   begin
      case Natural'Value (S) is
         when 9600   => return B9600;
         when 19200  => return B19200;
         when 38400  => return B38400;
         when 57600  => return B57600;
         when 115200 => return B115200;
         when others => return B9600;
      end case;
   end Parse_Baud;

begin
   Put_Line ("=== Modbus ASCII Master Demo ===");
   New_Line;

   --  Parse command line
   if Ada.Command_Line.Argument_Count < 1 then
      Print_Usage;
      return;
   end if;

   declare
      Arg : constant String := Ada.Command_Line.Argument (1);
   begin
      Port_Len := Arg'Length;
      Port_Name (1 .. Port_Len) := Arg;
   end;

   if Ada.Command_Line.Argument_Count >= 2 then
      Baud := Parse_Baud (Ada.Command_Line.Argument (2));
   end if;

   if Ada.Command_Line.Argument_Count >= 3 then
      Slave_Id := Unit_Id (Natural'Value (Ada.Command_Line.Argument (3)));
   end if;

   Put_Line ("Port:     " & Port_Name (1 .. Port_Len));
   Put_Line ("Baud:     " & Baud'Image);
   Put_Line ("Slave ID: " & Slave_Id'Image);
   Put_Line ("Mode:     ASCII");
   New_Line;

   --  Open serial port
   Open (Connection, Port_Name (1 .. Port_Len),
         (Rate => Baud, Parity => None, Stop_Bits => One, Data_Bits => Eight),
         Result);
   if Result /= Success then
      Put_Line ("ERROR: Failed to open port: " & Result'Image);
      return;
   end if;

   Put_Line ("Port opened successfully.");
   New_Line;

   --  Initialize master in ASCII mode
   Config.Mode := My_Master.ASCII;
   Config.Default_Slave := Slave_Id;
   Config.Default_Timeout := 2000;  --  2 second timeout
   My_Master.Initialize (Master_Ctx, Config, Conn_Ptr);

   --  Test 1: Read Holding Registers
   Put_Line ("Test 1: Read Holding Registers (FC 03)");
   Put ("  Reading 5 registers from address 0... ");

   Result := My_Master.Read_Holding_Registers
     (Master_Ctx, Slave => Slave_Id, Start_Address => 0,
      Quantity => 5, Values => Values);

   if Result = Success then
      Put_Line ("OK");
      for I in 0 .. 4 loop
         Put_Line ("    Register " & I'Image & " = " & Values (I)'Image);
      end loop;
   else
      Put_Line ("FAILED: " & Result'Image);
   end if;
   New_Line;

   --  Test 2: Write Single Register
   Put_Line ("Test 2: Write Single Register (FC 06)");
   Put ("  Writing 0xABCD to register 10... ");

   Result := My_Master.Write_Single_Register
     (Master_Ctx, Slave => Slave_Id, Address => 10, Value => 16#ABCD#);

   if Result = Success then
      Put_Line ("OK");
   else
      Put_Line ("FAILED: " & Result'Image);
   end if;
   New_Line;

   --  Test 3: Read Coils
   Put_Line ("Test 3: Read Coils (FC 01)");
   Put ("  Reading 8 coils from address 0... ");

   Result := My_Master.Read_Coils
     (Master_Ctx, Slave => Slave_Id, Start_Address => 0,
      Quantity => 8, Values => Coils);

   if Result = Success then
      Put_Line ("OK");
      Put ("    Coils: ");
      for I in 0 .. 7 loop
         Put ((if Coils (I) then "1" else "0"));
      end loop;
      New_Line;
   else
      Put_Line ("FAILED: " & Result'Image);
   end if;
   New_Line;

   --  Test 4: Write Single Coil
   Put_Line ("Test 4: Write Single Coil (FC 05)");
   Put ("  Setting coil 5 to ON... ");

   Result := My_Master.Write_Single_Coil
     (Master_Ctx, Slave => Slave_Id, Address => 5, Value => True);

   if Result = Success then
      Put_Line ("OK");
   else
      Put_Line ("FAILED: " & Result'Image);
   end if;
   New_Line;

   --  Verify write
   Put_Line ("Test 5: Verify Written Values");
   Put ("  Re-reading register 10... ");

   Result := My_Master.Read_Holding_Registers
     (Master_Ctx, Slave => Slave_Id, Start_Address => 10,
      Quantity => 1, Values => Values);

   if Result = Success then
      Put_Line ("OK");
      Put_Line ("    Register 10 = " & Values (0)'Image &
                (if Values (0) = 16#ABCD# then " (verified)" else " (mismatch!)"));
   else
      Put_Line ("FAILED: " & Result'Image);
   end if;

   New_Line;
   Put_Line ("=== Tests Complete ===");

   Close (Connection);

exception
   when E : others =>
      Put_Line ("Exception: " & Ada.Exceptions.Exception_Message (E));
      Close (Connection);
end ASCII_Master;
