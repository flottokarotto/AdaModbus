--  ASCII_Slave - Modbus ASCII Slave Demo
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Demonstrates Modbus ASCII slave over serial port.
--  Usage: ascii_slave <port> [baud] [unit_id]
--  Example: ascii_slave COM3 9600 1

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Exceptions;

with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.Slave; use Ada_Modbus.Slave;
with Ada_Modbus.Transport.Serial; use Ada_Modbus.Transport.Serial;
with Ada_Modbus.Protocol.ASCII;

procedure ASCII_Slave is

   --  Data stores
   Holding_Registers : Register_Array (0 .. 99) := [others => 0];
   Input_Registers   : Register_Array (0 .. 49) := [others => 0];
   Coils             : Coil_Array (0 .. 63) := [others => False];
   Discrete_Inputs   : constant Coil_Array (0 .. 63) := [others => False];

   --  Slave callbacks
   function Read_Holding_Registers_CB
     (Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array) return Status
   is
      Start : constant Natural := Natural (Start_Address);
   begin
      if Start + Natural (Quantity) - 1 > Holding_Registers'Last then
         return Exception_Illegal_Address;
      end if;

      for I in 0 .. Natural (Quantity) - 1 loop
         Values (Values'First + I) := Holding_Registers (Start + I);
      end loop;

      Put_Line ("  -> Read Holding Registers: Start=" & Start'Image &
                " Qty=" & Quantity'Image);
      return Success;
   end Read_Holding_Registers_CB;

   function Read_Input_Registers_CB
     (Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array) return Status
   is
      Start : constant Natural := Natural (Start_Address);
   begin
      if Start + Natural (Quantity) - 1 > Input_Registers'Last then
         return Exception_Illegal_Address;
      end if;

      for I in 0 .. Natural (Quantity) - 1 loop
         Values (Values'First + I) := Input_Registers (Start + I);
      end loop;

      Put_Line ("  -> Read Input Registers: Start=" & Start'Image &
                " Qty=" & Quantity'Image);
      return Success;
   end Read_Input_Registers_CB;

   function Write_Single_Register_CB
     (Address : Register_Address;
      Value   : Register_Value) return Status
   is
      Addr : constant Natural := Natural (Address);
   begin
      if Addr > Holding_Registers'Last then
         return Exception_Illegal_Address;
      end if;

      Holding_Registers (Addr) := Value;
      Put_Line ("  -> Write Single Register: Addr=" & Addr'Image &
                " Value=" & Value'Image);
      return Success;
   end Write_Single_Register_CB;

   function Write_Multiple_Registers_CB
     (Start_Address : Register_Address;
      Values        : Register_Array) return Status
   is
      Start : constant Natural := Natural (Start_Address);
   begin
      if Start + Values'Length - 1 > Holding_Registers'Last then
         return Exception_Illegal_Address;
      end if;

      for I in Values'Range loop
         Holding_Registers (Start + I - Values'First) := Values (I);
      end loop;

      Put_Line ("  -> Write Multiple Registers: Start=" & Start'Image &
                " Qty=" & Values'Length'Image);
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
         return Exception_Illegal_Address;
      end if;

      for I in 0 .. Natural (Quantity) - 1 loop
         Values (Values'First + I) := Coils (Start + I);
      end loop;

      Put_Line ("  -> Read Coils: Start=" & Start'Image &
                " Qty=" & Quantity'Image);
      return Success;
   end Read_Coils_CB;

   function Read_Discrete_Inputs_CB
     (Start_Address : Coil_Address;
      Quantity      : Coil_Count;
      Values        : out Coil_Array) return Status
   is
      Start : constant Natural := Natural (Start_Address);
   begin
      if Start + Natural (Quantity) - 1 > Discrete_Inputs'Last then
         return Exception_Illegal_Address;
      end if;

      for I in 0 .. Natural (Quantity) - 1 loop
         Values (Values'First + I) := Discrete_Inputs (Start + I);
      end loop;

      Put_Line ("  -> Read Discrete Inputs: Start=" & Start'Image &
                " Qty=" & Quantity'Image);
      return Success;
   end Read_Discrete_Inputs_CB;

   function Write_Single_Coil_CB
     (Address : Coil_Address;
      Value   : Coil_Value) return Status
   is
      Addr : constant Natural := Natural (Address);
   begin
      if Addr > Coils'Last then
         return Exception_Illegal_Address;
      end if;

      Coils (Addr) := Value;
      Put_Line ("  -> Write Single Coil: Addr=" & Addr'Image &
                " Value=" & Value'Image);
      return Success;
   end Write_Single_Coil_CB;

   function Write_Multiple_Coils_CB
     (Start_Address : Coil_Address;
      Values        : Coil_Array) return Status
   is
      Start : constant Natural := Natural (Start_Address);
   begin
      if Start + Values'Length - 1 > Coils'Last then
         return Exception_Illegal_Address;
      end if;

      for I in Values'Range loop
         Coils (Start + I - Values'First) := Values (I);
      end loop;

      Put_Line ("  -> Write Multiple Coils: Start=" & Start'Image &
                " Qty=" & Values'Length'Image);
      return Success;
   end Write_Multiple_Coils_CB;

   --  Slave configuration
   My_Unit_Id : Unit_Id := 1;

   Config : Slave_Config :=
     (Mode      => Slave.ASCII,
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
         Read_Exception_Status    => null,
         Diagnostics              => null,
         Report_Server_Id         => null,
         Mask_Write_Register      => null,
         Read_Write_Registers     => null));

   Connection : aliased Serial_Connection;
   Port_Name  : String (1 .. 256) := [others => ' '];
   Port_Len   : Natural := 0;
   Baud       : Baud_Rate := B9600;
   Result     : Status;

   Request_Buffer  : Byte_Array (0 .. Protocol.ASCII.Max_Frame_Size - 1) := [others => 0];
   Response_Buffer : Byte_Array (0 .. Protocol.ASCII.Max_Frame_Size - 1) := [others => 0];
   Request_Length  : Natural;
   Response_Length : Natural;
   Send_Response   : Boolean;
   Request_Count   : Natural := 0;

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

   procedure Print_Usage is
   begin
      Put_Line ("Usage: ascii_slave <port> [baud] [unit_id]");
      Put_Line ("  port    - Serial port (COM3, /dev/ttyUSB0, etc.)");
      Put_Line ("  baud    - Baud rate (default: 9600)");
      Put_Line ("  unit_id - Slave address 1-247 (default: 1)");
   end Print_Usage;

begin
   Put_Line ("=== Modbus ASCII Slave Demo ===");
   New_Line;

   --  Parse arguments
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
      My_Unit_Id := Unit_Id (Natural'Value (Ada.Command_Line.Argument (3)));
      Config.Unit_Id := My_Unit_Id;
   end if;

   Put_Line ("Port:     " & Port_Name (1 .. Port_Len));
   Put_Line ("Baud:     " & Baud'Image);
   Put_Line ("Unit ID:  " & My_Unit_Id'Image);
   Put_Line ("Mode:     ASCII");
   New_Line;

   --  Initialize some test data
   for I in Holding_Registers'Range loop
      Holding_Registers (I) := Register_Value (I * 100);
   end loop;
   for I in Input_Registers'Range loop
      Input_Registers (I) := Register_Value (I * 10 + 1000);
   end loop;
   for I in 0 .. 7 loop
      Coils (I) := (I mod 2) = 0;  --  Alternating pattern
   end loop;

   Put_Line ("Test data initialized.");
   Put_Line ("  Holding Registers[0..4]: 0, 100, 200, 300, 400");
   Put_Line ("  Input Registers[0..4]: 1000, 1010, 1020, 1030, 1040");
   Put_Line ("  Coils[0..7]: 1 0 1 0 1 0 1 0");
   New_Line;

   --  Open port
   Open (Connection, Port_Name (1 .. Port_Len),
         (Rate => Baud, Parity => None, Stop_Bits => One, Data_Bits => Eight),
         Result);
   if Result /= Success then
      Put_Line ("ERROR: Failed to open port: " & Result'Image);
      return;
   end if;

   Put_Line ("Port opened. Waiting for requests...");
   Put_Line ("Press Ctrl+C to exit.");
   New_Line;

   --  Main loop
   loop
      --  Receive ASCII frame (wait for : ... CRLF)
      Request_Length := Receive (Connection, Request_Buffer,
                                 Protocol.ASCII.Max_Frame_Size, 100);

      if Request_Length > 0 then
         Request_Count := Request_Count + 1;
         Put_Line ("Request #" & Request_Count'Image & " received (" &
                   Request_Length'Image & " bytes)");

         --  Process request
         Process_Request (Config,
                          Request_Buffer, Request_Length,
                          Response_Buffer, Response_Length,
                          Send_Response);

         --  Send response if needed
         if Send_Response and Response_Length > 0 then
            declare
               Sent : Natural;
            begin
               Sent := Send (Connection, Response_Buffer (0 .. Response_Length - 1));
               if Sent /= Response_Length then
                  Put_Line ("  Warning: Only sent " & Sent'Image & " of " &
                            Response_Length'Image & " bytes");
               else
                  Put_Line ("  Response sent (" & Response_Length'Image & " bytes)");
               end if;
            end;
         end if;

         New_Line;
      end if;
   end loop;

exception
   when E : others =>
      Put_Line ("Exception: " & Ada.Exceptions.Exception_Message (E));
      Close (Connection);
end ASCII_Slave;
