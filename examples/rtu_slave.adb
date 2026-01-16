--  RTU_Slave - Modbus RTU Slave (Server) demo
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  A simple Modbus RTU server that:
--  - Listens on a serial port
--  - Responds to Modbus requests
--  - Demonstrates Slave API with RTU framing
--
--  Usage: rtu_slave <port> [baud] [unit-id]
--  Examples:
--    Windows: rtu_slave COM3 9600 1
--    Linux:   rtu_slave /dev/ttyUSB0 19200 1

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.Slave; use Ada_Modbus.Slave;
with Ada_Modbus.Transport.Serial;

procedure RTU_Slave is

   use Ada_Modbus.Transport.Serial;

   --  Simulated data stores
   Holding_Registers : Register_Array (0 .. 99) := [others => 0];
   Input_Registers   : Register_Array (0 .. 99) := [others => 0];
   Coils             : Coil_Array (0 .. 99) := [others => False];
   Discrete_Inputs   : constant Coil_Array (0 .. 99) := [others => False];

   --  Callback implementations
   function Read_Holding_Registers_Cb
     (Start : Register_Address;
      Qty   : Register_Count;
      Values : out Register_Array) return Status
   is
   begin
      if Natural (Start) + Natural (Qty) - 1 > Holding_Registers'Last then
         return Exception_Illegal_Address;
      end if;

      for I in 0 .. Natural (Qty) - 1 loop
         Values (Values'First + I) := Holding_Registers (Natural (Start) + I);
      end loop;
      return Success;
   end Read_Holding_Registers_Cb;

   function Read_Input_Registers_Cb
     (Start : Register_Address;
      Qty   : Register_Count;
      Values : out Register_Array) return Status
   is
   begin
      if Natural (Start) + Natural (Qty) - 1 > Input_Registers'Last then
         return Exception_Illegal_Address;
      end if;

      for I in 0 .. Natural (Qty) - 1 loop
         Values (Values'First + I) := Input_Registers (Natural (Start) + I);
      end loop;
      return Success;
   end Read_Input_Registers_Cb;

   function Write_Single_Register_Cb
     (Addr  : Register_Address;
      Value : Register_Value) return Status
   is
   begin
      if Natural (Addr) > Holding_Registers'Last then
         return Exception_Illegal_Address;
      end if;

      Holding_Registers (Natural (Addr)) := Value;
      Put_Line ("  Write Register[" & Register_Address'Image (Addr) & "] ="
                & Register_Value'Image (Value));
      return Success;
   end Write_Single_Register_Cb;

   function Write_Multiple_Registers_Cb
     (Start  : Register_Address;
      Values : Register_Array) return Status
   is
   begin
      if Natural (Start) + Values'Length - 1 > Holding_Registers'Last then
         return Exception_Illegal_Address;
      end if;

      for I in Values'Range loop
         Holding_Registers (Natural (Start) + I - Values'First) := Values (I);
      end loop;
      Put_Line ("  Write" & Natural'Image (Values'Length) & " Registers at" &
                Register_Address'Image (Start));
      return Success;
   end Write_Multiple_Registers_Cb;

   function Read_Coils_Cb
     (Start : Coil_Address;
      Qty   : Coil_Count;
      Values : out Coil_Array) return Status
   is
   begin
      if Natural (Start) + Natural (Qty) - 1 > Coils'Last then
         return Exception_Illegal_Address;
      end if;

      for I in 0 .. Natural (Qty) - 1 loop
         Values (Values'First + I) := Coils (Natural (Start) + I);
      end loop;
      return Success;
   end Read_Coils_Cb;

   function Read_Discrete_Inputs_Cb
     (Start : Coil_Address;
      Qty   : Coil_Count;
      Values : out Coil_Array) return Status
   is
   begin
      if Natural (Start) + Natural (Qty) - 1 > Discrete_Inputs'Last then
         return Exception_Illegal_Address;
      end if;

      for I in 0 .. Natural (Qty) - 1 loop
         Values (Values'First + I) := Discrete_Inputs (Natural (Start) + I);
      end loop;
      return Success;
   end Read_Discrete_Inputs_Cb;

   function Write_Single_Coil_Cb
     (Addr  : Coil_Address;
      Value : Boolean) return Status
   is
   begin
      if Natural (Addr) > Coils'Last then
         return Exception_Illegal_Address;
      end if;

      Coils (Natural (Addr)) := Value;
      Put_Line ("  Write Coil[" & Coil_Address'Image (Addr) & "] = " &
                (if Value then "ON" else "OFF"));
      return Success;
   end Write_Single_Coil_Cb;

   function Write_Multiple_Coils_Cb
     (Start  : Coil_Address;
      Values : Coil_Array) return Status
   is
   begin
      if Natural (Start) + Values'Length - 1 > Coils'Last then
         return Exception_Illegal_Address;
      end if;

      for I in Values'Range loop
         Coils (Natural (Start) + I - Values'First) := Values (I);
      end loop;
      Put_Line ("  Write" & Natural'Image (Values'Length) & " Coils at" &
                Coil_Address'Image (Start));
      return Success;
   end Write_Multiple_Coils_Cb;

   --  Slave configuration
   Config : constant Slave_Config :=
     (Mode      => RTU,
      Unit_Id   => 1,  --  Will be set from command line
      Callbacks =>
        (Read_Holding_Registers  => Read_Holding_Registers_Cb'Unrestricted_Access,
         Read_Input_Registers    => Read_Input_Registers_Cb'Unrestricted_Access,
         Write_Single_Register   => Write_Single_Register_Cb'Unrestricted_Access,
         Write_Multiple_Registers => Write_Multiple_Registers_Cb'Unrestricted_Access,
         Read_Coils              => Read_Coils_Cb'Unrestricted_Access,
         Read_Discrete_Inputs    => Read_Discrete_Inputs_Cb'Unrestricted_Access,
         Write_Single_Coil       => Write_Single_Coil_Cb'Unrestricted_Access,
         Write_Multiple_Coils    => Write_Multiple_Coils_Cb'Unrestricted_Access,
         Read_Exception_Status   => null,
         Diagnostics             => null,
         Report_Server_Id        => null,
         Mask_Write_Register     => null,
         Read_Write_Registers    => null));

   Connection    : aliased Serial_Connection;
   Port_Name     : String (1 .. 256) := [others => ' '];
   Port_Len      : Natural := 0;
   Baud          : Baud_Rate := B9600;
   My_Unit_Id    : Unit_Id := 1;
   Result        : Status;

   function Parse_Natural (S : String) return Natural is
   begin
      return Natural'Value (S);
   exception
      when others => return 0;
   end Parse_Natural;

   function Parse_Baud (S : String) return Baud_Rate is
      Val : constant Natural := Parse_Natural (S);
   begin
      case Val is
         when 1200   => return B1200;
         when 2400   => return B2400;
         when 4800   => return B4800;
         when 9600   => return B9600;
         when 19200  => return B19200;
         when 38400  => return B38400;
         when 57600  => return B57600;
         when 115200 => return B115200;
         when others => return B9600;
      end case;
   end Parse_Baud;

   --  Local config with command-line unit ID
   Local_Config : Slave_Config := Config;

begin
   --  Initialize some test data
   for I in Holding_Registers'Range loop
      Holding_Registers (I) := Register_Value (I * 10);
   end loop;

   for I in Input_Registers'Range loop
      Input_Registers (I) := Register_Value (1000 + I);
   end loop;

   --  Parse command line arguments
   if Ada.Command_Line.Argument_Count < 1 then
      Put_Line ("Usage: rtu_slave <port> [baud] [unit-id]");
      Put_Line ("  port:    COM3 (Windows) or /dev/ttyUSB0 (Linux)");
      Put_Line ("  baud:    1200|2400|4800|9600|19200|38400|57600|115200");
      Put_Line ("           (default: 9600)");
      Put_Line ("  unit-id: Modbus slave address 1-247 (default: 1)");
      return;
   end if;

   declare
      Arg1 : constant String := Ada.Command_Line.Argument (1);
   begin
      Port_Len := Arg1'Length;
      Port_Name (1 .. Port_Len) := Arg1;
   end;

   if Ada.Command_Line.Argument_Count >= 2 then
      Baud := Parse_Baud (Ada.Command_Line.Argument (2));
   end if;

   if Ada.Command_Line.Argument_Count >= 3 then
      My_Unit_Id := Unit_Id (Parse_Natural (Ada.Command_Line.Argument (3)));
      Local_Config.Unit_Id := My_Unit_Id;
   end if;

   Put ("Opening " & Port_Name (1 .. Port_Len));
   Put (" at " & Baud_Rate'Image (Baud));
   Put_Line (", Unit ID:" & Unit_Id'Image (My_Unit_Id));

   --  Open serial port
   Open (Connection,
         Port_Name (1 .. Port_Len),
         (Rate => Baud, Parity => None, Stop_Bits => One, Data_Bits => Eight),
         Result);

   if Result /= Success then
      Put_Line ("Failed to open serial port: " & Last_Error (Connection));
      return;
   end if;

   Put_Line ("Listening for Modbus RTU requests... (Ctrl+C to stop)");
   New_Line;

   --  Main loop
   declare
      Rx_Buffer  : Byte_Array (0 .. 259);
      Tx_Buffer  : Byte_Array (0 .. 259);
      Rx_Len     : Natural;
      Tx_Len     : Natural;
      Send_Resp  : Boolean;
      Bytes_Read : Natural;
   begin
      loop
         --  Read incoming data
         Bytes_Read := Receive (Connection, Rx_Buffer, Rx_Buffer'Length, 100);

         if Bytes_Read > 0 then
            Rx_Len := Bytes_Read;

            --  Try to read more data (inter-frame detection)
            delay Duration (Get_Inter_Frame_Delay_Us (Baud)) / 1_000_000.0;

            declare
               More : constant Natural :=
                 Receive (Connection, Rx_Buffer (Rx_Len .. Rx_Buffer'Last),
                          Rx_Buffer'Length - Rx_Len, 50);
            begin
               Rx_Len := Rx_Len + More;
            end;

            Put ("RX (" & Natural'Image (Rx_Len) & " bytes):");
            for I in 0 .. Rx_Len - 1 loop
               Put (" " & Byte'Image (Rx_Buffer (I)));
            end loop;
            New_Line;

            --  Process the request
            Process_Request (Local_Config,
                             Rx_Buffer (0 .. Rx_Len - 1),
                             Rx_Len,
                             Tx_Buffer,
                             Tx_Len,
                             Send_Resp);

            if Send_Resp and then Tx_Len > 0 then
               Put ("TX (" & Natural'Image (Tx_Len) & " bytes):");
               for I in 0 .. Tx_Len - 1 loop
                  Put (" " & Byte'Image (Tx_Buffer (I)));
               end loop;
               New_Line;

               --  Send response
               declare
                  Sent : constant Natural := Send (Connection, Tx_Buffer (0 .. Tx_Len - 1));
               begin
                  if Sent /= Tx_Len then
                     Put_Line ("Warning: Only sent" & Natural'Image (Sent) & " bytes");
                  end if;
               end;
            end if;

            New_Line;
         end if;
      end loop;
   end;

exception
   when E : others =>
      Put_Line ("Exception: " & Ada.Exceptions.Exception_Message (E));
      Close (Connection);
end RTU_Slave;
