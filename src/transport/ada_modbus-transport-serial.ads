--  Ada_Modbus.Transport.Serial - Serial port transport backend
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Provides serial port transport using GNAT.Serial_Communications
--  For Modbus RTU and ASCII over RS-232/RS-485

with GNAT.Serial_Communications;

package Ada_Modbus.Transport.Serial is

   package GSC renames GNAT.Serial_Communications;

   --  Serial Connection handle
   type Serial_Connection is limited private;

   --  Connection state
   type Connection_State is (Closed, Open, Error);

   --  Baud rates
   type Baud_Rate is
     (B1200, B2400, B4800, B9600, B19200, B38400, B57600, B115200);

   --  Parity modes
   type Parity_Mode is (None, Even, Odd);

   --  Stop bits
   type Stop_Bits_Count is (One, Two);

   --  Data bits
   type Data_Bits_Count is (Seven, Eight);

   --  Serial port configuration
   type Serial_Config is record
      Rate      : Baud_Rate := B9600;
      Parity    : Parity_Mode := None;
      Stop_Bits : Stop_Bits_Count := One;
      Data_Bits : Data_Bits_Count := Eight;
   end record;

   --  Default configuration for Modbus RTU: 9600 8N1
   Default_Config : constant Serial_Config :=
     (Rate => B9600, Parity => None, Stop_Bits => One, Data_Bits => Eight);

   --  Get current connection state
   function State (Conn : Serial_Connection) return Connection_State;

   --  Get last error message
   function Last_Error (Conn : Serial_Connection) return String;

   -----------------------
   --  Open/Close       --
   -----------------------

   --  Open serial port
   --  Port_Name: "COM1", "COM3" (Windows) or "/dev/ttyUSB0" (Linux)
   procedure Open
     (Conn      : in out Serial_Connection;
      Port_Name : String;
      Config    : Serial_Config := Default_Config;
      Result    : out Status);

   --  Close serial port
   procedure Close (Conn : in out Serial_Connection);

   ------------------------
   --  I/O functions     --
   ------------------------

   --  Send data (for use with Master generic)
   function Send (Conn : in out Serial_Connection; Data : Byte_Array) return Natural;

   --  Receive data with timeout (for use with Master generic)
   --  Returns number of bytes read (may be 0 if timeout)
   function Receive
     (Conn       : in out Serial_Connection;
      Buffer     : out Byte_Array;
      Max_Length : Natural;
      Timeout_Ms : Natural) return Natural;

   --  Flush input buffer (discard pending data)
   procedure Flush_Input (Conn : in out Serial_Connection);

   --  Flush output buffer (wait until all data sent)
   procedure Flush_Output (Conn : in out Serial_Connection);

   -----------------------------------
   --  RTU timing helper functions  --
   -----------------------------------

   --  Calculate inter-character timeout in microseconds
   --  Modbus RTU requires max 1.5 character times between bytes
   function Get_Inter_Char_Timeout_Us (Rate : Baud_Rate) return Natural;

   --  Calculate inter-frame delay in microseconds
   --  Modbus RTU requires min 3.5 character times between frames
   function Get_Inter_Frame_Delay_Us (Rate : Baud_Rate) return Natural;

private

   type Serial_Connection is limited record
      Port          : GSC.Serial_Port;
      Current_State : Connection_State := Closed;
      Current_Rate  : Baud_Rate := B9600;
      Error_Msg     : String (1 .. 256) := [others => ' '];
      Error_Len     : Natural := 0;
   end record;

end Ada_Modbus.Transport.Serial;
