--  Ada_Modbus.Transport.Serial - Serial port transport implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with Ada.Streams;
with Ada.Calendar;

package body Ada_Modbus.Transport.Serial is

   use type Ada.Streams.Stream_Element_Offset;

   --  Convert our Baud_Rate to GNAT's Data_Rate
   function To_Data_Rate (Rate : Baud_Rate) return GSC.Data_Rate is
   begin
      case Rate is
         when B1200   => return GSC.B1200;
         when B2400   => return GSC.B2400;
         when B4800   => return GSC.B4800;
         when B9600   => return GSC.B9600;
         when B19200  => return GSC.B19200;
         when B38400  => return GSC.B38400;
         when B57600  => return GSC.B57600;
         when B115200 => return GSC.B115200;
      end case;
   end To_Data_Rate;

   --  Convert our Parity_Mode to GNAT's Parity_Check
   function To_Parity (P : Parity_Mode) return GSC.Parity_Check is
   begin
      case P is
         when None => return GSC.None;
         when Even => return GSC.Even;
         when Odd  => return GSC.Odd;
      end case;
   end To_Parity;

   --  Convert our Stop_Bits_Count to GNAT's Stop_Bits_Number
   function To_Stop_Bits (S : Stop_Bits_Count) return GSC.Stop_Bits_Number is
   begin
      case S is
         when One => return GSC.One;
         when Two => return GSC.Two;
      end case;
   end To_Stop_Bits;

   --  Convert our Data_Bits_Count to GNAT's Data_Bits
   function To_Data_Bits (D : Data_Bits_Count) return GSC.Data_Bits is
   begin
      case D is
         when Seven => return GSC.CS7;
         when Eight => return GSC.CS8;
      end case;
   end To_Data_Bits;

   --  Set error message
   procedure Set_Error (Conn : in out Serial_Connection; Msg : String) is
      Len : constant Natural := Natural'Min (Msg'Length, Conn.Error_Msg'Length);
   begin
      Conn.Error_Msg := [others => ' '];
      Conn.Error_Msg (1 .. Len) := Msg (Msg'First .. Msg'First + Len - 1);
      Conn.Error_Len := Len;
      Conn.Current_State := Error;
   end Set_Error;

   -----------
   -- State --
   -----------

   function State (Conn : Serial_Connection) return Connection_State is
   begin
      return Conn.Current_State;
   end State;

   ----------------
   -- Last_Error --
   ----------------

   function Last_Error (Conn : Serial_Connection) return String is
   begin
      return Conn.Error_Msg (1 .. Conn.Error_Len);
   end Last_Error;

   ----------
   -- Open --
   ----------

   procedure Open
     (Conn      : in out Serial_Connection;
      Port_Name : String;
      Config    : Serial_Config := Default_Config;
      Result    : out Status)
   is
   begin
      --  Close if already open
      if Conn.Current_State = Open then
         Close (Conn);
      end if;

      --  Open the port
      GSC.Open (Conn.Port, GSC.Port_Name (Port_Name));

      --  Configure the port
      GSC.Set (Conn.Port,
               Rate      => To_Data_Rate (Config.Rate),
               Parity    => To_Parity (Config.Parity),
               Bits      => To_Data_Bits (Config.Data_Bits),
               Stop_Bits => To_Stop_Bits (Config.Stop_Bits),
               Block     => False);  -- Non-blocking for timeout support

      Conn.Current_State := Open;
      Conn.Current_Rate := Config.Rate;
      Conn.Error_Len := 0;
      Result := Success;
   exception
      when GSC.Serial_Error =>
         Set_Error (Conn, "Failed to open serial port: " & Port_Name);
         Result := Frame_Error;
      when others =>
         Set_Error (Conn, "Unknown error opening serial port");
         Result := Frame_Error;
   end Open;

   -----------
   -- Close --
   -----------

   procedure Close (Conn : in out Serial_Connection) is
   begin
      if Conn.Current_State = Open then
         GSC.Close (Conn.Port);
         Conn.Current_State := Closed;
      end if;
   exception
      when others =>
         Conn.Current_State := Closed;
   end Close;

   ----------
   -- Send --
   ----------

   function Send (Conn : in out Serial_Connection; Data : Byte_Array) return Natural is
      Buffer : Ada.Streams.Stream_Element_Array (1 .. Data'Length);
   begin
      if Conn.Current_State /= Open then
         return 0;
      end if;

      --  Convert Byte_Array to Stream_Element_Array
      for I in Data'Range loop
         Buffer (Ada.Streams.Stream_Element_Offset (I - Data'First + 1)) :=
           Ada.Streams.Stream_Element (Data (I));
      end loop;

      GSC.Write (Conn.Port, Buffer);
      return Data'Length;
   exception
      when others =>
         Set_Error (Conn, "Send failed");
         return 0;
   end Send;

   -------------
   -- Receive --
   -------------

   function Receive
     (Conn       : in out Serial_Connection;
      Buffer     : out Byte_Array;
      Max_Length : Natural;
      Timeout_Ms : Natural) return Natural
   is
      use Ada.Calendar;

      Temp     : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (Max_Length));
      Last     : Ada.Streams.Stream_Element_Offset;
      Total    : Natural := 0;
      Start    : constant Time := Clock;
      Deadline : constant Time := Start + Duration (Timeout_Ms) / 1000.0;
   begin
      if Conn.Current_State /= Open then
         Buffer := [others => 0];
         return 0;
      end if;

      --  Initialize buffer
      Buffer := [others => 0];

      --  Read with timeout
      while Clock < Deadline and then Total < Max_Length loop
         begin
            GSC.Read (Conn.Port, Temp (1 .. Ada.Streams.Stream_Element_Offset (Max_Length - Total)), Last);

            if Last >= 1 then
               for I in 1 .. Last loop
                  Buffer (Buffer'First + Total) := Byte (Temp (I));
                  Total := Total + 1;
               end loop;

               --  If we got data, check for inter-character timeout (RTU framing)
               --  For now, return after first successful read
               exit;
            end if;
         exception
            when others =>
               null;  -- Continue waiting
         end;

         --  Small delay to avoid busy-waiting
         delay 0.001;
      end loop;

      return Total;
   exception
      when others =>
         return Total;
   end Receive;

   -----------------
   -- Flush_Input --
   -----------------

   procedure Flush_Input (Conn : in out Serial_Connection) is
      Temp : Ada.Streams.Stream_Element_Array (1 .. 256);
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      if Conn.Current_State /= Open then
         return;
      end if;

      --  Read and discard until empty
      loop
         begin
            GSC.Read (Conn.Port, Temp, Last);
            exit when Last < 1;
         exception
            when others => exit;
         end;
      end loop;
   exception
      when others => null;
   end Flush_Input;

   ------------------
   -- Flush_Output --
   ------------------

   procedure Flush_Output (Conn : in out Serial_Connection) is
   begin
      if Conn.Current_State /= Open then
         return;
      end if;

      --  GNAT.Serial_Communications doesn't have explicit flush
      --  Small delay to allow data to be transmitted
      delay 0.01;
   end Flush_Output;

   --------------------------------
   -- Get_Inter_Char_Timeout_Us --
   --------------------------------

   function Get_Inter_Char_Timeout_Us (Rate : Baud_Rate) return Natural is
      --  1.5 character times
      --  One character = start + data + parity + stop = ~11 bits typically
      Bits_Per_Char : constant := 11;
      Baud : Natural;
   begin
      case Rate is
         when B1200   => Baud := 1200;
         when B2400   => Baud := 2400;
         when B4800   => Baud := 4800;
         when B9600   => Baud := 9600;
         when B19200  => Baud := 19200;
         when B38400  => Baud := 38400;
         when B57600  => Baud := 57600;
         when B115200 => Baud := 115200;
      end case;

      --  1.5 chars * 11 bits * 1_000_000 us / baud
      return (15 * Bits_Per_Char * 100_000) / Baud;
   end Get_Inter_Char_Timeout_Us;

   -------------------------------
   -- Get_Inter_Frame_Delay_Us --
   -------------------------------

   function Get_Inter_Frame_Delay_Us (Rate : Baud_Rate) return Natural is
      --  3.5 character times
      Bits_Per_Char : constant := 11;
      Baud : Natural;
   begin
      case Rate is
         when B1200   => Baud := 1200;
         when B2400   => Baud := 2400;
         when B4800   => Baud := 4800;
         when B9600   => Baud := 9600;
         when B19200  => Baud := 19200;
         when B38400  => Baud := 38400;
         when B57600  => Baud := 57600;
         when B115200 => Baud := 115200;
      end case;

      --  3.5 chars * 11 bits * 1_000_000 us / baud
      return (35 * Bits_Per_Char * 100_000) / Baud;
   end Get_Inter_Frame_Delay_Us;

end Ada_Modbus.Transport.Serial;
