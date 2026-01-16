--  Ada_Modbus.C_API - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with Ada.Calendar;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;
with Ada_Modbus.Transport.TCP;
with Ada_Modbus.Master;
with Ada_Modbus.Slave;

package body Ada_Modbus.C_API is

   use Ada_Modbus.Transport.TCP;

   --  Version string (statically allocated)
   Version_Str : aliased constant String := "0.1.0-dev" & ASCII.NUL;

   --  Status strings
   type Status_String is new String (1 .. 32);
   Status_Strings : constant array (0 .. 16) of Status_String :=
     [0  => "Success" & [8 .. 32 => ASCII.NUL],
      1  => "Timeout" & [8 .. 32 => ASCII.NUL],
      2  => "CRC Error" & [10 .. 32 => ASCII.NUL],
      3  => "LRC Error" & [10 .. 32 => ASCII.NUL],
      4  => "Frame Error" & [12 .. 32 => ASCII.NUL],
      5  => "Invalid Response" & [17 .. 32 => ASCII.NUL],
      6  => "Invalid Request" & [16 .. 32 => ASCII.NUL],
      7  => "Buffer Too Small" & [17 .. 32 => ASCII.NUL],
      8  => "Not Implemented" & [16 .. 32 => ASCII.NUL],
      9  => "Illegal Function (01)" & [22 .. 32 => ASCII.NUL],
      10 => "Illegal Address (02)" & [21 .. 32 => ASCII.NUL],
      11 => "Illegal Value (03)" & [19 .. 32 => ASCII.NUL],
      12 => "Slave Failure (04)" & [19 .. 32 => ASCII.NUL],
      13 => "Acknowledge (05)" & [17 .. 32 => ASCII.NUL],
      14 => "Slave Busy (06)" & [16 .. 32 => ASCII.NUL],
      15 => "Gateway Path (10)" & [18 .. 32 => ASCII.NUL],
      16 => "Gateway Target (11)" & [20 .. 32 => ASCII.NUL]];

   --  Last error buffer for TCP handles
   Error_Buffer : aliased String (1 .. 256) := [others => ASCII.NUL];

   ---------------------
   --  Internal Types
   ---------------------

   type TCP_Connection_Ptr is access all TCP_Connection;

   procedure Free_Connection is new Ada.Unchecked_Deallocation
     (TCP_Connection, TCP_Connection_Ptr);

   function To_Ptr is new Ada.Unchecked_Conversion
     (C_TCP_Handle, TCP_Connection_Ptr);
   function To_Handle is new Ada.Unchecked_Conversion
     (TCP_Connection_Ptr, C_TCP_Handle);

   ---------------------
   --  Master with TCP transport
   ---------------------

   function Master_Send
     (Ctx  : in out TCP_Connection_Ptr;
      Data : Byte_Array) return Natural is
   begin
      if Ctx = null then
         return 0;
      end if;
      return Send (Ctx.all, Data);
   end Master_Send;

   function Master_Receive
     (Ctx        : in out TCP_Connection_Ptr;
      Buffer     : out Byte_Array;
      Max_Length : Natural;
      Timeout_Ms : Natural) return Natural is
   begin
      if Ctx = null then
         return 0;
      end if;
      return Receive (Ctx.all, Buffer, Max_Length, Timeout_Ms);
   end Master_Receive;

   function Get_Tick return Unsigned_32 is
      use Ada.Calendar;
      Now : constant Time := Clock;
      Seconds : constant Duration := Now - Time_Of (1970, 1, 1, 0.0);
   begin
      return Unsigned_32 (Seconds * 1000.0) mod Unsigned_32'Last;
   end Get_Tick;

   package TCP_Master is new Ada_Modbus.Master
     (Transport_Context => TCP_Connection_Ptr,
      Send              => Master_Send,
      Receive           => Master_Receive,
      Get_Tick_Ms       => Get_Tick);

   --  Master context wrapper (for heap allocation)
   type Master_Context_Wrapper is record
      Ctx : TCP_Master.Master_Context;
   end record;

   type Master_Context_Ptr is access all Master_Context_Wrapper;

   procedure Free_Master is new Ada.Unchecked_Deallocation
     (Master_Context_Wrapper, Master_Context_Ptr);

   function To_Master_Ptr is new Ada.Unchecked_Conversion
     (C_Master_Handle, Master_Context_Ptr);
   function To_Master_Handle is new Ada.Unchecked_Conversion
     (Master_Context_Ptr, C_Master_Handle);

   --  Slave context wrapper
   type Slave_Context_Wrapper is record
      Config      : Ada_Modbus.Slave.Slave_Config;
      C_Callbacks : aliased Slave_Callbacks;
   end record;
   type Slave_Context_Ptr is access all Slave_Context_Wrapper;

   procedure Free_Slave is new Ada.Unchecked_Deallocation
     (Slave_Context_Wrapper, Slave_Context_Ptr);

   function To_Slave_Ptr is new Ada.Unchecked_Conversion
     (C_Slave_Handle, Slave_Context_Ptr);
   function To_Slave_Handle is new Ada.Unchecked_Conversion
     (Slave_Context_Ptr, C_Slave_Handle);

   ---------------------
   --  Helper Functions
   ---------------------

   function To_Ada_Status (S : C_Status) return Status is
   begin
      case S is
         when 0  => return Success;
         when 1  => return Timeout;
         when 2  => return CRC_Error;
         when 3  => return LRC_Error;
         when 4  => return Frame_Error;
         when 5  => return Invalid_Response;
         when 6  => return Invalid_Request;
         when 7  => return Buffer_Too_Small;
         when 8  => return Not_Implemented;
         when 9  => return Exception_Illegal_Function;
         when 10 => return Exception_Illegal_Address;
         when 11 => return Exception_Illegal_Value;
         when 12 => return Exception_Slave_Failure;
         when 13 => return Exception_Acknowledge;
         when 14 => return Exception_Slave_Busy;
         when 15 => return Exception_Gateway_Path;
         when 16 => return Exception_Gateway_Target;
         when others => return Invalid_Request;
      end case;
   end To_Ada_Status;

   function To_C_Status (S : Status) return C_Status is
   begin
      case S is
         when Success                     => return 0;
         when Timeout                     => return 1;
         when CRC_Error                   => return 2;
         when LRC_Error                   => return 3;
         when Frame_Error                 => return 4;
         when Invalid_Response            => return 5;
         when Invalid_Request             => return 6;
         when Buffer_Too_Small            => return 7;
         when Not_Implemented             => return 8;
         when Exception_Illegal_Function  => return 9;
         when Exception_Illegal_Address   => return 10;
         when Exception_Illegal_Value     => return 11;
         when Exception_Slave_Failure     => return 12;
         when Exception_Acknowledge       => return 13;
         when Exception_Slave_Busy        => return 14;
         when Exception_Gateway_Path      => return 15;
         when Exception_Gateway_Target    => return 16;
      end case;
   end To_C_Status;

   --  Pack coils into byte array
   procedure Pack_Coils
     (Coils  : Coil_Array;
      Bytes  : out Byte_Array)
   is
      Byte_Idx : Natural := Bytes'First;
      Bit_Idx  : Natural := 0;
      Current  : Byte := 0;
   begin
      Bytes := [others => 0];
      for I in Coils'Range loop
         if Coils (I) then
            Current := Current or Byte (Shift_Left (Unsigned_8 (1), Bit_Idx));
         end if;
         Bit_Idx := Bit_Idx + 1;
         if Bit_Idx = 8 then
            Bytes (Byte_Idx) := Current;
            Byte_Idx := Byte_Idx + 1;
            Bit_Idx := 0;
            Current := 0;
         end if;
      end loop;
      if Bit_Idx > 0 then
         Bytes (Byte_Idx) := Current;
      end if;
   end Pack_Coils;

   --  Unpack coils from byte array
   procedure Unpack_Coils
     (Bytes  : Byte_Array;
      Coils  : out Coil_Array)
   is
   begin
      for I in Coils'Range loop
         declare
            Idx      : constant Natural := I - Coils'First;
            Byte_Idx : constant Natural := Idx / 8;
            Bit_Idx  : constant Natural := Idx mod 8;
         begin
            Coils (I) := (Bytes (Bytes'First + Byte_Idx) and
              Byte (Shift_Left (Unsigned_8 (1), Bit_Idx))) /= 0;
         end;
      end loop;
   end Unpack_Coils;

   ---------------------
   --  TCP Connection Implementation
   ---------------------

   function Modbus_TCP_Create return C_TCP_Handle is
      Ptr : constant TCP_Connection_Ptr := new TCP_Connection;
   begin
      return To_Handle (Ptr);
   end Modbus_TCP_Create;

   procedure Modbus_TCP_Destroy (Handle : C_TCP_Handle) is
      Ptr : TCP_Connection_Ptr := To_Ptr (Handle);
   begin
      if Ptr /= null then
         if State (Ptr.all) = Connected then
            Disconnect (Ptr.all);
         elsif State (Ptr.all) = Listening then
            Close_Server (Ptr.all);
         end if;
         Free_Connection (Ptr);
      end if;
   end Modbus_TCP_Destroy;

   function Modbus_TCP_Connect
     (Handle     : C_TCP_Handle;
      Host       : Interfaces.C.Strings.chars_ptr;
      Port       : int;
      Timeout_Ms : int) return C_Status
   is
      Ptr : constant TCP_Connection_Ptr := To_Ptr (Handle);
      Result : Status;
      Host_Str : constant String := Interfaces.C.Strings.Value (Host);
      Timeout_Dur : constant Duration := Duration (Timeout_Ms) / 1000.0;
   begin
      if Ptr = null then
         return C_Invalid_Request;
      end if;

      Connect (Ptr.all, Host_Str, Natural (Port), Timeout_Dur, Result);
      return To_C_Status (Result);
   end Modbus_TCP_Connect;

   procedure Modbus_TCP_Disconnect (Handle : C_TCP_Handle) is
      Ptr : constant TCP_Connection_Ptr := To_Ptr (Handle);
   begin
      if Ptr /= null then
         Disconnect (Ptr.all);
      end if;
   end Modbus_TCP_Disconnect;

   function Modbus_TCP_Listen
     (Handle : C_TCP_Handle;
      Port   : int) return C_Status
   is
      Ptr : constant TCP_Connection_Ptr := To_Ptr (Handle);
      Result : Status;
   begin
      if Ptr = null then
         return C_Invalid_Request;
      end if;

      Listen (Ptr.all, Natural (Port), Result);
      return To_C_Status (Result);
   end Modbus_TCP_Listen;

   function Modbus_TCP_Accept
     (Server_Handle : C_TCP_Handle;
      Client_Handle : access C_TCP_Handle) return C_Status
   is
      Server_Ptr : constant TCP_Connection_Ptr := To_Ptr (Server_Handle);
      Client_Ptr : TCP_Connection_Ptr;
      Result : Status;
   begin
      if Server_Ptr = null or else Client_Handle = null then
         return C_Invalid_Request;
      end if;

      Client_Ptr := new TCP_Connection;
      Accept_Connection (Server_Ptr.all, Client_Ptr.all, Result);

      if Result = Success then
         Client_Handle.all := To_Handle (Client_Ptr);
      else
         Free_Connection (Client_Ptr);
         Client_Handle.all := System.Null_Address;
      end if;

      return To_C_Status (Result);
   end Modbus_TCP_Accept;

   procedure Modbus_TCP_Close_Server (Handle : C_TCP_Handle) is
      Ptr : constant TCP_Connection_Ptr := To_Ptr (Handle);
   begin
      if Ptr /= null then
         Close_Server (Ptr.all);
      end if;
   end Modbus_TCP_Close_Server;

   function Modbus_TCP_State (Handle : C_TCP_Handle) return int is
      Ptr : constant TCP_Connection_Ptr := To_Ptr (Handle);
   begin
      if Ptr = null then
         return 3;  --  Error
      end if;

      case State (Ptr.all) is
         when Disconnected => return 0;
         when Connected    => return 1;
         when Listening    => return 2;
         when Error        => return 3;
      end case;
   end Modbus_TCP_State;

   function Modbus_TCP_Last_Error
     (Handle : C_TCP_Handle) return Interfaces.C.Strings.chars_ptr
   is
      Ptr : constant TCP_Connection_Ptr := To_Ptr (Handle);
      Msg : constant String :=
        (if Ptr /= null then Last_Error (Ptr.all) else "Invalid handle");
      Len : constant Natural := Natural'Min (Msg'Length, Error_Buffer'Length - 1);
   begin
      Error_Buffer := [others => ASCII.NUL];
      Error_Buffer (1 .. Len) := Msg (Msg'First .. Msg'First + Len - 1);
      return Interfaces.C.Strings.New_String (Error_Buffer (1 .. Len));
   end Modbus_TCP_Last_Error;

   ---------------------
   --  Master Implementation
   ---------------------

   function Modbus_Master_Create
     (TCP_Handle : C_TCP_Handle;
      Mode       : C_Protocol_Mode;
      Unit_Id    : unsigned_char;
      Timeout_Ms : int) return C_Master_Handle
   is
      Ptr : constant TCP_Connection_Ptr := To_Ptr (TCP_Handle);
      Master_Ptr : Master_Context_Ptr;
      Ada_Mode : TCP_Master.Protocol_Mode;
   begin
      if Ptr = null then
         return System.Null_Address;
      end if;

      case Mode is
         when C_Mode_RTU   => Ada_Mode := TCP_Master.RTU;
         when C_Mode_ASCII => Ada_Mode := TCP_Master.ASCII;
         when others       => Ada_Mode := TCP_Master.TCP;
      end case;

      Master_Ptr := new Master_Context_Wrapper;

      TCP_Master.Initialize
        (Master_Ptr.Ctx,
         (Mode            => Ada_Mode,
          Default_Slave   => Ada_Modbus.Unit_Id (Unit_Id),
          Default_Timeout => Natural (Timeout_Ms)),
         Ptr);

      return To_Master_Handle (Master_Ptr);
   end Modbus_Master_Create;

   procedure Modbus_Master_Destroy (Handle : C_Master_Handle) is
      Ptr : Master_Context_Ptr := To_Master_Ptr (Handle);
   begin
      if Ptr /= null then
         Free_Master (Ptr);
      end if;
   end Modbus_Master_Destroy;

   function Modbus_Read_Coils
     (Handle        : C_Master_Handle;
      Slave         : unsigned_char;
      Start_Address : unsigned_short;
      Quantity      : unsigned_short;
      Values        : access unsigned_char;
      Timeout_Ms    : int) return C_Status
   is
      Ptr : constant Master_Context_Ptr := To_Master_Ptr (Handle);
      Coils : Coil_Array (0 .. Natural (Quantity) - 1);
      Bytes : Byte_Array (0 .. (Natural (Quantity) + 7) / 8 - 1);
      Result : Status;

      --  C array access
      type C_Byte_Array is array (Natural range <>) of aliased unsigned_char
        with Convention => C;
      type C_Byte_Ptr is access all C_Byte_Array (0 .. 255);
      function To_C_Ptr is new Ada.Unchecked_Conversion
        (System.Address, C_Byte_Ptr);
      C_Values : constant C_Byte_Ptr := To_C_Ptr (Values.all'Address);
   begin
      if Ptr = null or else Values = null then
         return C_Invalid_Request;
      end if;


      Result := TCP_Master.Read_Coils
        (Ptr.Ctx,
         Ada_Modbus.Unit_Id (Slave),
         Coil_Address (Start_Address),
         Coil_Count (Quantity),
         Coils,
         Natural (Timeout_Ms));

      if Result = Success then
         Pack_Coils (Coils, Bytes);
         for I in Bytes'Range loop
            C_Values (I) := unsigned_char (Bytes (I));
         end loop;
      end if;

      return To_C_Status (Result);
   end Modbus_Read_Coils;

   function Modbus_Read_Discrete_Inputs
     (Handle        : C_Master_Handle;
      Slave         : unsigned_char;
      Start_Address : unsigned_short;
      Quantity      : unsigned_short;
      Values        : access unsigned_char;
      Timeout_Ms    : int) return C_Status
   is
      Ptr : constant Master_Context_Ptr := To_Master_Ptr (Handle);
      Inputs : Coil_Array (0 .. Natural (Quantity) - 1);
      Bytes : Byte_Array (0 .. (Natural (Quantity) + 7) / 8 - 1);
      Result : Status;

      type C_Byte_Array is array (Natural range <>) of aliased unsigned_char
        with Convention => C;
      type C_Byte_Ptr is access all C_Byte_Array (0 .. 255);
      function To_C_Ptr is new Ada.Unchecked_Conversion
        (System.Address, C_Byte_Ptr);
      C_Values : constant C_Byte_Ptr := To_C_Ptr (Values.all'Address);
   begin
      if Ptr = null or else Values = null then
         return C_Invalid_Request;
      end if;


      Result := TCP_Master.Read_Discrete_Inputs
        (Ptr.Ctx,
         Ada_Modbus.Unit_Id (Slave),
         Coil_Address (Start_Address),
         Coil_Count (Quantity),
         Inputs,
         Natural (Timeout_Ms));

      if Result = Success then
         Pack_Coils (Inputs, Bytes);
         for I in Bytes'Range loop
            C_Values (I) := unsigned_char (Bytes (I));
         end loop;
      end if;

      return To_C_Status (Result);
   end Modbus_Read_Discrete_Inputs;

   function Modbus_Read_Holding_Registers
     (Handle        : C_Master_Handle;
      Slave         : unsigned_char;
      Start_Address : unsigned_short;
      Quantity      : unsigned_short;
      Values        : access unsigned_short;
      Timeout_Ms    : int) return C_Status
   is
      Ptr : constant Master_Context_Ptr := To_Master_Ptr (Handle);
      Regs : Register_Array (0 .. Natural (Quantity) - 1);
      Result : Status;

      type C_Reg_Array is array (Natural range <>) of aliased unsigned_short
        with Convention => C;
      type C_Reg_Ptr is access all C_Reg_Array (0 .. 127);
      function To_C_Ptr is new Ada.Unchecked_Conversion
        (System.Address, C_Reg_Ptr);
      C_Values : constant C_Reg_Ptr := To_C_Ptr (Values.all'Address);
   begin
      if Ptr = null or else Values = null then
         return C_Invalid_Request;
      end if;


      Result := TCP_Master.Read_Holding_Registers
        (Ptr.Ctx,
         Ada_Modbus.Unit_Id (Slave),
         Register_Address (Start_Address),
         Register_Count (Quantity),
         Regs,
         Natural (Timeout_Ms));

      if Result = Success then
         for I in Regs'Range loop
            C_Values (I) := unsigned_short (Regs (I));
         end loop;
      end if;

      return To_C_Status (Result);
   end Modbus_Read_Holding_Registers;

   function Modbus_Read_Input_Registers
     (Handle        : C_Master_Handle;
      Slave         : unsigned_char;
      Start_Address : unsigned_short;
      Quantity      : unsigned_short;
      Values        : access unsigned_short;
      Timeout_Ms    : int) return C_Status
   is
      Ptr : constant Master_Context_Ptr := To_Master_Ptr (Handle);
      Regs : Register_Array (0 .. Natural (Quantity) - 1);
      Result : Status;

      type C_Reg_Array is array (Natural range <>) of aliased unsigned_short
        with Convention => C;
      type C_Reg_Ptr is access all C_Reg_Array (0 .. 127);
      function To_C_Ptr is new Ada.Unchecked_Conversion
        (System.Address, C_Reg_Ptr);
      C_Values : constant C_Reg_Ptr := To_C_Ptr (Values.all'Address);
   begin
      if Ptr = null or else Values = null then
         return C_Invalid_Request;
      end if;


      Result := TCP_Master.Read_Input_Registers
        (Ptr.Ctx,
         Ada_Modbus.Unit_Id (Slave),
         Register_Address (Start_Address),
         Register_Count (Quantity),
         Regs,
         Natural (Timeout_Ms));

      if Result = Success then
         for I in Regs'Range loop
            C_Values (I) := unsigned_short (Regs (I));
         end loop;
      end if;

      return To_C_Status (Result);
   end Modbus_Read_Input_Registers;

   function Modbus_Write_Single_Coil
     (Handle     : C_Master_Handle;
      Slave      : unsigned_char;
      Address    : unsigned_short;
      Value      : int;
      Timeout_Ms : int) return C_Status
   is
      Ptr : constant Master_Context_Ptr := To_Master_Ptr (Handle);
      Result : Status;
   begin
      if Ptr = null then
         return C_Invalid_Request;
      end if;


      Result := TCP_Master.Write_Single_Coil
        (Ptr.Ctx,
         Ada_Modbus.Unit_Id (Slave),
         Coil_Address (Address),
         Value /= 0,
         Natural (Timeout_Ms));

      return To_C_Status (Result);
   end Modbus_Write_Single_Coil;

   function Modbus_Write_Single_Register
     (Handle     : C_Master_Handle;
      Slave      : unsigned_char;
      Address    : unsigned_short;
      Value      : unsigned_short;
      Timeout_Ms : int) return C_Status
   is
      Ptr : constant Master_Context_Ptr := To_Master_Ptr (Handle);
      Result : Status;
   begin
      if Ptr = null then
         return C_Invalid_Request;
      end if;


      Result := TCP_Master.Write_Single_Register
        (Ptr.Ctx,
         Ada_Modbus.Unit_Id (Slave),
         Register_Address (Address),
         Register_Value (Value),
         Natural (Timeout_Ms));

      return To_C_Status (Result);
   end Modbus_Write_Single_Register;

   function Modbus_Write_Multiple_Coils
     (Handle        : C_Master_Handle;
      Slave         : unsigned_char;
      Start_Address : unsigned_short;
      Quantity      : unsigned_short;
      Values        : access unsigned_char;
      Timeout_Ms    : int) return C_Status
   is
      Ptr : constant Master_Context_Ptr := To_Master_Ptr (Handle);
      Coils : Coil_Array (0 .. Natural (Quantity) - 1);
      Bytes : Byte_Array (0 .. (Natural (Quantity) + 7) / 8 - 1);
      Result : Status;

      type C_Byte_Array is array (Natural range <>) of aliased unsigned_char
        with Convention => C;
      type C_Byte_Ptr is access all C_Byte_Array (0 .. 255);
      function To_C_Ptr is new Ada.Unchecked_Conversion
        (System.Address, C_Byte_Ptr);
      C_Values : constant C_Byte_Ptr := To_C_Ptr (Values.all'Address);
   begin
      if Ptr = null or else Values = null then
         return C_Invalid_Request;
      end if;

      --  Copy C bytes to Ada bytes
      for I in Bytes'Range loop
         Bytes (I) := Byte (C_Values (I));
      end loop;

      --  Unpack to coils
      Unpack_Coils (Bytes, Coils);


      Result := TCP_Master.Write_Multiple_Coils
        (Ptr.Ctx,
         Ada_Modbus.Unit_Id (Slave),
         Coil_Address (Start_Address),
         Coils,
         Natural (Timeout_Ms));

      return To_C_Status (Result);
   end Modbus_Write_Multiple_Coils;

   function Modbus_Write_Multiple_Registers
     (Handle        : C_Master_Handle;
      Slave         : unsigned_char;
      Start_Address : unsigned_short;
      Quantity      : unsigned_short;
      Values        : access unsigned_short;
      Timeout_Ms    : int) return C_Status
   is
      Ptr : constant Master_Context_Ptr := To_Master_Ptr (Handle);
      Regs : Register_Array (0 .. Natural (Quantity) - 1);
      Result : Status;

      type C_Reg_Array is array (Natural range <>) of aliased unsigned_short
        with Convention => C;
      type C_Reg_Ptr is access all C_Reg_Array (0 .. 127);
      function To_C_Ptr is new Ada.Unchecked_Conversion
        (System.Address, C_Reg_Ptr);
      C_Values : constant C_Reg_Ptr := To_C_Ptr (Values.all'Address);
   begin
      if Ptr = null or else Values = null then
         return C_Invalid_Request;
      end if;

      --  Copy values
      for I in Regs'Range loop
         Regs (I) := Register_Value (C_Values (I));
      end loop;


      Result := TCP_Master.Write_Multiple_Registers
        (Ptr.Ctx,
         Ada_Modbus.Unit_Id (Slave),
         Register_Address (Start_Address),
         Regs,
         Natural (Timeout_Ms));

      return To_C_Status (Result);
   end Modbus_Write_Multiple_Registers;

   ---------------------
   --  Slave Implementation
   ---------------------

   --  Global callbacks storage for slave wrapper functions
   Global_Slave_Callbacks : access Slave_Callbacks := null;

   function Wrap_Read_Coils
     (Start_Address : Coil_Address;
      Quantity      : Coil_Count;
      Values        : out Coil_Array) return Status
   is
      Bytes : Byte_Array (0 .. (Natural (Quantity) + 7) / 8 - 1) :=
        [others => 0];
      type C_Byte_Array is array (Natural range <>) of aliased unsigned_char
        with Convention => C;
      C_Bytes : aliased C_Byte_Array (0 .. Bytes'Last) := [others => 0];
      Res : C_Status;
   begin
      if Global_Slave_Callbacks = null or else
         Global_Slave_Callbacks.Read_Coils = null
      then
         return Exception_Illegal_Function;
      end if;

      Res := Global_Slave_Callbacks.Read_Coils
        (Global_Slave_Callbacks.User_Data,
         unsigned_short (Start_Address),
         unsigned_short (Quantity),
         C_Bytes (0)'Access);

      if Res = C_Success then
         for I in Bytes'Range loop
            Bytes (I) := Byte (C_Bytes (I));
         end loop;
         Unpack_Coils (Bytes, Values);
      end if;

      return To_Ada_Status (Res);
   end Wrap_Read_Coils;

   function Wrap_Read_Discrete_Inputs
     (Start_Address : Coil_Address;
      Quantity      : Coil_Count;
      Values        : out Coil_Array) return Status
   is
      Bytes : Byte_Array (0 .. (Natural (Quantity) + 7) / 8 - 1) :=
        [others => 0];
      type C_Byte_Array is array (Natural range <>) of aliased unsigned_char
        with Convention => C;
      C_Bytes : aliased C_Byte_Array (0 .. Bytes'Last) := [others => 0];
      Res : C_Status;
   begin
      if Global_Slave_Callbacks = null or else
         Global_Slave_Callbacks.Read_Discrete_Inputs = null
      then
         return Exception_Illegal_Function;
      end if;

      Res := Global_Slave_Callbacks.Read_Discrete_Inputs
        (Global_Slave_Callbacks.User_Data,
         unsigned_short (Start_Address),
         unsigned_short (Quantity),
         C_Bytes (0)'Access);

      if Res = C_Success then
         for I in Bytes'Range loop
            Bytes (I) := Byte (C_Bytes (I));
         end loop;
         Unpack_Coils (Bytes, Values);
      end if;

      return To_Ada_Status (Res);
   end Wrap_Read_Discrete_Inputs;

   function Wrap_Read_Holding_Registers
     (Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array) return Status
   is
      type C_Reg_Array is array (Natural range <>) of aliased unsigned_short
        with Convention => C;
      C_Regs : aliased C_Reg_Array (0 .. Natural (Quantity) - 1) :=
        [others => 0];
      Res : C_Status;
   begin
      if Global_Slave_Callbacks = null or else
         Global_Slave_Callbacks.Read_Holding_Registers = null
      then
         return Exception_Illegal_Function;
      end if;

      Res := Global_Slave_Callbacks.Read_Holding_Registers
        (Global_Slave_Callbacks.User_Data,
         unsigned_short (Start_Address),
         unsigned_short (Quantity),
         C_Regs (0)'Access);

      if Res = C_Success then
         for I in 0 .. Natural (Quantity) - 1 loop
            Values (Values'First + I) := Register_Value (C_Regs (I));
         end loop;
      end if;

      return To_Ada_Status (Res);
   end Wrap_Read_Holding_Registers;

   function Wrap_Read_Input_Registers
     (Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array) return Status
   is
      type C_Reg_Array is array (Natural range <>) of aliased unsigned_short
        with Convention => C;
      C_Regs : aliased C_Reg_Array (0 .. Natural (Quantity) - 1) :=
        [others => 0];
      Res : C_Status;
   begin
      if Global_Slave_Callbacks = null or else
         Global_Slave_Callbacks.Read_Input_Registers = null
      then
         return Exception_Illegal_Function;
      end if;

      Res := Global_Slave_Callbacks.Read_Input_Registers
        (Global_Slave_Callbacks.User_Data,
         unsigned_short (Start_Address),
         unsigned_short (Quantity),
         C_Regs (0)'Access);

      if Res = C_Success then
         for I in 0 .. Natural (Quantity) - 1 loop
            Values (Values'First + I) := Register_Value (C_Regs (I));
         end loop;
      end if;

      return To_Ada_Status (Res);
   end Wrap_Read_Input_Registers;

   function Wrap_Write_Single_Coil
     (Address : Coil_Address;
      Value   : Coil_Value) return Status
   is
      Res : C_Status;
   begin
      if Global_Slave_Callbacks = null or else
         Global_Slave_Callbacks.Write_Single_Coil = null
      then
         return Exception_Illegal_Function;
      end if;

      Res := Global_Slave_Callbacks.Write_Single_Coil
        (Global_Slave_Callbacks.User_Data,
         unsigned_short (Address),
         (if Value then 1 else 0));

      return To_Ada_Status (Res);
   end Wrap_Write_Single_Coil;

   function Wrap_Write_Single_Register
     (Address : Register_Address;
      Value   : Register_Value) return Status
   is
      Res : C_Status;
   begin
      if Global_Slave_Callbacks = null or else
         Global_Slave_Callbacks.Write_Single_Register = null
      then
         return Exception_Illegal_Function;
      end if;

      Res := Global_Slave_Callbacks.Write_Single_Register
        (Global_Slave_Callbacks.User_Data,
         unsigned_short (Address),
         unsigned_short (Value));

      return To_Ada_Status (Res);
   end Wrap_Write_Single_Register;

   function Wrap_Write_Multiple_Coils
     (Start_Address : Coil_Address;
      Values        : Coil_Array) return Status
   is
      Bytes : Byte_Array (0 .. (Values'Length + 7) / 8 - 1) := [others => 0];
      type C_Byte_Array is array (Natural range <>) of aliased unsigned_char
        with Convention => C;
      C_Bytes : aliased C_Byte_Array (0 .. Bytes'Last) := [others => 0];
      Res : C_Status;
   begin
      if Global_Slave_Callbacks = null or else
         Global_Slave_Callbacks.Write_Multiple_Coils = null
      then
         return Exception_Illegal_Function;
      end if;

      Pack_Coils (Values, Bytes);
      for I in Bytes'Range loop
         C_Bytes (I) := unsigned_char (Bytes (I));
      end loop;

      Res := Global_Slave_Callbacks.Write_Multiple_Coils
        (Global_Slave_Callbacks.User_Data,
         unsigned_short (Start_Address),
         unsigned_short (Values'Length),
         C_Bytes (0)'Access);

      return To_Ada_Status (Res);
   end Wrap_Write_Multiple_Coils;

   function Wrap_Write_Multiple_Registers
     (Start_Address : Register_Address;
      Values        : Register_Array) return Status
   is
      type C_Reg_Array is array (Natural range <>) of aliased unsigned_short
        with Convention => C;
      C_Regs : aliased C_Reg_Array (0 .. Values'Length - 1);
      Res : C_Status;
   begin
      if Global_Slave_Callbacks = null or else
         Global_Slave_Callbacks.Write_Multiple_Registers = null
      then
         return Exception_Illegal_Function;
      end if;

      for I in Values'Range loop
         C_Regs (I - Values'First) := unsigned_short (Values (I));
      end loop;

      Res := Global_Slave_Callbacks.Write_Multiple_Registers
        (Global_Slave_Callbacks.User_Data,
         unsigned_short (Start_Address),
         unsigned_short (Values'Length),
         C_Regs (0)'Access);

      return To_Ada_Status (Res);
   end Wrap_Write_Multiple_Registers;

   function Modbus_Slave_Create
     (Mode      : C_Protocol_Mode;
      Unit_Id   : unsigned_char;
      Callbacks : access Slave_Callbacks) return C_Slave_Handle
   is
      use Ada_Modbus.Slave;

      Ptr : Slave_Context_Ptr;
      Ada_Mode : Ada_Modbus.Slave.Protocol_Mode;
   begin
      if Callbacks = null then
         return System.Null_Address;
      end if;

      case Mode is
         when C_Mode_RTU   => Ada_Mode := Ada_Modbus.Slave.RTU;
         when C_Mode_ASCII => Ada_Mode := Ada_Modbus.Slave.ASCII;
         when others       => Ada_Mode := Ada_Modbus.Slave.TCP;
      end case;

      Ptr := new Slave_Context_Wrapper;
      Ptr.C_Callbacks := Callbacks.all;

      --  Store callbacks globally for wrapper functions
      Global_Slave_Callbacks := Callbacks;

      Ptr.Config :=
        (Mode      => Ada_Mode,
         Unit_Id   => Ada_Modbus.Unit_Id (Unit_Id),
         Callbacks =>
           (Read_Coils               => Wrap_Read_Coils'Unrestricted_Access,
            Read_Discrete_Inputs     =>
              Wrap_Read_Discrete_Inputs'Unrestricted_Access,
            Read_Holding_Registers   =>
              Wrap_Read_Holding_Registers'Unrestricted_Access,
            Read_Input_Registers     =>
              Wrap_Read_Input_Registers'Unrestricted_Access,
            Write_Single_Coil        => Wrap_Write_Single_Coil'Unrestricted_Access,
            Write_Single_Register    =>
              Wrap_Write_Single_Register'Unrestricted_Access,
            Write_Multiple_Coils     =>
              Wrap_Write_Multiple_Coils'Unrestricted_Access,
            Write_Multiple_Registers =>
              Wrap_Write_Multiple_Registers'Unrestricted_Access,
            --  New FCs not yet exposed via C API
            Read_Exception_Status    => null,
            Diagnostics              => null,
            Report_Server_Id         => null,
            Mask_Write_Register      => null,
            Read_Write_Registers     => null));

      return To_Slave_Handle (Ptr);
   end Modbus_Slave_Create;

   procedure Modbus_Slave_Destroy (Handle : C_Slave_Handle) is
      Ptr : Slave_Context_Ptr := To_Slave_Ptr (Handle);
   begin
      if Ptr /= null then
         Global_Slave_Callbacks := null;
         Free_Slave (Ptr);
      end if;
   end Modbus_Slave_Destroy;

   function Modbus_Slave_Process
     (Handle          : C_Slave_Handle;
      Request         : access unsigned_char;
      Request_Length  : int;
      Response        : access unsigned_char;
      Response_Size   : int) return int
   is
      Ptr : constant Slave_Context_Ptr := To_Slave_Ptr (Handle);
      Req_Buf  : Byte_Array (0 .. Natural (Request_Length) - 1);
      Resp_Buf : Byte_Array (0 .. Natural (Response_Size) - 1);
      Resp_Len : Natural;
      Send_Resp : Boolean;

      type C_Byte_Array is array (Natural range <>) of aliased unsigned_char
        with Convention => C;
      type C_Byte_Ptr is access all C_Byte_Array (0 .. 259);
      function To_C_Ptr is new Ada.Unchecked_Conversion
        (System.Address, C_Byte_Ptr);

      C_Req : constant C_Byte_Ptr := To_C_Ptr (Request.all'Address);
      C_Resp : constant C_Byte_Ptr := To_C_Ptr (Response.all'Address);
   begin
      if Ptr = null or else Request = null or else Response = null then
         return -1;
      end if;

      --  Store callbacks for wrapper functions
      Global_Slave_Callbacks := Ptr.C_Callbacks'Unchecked_Access;

      --  Copy request
      for I in Req_Buf'Range loop
         Req_Buf (I) := Byte (C_Req (I));
      end loop;

      Ada_Modbus.Slave.Process_Request
        (Ptr.Config, Req_Buf, Natural (Request_Length),
         Resp_Buf, Resp_Len, Send_Resp);

      if not Send_Resp then
         return 0;  --  Broadcast, no response
      end if;

      --  Copy response
      for I in 0 .. Resp_Len - 1 loop
         C_Resp (I) := unsigned_char (Resp_Buf (I));
      end loop;

      return int (Resp_Len);
   end Modbus_Slave_Process;

   function Modbus_TCP_Receive_Frame
     (Handle      : C_TCP_Handle;
      Buffer      : access unsigned_char;
      Buffer_Size : int;
      Timeout_Ms  : int;
      Length      : access int) return C_Status
   is
      Ptr : constant TCP_Connection_Ptr := To_Ptr (Handle);
      Ada_Buf : Byte_Array (0 .. Natural (Buffer_Size) - 1);
      Ada_Len : Natural;
      Result : Status;

      type C_Byte_Array is array (Natural range <>) of aliased unsigned_char
        with Convention => C;
      type C_Byte_Ptr is access all C_Byte_Array (0 .. 259);
      function To_C_Ptr is new Ada.Unchecked_Conversion
        (System.Address, C_Byte_Ptr);
      C_Buf : constant C_Byte_Ptr := To_C_Ptr (Buffer.all'Address);
   begin
      if Ptr = null or else Buffer = null or else Length = null then
         return C_Invalid_Request;
      end if;

      Receive_Frame (Ptr.all, Ada_Buf, Ada_Len, Natural (Timeout_Ms), Result);

      if Result = Success then
         for I in 0 .. Ada_Len - 1 loop
            C_Buf (I) := unsigned_char (Ada_Buf (I));
         end loop;
         Length.all := int (Ada_Len);
      else
         Length.all := 0;
      end if;

      return To_C_Status (Result);
   end Modbus_TCP_Receive_Frame;

   function Modbus_TCP_Send_Frame
     (Handle : C_TCP_Handle;
      Frame  : access unsigned_char;
      Length : int) return C_Status
   is
      Ptr : constant TCP_Connection_Ptr := To_Ptr (Handle);
      Ada_Frame : Byte_Array (0 .. Natural (Length) - 1);
      Result : Status;

      type C_Byte_Array is array (Natural range <>) of aliased unsigned_char
        with Convention => C;
      type C_Byte_Ptr is access all C_Byte_Array (0 .. 259);
      function To_C_Ptr is new Ada.Unchecked_Conversion
        (System.Address, C_Byte_Ptr);
      C_Frame : constant C_Byte_Ptr := To_C_Ptr (Frame.all'Address);
   begin
      if Ptr = null or else Frame = null then
         return C_Invalid_Request;
      end if;

      for I in Ada_Frame'Range loop
         Ada_Frame (I) := Byte (C_Frame (I));
      end loop;

      Send_Frame (Ptr.all, Ada_Frame, Result);
      return To_C_Status (Result);
   end Modbus_TCP_Send_Frame;

   ---------------------
   --  Utility Functions
   ---------------------

   function Modbus_Status_String
     (Status : C_Status) return Interfaces.C.Strings.chars_ptr is
   begin
      if Status >= 0 and then Status <= 16 then
         return Interfaces.C.Strings.New_String
           (String (Status_Strings (Integer (Status))));
      else
         return Interfaces.C.Strings.New_String ("Unknown Status");
      end if;
   end Modbus_Status_String;

   function Modbus_Version return Interfaces.C.Strings.chars_ptr is
   begin
      return Interfaces.C.Strings.New_String
        (Version_Str (1 .. Version_Str'Length - 1));
   end Modbus_Version;

end Ada_Modbus.C_API;
