--  Ada_Modbus.C_API.Serial - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with Ada.Calendar;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;

with Ada_Modbus.Transport.Serial;
with Ada_Modbus.Master;

package body Ada_Modbus.C_API.Serial is

   use Ada_Modbus.Transport.Serial;

   --  Internal serial connection wrapper
   type Serial_Context is record
      Connection : aliased Serial_Connection;
   end record;

   type Serial_Context_Access is access all Serial_Context;

   procedure Free is new Ada.Unchecked_Deallocation
     (Serial_Context, Serial_Context_Access);

   function To_Handle is new Ada.Unchecked_Conversion
     (Serial_Context_Access, C_Serial_Handle);

   function To_Context is new Ada.Unchecked_Conversion
     (C_Serial_Handle, Serial_Context_Access);

   --  RTU Master context wrapper
   type RTU_Context;
   type RTU_Context_Access is access all RTU_Context;

   --  Transport callbacks for RTU master
   function RTU_Send
     (Ctx  : in out RTU_Context_Access;
      Data : Byte_Array) return Natural;

   function RTU_Receive
     (Ctx        : in out RTU_Context_Access;
      Buffer     : out Byte_Array;
      Max_Length : Natural;
      Timeout_Ms : Natural) return Natural;

   function RTU_Get_Tick return Unsigned_32;

   package RTU_Master is new Ada_Modbus.Master
     (Transport_Context => RTU_Context_Access,
      Send              => RTU_Send,
      Receive           => RTU_Receive,
      Get_Tick_Ms       => RTU_Get_Tick);

   type RTU_Context is record
      Serial : Serial_Context_Access;
      Master : RTU_Master.Master_Context;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation
     (RTU_Context, RTU_Context_Access);

   function To_RTU_Handle is new Ada.Unchecked_Conversion
     (RTU_Context_Access, C_RTU_Master_Handle);

   function To_RTU_Context is new Ada.Unchecked_Conversion
     (C_RTU_Master_Handle, RTU_Context_Access);

   --------------
   -- RTU_Send --
   --------------

   function RTU_Send
     (Ctx  : in out RTU_Context_Access;
      Data : Byte_Array) return Natural
   is
   begin
      return Send (Ctx.Serial.Connection, Data);
   end RTU_Send;

   -----------------
   -- RTU_Receive --
   -----------------

   function RTU_Receive
     (Ctx        : in out RTU_Context_Access;
      Buffer     : out Byte_Array;
      Max_Length : Natural;
      Timeout_Ms : Natural) return Natural
   is
   begin
      return Receive (Ctx.Serial.Connection, Buffer, Max_Length, Timeout_Ms);
   end RTU_Receive;

   ------------------
   -- RTU_Get_Tick --
   ------------------

   function RTU_Get_Tick return Unsigned_32 is
      use Ada.Calendar;
      Seconds : constant Day_Duration := Ada.Calendar.Seconds (Clock);
   begin
      return Unsigned_32 (Seconds * 1000.0) mod Unsigned_32'Last;
   end RTU_Get_Tick;

   ---------------------------
   -- Modbus_Serial_Create --
   ---------------------------

   function Modbus_Serial_Create return C_Serial_Handle is
      Ctx : constant Serial_Context_Access := new Serial_Context;
   begin
      return To_Handle (Ctx);
   end Modbus_Serial_Create;

   ----------------------------
   -- Modbus_Serial_Destroy --
   ----------------------------

   procedure Modbus_Serial_Destroy (Handle : C_Serial_Handle) is
      Ctx : Serial_Context_Access := To_Context (Handle);
   begin
      if Ctx /= null then
         if State (Ctx.Connection) = Open then
            Close (Ctx.Connection);
         end if;
         Free (Ctx);
      end if;
   end Modbus_Serial_Destroy;

   -------------------------
   -- Modbus_Serial_Open --
   -------------------------

   function Modbus_Serial_Open
     (Handle    : C_Serial_Handle;
      Port      : Interfaces.C.Strings.chars_ptr;
      Baud      : int;
      Data_Bits : int;
      Parity    : int;
      Stop_Bits : int) return C_Status
   is
      use Interfaces.C.Strings;
      Ctx       : constant Serial_Context_Access := To_Context (Handle);
      Port_Name : constant String := Value (Port);
      Config    : Serial_Config;
      Result    : Status;
   begin
      if Ctx = null then
         return C_Invalid_Request;
      end if;

      --  Convert baud rate
      Config.Rate := (case Baud is
                         when 1200   => B1200,
                         when 2400   => B2400,
                         when 4800   => B4800,
                         when 9600   => B9600,
                         when 19200  => B19200,
                         when 38400  => B38400,
                         when 57600  => B57600,
                         when 115200 => B115200,
                         when others => B9600);

      --  Convert data bits
      Config.Data_Bits := (if Data_Bits = 7 then Seven else Eight);

      --  Convert parity
      Config.Parity := (case Parity is
                           when C_Parity_Even => Even,
                           when C_Parity_Odd  => Odd,
                           when others        => None);

      --  Convert stop bits
      Config.Stop_Bits := (if Stop_Bits = 2 then Two else One);

      Open (Ctx.Connection, Port_Name, Config, Result);

      if Result = Success then
         return C_Success;
      else
         return C_Frame_Error;  --  Generic connection error
      end if;
   end Modbus_Serial_Open;

   --------------------------
   -- Modbus_Serial_Close --
   --------------------------

   procedure Modbus_Serial_Close (Handle : C_Serial_Handle) is
      Ctx : constant Serial_Context_Access := To_Context (Handle);
   begin
      if Ctx /= null then
         Close (Ctx.Connection);
      end if;
   end Modbus_Serial_Close;

   --------------------------
   -- Modbus_Serial_State --
   --------------------------

   function Modbus_Serial_State (Handle : C_Serial_Handle) return int is
      Ctx : constant Serial_Context_Access := To_Context (Handle);
   begin
      if Ctx = null then
         return C_Serial_Error;
      end if;

      return (case State (Ctx.Connection) is
                 when Closed => C_Serial_Disconnected,
                 when Open   => C_Serial_Connected,
                 when Error  => C_Serial_Error);
   end Modbus_Serial_State;

   -------------------------------
   -- Modbus_Serial_Last_Error --
   -------------------------------

   function Modbus_Serial_Last_Error
     (Handle : C_Serial_Handle) return Interfaces.C.Strings.chars_ptr
   is
      use Interfaces.C.Strings;
      Ctx : constant Serial_Context_Access := To_Context (Handle);
   begin
      if Ctx = null then
         return New_String ("Invalid handle");
      end if;
      return New_String (Last_Error (Ctx.Connection));
   end Modbus_Serial_Last_Error;

   -------------------------------
   -- Modbus_RTU_Master_Create --
   -------------------------------

   function Modbus_RTU_Master_Create
     (Serial_Handle : C_Serial_Handle;
      Unit_Id       : unsigned_char;
      Timeout_Ms    : int) return C_RTU_Master_Handle
   is
      Serial_Ctx : constant Serial_Context_Access := To_Context (Serial_Handle);
      RTU_Ctx    : RTU_Context_Access;
   begin
      if Serial_Ctx = null then
         return System.Null_Address;
      end if;

      RTU_Ctx := new RTU_Context;
      RTU_Ctx.Serial := Serial_Ctx;

      RTU_Master.Initialize
        (RTU_Ctx.Master,
         (Mode            => RTU_Master.RTU,
          Default_Slave   => Ada_Modbus.Unit_Id (Unit_Id),
          Default_Timeout => Natural (Timeout_Ms)),
         RTU_Ctx);

      return To_RTU_Handle (RTU_Ctx);
   end Modbus_RTU_Master_Create;

   --------------------------------
   -- Modbus_RTU_Master_Destroy --
   --------------------------------

   procedure Modbus_RTU_Master_Destroy (Handle : C_RTU_Master_Handle) is
      Ctx : RTU_Context_Access := To_RTU_Context (Handle);
   begin
      if Ctx /= null then
         Free (Ctx);
      end if;
   end Modbus_RTU_Master_Destroy;

   ---------------------------------------
   -- Modbus_RTU_Read_Holding_Registers --
   ---------------------------------------

   function Modbus_RTU_Read_Holding_Registers
     (Handle        : C_RTU_Master_Handle;
      Slave         : unsigned_char;
      Start_Address : unsigned_short;
      Quantity      : unsigned_short;
      Values        : access unsigned_short;
      Timeout_Ms    : int) return C_Status
   is
      pragma Unreferenced (Timeout_Ms);
      Ctx    : constant RTU_Context_Access := To_RTU_Context (Handle);
      Regs   : Register_Array (0 .. Natural (Quantity) - 1);
      Result : Status;

      type U16_Array is array (Natural range <>) of aliased unsigned_short
        with Convention => C;
      type U16_Ptr is access all unsigned_short with Convention => C;

      function To_Array is new Ada.Unchecked_Conversion (U16_Ptr, System.Address);
   begin
      if Ctx = null then
         return C_Invalid_Request;
      end if;

      Result := RTU_Master.Read_Holding_Registers
        (Ctx.Master,
         Unit_Id (Slave),
         Register_Address (Start_Address),
         Register_Count (Quantity),
         Regs);

      if Result = Success then
         --  Copy results to C array
         declare
            C_Array : U16_Array (0 .. Natural (Quantity) - 1)
              with Import, Address => To_Array (U16_Ptr (Values));
         begin
            for I in Regs'Range loop
               C_Array (I) := unsigned_short (Regs (I));
            end loop;
         end;
         return C_Success;
      else
         return C_Status (Status'Pos (Result));
      end if;
   end Modbus_RTU_Read_Holding_Registers;

   -------------------------------------
   -- Modbus_RTU_Read_Input_Registers --
   -------------------------------------

   function Modbus_RTU_Read_Input_Registers
     (Handle        : C_RTU_Master_Handle;
      Slave         : unsigned_char;
      Start_Address : unsigned_short;
      Quantity      : unsigned_short;
      Values        : access unsigned_short;
      Timeout_Ms    : int) return C_Status
   is
      pragma Unreferenced (Timeout_Ms);
      Ctx    : constant RTU_Context_Access := To_RTU_Context (Handle);
      Regs   : Register_Array (0 .. Natural (Quantity) - 1);
      Result : Status;

      type U16_Array is array (Natural range <>) of aliased unsigned_short
        with Convention => C;
      type U16_Ptr is access all unsigned_short with Convention => C;

      function To_Array is new Ada.Unchecked_Conversion (U16_Ptr, System.Address);
   begin
      if Ctx = null then
         return C_Invalid_Request;
      end if;

      Result := RTU_Master.Read_Input_Registers
        (Ctx.Master,
         Unit_Id (Slave),
         Register_Address (Start_Address),
         Register_Count (Quantity),
         Regs);

      if Result = Success then
         declare
            C_Array : U16_Array (0 .. Natural (Quantity) - 1)
              with Import, Address => To_Array (U16_Ptr (Values));
         begin
            for I in Regs'Range loop
               C_Array (I) := unsigned_short (Regs (I));
            end loop;
         end;
         return C_Success;
      else
         return C_Status (Status'Pos (Result));
      end if;
   end Modbus_RTU_Read_Input_Registers;

   --------------------------------------
   -- Modbus_RTU_Write_Single_Register --
   --------------------------------------

   function Modbus_RTU_Write_Single_Register
     (Handle     : C_RTU_Master_Handle;
      Slave      : unsigned_char;
      Address    : unsigned_short;
      Value      : unsigned_short;
      Timeout_Ms : int) return C_Status
   is
      pragma Unreferenced (Timeout_Ms);
      Ctx    : constant RTU_Context_Access := To_RTU_Context (Handle);
      Result : Status;
   begin
      if Ctx = null then
         return C_Invalid_Request;
      end if;

      Result := RTU_Master.Write_Single_Register
        (Ctx.Master,
         Unit_Id (Slave),
         Register_Address (Address),
         Register_Value (Value));

      return C_Status (Status'Pos (Result));
   end Modbus_RTU_Write_Single_Register;

   -----------------------------------------
   -- Modbus_RTU_Write_Multiple_Registers --
   -----------------------------------------

   function Modbus_RTU_Write_Multiple_Registers
     (Handle        : C_RTU_Master_Handle;
      Slave         : unsigned_char;
      Start_Address : unsigned_short;
      Quantity      : unsigned_short;
      Values        : access unsigned_short;
      Timeout_Ms    : int) return C_Status
   is
      pragma Unreferenced (Timeout_Ms);
      Ctx    : constant RTU_Context_Access := To_RTU_Context (Handle);
      Regs   : Register_Array (0 .. Natural (Quantity) - 1);
      Result : Status;

      type U16_Array is array (Natural range <>) of aliased unsigned_short
        with Convention => C;
      type U16_Ptr is access all unsigned_short with Convention => C;

      function To_Array is new Ada.Unchecked_Conversion (U16_Ptr, System.Address);
   begin
      if Ctx = null then
         return C_Invalid_Request;
      end if;

      --  Copy from C array
      declare
         C_Array : U16_Array (0 .. Natural (Quantity) - 1)
           with Import, Address => To_Array (U16_Ptr (Values));
      begin
         for I in Regs'Range loop
            Regs (I) := Register_Value (C_Array (I));
         end loop;
      end;

      Result := RTU_Master.Write_Multiple_Registers
        (Ctx.Master,
         Unit_Id (Slave),
         Register_Address (Start_Address),
         Regs);

      return C_Status (Status'Pos (Result));
   end Modbus_RTU_Write_Multiple_Registers;

   --------------------------
   -- Modbus_RTU_Read_Coils --
   --------------------------

   function Modbus_RTU_Read_Coils
     (Handle        : C_RTU_Master_Handle;
      Slave         : unsigned_char;
      Start_Address : unsigned_short;
      Quantity      : unsigned_short;
      Values        : access unsigned_char;
      Timeout_Ms    : int) return C_Status
   is
      pragma Unreferenced (Timeout_Ms);
      Ctx    : constant RTU_Context_Access := To_RTU_Context (Handle);
      Coils  : Coil_Array (0 .. Natural (Quantity) - 1);
      Result : Status;

      type U8_Array is array (Natural range <>) of aliased unsigned_char
        with Convention => C;
      type U8_Ptr is access all unsigned_char with Convention => C;

      function To_Array is new Ada.Unchecked_Conversion (U8_Ptr, System.Address);
   begin
      if Ctx = null then
         return C_Invalid_Request;
      end if;

      Result := RTU_Master.Read_Coils
        (Ctx.Master,
         Unit_Id (Slave),
         Coil_Address (Start_Address),
         Coil_Count (Quantity),
         Coils);

      if Result = Success then
         --  Pack booleans into bytes
         declare
            Num_Bytes : constant Natural := (Natural (Quantity) + 7) / 8;
            C_Array   : U8_Array (0 .. Num_Bytes - 1)
              with Import, Address => To_Array (U8_Ptr (Values));
            Bit_Idx   : Natural := 0;
            Byte_Idx  : Natural := 0;
            Byte_Val  : unsigned_char := 0;
         begin
            C_Array := [others => 0];
            for I in Coils'Range loop
               if Coils (I) then
                  Byte_Val := Byte_Val or unsigned_char (2 ** Bit_Idx);
               end if;
               Bit_Idx := Bit_Idx + 1;
               if Bit_Idx = 8 then
                  C_Array (Byte_Idx) := Byte_Val;
                  Byte_Idx := Byte_Idx + 1;
                  Bit_Idx := 0;
                  Byte_Val := 0;
               end if;
            end loop;
            if Bit_Idx > 0 then
               C_Array (Byte_Idx) := Byte_Val;
            end if;
         end;
         return C_Success;
      else
         return C_Status (Status'Pos (Result));
      end if;
   end Modbus_RTU_Read_Coils;

   ---------------------------------
   -- Modbus_RTU_Write_Single_Coil --
   ---------------------------------

   function Modbus_RTU_Write_Single_Coil
     (Handle     : C_RTU_Master_Handle;
      Slave      : unsigned_char;
      Address    : unsigned_short;
      Value      : int;
      Timeout_Ms : int) return C_Status
   is
      pragma Unreferenced (Timeout_Ms);
      Ctx    : constant RTU_Context_Access := To_RTU_Context (Handle);
      Result : Status;
   begin
      if Ctx = null then
         return C_Invalid_Request;
      end if;

      Result := RTU_Master.Write_Single_Coil
        (Ctx.Master,
         Unit_Id (Slave),
         Coil_Address (Address),
         Value /= 0);

      return C_Status (Status'Pos (Result));
   end Modbus_RTU_Write_Single_Coil;

end Ada_Modbus.C_API.Serial;
