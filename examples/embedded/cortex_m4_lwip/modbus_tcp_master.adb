--  Modbus_TCP_Master - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with Interfaces; use Interfaces;
with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.Master;
with Ada_Modbus.Transport.LwIP; use Ada_Modbus.Transport.LwIP;

package body Modbus_TCP_Master is

   --  Connection handle
   type Connection_Access is access all TCP_Connection;
   Connection     : aliased TCP_Connection;
   Connection_Ptr : constant Connection_Access := Connection'Access;

   --  Transport callbacks for Master generic

   function Master_Send
     (Ctx  : in Out Connection_Access;
      Data : Byte_Array) return Natural
   is
   begin
      if Ctx = null then
         return 0;
      end if;
      return Send (Ctx.all, Data);
   end Master_Send;

   function Master_Receive
     (Ctx        : in Out Connection_Access;
      Buffer     : out Byte_Array;
      Max_Length : Natural;
      Timeout_Ms : Natural) return Natural
   is
   begin
      if Ctx = null then
         Buffer := [others => 0];
         return 0;
      end if;
      return Receive (Ctx.all, Buffer, Max_Length, Timeout_Ms);
   end Master_Receive;

   --  Tick counter for timeouts (should be updated by system timer)
   Tick_Counter : Unsigned_32 := 0
     with Volatile;

   function Get_Tick return Unsigned_32 is
   begin
      return Tick_Counter;
   end Get_Tick;

   --  Call this from SysTick handler or timer interrupt
   procedure Increment_Tick is
   begin
      Tick_Counter := Tick_Counter + 1;
   end Increment_Tick;

   --  Instantiate Master
   package LwIP_Master is new Ada_Modbus.Master
     (Transport_Context => Connection_Access,
      Send              => Master_Send,
      Receive           => Master_Receive,
      Get_Tick_Ms       => Get_Tick);

   Master_Ctx    : LwIP_Master.Master_Context;
   Is_Init       : Boolean := False;
   Connected     : Boolean := False;

   --------------------
   -- Modbus_Connect --
   --------------------

   procedure Modbus_Connect
     (IP_A, IP_B, IP_C, IP_D : Unsigned_8;
      Port                   : Unsigned_16;
      Result                 : out Status)
   is
      IP_Addr : constant Unsigned_32 := Make_IP_Addr (IP_A, IP_B, IP_C, IP_D);
   begin
      --  Disconnect if already connected
      if Connected then
         Disconnect (Connection);
         Connected := False;
      end if;

      --  Connect to slave
      Connect (Connection, IP_Addr, Port, Result);

      if Result = Success then
         --  Initialize master context
         LwIP_Master.Initialize
           (Master_Ctx,
            (Mode            => LwIP_Master.TCP,
             Default_Slave   => 1,
             Default_Timeout => 3000),
            Connection_Ptr);

         Is_Init := True;
         Connected := True;
      end if;
   end Modbus_Connect;

   -----------------------
   -- Modbus_Disconnect --
   -----------------------

   procedure Modbus_Disconnect is
   begin
      if Connected then
         Disconnect (Connection);
         Connected := False;
      end if;
   end Modbus_Disconnect;

   ------------------
   -- Is_Connected --
   ------------------

   function Is_Connected return Boolean is
   begin
      return Connected;
   end Is_Connected;

   -----------------------------
   -- Read_Holding_Registers --
   -----------------------------

   procedure Read_Holding_Registers
     (Slave_ID      : Unit_Id;
      Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array;
      Result        : out Status)
   is
   begin
      Values := [others => 0];

      if not Connected then
         Result := Frame_Error;
         return;
      end if;

      Result := LwIP_Master.Read_Holding_Registers
        (Master_Ctx, Slave_ID, Start_Address, Quantity, Values, 3000);
   end Read_Holding_Registers;

   ---------------------------
   -- Read_Input_Registers --
   ---------------------------

   procedure Read_Input_Registers
     (Slave_ID      : Unit_Id;
      Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array;
      Result        : out Status)
   is
   begin
      Values := [others => 0];

      if not Connected then
         Result := Frame_Error;
         return;
      end if;

      Result := LwIP_Master.Read_Input_Registers
        (Master_Ctx, Slave_ID, Start_Address, Quantity, Values, 3000);
   end Read_Input_Registers;

   ----------------------------
   -- Write_Single_Register --
   ----------------------------

   procedure Write_Single_Register
     (Slave_ID : Unit_Id;
      Address  : Register_Address;
      Value    : Register_Value;
      Result   : out Status)
   is
   begin
      if not Connected then
         Result := Frame_Error;
         return;
      end if;

      Result := LwIP_Master.Write_Single_Register
        (Master_Ctx, Slave_ID, Address, Value, 3000);
   end Write_Single_Register;

   -------------------------------
   -- Write_Multiple_Registers --
   -------------------------------

   procedure Write_Multiple_Registers
     (Slave_ID      : Unit_Id;
      Start_Address : Register_Address;
      Values        : Register_Array;
      Result        : out Status)
   is
   begin
      if not Connected then
         Result := Frame_Error;
         return;
      end if;

      Result := LwIP_Master.Write_Multiple_Registers
        (Master_Ctx, Slave_ID, Start_Address, Values, 3000);
   end Write_Multiple_Registers;

   ----------------
   -- Read_Coils --
   ----------------

   procedure Read_Coils
     (Slave_ID      : Unit_Id;
      Start_Address : Coil_Address;
      Quantity      : Coil_Count;
      Values        : out Coil_Array;
      Result        : out Status)
   is
   begin
      Values := [others => False];

      if not Connected then
         Result := Frame_Error;
         return;
      end if;

      Result := LwIP_Master.Read_Coils
        (Master_Ctx, Slave_ID, Start_Address, Quantity, Values, 3000);
   end Read_Coils;

   -----------------------
   -- Write_Single_Coil --
   -----------------------

   procedure Write_Single_Coil
     (Slave_ID : Unit_Id;
      Address  : Coil_Address;
      Value    : Coil_Value;
      Result   : out Status)
   is
   begin
      if not Connected then
         Result := Frame_Error;
         return;
      end if;

      Result := LwIP_Master.Write_Single_Coil
        (Master_Ctx, Slave_ID, Address, Value, 3000);
   end Write_Single_Coil;

end Modbus_TCP_Master;
