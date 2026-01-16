--  Main_Loopback - Vollständiges Loopback-Beispiel für Embedded
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Dieses Beispiel demonstriert Modbus ohne echtes Netzwerk.
--  Master und Slave kommunizieren über einen gemeinsamen Puffer.
--
--  QEMU-Ausführung:
--    qemu-system-arm -M lm3s6965evb -nographic -semihosting \
--      -kernel bin/main_loopback.elf

with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);
with Interfaces; use Interfaces;
with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.Slave;
with Ada_Modbus.Master;
with Loopback_Slave;
with Semihosting;

procedure Main_Loopback is

   ---------------------
   --  Loopback buffers
   ---------------------
   Request_Buffer  : Byte_Array (0 .. 260);
   Request_Length  : Natural := 0;
   Response_Buffer : Byte_Array (0 .. 260);
   Response_Length : Natural := 0;

   -------------------------
   --  Master Transport
   -------------------------

   type Null_Context is null record;
   Master_Transport : Null_Context;

   function Master_Send
     (Ctx  : in Out Null_Context;
      Data : Byte_Array) return Natural
   is
      pragma Unreferenced (Ctx);
   begin
      if Data'Length > Request_Buffer'Length then
         return 0;
      end if;
      for I in Data'Range loop
         Request_Buffer (I - Data'First) := Data (I);
      end loop;
      Request_Length := Data'Length;
      return Data'Length;
   end Master_Send;

   function Master_Receive
     (Ctx        : in Out Null_Context;
      Buffer     : out Byte_Array;
      Max_Length : Natural;
      Timeout_Ms : Natural) return Natural
   is
      pragma Unreferenced (Ctx, Timeout_Ms);
      Len : Natural;
   begin
      Len := Natural'Min (Response_Length, Max_Length);
      Len := Natural'Min (Len, Buffer'Length);

      for I in Buffer'Range loop
         Buffer (I) := 0;
      end loop;

      for I in 0 .. Len - 1 loop
         Buffer (Buffer'First + I) := Response_Buffer (I);
      end loop;
      return Len;
   end Master_Receive;

   Tick_Counter : Unsigned_32 := 0;

   function Get_Tick return Unsigned_32 is
   begin
      Tick_Counter := Tick_Counter + 1;
      return Tick_Counter;
   end Get_Tick;

   --  Instantiate Master
   package Loopback_Master is new Ada_Modbus.Master
     (Transport_Context => Null_Context,
      Send              => Master_Send,
      Receive           => Master_Receive,
      Get_Tick_Ms       => Get_Tick);

   Master_Ctx : Loopback_Master.Master_Context;

   ------------------
   --  Test Framework
   ------------------

   Test_Count   : Natural := 0;
   Pass_Count   : Natural := 0;
   Fail_Count   : Natural := 0;

   procedure Test_Start (Name : String) is
   begin
      Test_Count := Test_Count + 1;
      Semihosting.Put ("Test ");
      Semihosting.Put (Unsigned_32 (Test_Count));
      Semihosting.Put (": ");
      Semihosting.Put (Name);
      Semihosting.Put ("... ");

      --  Reset buffers
      Request_Length := 0;
      Response_Length := 0;
   end Test_Start;

   procedure Test_Pass is
   begin
      Pass_Count := Pass_Count + 1;
      Semihosting.Put_Line ("PASS");
   end Test_Pass;

   procedure Test_Fail (Reason : String) is
   begin
      Fail_Count := Fail_Count + 1;
      Semihosting.Put ("FAIL (");
      Semihosting.Put (Reason);
      Semihosting.Put_Line (")");
   end Test_Fail;

   --  Process loopback: Slave verarbeitet Request und erzeugt Response
   procedure Process_Loopback is
      Send_Response : Boolean;
   begin
      Semihosting.Put ("  Request length: ");
      Semihosting.Put (Unsigned_32 (Request_Length));
      Semihosting.New_Line;

      if Request_Length = 0 then
         Semihosting.Put_Line ("  No request to process");
         return;
      end if;

      Semihosting.Put ("  Request bytes: ");
      for I in 0 .. Natural'Min (Request_Length - 1, 15) loop
         Semihosting.Put_Hex (Unsigned_16 (Request_Buffer (I)));
         Semihosting.Put (" ");
      end loop;
      Semihosting.New_Line;

      --  Process request through slave
      Ada_Modbus.Slave.Process_Request
        (Loopback_Slave.Slave_Cfg,
         Request_Buffer,
         Request_Length,
         Response_Buffer,
         Response_Length,
         Send_Response);

      Semihosting.Put ("  Response length: ");
      Semihosting.Put (Unsigned_32 (Response_Length));
      Semihosting.New_Line;
   end Process_Loopback;

   ------------------
   --  Test Variables
   ------------------

   Values      : Register_Array (0 .. 9);
   Coil_Values : Coil_Array (0 .. 7);
   Result      : Status;
   pragma Unreferenced (Coil_Values);  --  Reserved for future coil tests
   pragma Warnings (Off, Result);  --  Result checked via data verification

begin
   --  Clear buffers
   for I in Request_Buffer'Range loop
      Request_Buffer (I) := 0;
   end loop;
   for I in Response_Buffer'Range loop
      Response_Buffer (I) := 0;
   end loop;
   for I in Values'Range loop
      Values (I) := 0;
   end loop;
   --  Coil_Values not initialized - reserved for future coil tests

   Semihosting.Put_Line ("=== Modbus Loopback Test ===");
   Semihosting.New_Line;

   --  Initialize slave data
   Loopback_Slave.Holding_Registers (0) := 16#1111#;
   Loopback_Slave.Holding_Registers (1) := 16#2222#;
   Loopback_Slave.Holding_Registers (2) := 16#3333#;
   Loopback_Slave.Input_Registers (0) := 16#AAAA#;
   Loopback_Slave.Input_Registers (1) := 16#BBBB#;
   Loopback_Slave.Coils (0) := True;
   Loopback_Slave.Coils (1) := False;
   Loopback_Slave.Coils (2) := True;

   Semihosting.Put_Line ("Initializing Master...");

   --  Initialize Master
   Loopback_Master.Initialize
     (Master_Ctx,
      (Mode => Loopback_Master.RTU, Default_Slave => 1, Default_Timeout => 1000),
      Master_Transport);

   Semihosting.Put_Line ("Master initialized.");

   --------------------------------
   --  Test 1: Write Single Register
   --------------------------------
   Test_Start ("Write Single Register (FC06)");

   Semihosting.Put_Line ("  Calling Write_Single_Register...");

   Result := Loopback_Master.Write_Single_Register
     (Master_Ctx, Slave => 1, Address => 10, Value => 16#DEAD#,
      Timeout_Ms => 1000);

   Semihosting.Put_Line ("  Master sent request.");

   Process_Loopback;

   Semihosting.Put_Line ("  Slave processed request.");

   if Loopback_Slave.Holding_Registers (10) = 16#DEAD# then
      Test_Pass;
   else
      Test_Fail ("register not updated");
   end if;

   --------------------------------
   --  Test 2: Read Holding Registers
   --------------------------------
   Test_Start ("Read Holding Registers (FC03)");

   Result := Loopback_Master.Read_Holding_Registers
     (Master_Ctx, Slave => 1, Start_Address => 0, Quantity => 3,
      Values => Values, Timeout_Ms => 1000);
   Process_Loopback;

   --  Second call to get response
   Result := Loopback_Master.Read_Holding_Registers
     (Master_Ctx, Slave => 1, Start_Address => 0, Quantity => 3,
      Values => Values, Timeout_Ms => 1000);
   Process_Loopback;

   if Values (0) = 16#1111# and Values (1) = 16#2222# and Values (2) = 16#3333# then
      Test_Pass;
   else
      Test_Fail ("wrong values");
   end if;

   --------------------------------
   --  Summary
   --------------------------------
   Semihosting.New_Line;
   Semihosting.Put_Line ("=== Test Summary ===");
   Semihosting.Put ("Passed: ");
   Semihosting.Put (Unsigned_32 (Pass_Count));
   Semihosting.New_Line;
   Semihosting.Put ("Failed: ");
   Semihosting.Put (Unsigned_32 (Fail_Count));
   Semihosting.New_Line;

   if Fail_Count = 0 then
      Semihosting.Put_Line ("All tests PASSED!");
   else
      Semihosting.Put_Line ("Some tests FAILED!");
   end if;

   --  Halt
   loop
      null;
   end loop;
end Main_Loopback;
