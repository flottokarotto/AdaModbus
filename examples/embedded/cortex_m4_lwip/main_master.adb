--  Main_Master - Modbus TCP Master Beispiel für Cortex-M
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Demonstriert einen Modbus TCP Master (Client), der sich mit einem
--  Slave verbindet und Register liest/schreibt.
--
--  Konfiguration:
--    - Slave IP-Adresse im Code anpassen (siehe SLAVE_IP_*)
--    - Standardmäßig: 192.168.1.100:502
--
--  QEMU mit Netzwerk:
--    qemu-system-arm -M netduino2 -nographic -semihosting \
--      -netdev user,id=net0,hostfwd=tcp::5502-:502 \
--      -device virtio-net-device,netdev=net0 \
--      -kernel bin/main_master.elf

with Last_Chance_Handler;  pragma Unreferenced (Last_Chance_Handler);
with Interfaces; use Interfaces;
with Ada_Modbus; use Ada_Modbus;
with Modbus_TCP_Master;
with Semihosting;

procedure Main_Master is

   --  Target Slave Configuration
   SLAVE_IP_A : constant Unsigned_8 := 192;
   SLAVE_IP_B : constant Unsigned_8 := 168;
   SLAVE_IP_C : constant Unsigned_8 := 1;
   SLAVE_IP_D : constant Unsigned_8 := 100;
   SLAVE_PORT : constant Unsigned_16 := 502;
   SLAVE_ID   : constant Unit_Id := 1;

   --  Delay zwischen Requests (in Busy-Wait-Zyklen)
   procedure Delay_Cycles (Count : Natural) is
   begin
      for I in 1 .. Count loop
         null;
      end loop;
   end Delay_Cycles;

   --  Status ausgeben
   procedure Print_Status (S : Status) is
   begin
      case S is
         when Success =>
            Semihosting.Put ("OK");
         when Timeout =>
            Semihosting.Put ("TIMEOUT");
         when Frame_Error =>
            Semihosting.Put ("FRAME_ERROR");
         when CRC_Error =>
            Semihosting.Put ("CRC_ERROR");
         when Invalid_Response =>
            Semihosting.Put ("INVALID_RESPONSE");
         when Exception_Illegal_Function =>
            Semihosting.Put ("ILLEGAL_FUNCTION");
         when Exception_Illegal_Address =>
            Semihosting.Put ("ILLEGAL_ADDRESS");
         when Exception_Illegal_Value =>
            Semihosting.Put ("ILLEGAL_VALUE");
         when Exception_Server_Failure =>
            Semihosting.Put ("SERVER_FAILURE");
         when others =>
            Semihosting.Put ("ERROR");
      end case;
   end Print_Status;

   Result    : Status;
   Values    : Register_Array (0 .. 9);
   Coils     : Coil_Array (0 .. 7);
   Cycle     : Unsigned_32 := 0;

begin
   Semihosting.Put_Line ("=== Modbus TCP Master ===");
   Semihosting.New_Line;

   --  Verbindung zum Slave herstellen
   Semihosting.Put ("Connecting to ");
   Semihosting.Put (Unsigned_32 (SLAVE_IP_A));
   Semihosting.Put (".");
   Semihosting.Put (Unsigned_32 (SLAVE_IP_B));
   Semihosting.Put (".");
   Semihosting.Put (Unsigned_32 (SLAVE_IP_C));
   Semihosting.Put (".");
   Semihosting.Put (Unsigned_32 (SLAVE_IP_D));
   Semihosting.Put (":");
   Semihosting.Put (Unsigned_32 (SLAVE_PORT));
   Semihosting.Put ("... ");

   Modbus_TCP_Master.Modbus_Connect
     (SLAVE_IP_A, SLAVE_IP_B, SLAVE_IP_C, SLAVE_IP_D,
      SLAVE_PORT, Result);

   if Result /= Success then
      Print_Status (Result);
      Semihosting.New_Line;
      Semihosting.Put_Line ("Connection failed! Halting.");
      loop
         null;
      end loop;
   end if;

   Semihosting.Put_Line ("Connected!");
   Semihosting.New_Line;

   --  Haupt-Polling-Loop
   Semihosting.Put_Line ("Starting polling loop...");
   Semihosting.New_Line;

   loop
      Cycle := Cycle + 1;
      Semihosting.Put ("--- Cycle ");
      Semihosting.Put (Cycle);
      Semihosting.Put_Line (" ---");

      --  1. Read Holding Registers (FC03)
      Semihosting.Put ("  Read Holding Regs 0-4: ");
      Modbus_TCP_Master.Read_Holding_Registers
        (SLAVE_ID, Start_Address => 0, Quantity => 5, Values => Values, Result => Result);

      if Result = Success then
         Semihosting.Put ("[");
         for I in 0 .. 4 loop
            Semihosting.Put_Hex (Values (I));
            if I < 4 then
               Semihosting.Put (" ");
            end if;
         end loop;
         Semihosting.Put_Line ("]");
      else
         Print_Status (Result);
         Semihosting.New_Line;
      end if;

      --  2. Read Input Registers (FC04)
      Semihosting.Put ("  Read Input Regs 0-1: ");
      Modbus_TCP_Master.Read_Input_Registers
        (SLAVE_ID, Start_Address => 0, Quantity => 2, Values => Values, Result => Result);

      if Result = Success then
         Semihosting.Put ("[");
         Semihosting.Put_Hex (Values (0));
         Semihosting.Put (" ");
         Semihosting.Put_Hex (Values (1));
         Semihosting.Put_Line ("]");
      else
         Print_Status (Result);
         Semihosting.New_Line;
      end if;

      --  3. Write Single Register (FC06)
      Semihosting.Put ("  Write Reg 10 = ");
      Semihosting.Put_Hex (Unsigned_16 (Cycle mod 65536));
      Semihosting.Put (": ");
      Modbus_TCP_Master.Write_Single_Register
        (SLAVE_ID, Address => 10, Value => Register_Value (Cycle mod 65536), Result => Result);
      Print_Status (Result);
      Semihosting.New_Line;

      --  4. Read Coils (FC01)
      Semihosting.Put ("  Read Coils 0-7: ");
      Modbus_TCP_Master.Read_Coils
        (SLAVE_ID, Start_Address => 0, Quantity => 8, Values => Coils, Result => Result);

      if Result = Success then
         Semihosting.Put ("[");
         for I in 0 .. 7 loop
            if Coils (I) then
               Semihosting.Put ("1");
            else
               Semihosting.Put ("0");
            end if;
         end loop;
         Semihosting.Put_Line ("]");
      else
         Print_Status (Result);
         Semihosting.New_Line;
      end if;

      --  5. Write Single Coil (FC05)
      declare
         Coil_State : constant Boolean := (Cycle mod 2) = 0;
      begin
         Semihosting.Put ("  Write Coil 0 = ");
         if Coil_State then
            Semihosting.Put ("ON");
         else
            Semihosting.Put ("OFF");
         end if;
         Semihosting.Put (": ");
         Modbus_TCP_Master.Write_Single_Coil
           (SLAVE_ID, Address => 0, Value => Coil_State, Result => Result);
         Print_Status (Result);
         Semihosting.New_Line;
      end;

      Semihosting.New_Line;

      --  Pause zwischen Zyklen
      Delay_Cycles (1_000_000);

      --  Nach 10 Zyklen stoppen (für Demo)
      if Cycle >= 10 then
         Semihosting.Put_Line ("Demo complete. Disconnecting...");
         Modbus_TCP_Master.Modbus_Disconnect;
         Semihosting.Put_Line ("Done.");
         exit;
      end if;
   end loop;

   --  Halt
   loop
      null;
   end loop;
end Main_Master;
