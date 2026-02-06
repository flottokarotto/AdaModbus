--  KSEM_Client - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with Config;
with HAL_Stubs;

package body KSEM_Client is

   --  KSEM SunSpec Register Addresses
   --  Based on SunSpec Model 203 (Three Phase Wye Meter)
   --  Layout: [2-reg header] [data block: 105 regs]
   --  Offsets include the 2-register model header (ID + Length)
   --  Note: For KSEM firmware >= 2.6.0, base address is 40070 instead
   KSEM_Base_Address : constant Register_Address := 40069;  --  Model 203 header
   Reg_Total_Power   : constant Register_Address := KSEM_Base_Address + 18;  --  W (INT16 * SF)
   Reg_Phase_Power_A : constant Register_Address := KSEM_Base_Address + 19;  --  WphA
   pragma Unreferenced (Reg_Phase_Power_A);  --  Read as consecutive block from Reg_Total_Power
   Reg_Power_SF      : constant Register_Address := KSEM_Base_Address + 22;  --  W_SF
   pragma Unreferenced (Reg_Power_SF);  --  Read as consecutive block from Reg_Total_Power

   --  Internal state
   Connected    : Boolean := False;
   Last_Power   : Power_Data := (0, 0, 0, 0, False);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Connected := False;
      Last_Power := (0, 0, 0, 0, False);
   end Initialize;

   -------------
   -- Connect --
   -------------

   procedure Connect (Result : out Status) is
   begin
      --  Connect via LwIP TCP
      HAL_Stubs.TCP_Connect
        (Config.KSEM_IP_A,
         Config.KSEM_IP_B,
         Config.KSEM_IP_C,
         Config.KSEM_IP_D,
         Config.KSEM_Port,
         Config.Modbus_Timeout_Ms,
         Result);

      if Result = Success then
         Connected := True;
      else
         Connected := False;
      end if;
   end Connect;

   ----------------
   -- Disconnect --
   ----------------

   procedure Disconnect is
   begin
      if Connected then
         HAL_Stubs.TCP_Disconnect;
         Connected := False;
      end if;
   end Disconnect;

   ------------------
   -- Is_Connected --
   ------------------

   function Is_Connected return Boolean is
   begin
      return Connected;
   end Is_Connected;

   ----------------
   -- Read_Power --
   ----------------

   procedure Read_Power (Data : out Power_Data; Result : out Status) is
      --  Read 5 registers: W, WphA, WphB, WphC, W_SF
      Values : Register_Array (0 .. 4);
      Count  : Natural;
      Scale_Factor : Integer_16;
      Raw_Total    : Integer_16;
      Raw_L1, Raw_L2, Raw_L3 : Integer_16;
   begin
      Data := (0, 0, 0, 0, False);

      if not Connected then
         Result := Invalid_Request;
         return;
      end if;

      --  Read power registers from SunSpec Model 203
      --  Registers: W(+0), WphA(+1), WphB(+2), WphC(+3), W_SF(+4)
      --  KSEM uses FC 03 (Read Holding Registers) for SunSpec data
      HAL_Stubs.Modbus_TCP_Read_Holding_Registers
        (Unit_Id       => 1,
         Start_Address => Reg_Total_Power,
         Quantity      => 5,
         Values        => Values,
         Count         => Count,
         Result        => Result);

      if Result = Success and then Count >= 5 then
         --  Extract scale factor (register 4 = W_SF)
         Scale_Factor := Integer_16 (Values (4));

         --  Extract raw values (signed INT16)
         Raw_Total := Integer_16 (Values (0));
         Raw_L1    := Integer_16 (Values (1));
         Raw_L2    := Integer_16 (Values (2));
         Raw_L3    := Integer_16 (Values (3));

         --  Apply scale factor (typically 0 or -1)
         --  SF=0: value in Watts
         --  SF=-1: value * 0.1 = value / 10
         --  SF=1: value * 10
         if Scale_Factor = 0 then
            Data.Total_Power_W := Integer_32 (Raw_Total);
            Data.Phase_L1_W    := Integer_32 (Raw_L1);
            Data.Phase_L2_W    := Integer_32 (Raw_L2);
            Data.Phase_L3_W    := Integer_32 (Raw_L3);
         elsif Scale_Factor = -1 then
            Data.Total_Power_W := Integer_32 (Raw_Total) / 10;
            Data.Phase_L1_W    := Integer_32 (Raw_L1) / 10;
            Data.Phase_L2_W    := Integer_32 (Raw_L2) / 10;
            Data.Phase_L3_W    := Integer_32 (Raw_L3) / 10;
         elsif Scale_Factor = 1 then
            Data.Total_Power_W := Integer_32 (Raw_Total) * 10;
            Data.Phase_L1_W    := Integer_32 (Raw_L1) * 10;
            Data.Phase_L2_W    := Integer_32 (Raw_L2) * 10;
            Data.Phase_L3_W    := Integer_32 (Raw_L3) * 10;
         else
            --  Unsupported scale factor, use raw values
            Data.Total_Power_W := Integer_32 (Raw_Total);
            Data.Phase_L1_W    := Integer_32 (Raw_L1);
            Data.Phase_L2_W    := Integer_32 (Raw_L2);
            Data.Phase_L3_W    := Integer_32 (Raw_L3);
         end if;

         Data.Valid := True;
         Last_Power := Data;
      else
         Data.Valid := False;
      end if;
   end Read_Power;

   --------------------
   -- Get_Last_Power --
   --------------------

   function Get_Last_Power return Power_Data is
   begin
      return Last_Power;
   end Get_Last_Power;

end KSEM_Client;
