--  LM3S_UART - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  LM3S6965 UART driver for QEMU emulation.
--  Reference: LM3S6965 Datasheet (DS-LM3S6965-10503)

with System.Storage_Elements; use System.Storage_Elements;
with System;

package body LM3S_UART is

   --  UART base addresses
   UART0_Base : constant Integer_Address := 16#4000_C000#;
   UART1_Base : constant Integer_Address := 16#4000_D000#;
   UART2_Base : constant Integer_Address := 16#4000_E000#;

   --  UART Register offsets
   UARTDR_Offset   : constant := 16#000#;  --  Data Register
   UARTFR_Offset   : constant := 16#018#;  --  Flag Register
   UARTIBRD_Offset : constant := 16#024#;  --  Integer Baud Rate Divisor
   UARTFBRD_Offset : constant := 16#028#;  --  Fractional Baud Rate Divisor
   UARTLCRH_Offset : constant := 16#02C#;  --  Line Control Register
   UARTCTL_Offset  : constant := 16#030#;  --  Control Register

   --  Flag Register bits
   FR_TXFE : constant := 16#80#;  --  TX FIFO Empty
   FR_TXFF : constant := 16#20#;  --  TX FIFO Full
   FR_RXFE : constant := 16#10#;  --  RX FIFO Empty
   FR_BUSY : constant := 16#08#;  --  UART Busy

   --  Line Control Register bits
   LCRH_WLEN_8 : constant := 16#60#;  --  8 bit word length
   LCRH_FEN    : constant := 16#10#;  --  Enable FIFOs

   --  Control Register bits
   CTL_UARTEN : constant := 16#0001#;  --  UART Enable
   CTL_TXE    : constant := 16#0100#;  --  TX Enable
   CTL_RXE    : constant := 16#0200#;  --  RX Enable

   --  Register access type
   type Reg32 is new Unsigned_32 with Volatile;

   --  Get base address for UART
   function Get_Base (Port : UART_Peripheral) return Integer_Address is
   begin
      case Port is
         when UART0 => return UART0_Base;
         when UART1 => return UART1_Base;
         when UART2 => return UART2_Base;
      end case;
   end Get_Base;

   --  Read register
   function Read_Reg (Base : Integer_Address; Offset : Integer_Address)
     return Unsigned_32
   is
      Reg : Reg32 with Import, Address => To_Address (Base + Offset);
   begin
      return Unsigned_32 (Reg);
   end Read_Reg;

   --  Write register
   procedure Write_Reg (Base : Integer_Address; Offset : Integer_Address;
                        Value : Unsigned_32)
   is
      Reg : Reg32 with Import, Address => To_Address (Base + Offset);
   begin
      Reg := Reg32 (Value);
   end Write_Reg;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Port : UART_Peripheral; Rate : Baud_Rate) is
      Base : constant Integer_Address := Get_Base (Port);
      IBRD_Value : Unsigned_32;
      FBRD_Value : Unsigned_32;
   begin
      --  Disable UART first
      Write_Reg (Base, UARTCTL_Offset, 0);

      --  Calculate baud rate divisor
      --  Assuming ~16 MHz UART clock (QEMU default for lm3s)
      --  BRD = 16000000 / (16 * baud) = 1000000 / baud
      case Rate is
         when B9600 =>
            IBRD_Value := 104;   --  1000000 / 9600 = 104.17
            FBRD_Value := 11;    --  0.17 * 64 = ~11
         when B19200 =>
            IBRD_Value := 52;    --  1000000 / 19200 = 52.08
            FBRD_Value := 5;
         when B38400 =>
            IBRD_Value := 26;    --  1000000 / 38400 = 26.04
            FBRD_Value := 3;
         when B57600 =>
            IBRD_Value := 17;    --  1000000 / 57600 = 17.36
            FBRD_Value := 23;
         when B115200 =>
            IBRD_Value := 8;     --  1000000 / 115200 = 8.68
            FBRD_Value := 44;
      end case;

      --  Set baud rate
      Write_Reg (Base, UARTIBRD_Offset, IBRD_Value);
      Write_Reg (Base, UARTFBRD_Offset, FBRD_Value);

      --  8N1, enable FIFOs
      Write_Reg (Base, UARTLCRH_Offset, LCRH_WLEN_8 or LCRH_FEN);

      --  Enable UART, TX and RX
      Write_Reg (Base, UARTCTL_Offset, CTL_UARTEN or CTL_TXE or CTL_RXE);
   end Initialize;

   ---------------
   -- Send_Byte --
   ---------------

   procedure Send_Byte (Port : UART_Peripheral; Data : Unsigned_8) is
      Base : constant Integer_Address := Get_Base (Port);
   begin
      --  Wait for TX FIFO not full
      while (Read_Reg (Base, UARTFR_Offset) and FR_TXFF) /= 0 loop
         null;
      end loop;

      --  Write data
      Write_Reg (Base, UARTDR_Offset, Unsigned_32 (Data));
   end Send_Byte;

   ----------
   -- Send --
   ----------

   function Send (Port : UART_Peripheral; Data : Byte_Array) return Natural is
      Base : constant Integer_Address := Get_Base (Port);
   begin
      for I in Data'Range loop
         Send_Byte (Port, Data (I));
      end loop;

      --  Wait for transmission complete (TX FIFO empty and not busy)
      while (Read_Reg (Base, UARTFR_Offset) and FR_TXFE) = 0
        or (Read_Reg (Base, UARTFR_Offset) and FR_BUSY) /= 0
      loop
         null;
      end loop;

      return Data'Length;
   end Send;

   ------------------
   -- Receive_Byte --
   ------------------

   function Receive_Byte
     (Port       : UART_Peripheral;
      Data       : out Unsigned_8;
      Timeout_Ms : Natural) return Boolean
   is
      Base : constant Integer_Address := Get_Base (Port);
      Loops_Per_Ms : constant := 4000;  --  Approximate at 50 MHz
      Total_Loops : constant Natural := Timeout_Ms * Loops_Per_Ms;
      Count : Natural := 0;
   begin
      Data := 0;

      --  Wait for RX FIFO not empty with timeout
      loop
         if (Read_Reg (Base, UARTFR_Offset) and FR_RXFE) = 0 then
            Data := Unsigned_8 (Read_Reg (Base, UARTDR_Offset) and 16#FF#);
            return True;
         end if;

         Count := Count + 1;
         if Timeout_Ms > 0 and then Count >= Total_Loops then
            return False;  --  Timeout
         end if;
      end loop;
   end Receive_Byte;

   -------------
   -- Receive --
   -------------

   function Receive
     (Port       : UART_Peripheral;
      Buffer     : out Byte_Array;
      Max_Length : Natural;
      Timeout_Ms : Natural) return Natural
   is
      Received : Natural := 0;
      B : Unsigned_8;
      Actual_Max : constant Natural :=
        Natural'Min (Max_Length, Buffer'Length);
   begin
      --  Initialize buffer
      Buffer := [others => 0];

      --  Receive bytes until timeout or buffer full
      while Received < Actual_Max loop
         if Receive_Byte (Port, B, Timeout_Ms) then
            Buffer (Buffer'First + Received) := B;
            Received := Received + 1;
         else
            --  Timeout, return what we have
            exit;
         end if;
      end loop;

      return Received;
   end Receive;

   --------------
   -- TX_Ready --
   --------------

   function TX_Ready (Port : UART_Peripheral) return Boolean is
      Base : constant Integer_Address := Get_Base (Port);
   begin
      return (Read_Reg (Base, UARTFR_Offset) and FR_TXFF) = 0;
   end TX_Ready;

   ------------------
   -- RX_Available --
   ------------------

   function RX_Available (Port : UART_Peripheral) return Boolean is
      Base : constant Integer_Address := Get_Base (Port);
   begin
      return (Read_Reg (Base, UARTFR_Offset) and FR_RXFE) = 0;
   end RX_Available;

end LM3S_UART;
