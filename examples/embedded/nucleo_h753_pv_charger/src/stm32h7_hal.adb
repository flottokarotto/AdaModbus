--  STM32H7_HAL - Implementation using SVD-generated register definitions
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with A0B.STM32H753.SVD.RCC;   use A0B.STM32H753.SVD.RCC;
with A0B.STM32H753.SVD.GPIO;  use A0B.STM32H753.SVD.GPIO;
with A0B.STM32H753.SVD.USART; use A0B.STM32H753.SVD.USART;
with A0B.STM32H753.SVD.I2C;   use A0B.STM32H753.SVD.I2C;
with A0B.Types.SVD;           use A0B.Types.SVD;
with System;

package body STM32H7_HAL is

   --  System tick counter (incremented by SysTick interrupt)
   Tick_Counter : Unsigned_32 := 0
     with Volatile;

   --  System clock frequency (HSI = 64 MHz)
   System_Clock_Hz : constant := 64_000_000;
   APB1_Clock_Hz   : constant := 64_000_000;  -- Assuming no prescaler

   --  SysTick registers (ARM Cortex-M core peripheral)
   SysTick_Base : constant := 16#E000_E010#;

   type SysTick_Registers is record
      CTRL  : Unsigned_32;
      LOAD  : Unsigned_32;
      VAL   : Unsigned_32;
      CALIB : Unsigned_32;
   end record with Volatile;

   SysTick : SysTick_Registers
     with Import, Address => System'To_Address (SysTick_Base);

   SysTick_CTRL_ENABLE    : constant := 2 ** 0;
   SysTick_CTRL_TICKINT   : constant := 2 ** 1;
   SysTick_CTRL_CLKSOURCE : constant := 2 ** 2;

   --  GPIO Mode constants for MODER register (2 bits per pin)
   GPIO_Mode_Val_Input  : constant A0B.Types.SVD.UInt2 := 2#00#;
   GPIO_Mode_Val_Output : constant A0B.Types.SVD.UInt2 := 2#01#;
   GPIO_Mode_Val_AF     : constant A0B.Types.SVD.UInt2 := 2#10#;
   GPIO_Mode_Val_Analog : constant A0B.Types.SVD.UInt2 := 2#11#;

   -----------------
   -- System_Init --
   -----------------

   procedure System_Init is
   begin
      --  STM32H7 starts with HSI (64 MHz) by default
      --  For higher performance, configure PLL to 480 MHz
      --  For this example, we use HSI directly
      null;
   end System_Init;

   ------------------
   -- SysTick_Init --
   ------------------

   procedure SysTick_Init is
      Reload_Value : constant Unsigned_32 := (System_Clock_Hz / 1000) - 1;
   begin
      --  Configure SysTick for 1ms interrupt
      SysTick.LOAD := Reload_Value;
      SysTick.VAL := 0;
      SysTick.CTRL := SysTick_CTRL_CLKSOURCE or
                      SysTick_CTRL_TICKINT or
                      SysTick_CTRL_ENABLE;
   end SysTick_Init;

   --------------
   -- Get_Tick --
   --------------

   function Get_Tick return Unsigned_32 is
   begin
      return Tick_Counter;
   end Get_Tick;

   --------------
   -- Delay_Ms --
   --------------

   procedure Delay_Ms (Ms : Unsigned_32) is
      Start : constant Unsigned_32 := Get_Tick;
   begin
      while Get_Tick - Start < Ms loop
         null;  -- Busy wait
      end loop;
   end Delay_Ms;

   ---------------
   -- GPIO_Init --
   ---------------

   procedure GPIO_Init is
      AHB4 : AHB4ENR_Register;
   begin
      --  Enable GPIO clocks (GPIOA, GPIOB, GPIOC, GPIOD, GPIOG)
      AHB4 := RCC_Periph.AHB4ENR;
      AHB4.GPIOAEN := True;
      AHB4.GPIOBEN := True;
      AHB4.GPIOCEN := True;
      AHB4.GPIODEN := True;
      AHB4.GPIOGEN := True;
      RCC_Periph.AHB4ENR := AHB4;

      --  Small delay for clock to stabilize
      declare
         Dummy : AHB4ENR_Register := RCC_Periph.AHB4ENR;
         pragma Volatile (Dummy);
      begin
         null;
      end;

      --  Configure LED pins as outputs
      --  PB0 (LD1), PB7 (LD2), PB14 (LD3)
      GPIO_Set_Mode (Port_B, 0, Mode_Output);
      GPIO_Set_Mode (Port_B, 7, Mode_Output);
      GPIO_Set_Mode (Port_B, 14, Mode_Output);

      --  Configure PC13 (Button B1) as input
      GPIO_Set_Mode (Port_C, 13, Mode_Input);

      --  Configure PD12 (RS485 DE) as output
      GPIO_Set_Mode (Port_D, 12, Mode_Output);
      GPIO_Write (Port_D, 12, False);  -- RX mode by default
   end GPIO_Init;

   -------------------
   -- GPIO_Set_Mode --
   -------------------

   procedure GPIO_Set_Mode
     (Port : GPIO_Port;
      Pin  : GPIO_Pin;
      Mode : GPIO_Mode)
   is
      Mode_Val : A0B.Types.SVD.UInt2;
      Reg      : MODER_Register;
   begin
      case Mode is
         when Mode_Input  => Mode_Val := GPIO_Mode_Val_Input;
         when Mode_Output => Mode_Val := GPIO_Mode_Val_Output;
         when Mode_AF     => Mode_Val := GPIO_Mode_Val_AF;
         when Mode_Analog => Mode_Val := GPIO_Mode_Val_Analog;
      end case;

      case Port is
         when Port_B =>
            Reg := GPIOB_Periph.MODER;
            Reg.Arr (Natural (Pin)) := Mode_Val;
            GPIOB_Periph.MODER := Reg;
         when Port_C =>
            Reg := GPIOC_Periph.MODER;
            Reg.Arr (Natural (Pin)) := Mode_Val;
            GPIOC_Periph.MODER := Reg;
         when Port_D =>
            Reg := GPIOD_Periph.MODER;
            Reg.Arr (Natural (Pin)) := Mode_Val;
            GPIOD_Periph.MODER := Reg;
      end case;
   end GPIO_Set_Mode;

   -----------------
   -- GPIO_Set_AF --
   -----------------

   procedure GPIO_Set_AF
     (Port : GPIO_Port;
      Pin  : GPIO_Pin;
      AF   : GPIO_AF)
   is
      AF_Val : constant A0B.Types.SVD.UInt4 := A0B.Types.SVD.UInt4 (AF);
   begin
      if Pin <= 7 then
         case Port is
            when Port_B =>
               declare
                  Reg : AFRL_Register := GPIOB_Periph.AFRL;
               begin
                  Reg.Arr (Natural (Pin)) := AF_Val;
                  GPIOB_Periph.AFRL := Reg;
               end;
            when Port_C =>
               declare
                  Reg : AFRL_Register := GPIOC_Periph.AFRL;
               begin
                  Reg.Arr (Natural (Pin)) := AF_Val;
                  GPIOC_Periph.AFRL := Reg;
               end;
            when Port_D =>
               declare
                  Reg : AFRL_Register := GPIOD_Periph.AFRL;
               begin
                  Reg.Arr (Natural (Pin)) := AF_Val;
                  GPIOD_Periph.AFRL := Reg;
               end;
         end case;
      else
         case Port is
            when Port_B =>
               declare
                  Reg : AFRH_Register := GPIOB_Periph.AFRH;
               begin
                  Reg.Arr (Natural (Pin)) := AF_Val;
                  GPIOB_Periph.AFRH := Reg;
               end;
            when Port_C =>
               declare
                  Reg : AFRH_Register := GPIOC_Periph.AFRH;
               begin
                  Reg.Arr (Natural (Pin)) := AF_Val;
                  GPIOC_Periph.AFRH := Reg;
               end;
            when Port_D =>
               declare
                  Reg : AFRH_Register := GPIOD_Periph.AFRH;
               begin
                  Reg.Arr (Natural (Pin)) := AF_Val;
                  GPIOD_Periph.AFRH := Reg;
               end;
         end case;
      end if;
   end GPIO_Set_AF;

   ----------------
   -- GPIO_Write --
   ----------------

   procedure GPIO_Write
     (Port  : GPIO_Port;
      Pin   : GPIO_Pin;
      State : Boolean)
   is
      Reg : BSRR_Register := (others => <>);
   begin
      if State then
         Reg.BS := (As_Array => True, Arr => (others => False));
         Reg.BS.Arr (Natural (Pin)) := True;
      else
         Reg.BR := (As_Array => True, Arr => (others => False));
         Reg.BR.Arr (Natural (Pin)) := True;
      end if;

      case Port is
         when Port_B => GPIOB_Periph.BSRR := Reg;
         when Port_C => GPIOC_Periph.BSRR := Reg;
         when Port_D => GPIOD_Periph.BSRR := Reg;
      end case;
   end GPIO_Write;

   ---------------
   -- GPIO_Read --
   ---------------

   function GPIO_Read
     (Port : GPIO_Port;
      Pin  : GPIO_Pin) return Boolean
   is
   begin
      case Port is
         when Port_B => return GPIOB_Periph.IDR.ID.Arr (Natural (Pin));
         when Port_C => return GPIOC_Periph.IDR.ID.Arr (Natural (Pin));
         when Port_D => return GPIOD_Periph.IDR.ID.Arr (Natural (Pin));
      end case;
   end GPIO_Read;

   -----------------
   -- GPIO_Toggle --
   -----------------

   procedure GPIO_Toggle
     (Port : GPIO_Port;
      Pin  : GPIO_Pin)
   is
      Current : Boolean;
   begin
      case Port is
         when Port_B => Current := GPIOB_Periph.ODR.OD.Arr (Natural (Pin));
         when Port_C => Current := GPIOC_Periph.ODR.OD.Arr (Natural (Pin));
         when Port_D => Current := GPIOD_Periph.ODR.OD.Arr (Natural (Pin));
      end case;

      GPIO_Write (Port, Pin, not Current);
   end GPIO_Toggle;

   -----------------
   -- USART3_Init --
   -----------------

   procedure USART3_Init (Baud_Rate : Unsigned_32) is
      APB1L   : APB1LENR_Register;
      BRR_Val : Unsigned_32;
   begin
      --  Enable USART3 clock
      APB1L := RCC_Periph.APB1LENR;
      APB1L.USART3EN := True;
      RCC_Periph.APB1LENR := APB1L;

      --  Configure PD8 (TX) and PD9 (RX) as AF7
      GPIO_Set_Mode (Port_D, 8, Mode_AF);
      GPIO_Set_Mode (Port_D, 9, Mode_AF);
      GPIO_Set_AF (Port_D, 8, 7);
      GPIO_Set_AF (Port_D, 9, 7);

      --  Disable USART before configuration
      USART3_Periph.CR1 := (others => <>);

      --  Configure baud rate
      --  BRR = fck / baud_rate (for oversampling by 16)
      --  BRR register is split: bits 0-3 (fraction), bits 4-15 (mantissa)
      BRR_Val := APB1_Clock_Hz / Baud_Rate;
      USART3_Periph.BRR := (
         BRR_0_3  => A0B.Types.SVD.UInt4 (BRR_Val and 16#F#),
         BRR_4_15 => A0B.Types.SVD.UInt12 (Shift_Right (BRR_Val, 4) and 16#FFF#),
         others   => <>
      );

      --  Configure: 8N1, no hardware flow control
      USART3_Periph.CR2 := (others => <>);
      USART3_Periph.CR3 := (others => <>);

      --  Enable USART, TX, RX
      USART3_Periph.CR1 := (UE => True, TE => True, RE => True, others => <>);
   end USART3_Init;

   ----------------------
   -- USART3_Send_Byte --
   ----------------------

   procedure USART3_Send_Byte (Data : Unsigned_8) is
   begin
      --  Wait for TXE (transmit data register empty)
      while not USART3_Periph.ISR.TXE loop
         null;
      end loop;
      USART3_Periph.TDR := (TDR => A0B.Types.SVD.UInt9 (Data), others => <>);
   end USART3_Send_Byte;

   -----------------
   -- USART3_Send --
   -----------------

   procedure USART3_Send (Data : Byte_Array) is
   begin
      for I in Data'Range loop
         USART3_Send_Byte (Unsigned_8 (Data (I)));
      end loop;

      --  Wait for transmission complete
      while not USART3_Periph.ISR.TC loop
         null;
      end loop;
   end USART3_Send;

   -------------------------
   -- USART3_Receive_Byte --
   -------------------------

   procedure USART3_Receive_Byte
     (Data    : out Unsigned_8;
      Timeout : Unsigned_32;
      Success : out Boolean)
   is
      Start : constant Unsigned_32 := Get_Tick;
   begin
      Data := 0;
      Success := False;

      loop
         if USART3_Periph.ISR.RXNE then
            Data := Unsigned_8 (USART3_Periph.RDR.RDR and 16#FF#);
            Success := True;
            return;
         end if;

         if Get_Tick - Start >= Timeout then
            return;  -- Timeout
         end if;
      end loop;
   end USART3_Receive_Byte;

   --------------------
   -- USART3_Receive --
   --------------------

   procedure USART3_Receive
     (Data    : out Byte_Array;
      Length  : out Natural;
      Timeout : Unsigned_32)
   is
      B       : Unsigned_8;
      Success : Boolean;
   begin
      Length := 0;
      Data := (others => 0);

      for I in Data'Range loop
         USART3_Receive_Byte (B, Timeout, Success);
         if Success then
            Data (I) := Ada_Modbus.Byte (B);
            Length := Length + 1;
         else
            return;  -- Timeout or no more data
         end if;
      end loop;
   end USART3_Receive;

   ----------------------
   -- RS485_Set_TX_Mode --
   ----------------------

   procedure RS485_Set_TX_Mode is
   begin
      --  Set DE high (PD12)
      GPIO_Write (Port_D, 12, True);
      --  Small delay for transceiver to switch
      for I in 1 .. 100 loop
         null;
      end loop;
   end RS485_Set_TX_Mode;

   ----------------------
   -- RS485_Set_RX_Mode --
   ----------------------

   procedure RS485_Set_RX_Mode is
   begin
      --  Set DE low (PD12)
      GPIO_Write (Port_D, 12, False);
   end RS485_Set_RX_Mode;

   ---------------------
   -- RS485_Transceive --
   ---------------------

   procedure RS485_Transceive
     (TX_Data    : Byte_Array;
      RX_Data    : out Byte_Array;
      RX_Length  : out Natural;
      Timeout_Ms : Unsigned_32;
      Result     : out Status)
   is
   begin
      --  Switch to TX mode
      RS485_Set_TX_Mode;

      --  Send data
      USART3_Send (TX_Data);

      --  Switch to RX mode
      RS485_Set_RX_Mode;

      --  Receive response
      USART3_Receive (RX_Data, RX_Length, Timeout_Ms);

      if RX_Length > 0 then
         Result := Success;
      else
         Result := Timeout;
      end if;
   end RS485_Transceive;

   ---------------
   -- I2C1_Init --
   ---------------

   procedure I2C1_Init is
      APB1L  : APB1LENR_Register;
      OType  : OTYPER_Register;
   begin
      --  Enable I2C1 clock
      APB1L := RCC_Periph.APB1LENR;
      APB1L.I2C1EN := True;
      RCC_Periph.APB1LENR := APB1L;

      --  Configure PB8 (SCL) and PB9 (SDA) as AF4
      GPIO_Set_Mode (Port_B, 8, Mode_AF);
      GPIO_Set_Mode (Port_B, 9, Mode_AF);
      GPIO_Set_AF (Port_B, 8, 4);
      GPIO_Set_AF (Port_B, 9, 4);

      --  Configure open-drain for I2C
      OType := GPIOB_Periph.OTYPER;
      OType.OT.Arr (8) := True;
      OType.OT.Arr (9) := True;
      GPIOB_Periph.OTYPER := OType;

      --  Disable I2C before configuration
      I2C1_Periph.CR1 := (others => <>);

      --  Configure timing for 400 kHz Fast Mode
      --  Assuming 64 MHz clock
      --  PRESC=0, SCLDEL=4, SDADEL=2, SCLH=15, SCLL=19
      I2C1_Periph.TIMINGR := (
         SCLL   => 19,
         SCLH   => 15,
         SDADEL => 2,
         SCLDEL => 4,
         PRESC  => 0,
         others => <>
      );

      --  Enable I2C
      I2C1_Periph.CR1 := (PE => True, others => <>);
   end I2C1_Init;

   ----------------
   -- I2C1_Write --
   ----------------

   procedure I2C1_Write
     (Address : Unsigned_8;
      Data    : Byte_Array;
      Success : out Boolean)
   is
      Start_Time : constant Unsigned_32 := Get_Tick;
      Timeout    : constant Unsigned_32 := 100;  -- 100ms timeout
   begin
      Success := False;

      --  Wait until I2C is not busy
      while I2C1_Periph.ISR.BUSY loop
         if Get_Tick - Start_Time > Timeout then
            return;
         end if;
      end loop;

      --  Configure transfer: SADD, NBYTES, START, AUTOEND
      I2C1_Periph.CR2 := (
         SADD    => (As_Array => False, Val => A0B.Types.SVD.UInt10 (Address)),
         NBYTES  => A0B.Types.SVD.Byte (Data'Length),
         START   => True,
         AUTOEND => True,
         RD_WRN  => False,  -- Write
         others  => <>
      );

      --  Send data bytes
      for I in Data'Range loop
         --  Wait for TXIS
         while not I2C1_Periph.ISR.TXIS loop
            if Get_Tick - Start_Time > Timeout then
               return;
            end if;
         end loop;
         I2C1_Periph.TXDR := (TXDATA => A0B.Types.SVD.Byte (Data (I)), others => <>);
      end loop;

      --  Wait for transfer complete or STOP
      while not I2C1_Periph.ISR.STOPF loop
         if Get_Tick - Start_Time > Timeout then
            return;
         end if;
      end loop;

      --  Clear STOPF flag
      I2C1_Periph.ICR := (STOPCF => True, others => <>);

      Success := True;
   end I2C1_Write;

   ---------------
   -- I2C1_Read --
   ---------------

   procedure I2C1_Read
     (Address : Unsigned_8;
      Data    : out Byte_Array;
      Length  : Natural;
      Success : out Boolean)
   is
      Start_Time : constant Unsigned_32 := Get_Tick;
      Timeout    : constant Unsigned_32 := 100;
      Count      : Natural := 0;
   begin
      Success := False;
      Data := (others => 0);

      --  Wait until I2C is not busy
      while I2C1_Periph.ISR.BUSY loop
         if Get_Tick - Start_Time > Timeout then
            return;
         end if;
      end loop;

      --  Configure read transfer
      I2C1_Periph.CR2 := (
         SADD    => (As_Array => False, Val => A0B.Types.SVD.UInt10 (Address)),
         NBYTES  => A0B.Types.SVD.Byte (Length),
         START   => True,
         AUTOEND => True,
         RD_WRN  => True,  -- Read
         others  => <>
      );

      --  Read data bytes
      while Count < Length loop
         --  Wait for RXNE
         while not I2C1_Periph.ISR.RXNE loop
            if Get_Tick - Start_Time > Timeout then
               return;
            end if;
         end loop;
         Data (Data'First + Count) := Ada_Modbus.Byte (I2C1_Periph.RXDR.RXDATA);
         Count := Count + 1;
      end loop;

      --  Wait for STOP
      while not I2C1_Periph.ISR.STOPF loop
         if Get_Tick - Start_Time > Timeout then
            return;
         end if;
      end loop;

      --  Clear STOPF flag
      I2C1_Periph.ICR := (STOPCF => True, others => <>);

      Success := True;
   end I2C1_Read;

   -------------------
   -- ETH_GPIO_Init --
   -------------------

   procedure ETH_GPIO_Init is
   begin
      --  NUCLEO-H753ZI uses RMII interface with LAN8742A PHY
      --  Required pins (directly connected on NUCLEO board):
      --  PA1  - ETH_REF_CLK (AF11)
      --  PA2  - ETH_MDIO (AF11)
      --  PA7  - ETH_CRS_DV (AF11)
      --  PC1  - ETH_MDC (AF11)
      --  PC4  - ETH_RXD0 (AF11)
      --  PC5  - ETH_RXD1 (AF11)
      --  PG11 - ETH_TX_EN (AF11)
      --  PG13 - ETH_TXD0 (AF11)
      --  PB13 - ETH_TXD1 (AF11)

      --  Note: Full GPIO configuration handled by ethernetif.c (LwIP port)
      null;
   end ETH_GPIO_Init;

   ------------------
   -- ETH_MAC_Init --
   ------------------

   procedure ETH_MAC_Init is
      AHB1 : AHB1ENR_Register;
   begin
      --  Enable Ethernet clocks
      AHB1 := RCC_Periph.AHB1ENR;
      AHB1.ETH1MACEN := True;
      AHB1.ETH1TXEN := True;
      AHB1.ETH1RXEN := True;
      RCC_Periph.AHB1ENR := AHB1;

      --  Full Ethernet initialization handled by LwIP port (ethernetif.c)
      null;
   end ETH_MAC_Init;

   -----------------------------------
   --  SysTick Handler (Interrupt)  --
   -----------------------------------

   procedure SysTick_Handler
     with Export, Convention => C, External_Name => "stm32h7_hal__systick_handler";

   procedure SysTick_Handler is
   begin
      Tick_Counter := Tick_Counter + 1;
   end SysTick_Handler;

end STM32H7_HAL;
