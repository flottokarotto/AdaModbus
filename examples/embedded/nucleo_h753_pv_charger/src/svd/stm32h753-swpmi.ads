pragma Style_Checks (Off);

--  This spec has been automatically generated from STM32H753.svd


with System;

package STM32H753.SWPMI is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   --  SWPMI Configuration/Control register
   type CR_Register is record
      --  Reception DMA enable
      RXDMA          : Boolean := False;
      --  Transmission DMA enable
      TXDMA          : Boolean := False;
      --  Reception buffering mode
      RXMODE         : Boolean := False;
      --  Transmission buffering mode
      TXMODE         : Boolean := False;
      --  Loopback mode enable
      LPBK           : Boolean := False;
      --  Single wire protocol master interface activate
      SWPACT         : Boolean := False;
      --  unspecified
      Reserved_6_9   : STM32H753.UInt4 := 16#0#;
      --  Single wire protocol master interface deactivate
      DEACT          : Boolean := False;
      --  Single wire protocol master transceiver enable
      SWPTEN         : Boolean := False;
      --  unspecified
      Reserved_12_31 : STM32H753.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      RXDMA          at 0 range 0 .. 0;
      TXDMA          at 0 range 1 .. 1;
      RXMODE         at 0 range 2 .. 2;
      TXMODE         at 0 range 3 .. 3;
      LPBK           at 0 range 4 .. 4;
      SWPACT         at 0 range 5 .. 5;
      Reserved_6_9   at 0 range 6 .. 9;
      DEACT          at 0 range 10 .. 10;
      SWPTEN         at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype BRR_BR_Field is STM32H753.Byte;

   --  SWPMI Bitrate register
   type BRR_Register is record
      --  Bitrate prescaler
      BR            : BRR_BR_Field := 16#1#;
      --  unspecified
      Reserved_8_31 : STM32H753.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for BRR_Register use record
      BR            at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  SWPMI Interrupt and Status register
   type ISR_Register is record
      --  Read-only. Receive buffer full flag
      RXBFF          : Boolean;
      --  Read-only. Transmit buffer empty flag
      TXBEF          : Boolean;
      --  Read-only. Receive CRC error flag
      RXBERF         : Boolean;
      --  Read-only. Receive overrun error flag
      RXOVRF         : Boolean;
      --  Read-only. Transmit underrun error flag
      TXUNRF         : Boolean;
      --  Read-only. Receive data register not empty
      RXNE           : Boolean;
      --  Read-only. Transmit data register empty
      TXE            : Boolean;
      --  Read-only. Transfer complete flag
      TCF            : Boolean;
      --  Read-only. Slave resume flag
      SRF            : Boolean;
      --  Read-only. SUSPEND flag
      SUSP           : Boolean;
      --  Read-only. DEACTIVATED flag
      DEACTF         : Boolean;
      --  Read-only. transceiver ready flag
      RDYF           : Boolean;
      --  unspecified
      Reserved_12_31 : STM32H753.UInt20;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISR_Register use record
      RXBFF          at 0 range 0 .. 0;
      TXBEF          at 0 range 1 .. 1;
      RXBERF         at 0 range 2 .. 2;
      RXOVRF         at 0 range 3 .. 3;
      TXUNRF         at 0 range 4 .. 4;
      RXNE           at 0 range 5 .. 5;
      TXE            at 0 range 6 .. 6;
      TCF            at 0 range 7 .. 7;
      SRF            at 0 range 8 .. 8;
      SUSP           at 0 range 9 .. 9;
      DEACTF         at 0 range 10 .. 10;
      RDYF           at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   --  SWPMI Interrupt Flag Clear register
   type ICR_Register is record
      --  Write-only. Clear receive buffer full flag
      CRXBFF         : Boolean := False;
      --  Write-only. Clear transmit buffer empty flag
      CTXBEF         : Boolean := False;
      --  Write-only. Clear receive CRC error flag
      CRXBERF        : Boolean := False;
      --  Write-only. Clear receive overrun error flag
      CRXOVRF        : Boolean := False;
      --  Write-only. Clear transmit underrun error flag
      CTXUNRF        : Boolean := False;
      --  unspecified
      Reserved_5_6   : STM32H753.UInt2 := 16#0#;
      --  Write-only. Clear transfer complete flag
      CTCF           : Boolean := False;
      --  Write-only. Clear slave resume flag
      CSRF           : Boolean := False;
      --  unspecified
      Reserved_9_10  : STM32H753.UInt2 := 16#0#;
      --  Write-only. Clear transceiver ready flag
      CRDYF          : Boolean := False;
      --  unspecified
      Reserved_12_31 : STM32H753.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ICR_Register use record
      CRXBFF         at 0 range 0 .. 0;
      CTXBEF         at 0 range 1 .. 1;
      CRXBERF        at 0 range 2 .. 2;
      CRXOVRF        at 0 range 3 .. 3;
      CTXUNRF        at 0 range 4 .. 4;
      Reserved_5_6   at 0 range 5 .. 6;
      CTCF           at 0 range 7 .. 7;
      CSRF           at 0 range 8 .. 8;
      Reserved_9_10  at 0 range 9 .. 10;
      CRDYF          at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   --  SWPMI Interrupt Enable register
   type IER_Register is record
      --  Receive buffer full interrupt enable
      RXBFIE         : Boolean := False;
      --  Transmit buffer empty interrupt enable
      TXBEIE         : Boolean := False;
      --  Receive CRC error interrupt enable
      RXBERIE        : Boolean := False;
      --  Receive overrun error interrupt enable
      RXOVRIE        : Boolean := False;
      --  Transmit underrun error interrupt enable
      TXUNRIE        : Boolean := False;
      --  Receive interrupt enable
      RIE            : Boolean := False;
      --  Transmit interrupt enable
      TIE            : Boolean := False;
      --  Transmit complete interrupt enable
      TCIE           : Boolean := False;
      --  Slave resume interrupt enable
      SRIE           : Boolean := False;
      --  unspecified
      Reserved_9_10  : STM32H753.UInt2 := 16#0#;
      --  Transceiver ready interrupt enable
      RDYIE          : Boolean := False;
      --  unspecified
      Reserved_12_31 : STM32H753.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IER_Register use record
      RXBFIE         at 0 range 0 .. 0;
      TXBEIE         at 0 range 1 .. 1;
      RXBERIE        at 0 range 2 .. 2;
      RXOVRIE        at 0 range 3 .. 3;
      TXUNRIE        at 0 range 4 .. 4;
      RIE            at 0 range 5 .. 5;
      TIE            at 0 range 6 .. 6;
      TCIE           at 0 range 7 .. 7;
      SRIE           at 0 range 8 .. 8;
      Reserved_9_10  at 0 range 9 .. 10;
      RDYIE          at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype RFL_RFL_Field is STM32H753.UInt5;

   --  SWPMI Receive Frame Length register
   type RFL_Register is record
      --  Read-only. Receive frame length
      RFL           : RFL_RFL_Field;
      --  unspecified
      Reserved_5_31 : STM32H753.UInt27;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RFL_Register use record
      RFL           at 0 range 0 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   --  SWPMI Option register
   type OR_Register is record
      --  SWP transceiver bypass
      SWP_TBYP      : Boolean := False;
      --  SWP class selection
      SWP_CLASS     : Boolean := False;
      --  unspecified
      Reserved_2_31 : STM32H753.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OR_Register use record
      SWP_TBYP      at 0 range 0 .. 0;
      SWP_CLASS     at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Single Wire Protocol Master Interface
   type SWPMI_Peripheral is record
      --  SWPMI Configuration/Control register
      CR   : aliased CR_Register;
      --  SWPMI Bitrate register
      BRR  : aliased BRR_Register;
      --  SWPMI Interrupt and Status register
      ISR  : aliased ISR_Register;
      --  SWPMI Interrupt Flag Clear register
      ICR  : aliased ICR_Register;
      --  SWPMI Interrupt Enable register
      IER  : aliased IER_Register;
      --  SWPMI Receive Frame Length register
      RFL  : aliased RFL_Register;
      --  SWPMI Transmit data register
      TDR  : aliased STM32H753.UInt32;
      --  SWPMI Receive data register
      RDR  : aliased STM32H753.UInt32;
      --  SWPMI Option register
      OR_k : aliased OR_Register;
   end record
     with Volatile;

   for SWPMI_Peripheral use record
      CR   at 16#0# range 0 .. 31;
      BRR  at 16#4# range 0 .. 31;
      ISR  at 16#C# range 0 .. 31;
      ICR  at 16#10# range 0 .. 31;
      IER  at 16#14# range 0 .. 31;
      RFL  at 16#18# range 0 .. 31;
      TDR  at 16#1C# range 0 .. 31;
      RDR  at 16#20# range 0 .. 31;
      OR_k at 16#24# range 0 .. 31;
   end record;

   --  Single Wire Protocol Master Interface
   SWPMI_Periph : aliased SWPMI_Peripheral
     with Import, Address => SWPMI_Base;

end STM32H753.SWPMI;
