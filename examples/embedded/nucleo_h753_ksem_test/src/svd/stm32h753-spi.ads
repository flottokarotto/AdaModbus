pragma Style_Checks (Off);

--  This spec has been automatically generated from STM32H753.svd


with System;

package STM32H753.SPI is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   --  control register 1
   type CR1_Register is record
      --  Serial Peripheral Enable
      SPE            : Boolean := False;
      --  unspecified
      Reserved_1_7   : STM32H753.UInt7 := 16#0#;
      --  Master automatic SUSP in Receive mode
      MASRX          : Boolean := False;
      --  Read-only. Master transfer start
      CSTART         : Boolean := False;
      --  Write-only. Master SUSPend request
      CSUSP          : Boolean := False;
      --  Rx/Tx direction at Half-duplex mode
      HDDIR          : Boolean := False;
      --  Internal SS signal input level
      SSI            : Boolean := False;
      --  32-bit CRC polynomial configuration
      CRC33_17       : Boolean := False;
      --  CRC calculation initialization pattern control for receiver
      RCRCI          : Boolean := False;
      --  CRC calculation initialization pattern control for transmitter
      TCRCI          : Boolean := False;
      --  Read-only. Locking the AF configuration of associated IOs
      IOLOCK         : Boolean := False;
      --  unspecified
      Reserved_17_31 : STM32H753.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR1_Register use record
      SPE            at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      MASRX          at 0 range 8 .. 8;
      CSTART         at 0 range 9 .. 9;
      CSUSP          at 0 range 10 .. 10;
      HDDIR          at 0 range 11 .. 11;
      SSI            at 0 range 12 .. 12;
      CRC33_17       at 0 range 13 .. 13;
      RCRCI          at 0 range 14 .. 14;
      TCRCI          at 0 range 15 .. 15;
      IOLOCK         at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype CR2_TSIZE_Field is STM32H753.UInt16;
   subtype CR2_TSER_Field is STM32H753.UInt16;

   --  control register 2
   type CR2_Register is record
      --  Number of data at current transfer
      TSIZE : CR2_TSIZE_Field := 16#0#;
      --  Read-only. Number of data transfer extension to be reload into TSIZE
      --  just when a previous
      TSER  : CR2_TSER_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR2_Register use record
      TSIZE at 0 range 0 .. 15;
      TSER  at 0 range 16 .. 31;
   end record;

   subtype CFG1_DSIZE_Field is STM32H753.UInt5;
   subtype CFG1_FTHVL_Field is STM32H753.UInt4;
   subtype CFG1_UDRCFG_Field is STM32H753.UInt2;
   subtype CFG1_UDRDET_Field is STM32H753.UInt2;
   subtype CFG1_CRCSIZE_Field is STM32H753.UInt5;
   subtype CFG1_MBR_Field is STM32H753.UInt3;

   --  configuration register 1
   type CFG1_Register is record
      --  Number of bits in at single SPI data frame
      DSIZE          : CFG1_DSIZE_Field := 16#7#;
      --  threshold level
      FTHVL          : CFG1_FTHVL_Field := 16#0#;
      --  Behavior of slave transmitter at underrun condition
      UDRCFG         : CFG1_UDRCFG_Field := 16#0#;
      --  Detection of underrun condition at slave transmitter
      UDRDET         : CFG1_UDRDET_Field := 16#0#;
      --  unspecified
      Reserved_13_13 : STM32H753.Bit := 16#0#;
      --  Rx DMA stream enable
      RXDMAEN        : Boolean := False;
      --  Tx DMA stream enable
      TXDMAEN        : Boolean := False;
      --  Length of CRC frame to be transacted and compared
      CRCSIZE        : CFG1_CRCSIZE_Field := 16#7#;
      --  unspecified
      Reserved_21_21 : STM32H753.Bit := 16#0#;
      --  Hardware CRC computation enable
      CRCEN          : Boolean := False;
      --  unspecified
      Reserved_23_27 : STM32H753.UInt5 := 16#0#;
      --  Master baud rate
      MBR            : CFG1_MBR_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : STM32H753.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CFG1_Register use record
      DSIZE          at 0 range 0 .. 4;
      FTHVL          at 0 range 5 .. 8;
      UDRCFG         at 0 range 9 .. 10;
      UDRDET         at 0 range 11 .. 12;
      Reserved_13_13 at 0 range 13 .. 13;
      RXDMAEN        at 0 range 14 .. 14;
      TXDMAEN        at 0 range 15 .. 15;
      CRCSIZE        at 0 range 16 .. 20;
      Reserved_21_21 at 0 range 21 .. 21;
      CRCEN          at 0 range 22 .. 22;
      Reserved_23_27 at 0 range 23 .. 27;
      MBR            at 0 range 28 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype CFG2_MSSI_Field is STM32H753.UInt4;
   subtype CFG2_MIDI_Field is STM32H753.UInt4;
   subtype CFG2_COMM_Field is STM32H753.UInt2;
   subtype CFG2_SP_Field is STM32H753.UInt3;

   --  configuration register 2
   type CFG2_Register is record
      --  Master SS Idleness
      MSSI           : CFG2_MSSI_Field := 16#0#;
      --  Master Inter-Data Idleness
      MIDI           : CFG2_MIDI_Field := 16#0#;
      --  unspecified
      Reserved_8_14  : STM32H753.UInt7 := 16#0#;
      --  Swap functionality of MISO and MOSI pins
      IOSWP          : Boolean := False;
      --  unspecified
      Reserved_16_16 : STM32H753.Bit := 16#0#;
      --  SPI Communication Mode
      COMM           : CFG2_COMM_Field := 16#0#;
      --  Serial Protocol
      SP             : CFG2_SP_Field := 16#0#;
      --  SPI Master
      MASTER         : Boolean := False;
      --  Data frame format
      LSBFRST        : Boolean := False;
      --  Clock phase
      CPHA           : Boolean := False;
      --  Clock polarity
      CPOL           : Boolean := False;
      --  Software management of SS signal input
      SSM            : Boolean := False;
      --  unspecified
      Reserved_27_27 : STM32H753.Bit := 16#0#;
      --  SS input/output polarity
      SSIOP          : Boolean := False;
      --  SS output enable
      SSOE           : Boolean := False;
      --  SS output management in master mode
      SSOM           : Boolean := False;
      --  Alternate function GPIOs control
      AFCNTR         : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CFG2_Register use record
      MSSI           at 0 range 0 .. 3;
      MIDI           at 0 range 4 .. 7;
      Reserved_8_14  at 0 range 8 .. 14;
      IOSWP          at 0 range 15 .. 15;
      Reserved_16_16 at 0 range 16 .. 16;
      COMM           at 0 range 17 .. 18;
      SP             at 0 range 19 .. 21;
      MASTER         at 0 range 22 .. 22;
      LSBFRST        at 0 range 23 .. 23;
      CPHA           at 0 range 24 .. 24;
      CPOL           at 0 range 25 .. 25;
      SSM            at 0 range 26 .. 26;
      Reserved_27_27 at 0 range 27 .. 27;
      SSIOP          at 0 range 28 .. 28;
      SSOE           at 0 range 29 .. 29;
      SSOM           at 0 range 30 .. 30;
      AFCNTR         at 0 range 31 .. 31;
   end record;

   --  Interrupt Enable Register
   type IER_Register is record
      --  RXP Interrupt Enable
      RXPIE          : Boolean := False;
      --  Read-only. TXP interrupt enable
      TXPIE          : Boolean := False;
      --  Read-only. DXP interrupt enabled
      DPXPIE         : Boolean := False;
      --  EOT, SUSP and TXC interrupt enable
      EOTIE          : Boolean := False;
      --  TXTFIE interrupt enable
      TXTFIE         : Boolean := False;
      --  UDR interrupt enable
      UDRIE          : Boolean := False;
      --  OVR interrupt enable
      OVRIE          : Boolean := False;
      --  CRC Interrupt enable
      CRCEIE         : Boolean := False;
      --  TIFRE interrupt enable
      TIFREIE        : Boolean := False;
      --  Mode Fault interrupt enable
      MODFIE         : Boolean := False;
      --  Additional number of transactions reload interrupt enable
      TSERFIE        : Boolean := False;
      --  unspecified
      Reserved_11_31 : STM32H753.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IER_Register use record
      RXPIE          at 0 range 0 .. 0;
      TXPIE          at 0 range 1 .. 1;
      DPXPIE         at 0 range 2 .. 2;
      EOTIE          at 0 range 3 .. 3;
      TXTFIE         at 0 range 4 .. 4;
      UDRIE          at 0 range 5 .. 5;
      OVRIE          at 0 range 6 .. 6;
      CRCEIE         at 0 range 7 .. 7;
      TIFREIE        at 0 range 8 .. 8;
      MODFIE         at 0 range 9 .. 9;
      TSERFIE        at 0 range 10 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   subtype SR_RXPLVL_Field is STM32H753.UInt2;
   subtype SR_CTSIZE_Field is STM32H753.UInt16;

   --  Status Register
   type SR_Register is record
      --  Read-only. Rx-Packet available
      RXP    : Boolean;
      --  Read-only. Tx-Packet space available
      TXP    : Boolean;
      --  Read-only. Duplex Packet
      DXP    : Boolean;
      --  Read-only. End Of Transfer
      EOT    : Boolean;
      --  Read-only. Transmission Transfer Filled
      TXTF   : Boolean;
      --  Read-only. Underrun at slave transmission mode
      UDR    : Boolean;
      --  Read-only. Overrun
      OVR    : Boolean;
      --  Read-only. CRC Error
      CRCE   : Boolean;
      --  Read-only. TI frame format error
      TIFRE  : Boolean;
      --  Read-only. Mode Fault
      MODF   : Boolean;
      --  Read-only. Additional number of SPI data to be transacted was reload
      TSERF  : Boolean;
      --  Read-only. SUSPend
      SUSP   : Boolean;
      --  Read-only. TxFIFO transmission complete
      TXC    : Boolean;
      --  Read-only. RxFIFO Packing LeVeL
      RXPLVL : SR_RXPLVL_Field;
      --  Read-only. RxFIFO Word Not Empty
      RXWNE  : Boolean;
      --  Read-only. Number of data frames remaining in current TSIZE session
      CTSIZE : SR_CTSIZE_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register use record
      RXP    at 0 range 0 .. 0;
      TXP    at 0 range 1 .. 1;
      DXP    at 0 range 2 .. 2;
      EOT    at 0 range 3 .. 3;
      TXTF   at 0 range 4 .. 4;
      UDR    at 0 range 5 .. 5;
      OVR    at 0 range 6 .. 6;
      CRCE   at 0 range 7 .. 7;
      TIFRE  at 0 range 8 .. 8;
      MODF   at 0 range 9 .. 9;
      TSERF  at 0 range 10 .. 10;
      SUSP   at 0 range 11 .. 11;
      TXC    at 0 range 12 .. 12;
      RXPLVL at 0 range 13 .. 14;
      RXWNE  at 0 range 15 .. 15;
      CTSIZE at 0 range 16 .. 31;
   end record;

   --  Interrupt/Status Flags Clear Register
   type IFCR_Register is record
      --  unspecified
      Reserved_0_2   : STM32H753.UInt3 := 16#0#;
      --  Write-only. End Of Transfer flag clear
      EOTC           : Boolean := False;
      --  Write-only. Transmission Transfer Filled flag clear
      TXTFC          : Boolean := False;
      --  Write-only. Underrun flag clear
      UDRC           : Boolean := False;
      --  Write-only. Overrun flag clear
      OVRC           : Boolean := False;
      --  Write-only. CRC Error flag clear
      CRCEC          : Boolean := False;
      --  Write-only. TI frame format error flag clear
      TIFREC         : Boolean := False;
      --  Write-only. Mode Fault flag clear
      MODFC          : Boolean := False;
      --  Write-only. TSERFC flag clear
      TSERFC         : Boolean := False;
      --  Write-only. SUSPend flag clear
      SUSPC          : Boolean := False;
      --  unspecified
      Reserved_12_31 : STM32H753.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IFCR_Register use record
      Reserved_0_2   at 0 range 0 .. 2;
      EOTC           at 0 range 3 .. 3;
      TXTFC          at 0 range 4 .. 4;
      UDRC           at 0 range 5 .. 5;
      OVRC           at 0 range 6 .. 6;
      CRCEC          at 0 range 7 .. 7;
      TIFREC         at 0 range 8 .. 8;
      MODFC          at 0 range 9 .. 9;
      TSERFC         at 0 range 10 .. 10;
      SUSPC          at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype CGFR_I2SCFG_Field is STM32H753.UInt3;
   subtype CGFR_I2SSTD_Field is STM32H753.UInt2;
   subtype CGFR_DATLEN_Field is STM32H753.UInt2;
   subtype CGFR_I2SDIV_Field is STM32H753.Byte;

   --  configuration register
   type CGFR_Register is record
      --  I2S mode selection
      I2SMOD         : Boolean := False;
      --  I2S configuration mode
      I2SCFG         : CGFR_I2SCFG_Field := 16#0#;
      --  I2S standard selection
      I2SSTD         : CGFR_I2SSTD_Field := 16#0#;
      --  unspecified
      Reserved_6_6   : STM32H753.Bit := 16#0#;
      --  PCM frame synchronization
      PCMSYNC        : Boolean := False;
      --  Data length to be transferred
      DATLEN         : CGFR_DATLEN_Field := 16#0#;
      --  Channel length (number of bits per audio channel)
      CHLEN          : Boolean := False;
      --  Serial audio clock polarity
      CKPOL          : Boolean := False;
      --  Word select inversion
      FIXCH          : Boolean := False;
      --  Fixed channel length in SLAVE
      WSINV          : Boolean := False;
      --  Data format
      DATFMT         : Boolean := False;
      --  unspecified
      Reserved_15_15 : STM32H753.Bit := 16#0#;
      --  I2S linear prescaler
      I2SDIV         : CGFR_I2SDIV_Field := 16#0#;
      --  Odd factor for the prescaler
      ODD            : Boolean := False;
      --  Master clock output enable
      MCKOE          : Boolean := False;
      --  unspecified
      Reserved_26_31 : STM32H753.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CGFR_Register use record
      I2SMOD         at 0 range 0 .. 0;
      I2SCFG         at 0 range 1 .. 3;
      I2SSTD         at 0 range 4 .. 5;
      Reserved_6_6   at 0 range 6 .. 6;
      PCMSYNC        at 0 range 7 .. 7;
      DATLEN         at 0 range 8 .. 9;
      CHLEN          at 0 range 10 .. 10;
      CKPOL          at 0 range 11 .. 11;
      FIXCH          at 0 range 12 .. 12;
      WSINV          at 0 range 13 .. 13;
      DATFMT         at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      I2SDIV         at 0 range 16 .. 23;
      ODD            at 0 range 24 .. 24;
      MCKOE          at 0 range 25 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Serial peripheral interface
   type SPI_Peripheral is record
      --  control register 1
      CR1     : aliased CR1_Register;
      --  control register 2
      CR2     : aliased CR2_Register;
      --  configuration register 1
      CFG1    : aliased CFG1_Register;
      --  configuration register 2
      CFG2    : aliased CFG2_Register;
      --  Interrupt Enable Register
      IER     : aliased IER_Register;
      --  Status Register
      SR      : aliased SR_Register;
      --  Interrupt/Status Flags Clear Register
      IFCR    : aliased IFCR_Register;
      --  Transmit Data Register
      TXDR    : aliased STM32H753.UInt32;
      --  Receive Data Register
      RXDR    : aliased STM32H753.UInt32;
      --  Polynomial Register
      CRCPOLY : aliased STM32H753.UInt32;
      --  Transmitter CRC Register
      TXCRC   : aliased STM32H753.UInt32;
      --  Receiver CRC Register
      RXCRC   : aliased STM32H753.UInt32;
      --  Underrun Data Register
      UDRDR   : aliased STM32H753.UInt32;
      --  configuration register
      CGFR    : aliased CGFR_Register;
   end record
     with Volatile;

   for SPI_Peripheral use record
      CR1     at 16#0# range 0 .. 31;
      CR2     at 16#4# range 0 .. 31;
      CFG1    at 16#8# range 0 .. 31;
      CFG2    at 16#C# range 0 .. 31;
      IER     at 16#10# range 0 .. 31;
      SR      at 16#14# range 0 .. 31;
      IFCR    at 16#18# range 0 .. 31;
      TXDR    at 16#20# range 0 .. 31;
      RXDR    at 16#30# range 0 .. 31;
      CRCPOLY at 16#40# range 0 .. 31;
      TXCRC   at 16#44# range 0 .. 31;
      RXCRC   at 16#48# range 0 .. 31;
      UDRDR   at 16#4C# range 0 .. 31;
      CGFR    at 16#50# range 0 .. 31;
   end record;

   --  Serial peripheral interface
   SPI1_Periph : aliased SPI_Peripheral
     with Import, Address => SPI1_Base;

   --  Serial peripheral interface
   SPI2_Periph : aliased SPI_Peripheral
     with Import, Address => SPI2_Base;

   --  Serial peripheral interface
   SPI3_Periph : aliased SPI_Peripheral
     with Import, Address => SPI3_Base;

   --  Serial peripheral interface
   SPI4_Periph : aliased SPI_Peripheral
     with Import, Address => SPI4_Base;

   --  Serial peripheral interface
   SPI5_Periph : aliased SPI_Peripheral
     with Import, Address => SPI5_Base;

   --  Serial peripheral interface
   SPI6_Periph : aliased SPI_Peripheral
     with Import, Address => SPI6_Base;

end STM32H753.SPI;
