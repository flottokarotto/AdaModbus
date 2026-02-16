pragma Style_Checks (Off);

--  This spec has been automatically generated from STM32H753.svd


with System;

package STM32H753.CEC is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   --  CEC control register
   type CR_Register is record
      --  CEC Enable The CECEN bit is set and cleared by software. CECEN=1
      --  starts message reception and enables the TXSOM control. CECEN=0
      --  disables the CEC peripheral, clears all bits of CEC_CR register and
      --  aborts any on-going reception or transmission.
      CECEN         : Boolean := False;
      --  Tx Start Of Message TXSOM is set by software to command transmission
      --  of the first byte of a CEC message. If the CEC message consists of
      --  only one byte, TXEOM must be set before of TXSOM. Start-Bit is
      --  effectively started on the CEC line after SFT is counted. If TXSOM is
      --  set while a message reception is ongoing, transmission will start
      --  after the end of reception. TXSOM is cleared by hardware after the
      --  last byte of the message is sent with a positive acknowledge
      --  (TXEND=1), in case of transmission underrun (TXUDR=1), negative
      --  acknowledge (TXACKE=1), and transmission error (TXERR=1). It is also
      --  cleared by CECEN=0. It is not cleared and transmission is
      --  automatically retried in case of arbitration lost (ARBLST=1). TXSOM
      --  can be also used as a status bit informing application whether any
      --  transmission request is pending or under execution. The application
      --  can abort a transmission request at any time by clearing the CECEN
      --  bit. Note: TXSOM must be set when CECEN=1 TXSOM must be set when
      --  transmission data is available into TXDR HEADERs first four bits
      --  containing own peripheral address are taken from TXDR[7:4], not from
      --  CEC_CFGR.OAR which is used only for reception
      TXSOM         : Boolean := False;
      --  Tx End Of Message The TXEOM bit is set by software to command
      --  transmission of the last byte of a CEC message. TXEOM is cleared by
      --  hardware at the same time and under the same conditions as for TXSOM.
      --  Note: TXEOM must be set when CECEN=1 TXEOM must be set before writing
      --  transmission data to TXDR If TXEOM is set when TXSOM=0, transmitted
      --  message will consist of 1 byte (HEADER) only (PING message)
      TXEOM         : Boolean := False;
      --  unspecified
      Reserved_3_31 : STM32H753.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      CECEN         at 0 range 0 .. 0;
      TXSOM         at 0 range 1 .. 1;
      TXEOM         at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   subtype CFGR_SFT_Field is STM32H753.UInt3;
   subtype CFGR_OAR_Field is STM32H753.UInt15;

   --  This register is used to configure the HDMI-CEC controller. It is
   --  mandatory to write CEC_CFGR only when CECEN=0.
   type CFGR_Register is record
      --  Signal Free Time SFT bits are set by software. In the SFT=0x0
      --  configuration the number of nominal data bit periods waited before
      --  transmission is ruled by hardware according to the transmission
      --  history. In all the other configurations the SFT number is determined
      --  by software. * 0x0 ** 2.5 Data-Bit periods if CEC is the last bus
      --  initiator with unsuccessful transmission (ARBLST=1, TXERR=1, TXUDR=1
      --  or TXACKE= 1) ** 4 Data-Bit periods if CEC is the new bus initiator
      --  ** 6 Data-Bit periods if CEC is the last bus initiator with
      --  successful transmission (TXEOM=1) * 0x1: 0.5 nominal data bit periods
      --  * 0x2: 1.5 nominal data bit periods * 0x3: 2.5 nominal data bit
      --  periods * 0x4: 3.5 nominal data bit periods * 0x5: 4.5 nominal data
      --  bit periods * 0x6: 5.5 nominal data bit periods * 0x7: 6.5 nominal
      --  data bit periods
      SFT           : CFGR_SFT_Field := 16#0#;
      --  Rx-Tolerance The RXTOL bit is set and cleared by software. **
      --  Start-Bit, +/- 200 s rise, +/- 200 s fall. ** Data-Bit: +/- 200 s
      --  rise. +/- 350 s fall. ** Start-Bit: +/- 400 s rise, +/- 400 s fall **
      --  Data-Bit: +/-300 s rise, +/- 500 s fall
      RXTOL         : Boolean := False;
      --  Rx-Stop on Bit Rising Error The BRESTP bit is set and cleared by
      --  software.
      BRESTP        : Boolean := False;
      --  Generate Error-Bit on Bit Rising Error The BREGEN bit is set and
      --  cleared by software. Note: If BRDNOGEN=0, an Error-bit is generated
      --  upon BRE detection with BRESTP=1 in broadcast even if BREGEN=0
      BREGEN        : Boolean := False;
      --  Generate Error-Bit on Long Bit Period Error The LBPEGEN bit is set
      --  and cleared by software. Note: If BRDNOGEN=0, an Error-bit is
      --  generated upon LBPE detection in broadcast even if LBPEGEN=0
      LBPEGEN       : Boolean := False;
      --  Avoid Error-Bit Generation in Broadcast The BRDNOGEN bit is set and
      --  cleared by software.
      BRDNOGEN      : Boolean := False;
      --  SFT Option Bit The SFTOPT bit is set and cleared by software.
      SFTOPT        : Boolean := False;
      --  unspecified
      Reserved_9_15 : STM32H753.UInt7 := 16#0#;
      --  Own addresses configuration The OAR bits are set by software to
      --  select which destination logical addresses has to be considered in
      --  receive mode. Each bit, when set, enables the CEC logical address
      --  identified by the given bit position. At the end of HEADER reception,
      --  the received destination address is compared with the enabled
      --  addresses. In case of matching address, the incoming message is
      --  acknowledged and received. In case of non-matching address, the
      --  incoming message is received only in listen mode (LSTN=1), but
      --  without acknowledge sent. Broadcast messages are always received.
      --  Example: OAR = 0b000 0000 0010 0001 means that CEC acknowledges
      --  addresses 0x0 and 0x5. Consequently, each message directed to one of
      --  these addresses is received.
      OAR           : CFGR_OAR_Field := 16#0#;
      --  Listen mode LSTN bit is set and cleared by software.
      LSTN          : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CFGR_Register use record
      SFT           at 0 range 0 .. 2;
      RXTOL         at 0 range 3 .. 3;
      BRESTP        at 0 range 4 .. 4;
      BREGEN        at 0 range 5 .. 5;
      LBPEGEN       at 0 range 6 .. 6;
      BRDNOGEN      at 0 range 7 .. 7;
      SFTOPT        at 0 range 8 .. 8;
      Reserved_9_15 at 0 range 9 .. 15;
      OAR           at 0 range 16 .. 30;
      LSTN          at 0 range 31 .. 31;
   end record;

   subtype TXDR_TXD_Field is STM32H753.Byte;

   --  CEC Tx data register
   type TXDR_Register is record
      --  Write-only. Tx Data register. TXD is a write-only register containing
      --  the data byte to be transmitted. Note: TXD must be written when
      --  TXSTART=1
      TXD           : TXDR_TXD_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : STM32H753.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TXDR_Register use record
      TXD           at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype RXDR_RXD_Field is STM32H753.Byte;

   --  CEC Rx Data Register
   type RXDR_Register is record
      --  Read-only. Rx Data register. RXD is read-only and contains the last
      --  data byte which has been received from the CEC line.
      RXD           : RXDR_RXD_Field;
      --  unspecified
      Reserved_8_31 : STM32H753.UInt24;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RXDR_Register use record
      RXD           at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  CEC Interrupt and Status Register
   type ISR_Register is record
      --  Rx-Byte Received The RXBR bit is set by hardware to inform
      --  application that a new byte has been received from the CEC line and
      --  stored into the RXD buffer. RXBR is cleared by software write at 1.
      RXBR           : Boolean := False;
      --  End Of Reception RXEND is set by hardware to inform application that
      --  the last byte of a CEC message is received from the CEC line and
      --  stored into the RXD buffer. RXEND is set at the same time of RXBR.
      --  RXEND is cleared by software write at 1.
      RXEND          : Boolean := False;
      --  Rx-Overrun RXOVR is set by hardware if RXBR is not yet cleared at the
      --  time a new byte is received on the CEC line and stored into RXD.
      --  RXOVR assertion stops message reception so that no acknowledge is
      --  sent. In case of broadcast, a negative acknowledge is sent. RXOVR is
      --  cleared by software write at 1.
      RXOVR          : Boolean := False;
      --  Rx-Bit Rising Error BRE is set by hardware in case a Data-Bit
      --  waveform is detected with Bit Rising Error. BRE is set either at the
      --  time the misplaced rising edge occurs, or at the end of the maximum
      --  BRE tolerance allowed by RXTOL, in case rising edge is still longing.
      --  BRE stops message reception if BRESTP=1. BRE generates an Error-Bit
      --  on the CEC line if BREGEN=1. BRE is cleared by software write at 1.
      BRE            : Boolean := False;
      --  Rx-Short Bit Period Error SBPE is set by hardware in case a Data-Bit
      --  waveform is detected with Short Bit Period Error. SBPE is set at the
      --  time the anticipated falling edge occurs. SBPE generates an Error-Bit
      --  on the CEC line. SBPE is cleared by software write at 1.
      SBPE           : Boolean := False;
      --  Rx-Long Bit Period Error LBPE is set by hardware in case a Data-Bit
      --  waveform is detected with Long Bit Period Error. LBPE is set at the
      --  end of the maximum bit-extension tolerance allowed by RXTOL, in case
      --  falling edge is still longing. LBPE always stops reception of the CEC
      --  message. LBPE generates an Error-Bit on the CEC line if LBPEGEN=1. In
      --  case of broadcast, Error-Bit is generated even in case of LBPEGEN=0.
      --  LBPE is cleared by software write at 1.
      LBPE           : Boolean := False;
      --  Rx-Missing Acknowledge In receive mode, RXACKE is set by hardware to
      --  inform application that no acknowledge was seen on the CEC line.
      --  RXACKE applies only for broadcast messages and in listen mode also
      --  for not directly addressed messages (destination address not enabled
      --  in OAR). RXACKE aborts message reception. RXACKE is cleared by
      --  software write at 1.
      RXACKE         : Boolean := False;
      --  Arbitration Lost ARBLST is set by hardware to inform application that
      --  CEC device is switching to reception due to arbitration lost event
      --  following the TXSOM command. ARBLST can be due either to a contending
      --  CEC device starting earlier or starting at the same time but with
      --  higher HEADER priority. After ARBLST assertion TXSOM bit keeps
      --  pending for next transmission attempt. ARBLST is cleared by software
      --  write at 1.
      ARBLST         : Boolean := False;
      --  Tx-Byte Request TXBR is set by hardware to inform application that
      --  the next transmission data has to be written to TXDR. TXBR is set
      --  when the 4th bit of currently transmitted byte is sent. Application
      --  must write the next byte to TXDR within 6 nominal data-bit periods
      --  before transmission underrun error occurs (TXUDR). TXBR is cleared by
      --  software write at 1.
      TXBR           : Boolean := False;
      --  End of Transmission TXEND is set by hardware to inform application
      --  that the last byte of the CEC message has been successfully
      --  transmitted. TXEND clears the TXSOM and TXEOM control bits. TXEND is
      --  cleared by software write at 1.
      TXEND          : Boolean := False;
      --  Tx-Buffer Underrun In transmission mode, TXUDR is set by hardware if
      --  application was not in time to load TXDR before of next byte
      --  transmission. TXUDR aborts message transmission and clears TXSOM and
      --  TXEOM control bits. TXUDR is cleared by software write at 1
      TXUDR          : Boolean := False;
      --  Tx-Error In transmission mode, TXERR is set by hardware if the CEC
      --  initiator detects low impedance on the CEC line while it is released.
      --  TXERR aborts message transmission and clears TXSOM and TXEOM
      --  controls. TXERR is cleared by software write at 1.
      TXERR          : Boolean := False;
      --  Tx-Missing Acknowledge Error In transmission mode, TXACKE is set by
      --  hardware to inform application that no acknowledge was received. In
      --  case of broadcast transmission, TXACKE informs application that a
      --  negative acknowledge was received. TXACKE aborts message transmission
      --  and clears TXSOM and TXEOM controls. TXACKE is cleared by software
      --  write at 1.
      TXACKE         : Boolean := False;
      --  unspecified
      Reserved_13_31 : STM32H753.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISR_Register use record
      RXBR           at 0 range 0 .. 0;
      RXEND          at 0 range 1 .. 1;
      RXOVR          at 0 range 2 .. 2;
      BRE            at 0 range 3 .. 3;
      SBPE           at 0 range 4 .. 4;
      LBPE           at 0 range 5 .. 5;
      RXACKE         at 0 range 6 .. 6;
      ARBLST         at 0 range 7 .. 7;
      TXBR           at 0 range 8 .. 8;
      TXEND          at 0 range 9 .. 9;
      TXUDR          at 0 range 10 .. 10;
      TXERR          at 0 range 11 .. 11;
      TXACKE         at 0 range 12 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   --  CEC interrupt enable register
   type IER_Register is record
      --  Rx-Byte Received Interrupt Enable The RXBRIE bit is set and cleared
      --  by software.
      RXBRIE         : Boolean := False;
      --  End Of Reception Interrupt Enable The RXENDIE bit is set and cleared
      --  by software.
      RXENDIE        : Boolean := False;
      --  Rx-Buffer Overrun Interrupt Enable The RXOVRIE bit is set and cleared
      --  by software.
      RXOVRIE        : Boolean := False;
      --  Bit Rising Error Interrupt Enable The BREIE bit is set and cleared by
      --  software.
      BREIE          : Boolean := False;
      --  Short Bit Period Error Interrupt Enable The SBPEIE bit is set and
      --  cleared by software.
      SBPEIE         : Boolean := False;
      --  Long Bit Period Error Interrupt Enable The LBPEIE bit is set and
      --  cleared by software.
      LBPEIE         : Boolean := False;
      --  Rx-Missing Acknowledge Error Interrupt Enable The RXACKIE bit is set
      --  and cleared by software.
      RXACKIE        : Boolean := False;
      --  Arbitration Lost Interrupt Enable The ARBLSTIE bit is set and cleared
      --  by software.
      ARBLSTIE       : Boolean := False;
      --  Tx-Byte Request Interrupt Enable The TXBRIE bit is set and cleared by
      --  software.
      TXBRIE         : Boolean := False;
      --  Tx-End Of Message Interrupt Enable The TXENDIE bit is set and cleared
      --  by software.
      TXENDIE        : Boolean := False;
      --  Tx-Underrun Interrupt Enable The TXUDRIE bit is set and cleared by
      --  software.
      TXUDRIE        : Boolean := False;
      --  Tx-Error Interrupt Enable The TXERRIE bit is set and cleared by
      --  software.
      TXERRIE        : Boolean := False;
      --  Tx-Missing Acknowledge Error Interrupt Enable The TXACKEIE bit is set
      --  and cleared by software.
      TXACKIE        : Boolean := False;
      --  unspecified
      Reserved_13_31 : STM32H753.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IER_Register use record
      RXBRIE         at 0 range 0 .. 0;
      RXENDIE        at 0 range 1 .. 1;
      RXOVRIE        at 0 range 2 .. 2;
      BREIE          at 0 range 3 .. 3;
      SBPEIE         at 0 range 4 .. 4;
      LBPEIE         at 0 range 5 .. 5;
      RXACKIE        at 0 range 6 .. 6;
      ARBLSTIE       at 0 range 7 .. 7;
      TXBRIE         at 0 range 8 .. 8;
      TXENDIE        at 0 range 9 .. 9;
      TXUDRIE        at 0 range 10 .. 10;
      TXERRIE        at 0 range 11 .. 11;
      TXACKIE        at 0 range 12 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  CEC
   type CEC_Peripheral is record
      --  CEC control register
      CR   : aliased CR_Register;
      --  This register is used to configure the HDMI-CEC controller. It is
      --  mandatory to write CEC_CFGR only when CECEN=0.
      CFGR : aliased CFGR_Register;
      --  CEC Tx data register
      TXDR : aliased TXDR_Register;
      --  CEC Rx Data Register
      RXDR : aliased RXDR_Register;
      --  CEC Interrupt and Status Register
      ISR  : aliased ISR_Register;
      --  CEC interrupt enable register
      IER  : aliased IER_Register;
   end record
     with Volatile;

   for CEC_Peripheral use record
      CR   at 16#0# range 0 .. 31;
      CFGR at 16#4# range 0 .. 31;
      TXDR at 16#8# range 0 .. 31;
      RXDR at 16#C# range 0 .. 31;
      ISR  at 16#10# range 0 .. 31;
      IER  at 16#14# range 0 .. 31;
   end record;

   --  CEC
   CEC_Periph : aliased CEC_Peripheral
     with Import, Address => CEC_Base;

end STM32H753.CEC;
