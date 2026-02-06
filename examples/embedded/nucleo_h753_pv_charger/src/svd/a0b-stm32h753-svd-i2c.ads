pragma Style_Checks (Off);

--  This spec has been automatically generated from STM32H753.svd


with A0B.Types.SVD;
with System;

package A0B.STM32H753.SVD.I2C is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   subtype CR1_DNF_Field is A0B.Types.SVD.UInt4;

   --  Access: No wait states, except if a write access occurs while a write
   --  access to this register is ongoing. In this case, wait states are
   --  inserted in the second write access until the previous one is completed.
   --  The latency of the second write access can be up to 2 x PCLK1 + 6 x
   --  I2CCLK.
   type CR1_Register is record
      --  Peripheral enable Note: When PE=0, the I2C SCL and SDA lines are
      --  released. Internal state machines and status bits are put back to
      --  their reset value. When cleared, PE must be kept low for at least 3
      --  APB clock cycles.
      PE             : Boolean := False;
      --  TX Interrupt enable
      TXIE           : Boolean := False;
      --  RX Interrupt enable
      RXIE           : Boolean := False;
      --  Address match Interrupt enable (slave only)
      ADDRIE         : Boolean := False;
      --  Not acknowledge received Interrupt enable
      NACKIE         : Boolean := False;
      --  STOP detection Interrupt enable
      STOPIE         : Boolean := False;
      --  Transfer Complete interrupt enable Note: Any of these events will
      --  generate an interrupt: Transfer Complete (TC) Transfer Complete
      --  Reload (TCR)
      TCIE           : Boolean := False;
      --  Error interrupts enable Note: Any of these errors generate an
      --  interrupt: Arbitration Loss (ARLO) Bus Error detection (BERR)
      --  Overrun/Underrun (OVR) Timeout detection (TIMEOUT) PEC error
      --  detection (PECERR) Alert pin event detection (ALERT)
      ERRIE          : Boolean := False;
      --  Digital noise filter These bits are used to configure the digital
      --  noise filter on SDA and SCL input. The digital filter will filter
      --  spikes with a length of up to DNF[3:0] * tI2CCLK ... Note: If the
      --  analog filter is also enabled, the digital filter is added to the
      --  analog filter. This filter can only be programmed when the I2C is
      --  disabled (PE = 0).
      DNF            : CR1_DNF_Field := 16#0#;
      --  Analog noise filter OFF Note: This bit can only be programmed when
      --  the I2C is disabled (PE = 0).
      ANFOFF         : Boolean := False;
      --  unspecified
      Reserved_13_13 : A0B.Types.SVD.Bit := 16#0#;
      --  DMA transmission requests enable
      TXDMAEN        : Boolean := False;
      --  DMA reception requests enable
      RXDMAEN        : Boolean := False;
      --  Slave byte control This bit is used to enable hardware byte control
      --  in slave mode.
      SBC            : Boolean := False;
      --  Clock stretching disable This bit is used to disable clock stretching
      --  in slave mode. It must be kept cleared in master mode. Note: This bit
      --  can only be programmed when the I2C is disabled (PE = 0).
      NOSTRETCH      : Boolean := False;
      --  Wakeup from Stop mode enable Note: If the Wakeup from Stop mode
      --  feature is not supported, this bit is reserved and forced by hardware
      --  to 0. Please refer to Section25.3: I2C implementation. Note: WUPEN
      --  can be set only when DNF = 0000
      WUPEN          : Boolean := False;
      --  General call enable
      GCEN           : Boolean := False;
      --  SMBus Host address enable Note: If the SMBus feature is not
      --  supported, this bit is reserved and forced by hardware to 0. Please
      --  refer to Section25.3: I2C implementation.
      SMBHEN         : Boolean := False;
      --  SMBus Device Default address enable Note: If the SMBus feature is not
      --  supported, this bit is reserved and forced by hardware to 0. Please
      --  refer to Section25.3: I2C implementation.
      SMBDEN         : Boolean := False;
      --  SMBus alert enable Device mode (SMBHEN=0): Host mode (SMBHEN=1):
      --  Note: When ALERTEN=0, the SMBA pin can be used as a standard GPIO. If
      --  the SMBus feature is not supported, this bit is reserved and forced
      --  by hardware to 0. Please refer to Section25.3: I2C implementation.
      ALERTEN        : Boolean := False;
      --  PEC enable Note: If the SMBus feature is not supported, this bit is
      --  reserved and forced by hardware to 0. Please refer to Section25.3:
      --  I2C implementation.
      PECEN          : Boolean := False;
      --  unspecified
      Reserved_24_31 : A0B.Types.SVD.Byte := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for CR1_Register use record
      PE             at 0 range 0 .. 0;
      TXIE           at 0 range 1 .. 1;
      RXIE           at 0 range 2 .. 2;
      ADDRIE         at 0 range 3 .. 3;
      NACKIE         at 0 range 4 .. 4;
      STOPIE         at 0 range 5 .. 5;
      TCIE           at 0 range 6 .. 6;
      ERRIE          at 0 range 7 .. 7;
      DNF            at 0 range 8 .. 11;
      ANFOFF         at 0 range 12 .. 12;
      Reserved_13_13 at 0 range 13 .. 13;
      TXDMAEN        at 0 range 14 .. 14;
      RXDMAEN        at 0 range 15 .. 15;
      SBC            at 0 range 16 .. 16;
      NOSTRETCH      at 0 range 17 .. 17;
      WUPEN          at 0 range 18 .. 18;
      GCEN           at 0 range 19 .. 19;
      SMBHEN         at 0 range 20 .. 20;
      SMBDEN         at 0 range 21 .. 21;
      ALERTEN        at 0 range 22 .. 22;
      PECEN          at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  CR2_SADD array
   type CR2_SADD_Field_Array is array (0 .. 9) of Boolean
     with Component_Size => 1, Size => 10;

   --  Type definition for CR2_SADD
   type CR2_SADD_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SADD as a value
            Val : A0B.Types.SVD.UInt10;
         when True =>
            --  SADD as an array
            Arr : CR2_SADD_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 10;

   for CR2_SADD_Field use record
      Val at 0 range 0 .. 9;
      Arr at 0 range 0 .. 9;
   end record;

   subtype CR2_NBYTES_Field is A0B.Types.SVD.Byte;

   --  Access: No wait states, except if a write access occurs while a write
   --  access to this register is ongoing. In this case, wait states are
   --  inserted in the second write access until the previous one is completed.
   --  The latency of the second write access can be up to 2 x PCLK1 + 6 x
   --  I2CCLK.
   type CR2_Register is record
      --  Slave address bit 0 (master mode) In 7-bit addressing mode (ADD10 =
      --  0): This bit is dont care In 10-bit addressing mode (ADD10 = 1): This
      --  bit should be written with bit 0 of the slave address to be sent
      --  Note: Changing these bits when the START bit is set is not allowed.
      SADD           : CR2_SADD_Field := (As_Array => False, Val => 16#0#);
      --  Transfer direction (master mode) Note: Changing this bit when the
      --  START bit is set is not allowed.
      RD_WRN         : Boolean := False;
      --  10-bit addressing mode (master mode) Note: Changing this bit when the
      --  START bit is set is not allowed.
      ADD10          : Boolean := False;
      --  10-bit address header only read direction (master receiver mode)
      --  Note: Changing this bit when the START bit is set is not allowed.
      HEAD10R        : Boolean := False;
      --  Start generation This bit is set by software, and cleared by hardware
      --  after the Start followed by the address sequence is sent, by an
      --  arbitration loss, by a timeout error detection, or when PE = 0. It
      --  can also be cleared by software by writing 1 to the ADDRCF bit in the
      --  I2C_ICR register. If the I2C is already in master mode with AUTOEND =
      --  0, setting this bit generates a Repeated Start condition when
      --  RELOAD=0, after the end of the NBYTES transfer. Otherwise setting
      --  this bit will generate a START condition once the bus is free. Note:
      --  Writing 0 to this bit has no effect. The START bit can be set even if
      --  the bus is BUSY or I2C is in slave mode. This bit has no effect when
      --  RELOAD is set.
      START          : Boolean := False;
      --  Stop generation (master mode) The bit is set by software, cleared by
      --  hardware when a Stop condition is detected, or when PE = 0. In Master
      --  Mode: Note: Writing 0 to this bit has no effect.
      STOP           : Boolean := False;
      --  NACK generation (slave mode) The bit is set by software, cleared by
      --  hardware when the NACK is sent, or when a STOP condition or an
      --  Address matched is received, or when PE=0. Note: Writing 0 to this
      --  bit has no effect. This bit is used in slave mode only: in master
      --  receiver mode, NACK is automatically generated after last byte
      --  preceding STOP or RESTART condition, whatever the NACK bit value.
      --  When an overrun occurs in slave receiver NOSTRETCH mode, a NACK is
      --  automatically generated whatever the NACK bit value. When hardware
      --  PEC checking is enabled (PECBYTE=1), the PEC acknowledge value does
      --  not depend on the NACK value.
      NACK           : Boolean := False;
      --  Number of bytes The number of bytes to be transmitted/received is
      --  programmed there. This field is dont care in slave mode with SBC=0.
      --  Note: Changing these bits when the START bit is set is not allowed.
      NBYTES         : CR2_NBYTES_Field := 16#0#;
      --  NBYTES reload mode This bit is set and cleared by software.
      RELOAD         : Boolean := False;
      --  Automatic end mode (master mode) This bit is set and cleared by
      --  software. Note: This bit has no effect in slave mode or when the
      --  RELOAD bit is set.
      AUTOEND        : Boolean := False;
      --  Packet error checking byte This bit is set by software, and cleared
      --  by hardware when the PEC is transferred, or when a STOP condition or
      --  an Address matched is received, also when PE=0. Note: Writing 0 to
      --  this bit has no effect. This bit has no effect when RELOAD is set.
      --  This bit has no effect is slave mode when SBC=0. If the SMBus feature
      --  is not supported, this bit is reserved and forced by hardware to 0.
      --  Please refer to Section25.3: I2C implementation.
      PECBYTE        : Boolean := False;
      --  unspecified
      Reserved_27_31 : A0B.Types.SVD.UInt5 := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for CR2_Register use record
      SADD           at 0 range 0 .. 9;
      RD_WRN         at 0 range 10 .. 10;
      ADD10          at 0 range 11 .. 11;
      HEAD10R        at 0 range 12 .. 12;
      START          at 0 range 13 .. 13;
      STOP           at 0 range 14 .. 14;
      NACK           at 0 range 15 .. 15;
      NBYTES         at 0 range 16 .. 23;
      RELOAD         at 0 range 24 .. 24;
      AUTOEND        at 0 range 25 .. 25;
      PECBYTE        at 0 range 26 .. 26;
      Reserved_27_31 at 0 range 27 .. 31;
   end record;

   subtype OAR1_OA1_Field is A0B.Types.SVD.UInt10;

   --  Access: No wait states, except if a write access occurs while a write
   --  access to this register is ongoing. In this case, wait states are
   --  inserted in the second write access until the previous one is completed.
   --  The latency of the second write access can be up to 2 x PCLK1 + 6 x
   --  I2CCLK.
   type OAR1_Register is record
      --  Interface address 7-bit addressing mode: dont care 10-bit addressing
      --  mode: bits 9:8 of address Note: These bits can be written only when
      --  OA1EN=0. OA1[7:1]: Interface address Bits 7:1 of address Note: These
      --  bits can be written only when OA1EN=0. OA1[0]: Interface address
      --  7-bit addressing mode: dont care 10-bit addressing mode: bit 0 of
      --  address Note: This bit can be written only when OA1EN=0.
      OA1            : OAR1_OA1_Field := 16#0#;
      --  Own Address 1 10-bit mode Note: This bit can be written only when
      --  OA1EN=0.
      OA1MODE        : Boolean := False;
      --  unspecified
      Reserved_11_14 : A0B.Types.SVD.UInt4 := 16#0#;
      --  Own Address 1 enable
      OA1EN          : Boolean := False;
      --  unspecified
      Reserved_16_31 : A0B.Types.SVD.UInt16 := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for OAR1_Register use record
      OA1            at 0 range 0 .. 9;
      OA1MODE        at 0 range 10 .. 10;
      Reserved_11_14 at 0 range 11 .. 14;
      OA1EN          at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype OAR2_OA2_Field is A0B.Types.SVD.UInt7;
   subtype OAR2_OA2MSK_Field is A0B.Types.SVD.UInt3;

   --  Access: No wait states, except if a write access occurs while a write
   --  access to this register is ongoing. In this case, wait states are
   --  inserted in the second write access until the previous one is completed.
   --  The latency of the second write access can be up to 2 x PCLK1 + 6 x
   --  I2CCLK.
   type OAR2_Register is record
      --  unspecified
      Reserved_0_0   : A0B.Types.SVD.Bit := 16#0#;
      --  Interface address bits 7:1 of address Note: These bits can be written
      --  only when OA2EN=0.
      OA2            : OAR2_OA2_Field := 16#0#;
      --  Own Address 2 masks Note: These bits can be written only when
      --  OA2EN=0. As soon as OA2MSK is not equal to 0, the reserved I2C
      --  addresses (0b0000xxx and 0b1111xxx) are not acknowledged even if the
      --  comparison matches.
      OA2MSK         : OAR2_OA2MSK_Field := 16#0#;
      --  unspecified
      Reserved_11_14 : A0B.Types.SVD.UInt4 := 16#0#;
      --  Own Address 2 enable
      OA2EN          : Boolean := False;
      --  unspecified
      Reserved_16_31 : A0B.Types.SVD.UInt16 := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for OAR2_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      OA2            at 0 range 1 .. 7;
      OA2MSK         at 0 range 8 .. 10;
      Reserved_11_14 at 0 range 11 .. 14;
      OA2EN          at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype TIMINGR_SCLL_Field is A0B.Types.SVD.Byte;
   subtype TIMINGR_SCLH_Field is A0B.Types.SVD.Byte;
   subtype TIMINGR_SDADEL_Field is A0B.Types.SVD.UInt4;
   subtype TIMINGR_SCLDEL_Field is A0B.Types.SVD.UInt4;
   subtype TIMINGR_PRESC_Field is A0B.Types.SVD.UInt4;

   --  Access: No wait states
   type TIMINGR_Register is record
      --  SCL low period (master mode) This field is used to generate the SCL
      --  low period in master mode. tSCLL = (SCLL+1) x tPRESC Note: SCLL is
      --  also used to generate tBUF and tSU:STA timings.
      SCLL           : TIMINGR_SCLL_Field := 16#0#;
      --  SCL high period (master mode) This field is used to generate the SCL
      --  high period in master mode. tSCLH = (SCLH+1) x tPRESC Note: SCLH is
      --  also used to generate tSU:STO and tHD:STA timing.
      SCLH           : TIMINGR_SCLH_Field := 16#0#;
      --  Data hold time This field is used to generate the delay tSDADEL
      --  between SCL falling edge and SDA edge. In master mode and in slave
      --  mode with NOSTRETCH = 0, the SCL line is stretched low during
      --  tSDADEL. tSDADEL= SDADEL x tPRESC Note: SDADEL is used to generate
      --  tHD:DAT timing.
      SDADEL         : TIMINGR_SDADEL_Field := 16#0#;
      --  Data setup time This field is used to generate a delay tSCLDEL
      --  between SDA edge and SCL rising edge. In master mode and in slave
      --  mode with NOSTRETCH = 0, the SCL line is stretched low during
      --  tSCLDEL. tSCLDEL = (SCLDEL+1) x tPRESC Note: tSCLDEL is used to
      --  generate tSU:DAT timing.
      SCLDEL         : TIMINGR_SCLDEL_Field := 16#0#;
      --  unspecified
      Reserved_24_27 : A0B.Types.SVD.UInt4 := 16#0#;
      --  Timing prescaler This field is used to prescale I2CCLK in order to
      --  generate the clock period tPRESC used for data setup and hold
      --  counters (refer to I2C timings on page9) and for SCL high and low
      --  level counters (refer to I2C master initialization on page24). tPRESC
      --  = (PRESC+1) x tI2CCLK
      PRESC          : TIMINGR_PRESC_Field := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for TIMINGR_Register use record
      SCLL           at 0 range 0 .. 7;
      SCLH           at 0 range 8 .. 15;
      SDADEL         at 0 range 16 .. 19;
      SCLDEL         at 0 range 20 .. 23;
      Reserved_24_27 at 0 range 24 .. 27;
      PRESC          at 0 range 28 .. 31;
   end record;

   subtype TIMEOUTR_TIMEOUTA_Field is A0B.Types.SVD.UInt12;
   subtype TIMEOUTR_TIMEOUTB_Field is A0B.Types.SVD.UInt12;

   --  Access: No wait states, except if a write access occurs while a write
   --  access to this register is ongoing. In this case, wait states are
   --  inserted in the second write access until the previous one is completed.
   --  The latency of the second write access can be up to 2 x PCLK1 + 6 x
   --  I2CCLK.
   type TIMEOUTR_Register is record
      --  Bus Timeout A This field is used to configure: The SCL low timeout
      --  condition tTIMEOUT when TIDLE=0 tTIMEOUT= (TIMEOUTA+1) x 2048 x
      --  tI2CCLK The bus idle condition (both SCL and SDA high) when TIDLE=1
      --  tIDLE= (TIMEOUTA+1) x 4 x tI2CCLK Note: These bits can be written
      --  only when TIMOUTEN=0.
      TIMEOUTA       : TIMEOUTR_TIMEOUTA_Field := 16#0#;
      --  Idle clock timeout detection Note: This bit can be written only when
      --  TIMOUTEN=0.
      TIDLE          : Boolean := False;
      --  unspecified
      Reserved_13_14 : A0B.Types.SVD.UInt2 := 16#0#;
      --  Clock timeout enable
      TIMOUTEN       : Boolean := False;
      --  Bus timeout B This field is used to configure the cumulative clock
      --  extension timeout: In master mode, the master cumulative clock low
      --  extend time (tLOW:MEXT) is detected In slave mode, the slave
      --  cumulative clock low extend time (tLOW:SEXT) is detected tLOW:EXT=
      --  (TIMEOUTB+1) x 2048 x tI2CCLK Note: These bits can be written only
      --  when TEXTEN=0.
      TIMEOUTB       : TIMEOUTR_TIMEOUTB_Field := 16#0#;
      --  unspecified
      Reserved_28_30 : A0B.Types.SVD.UInt3 := 16#0#;
      --  Extended clock timeout enable
      TEXTEN         : Boolean := False;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for TIMEOUTR_Register use record
      TIMEOUTA       at 0 range 0 .. 11;
      TIDLE          at 0 range 12 .. 12;
      Reserved_13_14 at 0 range 13 .. 14;
      TIMOUTEN       at 0 range 15 .. 15;
      TIMEOUTB       at 0 range 16 .. 27;
      Reserved_28_30 at 0 range 28 .. 30;
      TEXTEN         at 0 range 31 .. 31;
   end record;

   subtype ISR_ADDCODE_Field is A0B.Types.SVD.UInt7;

   --  Access: No wait states
   type ISR_Register is record
      --  Transmit data register empty (transmitters) This bit is set by
      --  hardware when the I2C_TXDR register is empty. It is cleared when the
      --  next data to be sent is written in the I2C_TXDR register. This bit
      --  can be written to 1 by software in order to flush the transmit data
      --  register I2C_TXDR. Note: This bit is set by hardware when PE=0.
      TXE            : Boolean := True;
      --  Transmit interrupt status (transmitters) This bit is set by hardware
      --  when the I2C_TXDR register is empty and the data to be transmitted
      --  must be written in the I2C_TXDR register. It is cleared when the next
      --  data to be sent is written in the I2C_TXDR register. This bit can be
      --  written to 1 by software when NOSTRETCH=1 only, in order to generate
      --  a TXIS event (interrupt if TXIE=1 or DMA request if TXDMAEN=1). Note:
      --  This bit is cleared by hardware when PE=0.
      TXIS           : Boolean := False;
      --  Read-only. Receive data register not empty (receivers) This bit is
      --  set by hardware when the received data is copied into the I2C_RXDR
      --  register, and is ready to be read. It is cleared when I2C_RXDR is
      --  read. Note: This bit is cleared by hardware when PE=0.
      RXNE           : Boolean := False;
      --  Read-only. Address matched (slave mode) This bit is set by hardware
      --  as soon as the received slave address matched with one of the enabled
      --  slave addresses. It is cleared by software by setting ADDRCF bit.
      --  Note: This bit is cleared by hardware when PE=0.
      ADDR           : Boolean := False;
      --  Read-only. Not Acknowledge received flag This flag is set by hardware
      --  when a NACK is received after a byte transmission. It is cleared by
      --  software by setting the NACKCF bit. Note: This bit is cleared by
      --  hardware when PE=0.
      NACKF          : Boolean := False;
      --  Read-only. Stop detection flag This flag is set by hardware when a
      --  Stop condition is detected on the bus and the peripheral is involved
      --  in this transfer: either as a master, provided that the STOP
      --  condition is generated by the peripheral. or as a slave, provided
      --  that the peripheral has been addressed previously during this
      --  transfer. It is cleared by software by setting the STOPCF bit. Note:
      --  This bit is cleared by hardware when PE=0.
      STOPF          : Boolean := False;
      --  Read-only. Transfer Complete (master mode) This flag is set by
      --  hardware when RELOAD=0, AUTOEND=0 and NBYTES data have been
      --  transferred. It is cleared by software when START bit or STOP bit is
      --  set. Note: This bit is cleared by hardware when PE=0.
      TC             : Boolean := False;
      --  Read-only. Transfer Complete Reload This flag is set by hardware when
      --  RELOAD=1 and NBYTES data have been transferred. It is cleared by
      --  software when NBYTES is written to a non-zero value. Note: This bit
      --  is cleared by hardware when PE=0. This flag is only for master mode,
      --  or for slave mode when the SBC bit is set.
      TCR            : Boolean := False;
      --  Read-only. Bus error This flag is set by hardware when a misplaced
      --  Start or Stop condition is detected whereas the peripheral is
      --  involved in the transfer. The flag is not set during the address
      --  phase in slave mode. It is cleared by software by setting BERRCF bit.
      --  Note: This bit is cleared by hardware when PE=0.
      BERR           : Boolean := False;
      --  Read-only. Arbitration lost This flag is set by hardware in case of
      --  arbitration loss. It is cleared by software by setting the ARLOCF
      --  bit. Note: This bit is cleared by hardware when PE=0.
      ARLO           : Boolean := False;
      --  Read-only. Overrun/Underrun (slave mode) This flag is set by hardware
      --  in slave mode with NOSTRETCH=1, when an overrun/underrun error
      --  occurs. It is cleared by software by setting the OVRCF bit. Note:
      --  This bit is cleared by hardware when PE=0.
      OVR            : Boolean := False;
      --  Read-only. PEC Error in reception This flag is set by hardware when
      --  the received PEC does not match with the PEC register content. A NACK
      --  is automatically sent after the wrong PEC reception. It is cleared by
      --  software by setting the PECCF bit. Note: This bit is cleared by
      --  hardware when PE=0. If the SMBus feature is not supported, this bit
      --  is reserved and forced by hardware to 0. Please refer to Section25.3:
      --  I2C implementation.
      PECERR         : Boolean := False;
      --  Read-only. Timeout or tLOW detection flag This flag is set by
      --  hardware when a timeout or extended clock timeout occurred. It is
      --  cleared by software by setting the TIMEOUTCF bit. Note: This bit is
      --  cleared by hardware when PE=0. If the SMBus feature is not supported,
      --  this bit is reserved and forced by hardware to 0. Please refer to
      --  Section25.3: I2C implementation.
      TIMEOUT        : Boolean := False;
      --  Read-only. SMBus alert This flag is set by hardware when SMBHEN=1
      --  (SMBus host configuration), ALERTEN=1 and a SMBALERT event (falling
      --  edge) is detected on SMBA pin. It is cleared by software by setting
      --  the ALERTCF bit. Note: This bit is cleared by hardware when PE=0. If
      --  the SMBus feature is not supported, this bit is reserved and forced
      --  by hardware to 0. Please refer to Section25.3: I2C implementation.
      ALERT          : Boolean := False;
      --  unspecified
      Reserved_14_14 : A0B.Types.SVD.Bit := 16#0#;
      --  Read-only. Bus busy This flag indicates that a communication is in
      --  progress on the bus. It is set by hardware when a START condition is
      --  detected. It is cleared by hardware when a Stop condition is
      --  detected, or when PE=0.
      BUSY           : Boolean := False;
      --  Read-only. Transfer direction (Slave mode) This flag is updated when
      --  an address match event occurs (ADDR=1).
      DIR            : Boolean := False;
      --  Read-only. Address match code (Slave mode) These bits are updated
      --  with the received address when an address match event occurs (ADDR =
      --  1). In the case of a 10-bit address, ADDCODE provides the 10-bit
      --  header followed by the 2 MSBs of the address.
      ADDCODE        : ISR_ADDCODE_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : A0B.Types.SVD.Byte := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for ISR_Register use record
      TXE            at 0 range 0 .. 0;
      TXIS           at 0 range 1 .. 1;
      RXNE           at 0 range 2 .. 2;
      ADDR           at 0 range 3 .. 3;
      NACKF          at 0 range 4 .. 4;
      STOPF          at 0 range 5 .. 5;
      TC             at 0 range 6 .. 6;
      TCR            at 0 range 7 .. 7;
      BERR           at 0 range 8 .. 8;
      ARLO           at 0 range 9 .. 9;
      OVR            at 0 range 10 .. 10;
      PECERR         at 0 range 11 .. 11;
      TIMEOUT        at 0 range 12 .. 12;
      ALERT          at 0 range 13 .. 13;
      Reserved_14_14 at 0 range 14 .. 14;
      BUSY           at 0 range 15 .. 15;
      DIR            at 0 range 16 .. 16;
      ADDCODE        at 0 range 17 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  Access: No wait states
   type ICR_Register is record
      --  unspecified
      Reserved_0_2   : A0B.Types.SVD.UInt3 := 16#0#;
      --  Write-only. Address matched flag clear Writing 1 to this bit clears
      --  the ADDR flag in the I2C_ISR register. Writing 1 to this bit also
      --  clears the START bit in the I2C_CR2 register.
      ADDRCF         : Boolean := False;
      --  Write-only. Not Acknowledge flag clear Writing 1 to this bit clears
      --  the ACKF flag in I2C_ISR register.
      NACKCF         : Boolean := False;
      --  Write-only. Stop detection flag clear Writing 1 to this bit clears
      --  the STOPF flag in the I2C_ISR register.
      STOPCF         : Boolean := False;
      --  unspecified
      Reserved_6_7   : A0B.Types.SVD.UInt2 := 16#0#;
      --  Write-only. Bus error flag clear Writing 1 to this bit clears the
      --  BERRF flag in the I2C_ISR register.
      BERRCF         : Boolean := False;
      --  Write-only. Arbitration Lost flag clear Writing 1 to this bit clears
      --  the ARLO flag in the I2C_ISR register.
      ARLOCF         : Boolean := False;
      --  Write-only. Overrun/Underrun flag clear Writing 1 to this bit clears
      --  the OVR flag in the I2C_ISR register.
      OVRCF          : Boolean := False;
      --  Write-only. PEC Error flag clear Writing 1 to this bit clears the
      --  PECERR flag in the I2C_ISR register. Note: If the SMBus feature is
      --  not supported, this bit is reserved and forced by hardware to 0.
      --  Please refer to Section25.3: I2C implementation.
      PECCF          : Boolean := False;
      --  Write-only. Timeout detection flag clear Writing 1 to this bit clears
      --  the TIMEOUT flag in the I2C_ISR register. Note: If the SMBus feature
      --  is not supported, this bit is reserved and forced by hardware to 0.
      --  Please refer to Section25.3: I2C implementation.
      TIMOUTCF       : Boolean := False;
      --  Write-only. Alert flag clear Writing 1 to this bit clears the ALERT
      --  flag in the I2C_ISR register. Note: If the SMBus feature is not
      --  supported, this bit is reserved and forced by hardware to 0. Please
      --  refer to Section25.3: I2C implementation.
      ALERTCF        : Boolean := False;
      --  unspecified
      Reserved_14_31 : A0B.Types.SVD.UInt18 := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for ICR_Register use record
      Reserved_0_2   at 0 range 0 .. 2;
      ADDRCF         at 0 range 3 .. 3;
      NACKCF         at 0 range 4 .. 4;
      STOPCF         at 0 range 5 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      BERRCF         at 0 range 8 .. 8;
      ARLOCF         at 0 range 9 .. 9;
      OVRCF          at 0 range 10 .. 10;
      PECCF          at 0 range 11 .. 11;
      TIMOUTCF       at 0 range 12 .. 12;
      ALERTCF        at 0 range 13 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   subtype PECR_PEC_Field is A0B.Types.SVD.Byte;

   --  Access: No wait states
   type PECR_Register is record
      --  Read-only. Packet error checking register This field contains the
      --  internal PEC when PECEN=1. The PEC is cleared by hardware when PE=0.
      PEC           : PECR_PEC_Field;
      --  unspecified
      Reserved_8_31 : A0B.Types.SVD.UInt24;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for PECR_Register use record
      PEC           at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype RXDR_RXDATA_Field is A0B.Types.SVD.Byte;

   --  Access: No wait states
   type RXDR_Register is record
      --  Read-only. 8-bit receive data Data byte received from the I2C bus.
      RXDATA        : RXDR_RXDATA_Field;
      --  unspecified
      Reserved_8_31 : A0B.Types.SVD.UInt24;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for RXDR_Register use record
      RXDATA        at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype TXDR_TXDATA_Field is A0B.Types.SVD.Byte;

   --  Access: No wait states
   type TXDR_Register is record
      --  8-bit transmit data Data byte to be transmitted to the I2C bus. Note:
      --  These bits can be written only when TXE=1.
      TXDATA        : TXDR_TXDATA_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : A0B.Types.SVD.UInt24 := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for TXDR_Register use record
      TXDATA        at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  I2C
   type I2C_Peripheral is record
      --  Access: No wait states, except if a write access occurs while a write
      --  access to this register is ongoing. In this case, wait states are
      --  inserted in the second write access until the previous one is
      --  completed. The latency of the second write access can be up to 2 x
      --  PCLK1 + 6 x I2CCLK.
      CR1      : aliased CR1_Register;
      pragma Volatile_Full_Access (CR1);
      --  Access: No wait states, except if a write access occurs while a write
      --  access to this register is ongoing. In this case, wait states are
      --  inserted in the second write access until the previous one is
      --  completed. The latency of the second write access can be up to 2 x
      --  PCLK1 + 6 x I2CCLK.
      CR2      : aliased CR2_Register;
      pragma Volatile_Full_Access (CR2);
      --  Access: No wait states, except if a write access occurs while a write
      --  access to this register is ongoing. In this case, wait states are
      --  inserted in the second write access until the previous one is
      --  completed. The latency of the second write access can be up to 2 x
      --  PCLK1 + 6 x I2CCLK.
      OAR1     : aliased OAR1_Register;
      pragma Volatile_Full_Access (OAR1);
      --  Access: No wait states, except if a write access occurs while a write
      --  access to this register is ongoing. In this case, wait states are
      --  inserted in the second write access until the previous one is
      --  completed. The latency of the second write access can be up to 2 x
      --  PCLK1 + 6 x I2CCLK.
      OAR2     : aliased OAR2_Register;
      pragma Volatile_Full_Access (OAR2);
      --  Access: No wait states
      TIMINGR  : aliased TIMINGR_Register;
      pragma Volatile_Full_Access (TIMINGR);
      --  Access: No wait states, except if a write access occurs while a write
      --  access to this register is ongoing. In this case, wait states are
      --  inserted in the second write access until the previous one is
      --  completed. The latency of the second write access can be up to 2 x
      --  PCLK1 + 6 x I2CCLK.
      TIMEOUTR : aliased TIMEOUTR_Register;
      pragma Volatile_Full_Access (TIMEOUTR);
      --  Access: No wait states
      ISR      : aliased ISR_Register;
      pragma Volatile_Full_Access (ISR);
      --  Access: No wait states
      ICR      : aliased ICR_Register;
      pragma Volatile_Full_Access (ICR);
      --  Access: No wait states
      PECR     : aliased PECR_Register;
      pragma Volatile_Full_Access (PECR);
      --  Access: No wait states
      RXDR     : aliased RXDR_Register;
      pragma Volatile_Full_Access (RXDR);
      --  Access: No wait states
      TXDR     : aliased TXDR_Register;
      pragma Volatile_Full_Access (TXDR);
   end record
     with Volatile;

   for I2C_Peripheral use record
      CR1      at 16#0# range 0 .. 31;
      CR2      at 16#4# range 0 .. 31;
      OAR1     at 16#8# range 0 .. 31;
      OAR2     at 16#C# range 0 .. 31;
      TIMINGR  at 16#10# range 0 .. 31;
      TIMEOUTR at 16#14# range 0 .. 31;
      ISR      at 16#18# range 0 .. 31;
      ICR      at 16#1C# range 0 .. 31;
      PECR     at 16#20# range 0 .. 31;
      RXDR     at 16#24# range 0 .. 31;
      TXDR     at 16#28# range 0 .. 31;
   end record;

   --  I2C
   I2C1_Periph : aliased I2C_Peripheral
     with Import, Address => I2C1_Base;

   --  I2C
   I2C2_Periph : aliased I2C_Peripheral
     with Import, Address => I2C2_Base;

   --  I2C
   I2C3_Periph : aliased I2C_Peripheral
     with Import, Address => I2C3_Base;

   --  I2C
   I2C4_Periph : aliased I2C_Peripheral
     with Import, Address => I2C4_Base;

   --  I2C
   I2C5_Periph : aliased I2C_Peripheral
     with Import, Address => I2C5_Base;

end A0B.STM32H753.SVD.I2C;
