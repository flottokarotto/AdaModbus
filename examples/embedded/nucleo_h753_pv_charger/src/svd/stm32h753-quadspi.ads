pragma Style_Checks (Off);

--  This spec has been automatically generated from STM32H753.svd


with System;

package STM32H753.QUADSPI is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   subtype CR_FTHRES_Field is STM32H753.UInt5;
   subtype CR_PRESCALER_Field is STM32H753.Byte;

   --  QUADSPI control register
   type CR_Register is record
      --  Enable Enable the QUADSPI.
      EN             : Boolean := False;
      --  Abort request This bit aborts the on-going command sequence. It is
      --  automatically reset once the abort is complete. This bit stops the
      --  current transfer. In polling mode or memory-mapped mode, this bit
      --  also reset the APM bit or the DM bit.
      ABORT_k        : Boolean := False;
      --  DMA enable In indirect mode, DMA can be used to input or output data
      --  via the QUADSPI_DR register. DMA transfers are initiated when the
      --  FIFO threshold flag, FTF, is set.
      DMAEN          : Boolean := False;
      --  Timeout counter enable This bit is valid only when memory-mapped mode
      --  (FMODE = 11) is selected. Activating this bit causes the chip select
      --  (nCS) to be released (and thus reduces consumption) if there has not
      --  been an access after a certain amount of time, where this time is
      --  defined by TIMEOUT[15:0] (QUADSPI_LPTR). Enable the timeout counter.
      --  By default, the QUADSPI never stops its prefetch operation, keeping
      --  the previous read operation active with nCS maintained low, even if
      --  no access to the Flash memory occurs for a long time. Since Flash
      --  memories tend to consume more when nCS is held low, the application
      --  might want to activate the timeout counter (TCEN = 1, QUADSPI_CR[3])
      --  so that nCS is released after a period of TIMEOUT[15:0]
      --  (QUADSPI_LPTR) cycles have elapsed without an access since when the
      --  FIFO becomes full with prefetch data. This bit can be modified only
      --  when BUSY = 0.
      TCEN           : Boolean := False;
      --  Sample shift By default, the QUADSPI samples data 1/2 of a CLK cycle
      --  after the data is driven by the Flash memory. This bit allows the
      --  data is to be sampled later in order to account for external signal
      --  delays. Firmware must assure that SSHIFT = 0 when in DDR mode (when
      --  DDRM = 1). This field can be modified only when BUSY = 0.
      SSHIFT         : Boolean := False;
      --  unspecified
      Reserved_5_5   : STM32H753.Bit := 16#0#;
      --  Dual-flash mode This bit activates dual-flash mode, where two
      --  external Flash memories are used simultaneously to double throughput
      --  and capacity. This bit can be modified only when BUSY = 0.
      DFM            : Boolean := False;
      --  Flash memory selection This bit selects the Flash memory to be
      --  addressed in single flash mode (when DFM = 0). This bit can be
      --  modified only when BUSY = 0. This bit is ignored when DFM = 1.
      FSEL           : Boolean := False;
      --  FIFO threshold level Defines, in indirect mode, the threshold number
      --  of bytes in the FIFO that will cause the FIFO threshold flag (FTF,
      --  QUADSPI_SR[2]) to be set. In indirect write mode (FMODE = 00): ... In
      --  indirect read mode (FMODE = 01): ... If DMAEN = 1, then the DMA
      --  controller for the corresponding channel must be disabled before
      --  changing the FTHRES value.
      FTHRES         : CR_FTHRES_Field := 16#0#;
      --  unspecified
      Reserved_13_15 : STM32H753.UInt3 := 16#0#;
      --  Transfer error interrupt enable This bit enables the transfer error
      --  interrupt.
      TEIE           : Boolean := False;
      --  Transfer complete interrupt enable This bit enables the transfer
      --  complete interrupt.
      TCIE           : Boolean := False;
      --  FIFO threshold interrupt enable This bit enables the FIFO threshold
      --  interrupt.
      FTIE           : Boolean := False;
      --  Status match interrupt enable This bit enables the status match
      --  interrupt.
      SMIE           : Boolean := False;
      --  TimeOut interrupt enable This bit enables the TimeOut interrupt.
      TOIE           : Boolean := False;
      --  unspecified
      Reserved_21_21 : STM32H753.Bit := 16#0#;
      --  Automatic poll mode stop This bit determines if automatic polling is
      --  stopped after a match. This bit can be modified only when BUSY = 0.
      APMS           : Boolean := False;
      --  Polling match mode This bit indicates which method should be used for
      --  determining a match during automatic polling mode. This bit can be
      --  modified only when BUSY = 0.
      PMM            : Boolean := False;
      --  clock prescaler
      PRESCALER      : CR_PRESCALER_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      EN             at 0 range 0 .. 0;
      ABORT_k        at 0 range 1 .. 1;
      DMAEN          at 0 range 2 .. 2;
      TCEN           at 0 range 3 .. 3;
      SSHIFT         at 0 range 4 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      DFM            at 0 range 6 .. 6;
      FSEL           at 0 range 7 .. 7;
      FTHRES         at 0 range 8 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      TEIE           at 0 range 16 .. 16;
      TCIE           at 0 range 17 .. 17;
      FTIE           at 0 range 18 .. 18;
      SMIE           at 0 range 19 .. 19;
      TOIE           at 0 range 20 .. 20;
      Reserved_21_21 at 0 range 21 .. 21;
      APMS           at 0 range 22 .. 22;
      PMM            at 0 range 23 .. 23;
      PRESCALER      at 0 range 24 .. 31;
   end record;

   subtype DCR_CSHT_Field is STM32H753.UInt3;
   subtype DCR_FSIZE_Field is STM32H753.UInt5;

   --  QUADSPI device configuration register
   type DCR_Register is record
      --  indicates the level that clk takes between command
      CKMODE         : Boolean := False;
      --  unspecified
      Reserved_1_7   : STM32H753.UInt7 := 16#0#;
      --  Chip select high time CSHT+1 defines the minimum number of CLK cycles
      --  which the chip select (nCS) must remain high between commands issued
      --  to the Flash memory. ... This field can be modified only when BUSY =
      --  0.
      CSHT           : DCR_CSHT_Field := 16#0#;
      --  unspecified
      Reserved_11_15 : STM32H753.UInt5 := 16#0#;
      --  Flash memory size This field defines the size of external memory
      --  using the following formula: Number of bytes in Flash memory =
      --  2[FSIZE+1] FSIZE+1 is effectively the number of address bits required
      --  to address the Flash memory. The Flash memory capacity can be up to
      --  4GB (addressed using 32 bits) in indirect mode, but the addressable
      --  space in memory-mapped mode is limited to 256MB. If DFM = 1, FSIZE
      --  indicates the total capacity of the two Flash memories together. This
      --  field can be modified only when BUSY = 0.
      FSIZE          : DCR_FSIZE_Field := 16#0#;
      --  unspecified
      Reserved_21_31 : STM32H753.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DCR_Register use record
      CKMODE         at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      CSHT           at 0 range 8 .. 10;
      Reserved_11_15 at 0 range 11 .. 15;
      FSIZE          at 0 range 16 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   subtype SR_FLEVEL_Field is STM32H753.UInt6;

   --  QUADSPI status register
   type SR_Register is record
      --  Read-only. Transfer error flag This bit is set in indirect mode when
      --  an invalid address is being accessed in indirect mode. It is cleared
      --  by writing 1 to CTEF.
      TEF            : Boolean;
      --  Read-only. Transfer complete flag This bit is set in indirect mode
      --  when the programmed number of data has been transferred or in any
      --  mode when the transfer has been aborted.It is cleared by writing 1 to
      --  CTCF.
      TCF            : Boolean;
      --  Read-only. FIFO threshold flag In indirect mode, this bit is set when
      --  the FIFO threshold has been reached, or if there is any data left in
      --  the FIFO after reads from the Flash memory are complete. It is
      --  cleared automatically as soon as threshold condition is no longer
      --  true. In automatic polling mode this bit is set every time the status
      --  register is read, and the bit is cleared when the data register is
      --  read.
      FTF            : Boolean;
      --  Read-only. Status match flag This bit is set in automatic polling
      --  mode when the unmasked received data matches the corresponding bits
      --  in the match register (QUADSPI_PSMAR). It is cleared by writing 1 to
      --  CSMF.
      SMF            : Boolean;
      --  Read-only. Timeout flag This bit is set when timeout occurs. It is
      --  cleared by writing 1 to CTOF.
      TOF            : Boolean;
      --  Read-only. Busy This bit is set when an operation is on going. This
      --  bit clears automatically when the operation with the Flash memory is
      --  finished and the FIFO is empty.
      BUSY           : Boolean;
      --  unspecified
      Reserved_6_7   : STM32H753.UInt2;
      --  Read-only. FIFO level This field gives the number of valid bytes
      --  which are being held in the FIFO. FLEVEL = 0 when the FIFO is empty,
      --  and 16 when it is full. In memory-mapped mode and in automatic status
      --  polling mode, FLEVEL is zero.
      FLEVEL         : SR_FLEVEL_Field;
      --  unspecified
      Reserved_14_31 : STM32H753.UInt18;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register use record
      TEF            at 0 range 0 .. 0;
      TCF            at 0 range 1 .. 1;
      FTF            at 0 range 2 .. 2;
      SMF            at 0 range 3 .. 3;
      TOF            at 0 range 4 .. 4;
      BUSY           at 0 range 5 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      FLEVEL         at 0 range 8 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   --  QUADSPI flag clear register
   type FCR_Register is record
      --  Clear transfer error flag Writing 1 clears the TEF flag in the
      --  QUADSPI_SR register
      CTEF          : Boolean := False;
      --  Clear transfer complete flag Writing 1 clears the TCF flag in the
      --  QUADSPI_SR register
      CTCF          : Boolean := False;
      --  unspecified
      Reserved_2_2  : STM32H753.Bit := 16#0#;
      --  Clear status match flag Writing 1 clears the SMF flag in the
      --  QUADSPI_SR register
      CSMF          : Boolean := False;
      --  Clear timeout flag Writing 1 clears the TOF flag in the QUADSPI_SR
      --  register
      CTOF          : Boolean := False;
      --  unspecified
      Reserved_5_31 : STM32H753.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FCR_Register use record
      CTEF          at 0 range 0 .. 0;
      CTCF          at 0 range 1 .. 1;
      Reserved_2_2  at 0 range 2 .. 2;
      CSMF          at 0 range 3 .. 3;
      CTOF          at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   subtype CCR_INSTRUCTION_Field is STM32H753.Byte;
   subtype CCR_IMODE_Field is STM32H753.UInt2;
   subtype CCR_ADMODE_Field is STM32H753.UInt2;
   subtype CCR_ADSIZE_Field is STM32H753.UInt2;
   subtype CCR_ABMODE_Field is STM32H753.UInt2;
   subtype CCR_ABSIZE_Field is STM32H753.UInt2;
   subtype CCR_DCYC_Field is STM32H753.UInt5;
   subtype CCR_DMODE_Field is STM32H753.UInt2;
   subtype CCR_FMODE_Field is STM32H753.UInt2;

   --  QUADSPI communication configuration register
   type CCR_Register is record
      --  Instruction Instruction to be send to the external SPI device. This
      --  field can be written only when BUSY = 0.
      INSTRUCTION    : CCR_INSTRUCTION_Field := 16#0#;
      --  Instruction mode This field defines the instruction phase mode of
      --  operation: This field can be written only when BUSY = 0.
      IMODE          : CCR_IMODE_Field := 16#0#;
      --  Address mode This field defines the address phase mode of operation:
      --  This field can be written only when BUSY = 0.
      ADMODE         : CCR_ADMODE_Field := 16#0#;
      --  Address size This bit defines address size: This field can be written
      --  only when BUSY = 0.
      ADSIZE         : CCR_ADSIZE_Field := 16#0#;
      --  Alternate bytes mode This field defines the alternate-bytes phase
      --  mode of operation: This field can be written only when BUSY = 0.
      ABMODE         : CCR_ABMODE_Field := 16#0#;
      --  Alternate bytes size This bit defines alternate bytes size: This
      --  field can be written only when BUSY = 0.
      ABSIZE         : CCR_ABSIZE_Field := 16#0#;
      --  Number of dummy cycles This field defines the duration of the dummy
      --  phase. In both SDR and DDR modes, it specifies a number of CLK cycles
      --  (0-31). This field can be written only when BUSY = 0.
      DCYC           : CCR_DCYC_Field := 16#0#;
      --  unspecified
      Reserved_23_23 : STM32H753.Bit := 16#0#;
      --  Data mode This field defines the data phases mode of operation: This
      --  field also determines the dummy phase mode of operation. This field
      --  can be written only when BUSY = 0.
      DMODE          : CCR_DMODE_Field := 16#0#;
      --  Functional mode This field defines the QUADSPI functional mode of
      --  operation. If DMAEN = 1 already, then the DMA controller for the
      --  corresponding channel must be disabled before changing the FMODE
      --  value. This field can be written only when BUSY = 0.
      FMODE          : CCR_FMODE_Field := 16#0#;
      --  Send instruction only once mode See Section15.3.11: Sending the
      --  instruction only once on page13. This bit has no effect when IMODE =
      --  00. This field can be written only when BUSY = 0.
      SIOO           : Boolean := False;
      --  unspecified
      Reserved_29_29 : STM32H753.Bit := 16#0#;
      --  DDR hold Delay the data output by 1/4 of the QUADSPI output clock
      --  cycle in DDR mode: This feature is only active in DDR mode. This
      --  field can be written only when BUSY = 0.
      DHHC           : Boolean := False;
      --  Double data rate mode This bit sets the DDR mode for the address,
      --  alternate byte and data phase: This field can be written only when
      --  BUSY = 0.
      DDRM           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCR_Register use record
      INSTRUCTION    at 0 range 0 .. 7;
      IMODE          at 0 range 8 .. 9;
      ADMODE         at 0 range 10 .. 11;
      ADSIZE         at 0 range 12 .. 13;
      ABMODE         at 0 range 14 .. 15;
      ABSIZE         at 0 range 16 .. 17;
      DCYC           at 0 range 18 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      DMODE          at 0 range 24 .. 25;
      FMODE          at 0 range 26 .. 27;
      SIOO           at 0 range 28 .. 28;
      Reserved_29_29 at 0 range 29 .. 29;
      DHHC           at 0 range 30 .. 30;
      DDRM           at 0 range 31 .. 31;
   end record;

   subtype PIR_INTERVAL_Field is STM32H753.UInt16;

   --  QUADSPI polling interval register
   type PIR_Register is record
      --  Polling interval Number of CLK cycles between to read during
      --  automatic polling phases. This field can be written only when BUSY =
      --  0.
      INTERVAL       : PIR_INTERVAL_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PIR_Register use record
      INTERVAL       at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype LPTR_TIMEOUT_Field is STM32H753.UInt16;

   --  QUADSPI low-power timeout register
   type LPTR_Register is record
      --  Timeout period After each access in memory-mapped mode, the QUADSPI
      --  prefetches the subsequent bytes and holds these bytes in the FIFO.
      --  This field indicates how many CLK cycles the QUADSPI waits after the
      --  FIFO becomes full until it raises nCS, putting the Flash memory in a
      --  lower-consumption state. This field can be written only when BUSY =
      --  0.
      TIMEOUT        : LPTR_TIMEOUT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LPTR_Register use record
      TIMEOUT        at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  QUADSPI
   type QUADSPI_Peripheral is record
      --  QUADSPI control register
      CR    : aliased CR_Register;
      --  QUADSPI device configuration register
      DCR   : aliased DCR_Register;
      --  QUADSPI status register
      SR    : aliased SR_Register;
      --  QUADSPI flag clear register
      FCR   : aliased FCR_Register;
      --  QUADSPI data length register
      DLR   : aliased STM32H753.UInt32;
      --  QUADSPI communication configuration register
      CCR   : aliased CCR_Register;
      --  QUADSPI address register
      AR    : aliased STM32H753.UInt32;
      --  QUADSPI alternate bytes registers
      ABR   : aliased STM32H753.UInt32;
      --  QUADSPI data register
      DR    : aliased STM32H753.UInt32;
      --  QUADSPI polling status mask register
      PSMKR : aliased STM32H753.UInt32;
      --  QUADSPI polling status match register
      PSMAR : aliased STM32H753.UInt32;
      --  QUADSPI polling interval register
      PIR   : aliased PIR_Register;
      --  QUADSPI low-power timeout register
      LPTR  : aliased LPTR_Register;
   end record
     with Volatile;

   for QUADSPI_Peripheral use record
      CR    at 16#0# range 0 .. 31;
      DCR   at 16#4# range 0 .. 31;
      SR    at 16#8# range 0 .. 31;
      FCR   at 16#C# range 0 .. 31;
      DLR   at 16#10# range 0 .. 31;
      CCR   at 16#14# range 0 .. 31;
      AR    at 16#18# range 0 .. 31;
      ABR   at 16#1C# range 0 .. 31;
      DR    at 16#20# range 0 .. 31;
      PSMKR at 16#24# range 0 .. 31;
      PSMAR at 16#28# range 0 .. 31;
      PIR   at 16#2C# range 0 .. 31;
      LPTR  at 16#30# range 0 .. 31;
   end record;

   --  QUADSPI
   QUADSPI_Periph : aliased QUADSPI_Peripheral
     with Import, Address => QUADSPI_Base;

end STM32H753.QUADSPI;
