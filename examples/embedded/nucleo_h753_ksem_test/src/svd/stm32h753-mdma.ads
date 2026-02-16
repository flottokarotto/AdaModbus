pragma Style_Checks (Off);

--  This spec has been automatically generated from STM32H753.svd


with System;

package STM32H753.MDMA is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   --  MDMA_GISR0_GIF array
   type MDMA_GISR0_GIF_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for MDMA_GISR0_GIF
   type MDMA_GISR0_GIF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  GIF as a value
            Val : STM32H753.UInt16;
         when True =>
            --  GIF as an array
            Arr : MDMA_GISR0_GIF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for MDMA_GISR0_GIF_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  MDMA Global Interrupt/Status Register
   type MDMA_GISR0_Register is record
      --  Read-only. Channel x global interrupt flag (x=...) This bit is set
      --  and reset by hardware. It is a logical OR of all the Channel x
      --  interrupt flags (CTCIFx, BTIFx, BRTIFx, TEIFx) which are enabled in
      --  the interrupt mask register (CTCIEx, BTIEx, BRTIEx, TEIEx)
      GIF            : MDMA_GISR0_GIF_Field;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_GISR0_Register use record
      GIF            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  MDMA channel x interrupt/status register
   type MDMA_C0ISR_Register is record
      --  Read-only. Channel x transfer error interrupt flag This bit is set by
      --  hardware. It is cleared by software writing 1 to the corresponding
      --  bit in the DMA_IFCRy register.
      TEIF0          : Boolean;
      --  Read-only. Channel x Channel Transfer Complete interrupt flag This
      --  bit is set by hardware. It is cleared by software writing 1 to the
      --  corresponding bit in the DMA_IFCRy register. CTC is set when the last
      --  block was transferred and the channel has been automatically
      --  disabled. CTC is also set when the channel is suspended, as a result
      --  of writing EN bit to 0.
      CTCIF0         : Boolean;
      --  Read-only. Channel x block repeat transfer complete interrupt flag
      --  This bit is set by hardware. It is cleared by software writing 1 to
      --  the corresponding bit in the DMA_IFCRy register.
      BRTIF0         : Boolean;
      --  Read-only. Channel x block transfer complete interrupt flag This bit
      --  is set by hardware. It is cleared by software writing 1 to the
      --  corresponding bit in the DMA_IFCRy register.
      BTIF0          : Boolean;
      --  Read-only. channel x buffer transfer complete
      TCIF0          : Boolean;
      --  unspecified
      Reserved_5_15  : STM32H753.UInt11;
      --  Read-only. channel x request active flag
      CRQA0          : Boolean;
      --  unspecified
      Reserved_17_31 : STM32H753.UInt15;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C0ISR_Register use record
      TEIF0          at 0 range 0 .. 0;
      CTCIF0         at 0 range 1 .. 1;
      BRTIF0         at 0 range 2 .. 2;
      BTIF0          at 0 range 3 .. 3;
      TCIF0          at 0 range 4 .. 4;
      Reserved_5_15  at 0 range 5 .. 15;
      CRQA0          at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   --  MDMA channel x interrupt flag clear register
   type MDMA_C0IFCR_Register is record
      --  Write-only. Channel x clear transfer error interrupt flag Writing a 1
      --  into this bit clears TEIFx in the MDMA_ISRy register
      CTEIF0        : Boolean := False;
      --  Write-only. Clear Channel transfer complete interrupt flag for
      --  channel x Writing a 1 into this bit clears CTCIFx in the MDMA_ISRy
      --  register
      CCTCIF0       : Boolean := False;
      --  Write-only. Channel x clear block repeat transfer complete interrupt
      --  flag Writing a 1 into this bit clears BRTIFx in the MDMA_ISRy
      --  register
      CBRTIF0       : Boolean := False;
      --  Write-only. Channel x Clear block transfer complete interrupt flag
      --  Writing a 1 into this bit clears BTIFx in the MDMA_ISRy register
      CBTIF0        : Boolean := False;
      --  Write-only. CLear buffer Transfer Complete Interrupt Flag for channel
      --  x Writing a 1 into this bit clears TCIFx in the MDMA_ISRy register
      CLTCIF0       : Boolean := False;
      --  unspecified
      Reserved_5_31 : STM32H753.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C0IFCR_Register use record
      CTEIF0        at 0 range 0 .. 0;
      CCTCIF0       at 0 range 1 .. 1;
      CBRTIF0       at 0 range 2 .. 2;
      CBTIF0        at 0 range 3 .. 3;
      CLTCIF0       at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   subtype MDMA_C0ESR_TEA_Field is STM32H753.UInt7;

   --  MDMA Channel x error status register
   type MDMA_C0ESR_Register is record
      --  Read-only. Transfer Error Address These bits are set and cleared by
      --  HW, in case of an MDMA data transfer error. It is used in conjunction
      --  with TED. This field indicates the 7 LSBits of the address which
      --  generated a transfer/access error. It may be used by SW to retrieve
      --  the failing address, by adding this value (truncated to the buffer
      --  transfer length size) to the current SAR/DAR value. Note: The SAR/DAR
      --  current value doesnt reflect this last address due to the FIFO
      --  management system. The SAR/DAR are only updated at the end of a
      --  (buffer) transfer (of TLEN+1 bytes). Note: It is not set in case of a
      --  link data error.
      TEA            : MDMA_C0ESR_TEA_Field;
      --  Read-only. Transfer Error Direction These bit is set and cleared by
      --  HW, in case of an MDMA data transfer error.
      TED            : Boolean;
      --  Read-only. Transfer Error Link Data These bit is set by HW, in case
      --  of a transfer error while reading the block link data structure. It
      --  is cleared by software writing 1 to the CTEIFx bit in the DMA_IFCRy
      --  register.
      TELD           : Boolean;
      --  Read-only. Transfer Error Mask Data These bit is set by HW, in case
      --  of a transfer error while writing the Mask Data. It is cleared by
      --  software writing 1 to the CTEIFx bit in the DMA_IFCRy register.
      TEMD           : Boolean;
      --  Read-only. Address/Size Error These bit is set by HW, when the
      --  programmed address is not aligned with the data size. TED will
      --  indicate whether the problem is on the source or destination. It is
      --  cleared by software writing 1 to the CTEIFx bit in the DMA_IFCRy
      --  register.
      ASE            : Boolean;
      --  Read-only. Block Size Error These bit is set by HW, when the block
      --  size is not an integer multiple of the data size either for source or
      --  destination. TED will indicate whether the problem is on the source
      --  or destination. It is cleared by software writing 1 to the CTEIFx bit
      --  in the DMA_IFCRy register.
      BSE            : Boolean;
      --  unspecified
      Reserved_12_31 : STM32H753.UInt20;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C0ESR_Register use record
      TEA            at 0 range 0 .. 6;
      TED            at 0 range 7 .. 7;
      TELD           at 0 range 8 .. 8;
      TEMD           at 0 range 9 .. 9;
      ASE            at 0 range 10 .. 10;
      BSE            at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype MDMA_C0CR_PL_Field is STM32H753.UInt2;

   --  This register is used to control the concerned channel.
   type MDMA_C0CR_Register is record
      --  channel enable
      EN             : Boolean := False;
      --  Transfer error interrupt enable This bit is set and cleared by
      --  software.
      TEIE           : Boolean := False;
      --  Channel Transfer Complete interrupt enable This bit is set and
      --  cleared by software.
      CTCIE          : Boolean := False;
      --  Block Repeat transfer interrupt enable This bit is set and cleared by
      --  software.
      BRTIE          : Boolean := False;
      --  Block Transfer interrupt enable This bit is set and cleared by
      --  software.
      BTIE           : Boolean := False;
      --  buffer Transfer Complete interrupt enable This bit is set and cleared
      --  by software.
      TCIE           : Boolean := False;
      --  Priority level These bits are set and cleared by software. These bits
      --  are protected and can be written only if EN is 0.
      PL             : MDMA_C0CR_PL_Field := 16#0#;
      --  unspecified
      Reserved_8_11  : STM32H753.UInt4 := 16#0#;
      --  byte Endianness exchange
      BEX            : Boolean := False;
      --  Half word Endianes exchange
      HEX            : Boolean := False;
      --  Word Endianness exchange
      WEX            : Boolean := False;
      --  unspecified
      Reserved_15_15 : STM32H753.Bit := 16#0#;
      --  Write-only. SW ReQuest Writing a 1 into this bit sets the CRQAx in
      --  MDMA_ISRy register, activating the request on Channel x Note: Either
      --  the whole CxCR register or the 8-bit/16-bit register @ Address
      --  offset: 0x4E + 0x40 chn may be used for SWRQ activation. In case of a
      --  SW request, acknowledge is not generated (neither HW signal, nor
      --  CxMAR write access).
      SWRQ           : Boolean := False;
      --  unspecified
      Reserved_17_31 : STM32H753.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C0CR_Register use record
      EN             at 0 range 0 .. 0;
      TEIE           at 0 range 1 .. 1;
      CTCIE          at 0 range 2 .. 2;
      BRTIE          at 0 range 3 .. 3;
      BTIE           at 0 range 4 .. 4;
      TCIE           at 0 range 5 .. 5;
      PL             at 0 range 6 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      BEX            at 0 range 12 .. 12;
      HEX            at 0 range 13 .. 13;
      WEX            at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      SWRQ           at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype MDMA_C0TCR_SINC_Field is STM32H753.UInt2;
   subtype MDMA_C0TCR_DINC_Field is STM32H753.UInt2;
   subtype MDMA_C0TCR_SSIZE_Field is STM32H753.UInt2;
   subtype MDMA_C0TCR_DSIZE_Field is STM32H753.UInt2;
   subtype MDMA_C0TCR_SINCOS_Field is STM32H753.UInt2;
   subtype MDMA_C0TCR_DINCOS_Field is STM32H753.UInt2;
   subtype MDMA_C0TCR_SBURST_Field is STM32H753.UInt3;
   subtype MDMA_C0TCR_DBURST_Field is STM32H753.UInt3;
   subtype MDMA_C0TCR_TLEN_Field is STM32H753.UInt7;
   subtype MDMA_C0TCR_PAM_Field is STM32H753.UInt2;
   subtype MDMA_C0TCR_TRGM_Field is STM32H753.UInt2;

   --  This register is used to configure the concerned channel.
   type MDMA_C0TCR_Register is record
      --  Source increment mode These bits are set and cleared by software.
      --  These bits are protected and can be written only if EN is 0 Note:
      --  When source is AHB (SBUS=1), SINC = 00 is forbidden. In Linked List
      --  Mode, at the end of a block (single or last block in repeated block
      --  transfer mode), this register will be loaded from memory (from
      --  address given by current LAR[31:0] + 0x00).
      SINC   : MDMA_C0TCR_SINC_Field := 16#0#;
      --  Destination increment mode These bits are set and cleared by
      --  software. These bits are protected and can be written only if EN is 0
      --  Note: When destination is AHB (DBUS=1), DINC = 00 is forbidden.
      DINC   : MDMA_C0TCR_DINC_Field := 16#0#;
      --  Source data size These bits are set and cleared by software. These
      --  bits are protected and can be written only if EN is 0 Note: If a
      --  value of 11 is programmed for the TCM access/AHB port, a transfer
      --  error will occur (TEIF bit set) If SINCOS &lt; SSIZE and SINC &#8800;
      --  00, the result will be unpredictable. Note: SSIZE = 11 (double-word)
      --  is forbidden when source is TCM/AHB bus (SBUS=1).
      SSIZE  : MDMA_C0TCR_SSIZE_Field := 16#0#;
      --  Destination data size These bits are set and cleared by software.
      --  These bits are protected and can be written only if EN is 0. Note: If
      --  a value of 11 is programmed for the TCM access/AHB port, a transfer
      --  error will occur (TEIF bit set) If DINCOS &lt; DSIZE and DINC &#8800;
      --  00, the result will be unpredictable. Note: DSIZE = 11 (double-word)
      --  is forbidden when destination is TCM/AHB bus (DBUS=1).
      DSIZE  : MDMA_C0TCR_DSIZE_Field := 16#0#;
      --  source increment offset size
      SINCOS : MDMA_C0TCR_SINCOS_Field := 16#0#;
      --  Destination increment offset
      DINCOS : MDMA_C0TCR_DINCOS_Field := 16#0#;
      --  source burst transfer configuration
      SBURST : MDMA_C0TCR_SBURST_Field := 16#0#;
      --  Destination burst transfer configuration
      DBURST : MDMA_C0TCR_DBURST_Field := 16#0#;
      --  buffer transfer lengh
      TLEN   : MDMA_C0TCR_TLEN_Field := 16#0#;
      --  PacK Enable These bit is set and cleared by software. If the Source
      --  Size is smaller than the destination, it will be padded according to
      --  the PAM value. If the Source data size is larger than the destination
      --  one, it will be truncated. The alignment will be done according to
      --  the PAM[0] value. This bit is protected and can be written only if EN
      --  is 0
      PKE    : Boolean := False;
      --  Padding/Alignement Mode These bits are set and cleared by software.
      --  Case 1: Source data size smaller than destination data size - 3
      --  options are valid. Case 2: Source data size larger than destination
      --  data size. The remainder part is discarded. When PKE = 1 or
      --  DSIZE=SSIZE, these bits are ignored. These bits are protected and can
      --  be written only if EN is 0
      PAM    : MDMA_C0TCR_PAM_Field := 16#0#;
      --  Trigger Mode These bits are set and cleared by software. Note: If
      --  TRGM is 11 for the current block, all the values loaded at the end of
      --  the current block through the linked list mechanism must keep the
      --  same value (TRGM=11) and the same SWRM value, otherwise the result is
      --  undefined. These bits are protected and can be written only if EN is
      --  0.
      TRGM   : MDMA_C0TCR_TRGM_Field := 16#0#;
      --  SW Request Mode This bit is set and cleared by software. If a HW or
      --  SW request is currently active, the bit change will be delayed until
      --  the current transfer is completed. If the CxMAR contains a valid
      --  address, the CxMDR value will also be written @ CxMAR address. This
      --  bit is protected and can be written only if EN is 0.
      SWRM   : Boolean := False;
      --  Bufferable Write Mode This bit is set and cleared by software. This
      --  bit is protected and can be written only if EN is 0. Note: All MDMA
      --  destination accesses are non-cacheable.
      BWM    : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C0TCR_Register use record
      SINC   at 0 range 0 .. 1;
      DINC   at 0 range 2 .. 3;
      SSIZE  at 0 range 4 .. 5;
      DSIZE  at 0 range 6 .. 7;
      SINCOS at 0 range 8 .. 9;
      DINCOS at 0 range 10 .. 11;
      SBURST at 0 range 12 .. 14;
      DBURST at 0 range 15 .. 17;
      TLEN   at 0 range 18 .. 24;
      PKE    at 0 range 25 .. 25;
      PAM    at 0 range 26 .. 27;
      TRGM   at 0 range 28 .. 29;
      SWRM   at 0 range 30 .. 30;
      BWM    at 0 range 31 .. 31;
   end record;

   subtype MDMA_C0BNDTR_BNDT_Field is STM32H753.UInt17;
   subtype MDMA_C0BNDTR_BRC_Field is STM32H753.UInt12;

   --  MDMA Channel x block number of data register
   type MDMA_C0BNDTR_Register is record
      --  block number of data to transfer
      BNDT           : MDMA_C0BNDTR_BNDT_Field := 16#0#;
      --  unspecified
      Reserved_17_17 : STM32H753.Bit := 16#0#;
      --  Block Repeat Source address Update Mode These bits are protected and
      --  can be written only if EN is 0.
      BRSUM          : Boolean := False;
      --  Block Repeat Destination address Update Mode These bits are protected
      --  and can be written only if EN is 0.
      BRDUM          : Boolean := False;
      --  Block Repeat Count This field contains the number of repetitions of
      --  the current block (0 to 4095). When the channel is enabled, this
      --  register is read-only, indicating the remaining number of blocks,
      --  excluding the current one. This register decrements after each
      --  complete block transfer. Once the last block transfer has completed,
      --  this register can either stay at zero or be reloaded automatically
      --  from memory (in Linked List mode - i.e. Link Address valid). These
      --  bits are protected and can be written only if EN is 0.
      BRC            : MDMA_C0BNDTR_BRC_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C0BNDTR_Register use record
      BNDT           at 0 range 0 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      BRSUM          at 0 range 18 .. 18;
      BRDUM          at 0 range 19 .. 19;
      BRC            at 0 range 20 .. 31;
   end record;

   subtype MDMA_C0BRUR_SUV_Field is STM32H753.UInt16;
   subtype MDMA_C0BRUR_DUV_Field is STM32H753.UInt16;

   --  MDMA channel x Block Repeat address Update register
   type MDMA_C0BRUR_Register is record
      --  source adresse update value
      SUV : MDMA_C0BRUR_SUV_Field := 16#0#;
      --  destination address update
      DUV : MDMA_C0BRUR_DUV_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C0BRUR_Register use record
      SUV at 0 range 0 .. 15;
      DUV at 0 range 16 .. 31;
   end record;

   subtype MDMA_C0TBR_TSEL_Field is STM32H753.UInt6;

   --  MDMA channel x Trigger and Bus selection Register
   type MDMA_C0TBR_Register is record
      --  Trigger selection
      TSEL           : MDMA_C0TBR_TSEL_Field := 16#0#;
      --  unspecified
      Reserved_6_15  : STM32H753.UInt10 := 16#0#;
      --  Source BUS select This bit is protected and can be written only if EN
      --  is 0.
      SBUS           : Boolean := False;
      --  Destination BUS slect This bit is protected and can be written only
      --  if EN is 0.
      DBUS           : Boolean := False;
      --  unspecified
      Reserved_18_31 : STM32H753.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C0TBR_Register use record
      TSEL           at 0 range 0 .. 5;
      Reserved_6_15  at 0 range 6 .. 15;
      SBUS           at 0 range 16 .. 16;
      DBUS           at 0 range 17 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   --  MDMA channel x interrupt/status register
   type MDMA_C1ISR_Register is record
      --  Read-only. Channel x transfer error interrupt flag This bit is set by
      --  hardware. It is cleared by software writing 1 to the corresponding
      --  bit in the DMA_IFCRy register.
      TEIF1          : Boolean;
      --  Read-only. Channel x Channel Transfer Complete interrupt flag This
      --  bit is set by hardware. It is cleared by software writing 1 to the
      --  corresponding bit in the DMA_IFCRy register. CTC is set when the last
      --  block was transferred and the channel has been automatically
      --  disabled. CTC is also set when the channel is suspended, as a result
      --  of writing EN bit to 0.
      CTCIF1         : Boolean;
      --  Read-only. Channel x block repeat transfer complete interrupt flag
      --  This bit is set by hardware. It is cleared by software writing 1 to
      --  the corresponding bit in the DMA_IFCRy register.
      BRTIF1         : Boolean;
      --  Read-only. Channel x block transfer complete interrupt flag This bit
      --  is set by hardware. It is cleared by software writing 1 to the
      --  corresponding bit in the DMA_IFCRy register.
      BTIF1          : Boolean;
      --  Read-only. channel x buffer transfer complete
      TCIF1          : Boolean;
      --  unspecified
      Reserved_5_15  : STM32H753.UInt11;
      --  Read-only. channel x request active flag
      CRQA1          : Boolean;
      --  unspecified
      Reserved_17_31 : STM32H753.UInt15;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C1ISR_Register use record
      TEIF1          at 0 range 0 .. 0;
      CTCIF1         at 0 range 1 .. 1;
      BRTIF1         at 0 range 2 .. 2;
      BTIF1          at 0 range 3 .. 3;
      TCIF1          at 0 range 4 .. 4;
      Reserved_5_15  at 0 range 5 .. 15;
      CRQA1          at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   --  MDMA channel x interrupt flag clear register
   type MDMA_C1IFCR_Register is record
      --  Write-only. Channel x clear transfer error interrupt flag Writing a 1
      --  into this bit clears TEIFx in the MDMA_ISRy register
      CTEIF1        : Boolean := False;
      --  Write-only. Clear Channel transfer complete interrupt flag for
      --  channel x Writing a 1 into this bit clears CTCIFx in the MDMA_ISRy
      --  register
      CCTCIF1       : Boolean := False;
      --  Write-only. Channel x clear block repeat transfer complete interrupt
      --  flag Writing a 1 into this bit clears BRTIFx in the MDMA_ISRy
      --  register
      CBRTIF1       : Boolean := False;
      --  Write-only. Channel x Clear block transfer complete interrupt flag
      --  Writing a 1 into this bit clears BTIFx in the MDMA_ISRy register
      CBTIF1        : Boolean := False;
      --  Write-only. CLear buffer Transfer Complete Interrupt Flag for channel
      --  x Writing a 1 into this bit clears TCIFx in the MDMA_ISRy register
      CLTCIF1       : Boolean := False;
      --  unspecified
      Reserved_5_31 : STM32H753.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C1IFCR_Register use record
      CTEIF1        at 0 range 0 .. 0;
      CCTCIF1       at 0 range 1 .. 1;
      CBRTIF1       at 0 range 2 .. 2;
      CBTIF1        at 0 range 3 .. 3;
      CLTCIF1       at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   subtype MDMA_C1ESR_TEA_Field is STM32H753.UInt7;

   --  MDMA Channel x error status register
   type MDMA_C1ESR_Register is record
      --  Read-only. Transfer Error Address These bits are set and cleared by
      --  HW, in case of an MDMA data transfer error. It is used in conjunction
      --  with TED. This field indicates the 7 LSBits of the address which
      --  generated a transfer/access error. It may be used by SW to retrieve
      --  the failing address, by adding this value (truncated to the buffer
      --  transfer length size) to the current SAR/DAR value. Note: The SAR/DAR
      --  current value doesnt reflect this last address due to the FIFO
      --  management system. The SAR/DAR are only updated at the end of a
      --  (buffer) transfer (of TLEN+1 bytes). Note: It is not set in case of a
      --  link data error.
      TEA            : MDMA_C1ESR_TEA_Field;
      --  Read-only. Transfer Error Direction These bit is set and cleared by
      --  HW, in case of an MDMA data transfer error.
      TED            : Boolean;
      --  Read-only. Transfer Error Link Data These bit is set by HW, in case
      --  of a transfer error while reading the block link data structure. It
      --  is cleared by software writing 1 to the CTEIFx bit in the DMA_IFCRy
      --  register.
      TELD           : Boolean;
      --  Read-only. Transfer Error Mask Data These bit is set by HW, in case
      --  of a transfer error while writing the Mask Data. It is cleared by
      --  software writing 1 to the CTEIFx bit in the DMA_IFCRy register.
      TEMD           : Boolean;
      --  Read-only. Address/Size Error These bit is set by HW, when the
      --  programmed address is not aligned with the data size. TED will
      --  indicate whether the problem is on the source or destination. It is
      --  cleared by software writing 1 to the CTEIFx bit in the DMA_IFCRy
      --  register.
      ASE            : Boolean;
      --  Read-only. Block Size Error These bit is set by HW, when the block
      --  size is not an integer multiple of the data size either for source or
      --  destination. TED will indicate whether the problem is on the source
      --  or destination. It is cleared by software writing 1 to the CTEIFx bit
      --  in the DMA_IFCRy register.
      BSE            : Boolean;
      --  unspecified
      Reserved_12_31 : STM32H753.UInt20;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C1ESR_Register use record
      TEA            at 0 range 0 .. 6;
      TED            at 0 range 7 .. 7;
      TELD           at 0 range 8 .. 8;
      TEMD           at 0 range 9 .. 9;
      ASE            at 0 range 10 .. 10;
      BSE            at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype MDMA_C1CR_PL_Field is STM32H753.UInt2;

   --  This register is used to control the concerned channel.
   type MDMA_C1CR_Register is record
      --  channel enable
      EN             : Boolean := False;
      --  Transfer error interrupt enable This bit is set and cleared by
      --  software.
      TEIE           : Boolean := False;
      --  Channel Transfer Complete interrupt enable This bit is set and
      --  cleared by software.
      CTCIE          : Boolean := False;
      --  Block Repeat transfer interrupt enable This bit is set and cleared by
      --  software.
      BRTIE          : Boolean := False;
      --  Block Transfer interrupt enable This bit is set and cleared by
      --  software.
      BTIE           : Boolean := False;
      --  buffer Transfer Complete interrupt enable This bit is set and cleared
      --  by software.
      TCIE           : Boolean := False;
      --  Priority level These bits are set and cleared by software. These bits
      --  are protected and can be written only if EN is 0.
      PL             : MDMA_C1CR_PL_Field := 16#0#;
      --  unspecified
      Reserved_8_11  : STM32H753.UInt4 := 16#0#;
      --  byte Endianness exchange
      BEX            : Boolean := False;
      --  Half word Endianes exchange
      HEX            : Boolean := False;
      --  Word Endianness exchange
      WEX            : Boolean := False;
      --  unspecified
      Reserved_15_15 : STM32H753.Bit := 16#0#;
      --  Write-only. SW ReQuest Writing a 1 into this bit sets the CRQAx in
      --  MDMA_ISRy register, activating the request on Channel x Note: Either
      --  the whole CxCR register or the 8-bit/16-bit register @ Address
      --  offset: 0x4E + 0x40 chn may be used for SWRQ activation. In case of a
      --  SW request, acknowledge is not generated (neither HW signal, nor
      --  CxMAR write access).
      SWRQ           : Boolean := False;
      --  unspecified
      Reserved_17_31 : STM32H753.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C1CR_Register use record
      EN             at 0 range 0 .. 0;
      TEIE           at 0 range 1 .. 1;
      CTCIE          at 0 range 2 .. 2;
      BRTIE          at 0 range 3 .. 3;
      BTIE           at 0 range 4 .. 4;
      TCIE           at 0 range 5 .. 5;
      PL             at 0 range 6 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      BEX            at 0 range 12 .. 12;
      HEX            at 0 range 13 .. 13;
      WEX            at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      SWRQ           at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype MDMA_C1TCR_SINC_Field is STM32H753.UInt2;
   subtype MDMA_C1TCR_DINC_Field is STM32H753.UInt2;
   subtype MDMA_C1TCR_SSIZE_Field is STM32H753.UInt2;
   subtype MDMA_C1TCR_DSIZE_Field is STM32H753.UInt2;
   subtype MDMA_C1TCR_SINCOS_Field is STM32H753.UInt2;
   subtype MDMA_C1TCR_DINCOS_Field is STM32H753.UInt2;
   subtype MDMA_C1TCR_SBURST_Field is STM32H753.UInt3;
   subtype MDMA_C1TCR_DBURST_Field is STM32H753.UInt3;
   subtype MDMA_C1TCR_TLEN_Field is STM32H753.UInt7;
   subtype MDMA_C1TCR_PAM_Field is STM32H753.UInt2;
   subtype MDMA_C1TCR_TRGM_Field is STM32H753.UInt2;

   --  This register is used to configure the concerned channel.
   type MDMA_C1TCR_Register is record
      --  Source increment mode These bits are set and cleared by software.
      --  These bits are protected and can be written only if EN is 0 Note:
      --  When source is AHB (SBUS=1), SINC = 00 is forbidden. In Linked List
      --  Mode, at the end of a block (single or last block in repeated block
      --  transfer mode), this register will be loaded from memory (from
      --  address given by current LAR[31:0] + 0x00).
      SINC   : MDMA_C1TCR_SINC_Field := 16#0#;
      --  Destination increment mode These bits are set and cleared by
      --  software. These bits are protected and can be written only if EN is 0
      --  Note: When destination is AHB (DBUS=1), DINC = 00 is forbidden.
      DINC   : MDMA_C1TCR_DINC_Field := 16#0#;
      --  Source data size These bits are set and cleared by software. These
      --  bits are protected and can be written only if EN is 0 Note: If a
      --  value of 11 is programmed for the TCM access/AHB port, a transfer
      --  error will occur (TEIF bit set) If SINCOS &lt; SSIZE and SINC &#8800;
      --  00, the result will be unpredictable. Note: SSIZE = 11 (double-word)
      --  is forbidden when source is TCM/AHB bus (SBUS=1).
      SSIZE  : MDMA_C1TCR_SSIZE_Field := 16#0#;
      --  Destination data size These bits are set and cleared by software.
      --  These bits are protected and can be written only if EN is 0. Note: If
      --  a value of 11 is programmed for the TCM access/AHB port, a transfer
      --  error will occur (TEIF bit set) If DINCOS &lt; DSIZE and DINC &#8800;
      --  00, the result will be unpredictable. Note: DSIZE = 11 (double-word)
      --  is forbidden when destination is TCM/AHB bus (DBUS=1).
      DSIZE  : MDMA_C1TCR_DSIZE_Field := 16#0#;
      --  source increment offset size
      SINCOS : MDMA_C1TCR_SINCOS_Field := 16#0#;
      --  Destination increment offset
      DINCOS : MDMA_C1TCR_DINCOS_Field := 16#0#;
      --  source burst transfer configuration
      SBURST : MDMA_C1TCR_SBURST_Field := 16#0#;
      --  Destination burst transfer configuration
      DBURST : MDMA_C1TCR_DBURST_Field := 16#0#;
      --  buffer transfer lengh
      TLEN   : MDMA_C1TCR_TLEN_Field := 16#0#;
      --  PacK Enable These bit is set and cleared by software. If the Source
      --  Size is smaller than the destination, it will be padded according to
      --  the PAM value. If the Source data size is larger than the destination
      --  one, it will be truncated. The alignment will be done according to
      --  the PAM[0] value. This bit is protected and can be written only if EN
      --  is 0
      PKE    : Boolean := False;
      --  Padding/Alignement Mode These bits are set and cleared by software.
      --  Case 1: Source data size smaller than destination data size - 3
      --  options are valid. Case 2: Source data size larger than destination
      --  data size. The remainder part is discarded. When PKE = 1 or
      --  DSIZE=SSIZE, these bits are ignored. These bits are protected and can
      --  be written only if EN is 0
      PAM    : MDMA_C1TCR_PAM_Field := 16#0#;
      --  Trigger Mode These bits are set and cleared by software. Note: If
      --  TRGM is 11 for the current block, all the values loaded at the end of
      --  the current block through the linked list mechanism must keep the
      --  same value (TRGM=11) and the same SWRM value, otherwise the result is
      --  undefined. These bits are protected and can be written only if EN is
      --  0.
      TRGM   : MDMA_C1TCR_TRGM_Field := 16#0#;
      --  SW Request Mode This bit is set and cleared by software. If a HW or
      --  SW request is currently active, the bit change will be delayed until
      --  the current transfer is completed. If the CxMAR contains a valid
      --  address, the CxMDR value will also be written @ CxMAR address. This
      --  bit is protected and can be written only if EN is 0.
      SWRM   : Boolean := False;
      --  Bufferable Write Mode This bit is set and cleared by software. This
      --  bit is protected and can be written only if EN is 0. Note: All MDMA
      --  destination accesses are non-cacheable.
      BWM    : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C1TCR_Register use record
      SINC   at 0 range 0 .. 1;
      DINC   at 0 range 2 .. 3;
      SSIZE  at 0 range 4 .. 5;
      DSIZE  at 0 range 6 .. 7;
      SINCOS at 0 range 8 .. 9;
      DINCOS at 0 range 10 .. 11;
      SBURST at 0 range 12 .. 14;
      DBURST at 0 range 15 .. 17;
      TLEN   at 0 range 18 .. 24;
      PKE    at 0 range 25 .. 25;
      PAM    at 0 range 26 .. 27;
      TRGM   at 0 range 28 .. 29;
      SWRM   at 0 range 30 .. 30;
      BWM    at 0 range 31 .. 31;
   end record;

   subtype MDMA_C1BNDTR_BNDT_Field is STM32H753.UInt17;
   subtype MDMA_C1BNDTR_BRC_Field is STM32H753.UInt12;

   --  MDMA Channel x block number of data register
   type MDMA_C1BNDTR_Register is record
      --  block number of data to transfer
      BNDT           : MDMA_C1BNDTR_BNDT_Field := 16#0#;
      --  unspecified
      Reserved_17_17 : STM32H753.Bit := 16#0#;
      --  Block Repeat Source address Update Mode These bits are protected and
      --  can be written only if EN is 0.
      BRSUM          : Boolean := False;
      --  Block Repeat Destination address Update Mode These bits are protected
      --  and can be written only if EN is 0.
      BRDUM          : Boolean := False;
      --  Block Repeat Count This field contains the number of repetitions of
      --  the current block (0 to 4095). When the channel is enabled, this
      --  register is read-only, indicating the remaining number of blocks,
      --  excluding the current one. This register decrements after each
      --  complete block transfer. Once the last block transfer has completed,
      --  this register can either stay at zero or be reloaded automatically
      --  from memory (in Linked List mode - i.e. Link Address valid). These
      --  bits are protected and can be written only if EN is 0.
      BRC            : MDMA_C1BNDTR_BRC_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C1BNDTR_Register use record
      BNDT           at 0 range 0 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      BRSUM          at 0 range 18 .. 18;
      BRDUM          at 0 range 19 .. 19;
      BRC            at 0 range 20 .. 31;
   end record;

   subtype MDMA_C1BRUR_SUV_Field is STM32H753.UInt16;
   subtype MDMA_C1BRUR_DUV_Field is STM32H753.UInt16;

   --  MDMA channel x Block Repeat address Update register
   type MDMA_C1BRUR_Register is record
      --  source adresse update value
      SUV : MDMA_C1BRUR_SUV_Field := 16#0#;
      --  destination address update
      DUV : MDMA_C1BRUR_DUV_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C1BRUR_Register use record
      SUV at 0 range 0 .. 15;
      DUV at 0 range 16 .. 31;
   end record;

   subtype MDMA_C1TBR_TSEL_Field is STM32H753.UInt6;

   --  MDMA channel x Trigger and Bus selection Register
   type MDMA_C1TBR_Register is record
      --  Trigger selection
      TSEL           : MDMA_C1TBR_TSEL_Field := 16#0#;
      --  unspecified
      Reserved_6_15  : STM32H753.UInt10 := 16#0#;
      --  Source BUS select This bit is protected and can be written only if EN
      --  is 0.
      SBUS           : Boolean := False;
      --  Destination BUS slect This bit is protected and can be written only
      --  if EN is 0.
      DBUS           : Boolean := False;
      --  unspecified
      Reserved_18_31 : STM32H753.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C1TBR_Register use record
      TSEL           at 0 range 0 .. 5;
      Reserved_6_15  at 0 range 6 .. 15;
      SBUS           at 0 range 16 .. 16;
      DBUS           at 0 range 17 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   --  MDMA channel x interrupt/status register
   type MDMA_C2ISR_Register is record
      --  Read-only. Channel x transfer error interrupt flag This bit is set by
      --  hardware. It is cleared by software writing 1 to the corresponding
      --  bit in the DMA_IFCRy register.
      TEIF2          : Boolean;
      --  Read-only. Channel x Channel Transfer Complete interrupt flag This
      --  bit is set by hardware. It is cleared by software writing 1 to the
      --  corresponding bit in the DMA_IFCRy register. CTC is set when the last
      --  block was transferred and the channel has been automatically
      --  disabled. CTC is also set when the channel is suspended, as a result
      --  of writing EN bit to 0.
      CTCIF2         : Boolean;
      --  Read-only. Channel x block repeat transfer complete interrupt flag
      --  This bit is set by hardware. It is cleared by software writing 1 to
      --  the corresponding bit in the DMA_IFCRy register.
      BRTIF2         : Boolean;
      --  Read-only. Channel x block transfer complete interrupt flag This bit
      --  is set by hardware. It is cleared by software writing 1 to the
      --  corresponding bit in the DMA_IFCRy register.
      BTIF2          : Boolean;
      --  Read-only. channel x buffer transfer complete
      TCIF2          : Boolean;
      --  unspecified
      Reserved_5_15  : STM32H753.UInt11;
      --  Read-only. channel x request active flag
      CRQA2          : Boolean;
      --  unspecified
      Reserved_17_31 : STM32H753.UInt15;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C2ISR_Register use record
      TEIF2          at 0 range 0 .. 0;
      CTCIF2         at 0 range 1 .. 1;
      BRTIF2         at 0 range 2 .. 2;
      BTIF2          at 0 range 3 .. 3;
      TCIF2          at 0 range 4 .. 4;
      Reserved_5_15  at 0 range 5 .. 15;
      CRQA2          at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   --  MDMA channel x interrupt flag clear register
   type MDMA_C2IFCR_Register is record
      --  Write-only. Channel x clear transfer error interrupt flag Writing a 1
      --  into this bit clears TEIFx in the MDMA_ISRy register
      CTEIF2        : Boolean := False;
      --  Write-only. Clear Channel transfer complete interrupt flag for
      --  channel x Writing a 1 into this bit clears CTCIFx in the MDMA_ISRy
      --  register
      CCTCIF2       : Boolean := False;
      --  Write-only. Channel x clear block repeat transfer complete interrupt
      --  flag Writing a 1 into this bit clears BRTIFx in the MDMA_ISRy
      --  register
      CBRTIF2       : Boolean := False;
      --  Write-only. Channel x Clear block transfer complete interrupt flag
      --  Writing a 1 into this bit clears BTIFx in the MDMA_ISRy register
      CBTIF2        : Boolean := False;
      --  Write-only. CLear buffer Transfer Complete Interrupt Flag for channel
      --  x Writing a 1 into this bit clears TCIFx in the MDMA_ISRy register
      CLTCIF2       : Boolean := False;
      --  unspecified
      Reserved_5_31 : STM32H753.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C2IFCR_Register use record
      CTEIF2        at 0 range 0 .. 0;
      CCTCIF2       at 0 range 1 .. 1;
      CBRTIF2       at 0 range 2 .. 2;
      CBTIF2        at 0 range 3 .. 3;
      CLTCIF2       at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   subtype MDMA_C2ESR_TEA_Field is STM32H753.UInt7;

   --  MDMA Channel x error status register
   type MDMA_C2ESR_Register is record
      --  Read-only. Transfer Error Address These bits are set and cleared by
      --  HW, in case of an MDMA data transfer error. It is used in conjunction
      --  with TED. This field indicates the 7 LSBits of the address which
      --  generated a transfer/access error. It may be used by SW to retrieve
      --  the failing address, by adding this value (truncated to the buffer
      --  transfer length size) to the current SAR/DAR value. Note: The SAR/DAR
      --  current value doesnt reflect this last address due to the FIFO
      --  management system. The SAR/DAR are only updated at the end of a
      --  (buffer) transfer (of TLEN+1 bytes). Note: It is not set in case of a
      --  link data error.
      TEA            : MDMA_C2ESR_TEA_Field;
      --  Read-only. Transfer Error Direction These bit is set and cleared by
      --  HW, in case of an MDMA data transfer error.
      TED            : Boolean;
      --  Read-only. Transfer Error Link Data These bit is set by HW, in case
      --  of a transfer error while reading the block link data structure. It
      --  is cleared by software writing 1 to the CTEIFx bit in the DMA_IFCRy
      --  register.
      TELD           : Boolean;
      --  Read-only. Transfer Error Mask Data These bit is set by HW, in case
      --  of a transfer error while writing the Mask Data. It is cleared by
      --  software writing 1 to the CTEIFx bit in the DMA_IFCRy register.
      TEMD           : Boolean;
      --  Read-only. Address/Size Error These bit is set by HW, when the
      --  programmed address is not aligned with the data size. TED will
      --  indicate whether the problem is on the source or destination. It is
      --  cleared by software writing 1 to the CTEIFx bit in the DMA_IFCRy
      --  register.
      ASE            : Boolean;
      --  Read-only. Block Size Error These bit is set by HW, when the block
      --  size is not an integer multiple of the data size either for source or
      --  destination. TED will indicate whether the problem is on the source
      --  or destination. It is cleared by software writing 1 to the CTEIFx bit
      --  in the DMA_IFCRy register.
      BSE            : Boolean;
      --  unspecified
      Reserved_12_31 : STM32H753.UInt20;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C2ESR_Register use record
      TEA            at 0 range 0 .. 6;
      TED            at 0 range 7 .. 7;
      TELD           at 0 range 8 .. 8;
      TEMD           at 0 range 9 .. 9;
      ASE            at 0 range 10 .. 10;
      BSE            at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype MDMA_C2CR_PL_Field is STM32H753.UInt2;

   --  This register is used to control the concerned channel.
   type MDMA_C2CR_Register is record
      --  channel enable
      EN             : Boolean := False;
      --  Transfer error interrupt enable This bit is set and cleared by
      --  software.
      TEIE           : Boolean := False;
      --  Channel Transfer Complete interrupt enable This bit is set and
      --  cleared by software.
      CTCIE          : Boolean := False;
      --  Block Repeat transfer interrupt enable This bit is set and cleared by
      --  software.
      BRTIE          : Boolean := False;
      --  Block Transfer interrupt enable This bit is set and cleared by
      --  software.
      BTIE           : Boolean := False;
      --  buffer Transfer Complete interrupt enable This bit is set and cleared
      --  by software.
      TCIE           : Boolean := False;
      --  Priority level These bits are set and cleared by software. These bits
      --  are protected and can be written only if EN is 0.
      PL             : MDMA_C2CR_PL_Field := 16#0#;
      --  unspecified
      Reserved_8_11  : STM32H753.UInt4 := 16#0#;
      --  byte Endianness exchange
      BEX            : Boolean := False;
      --  Half word Endianes exchange
      HEX            : Boolean := False;
      --  Word Endianness exchange
      WEX            : Boolean := False;
      --  unspecified
      Reserved_15_15 : STM32H753.Bit := 16#0#;
      --  Write-only. SW ReQuest Writing a 1 into this bit sets the CRQAx in
      --  MDMA_ISRy register, activating the request on Channel x Note: Either
      --  the whole CxCR register or the 8-bit/16-bit register @ Address
      --  offset: 0x4E + 0x40 chn may be used for SWRQ activation. In case of a
      --  SW request, acknowledge is not generated (neither HW signal, nor
      --  CxMAR write access).
      SWRQ           : Boolean := False;
      --  unspecified
      Reserved_17_31 : STM32H753.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C2CR_Register use record
      EN             at 0 range 0 .. 0;
      TEIE           at 0 range 1 .. 1;
      CTCIE          at 0 range 2 .. 2;
      BRTIE          at 0 range 3 .. 3;
      BTIE           at 0 range 4 .. 4;
      TCIE           at 0 range 5 .. 5;
      PL             at 0 range 6 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      BEX            at 0 range 12 .. 12;
      HEX            at 0 range 13 .. 13;
      WEX            at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      SWRQ           at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype MDMA_C2TCR_SINC_Field is STM32H753.UInt2;
   subtype MDMA_C2TCR_DINC_Field is STM32H753.UInt2;
   subtype MDMA_C2TCR_SSIZE_Field is STM32H753.UInt2;
   subtype MDMA_C2TCR_DSIZE_Field is STM32H753.UInt2;
   subtype MDMA_C2TCR_SINCOS_Field is STM32H753.UInt2;
   subtype MDMA_C2TCR_DINCOS_Field is STM32H753.UInt2;
   subtype MDMA_C2TCR_SBURST_Field is STM32H753.UInt3;
   subtype MDMA_C2TCR_DBURST_Field is STM32H753.UInt3;
   subtype MDMA_C2TCR_TLEN_Field is STM32H753.UInt7;
   subtype MDMA_C2TCR_PAM_Field is STM32H753.UInt2;
   subtype MDMA_C2TCR_TRGM_Field is STM32H753.UInt2;

   --  This register is used to configure the concerned channel.
   type MDMA_C2TCR_Register is record
      --  Source increment mode These bits are set and cleared by software.
      --  These bits are protected and can be written only if EN is 0 Note:
      --  When source is AHB (SBUS=1), SINC = 00 is forbidden. In Linked List
      --  Mode, at the end of a block (single or last block in repeated block
      --  transfer mode), this register will be loaded from memory (from
      --  address given by current LAR[31:0] + 0x00).
      SINC   : MDMA_C2TCR_SINC_Field := 16#0#;
      --  Destination increment mode These bits are set and cleared by
      --  software. These bits are protected and can be written only if EN is 0
      --  Note: When destination is AHB (DBUS=1), DINC = 00 is forbidden.
      DINC   : MDMA_C2TCR_DINC_Field := 16#0#;
      --  Source data size These bits are set and cleared by software. These
      --  bits are protected and can be written only if EN is 0 Note: If a
      --  value of 11 is programmed for the TCM access/AHB port, a transfer
      --  error will occur (TEIF bit set) If SINCOS &lt; SSIZE and SINC &#8800;
      --  00, the result will be unpredictable. Note: SSIZE = 11 (double-word)
      --  is forbidden when source is TCM/AHB bus (SBUS=1).
      SSIZE  : MDMA_C2TCR_SSIZE_Field := 16#0#;
      --  Destination data size These bits are set and cleared by software.
      --  These bits are protected and can be written only if EN is 0. Note: If
      --  a value of 11 is programmed for the TCM access/AHB port, a transfer
      --  error will occur (TEIF bit set) If DINCOS &lt; DSIZE and DINC &#8800;
      --  00, the result will be unpredictable. Note: DSIZE = 11 (double-word)
      --  is forbidden when destination is TCM/AHB bus (DBUS=1).
      DSIZE  : MDMA_C2TCR_DSIZE_Field := 16#0#;
      --  source increment offset size
      SINCOS : MDMA_C2TCR_SINCOS_Field := 16#0#;
      --  Destination increment offset
      DINCOS : MDMA_C2TCR_DINCOS_Field := 16#0#;
      --  source burst transfer configuration
      SBURST : MDMA_C2TCR_SBURST_Field := 16#0#;
      --  Destination burst transfer configuration
      DBURST : MDMA_C2TCR_DBURST_Field := 16#0#;
      --  buffer transfer lengh
      TLEN   : MDMA_C2TCR_TLEN_Field := 16#0#;
      --  PacK Enable These bit is set and cleared by software. If the Source
      --  Size is smaller than the destination, it will be padded according to
      --  the PAM value. If the Source data size is larger than the destination
      --  one, it will be truncated. The alignment will be done according to
      --  the PAM[0] value. This bit is protected and can be written only if EN
      --  is 0
      PKE    : Boolean := False;
      --  Padding/Alignement Mode These bits are set and cleared by software.
      --  Case 1: Source data size smaller than destination data size - 3
      --  options are valid. Case 2: Source data size larger than destination
      --  data size. The remainder part is discarded. When PKE = 1 or
      --  DSIZE=SSIZE, these bits are ignored. These bits are protected and can
      --  be written only if EN is 0
      PAM    : MDMA_C2TCR_PAM_Field := 16#0#;
      --  Trigger Mode These bits are set and cleared by software. Note: If
      --  TRGM is 11 for the current block, all the values loaded at the end of
      --  the current block through the linked list mechanism must keep the
      --  same value (TRGM=11) and the same SWRM value, otherwise the result is
      --  undefined. These bits are protected and can be written only if EN is
      --  0.
      TRGM   : MDMA_C2TCR_TRGM_Field := 16#0#;
      --  SW Request Mode This bit is set and cleared by software. If a HW or
      --  SW request is currently active, the bit change will be delayed until
      --  the current transfer is completed. If the CxMAR contains a valid
      --  address, the CxMDR value will also be written @ CxMAR address. This
      --  bit is protected and can be written only if EN is 0.
      SWRM   : Boolean := False;
      --  Bufferable Write Mode This bit is set and cleared by software. This
      --  bit is protected and can be written only if EN is 0. Note: All MDMA
      --  destination accesses are non-cacheable.
      BWM    : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C2TCR_Register use record
      SINC   at 0 range 0 .. 1;
      DINC   at 0 range 2 .. 3;
      SSIZE  at 0 range 4 .. 5;
      DSIZE  at 0 range 6 .. 7;
      SINCOS at 0 range 8 .. 9;
      DINCOS at 0 range 10 .. 11;
      SBURST at 0 range 12 .. 14;
      DBURST at 0 range 15 .. 17;
      TLEN   at 0 range 18 .. 24;
      PKE    at 0 range 25 .. 25;
      PAM    at 0 range 26 .. 27;
      TRGM   at 0 range 28 .. 29;
      SWRM   at 0 range 30 .. 30;
      BWM    at 0 range 31 .. 31;
   end record;

   subtype MDMA_C2BNDTR_BNDT_Field is STM32H753.UInt17;
   subtype MDMA_C2BNDTR_BRC_Field is STM32H753.UInt12;

   --  MDMA Channel x block number of data register
   type MDMA_C2BNDTR_Register is record
      --  block number of data to transfer
      BNDT           : MDMA_C2BNDTR_BNDT_Field := 16#0#;
      --  unspecified
      Reserved_17_17 : STM32H753.Bit := 16#0#;
      --  Block Repeat Source address Update Mode These bits are protected and
      --  can be written only if EN is 0.
      BRSUM          : Boolean := False;
      --  Block Repeat Destination address Update Mode These bits are protected
      --  and can be written only if EN is 0.
      BRDUM          : Boolean := False;
      --  Block Repeat Count This field contains the number of repetitions of
      --  the current block (0 to 4095). When the channel is enabled, this
      --  register is read-only, indicating the remaining number of blocks,
      --  excluding the current one. This register decrements after each
      --  complete block transfer. Once the last block transfer has completed,
      --  this register can either stay at zero or be reloaded automatically
      --  from memory (in Linked List mode - i.e. Link Address valid). These
      --  bits are protected and can be written only if EN is 0.
      BRC            : MDMA_C2BNDTR_BRC_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C2BNDTR_Register use record
      BNDT           at 0 range 0 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      BRSUM          at 0 range 18 .. 18;
      BRDUM          at 0 range 19 .. 19;
      BRC            at 0 range 20 .. 31;
   end record;

   subtype MDMA_C2BRUR_SUV_Field is STM32H753.UInt16;
   subtype MDMA_C2BRUR_DUV_Field is STM32H753.UInt16;

   --  MDMA channel x Block Repeat address Update register
   type MDMA_C2BRUR_Register is record
      --  source adresse update value
      SUV : MDMA_C2BRUR_SUV_Field := 16#0#;
      --  destination address update
      DUV : MDMA_C2BRUR_DUV_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C2BRUR_Register use record
      SUV at 0 range 0 .. 15;
      DUV at 0 range 16 .. 31;
   end record;

   subtype MDMA_C2TBR_TSEL_Field is STM32H753.UInt6;

   --  MDMA channel x Trigger and Bus selection Register
   type MDMA_C2TBR_Register is record
      --  Trigger selection
      TSEL           : MDMA_C2TBR_TSEL_Field := 16#0#;
      --  unspecified
      Reserved_6_15  : STM32H753.UInt10 := 16#0#;
      --  Source BUS select This bit is protected and can be written only if EN
      --  is 0.
      SBUS           : Boolean := False;
      --  Destination BUS slect This bit is protected and can be written only
      --  if EN is 0.
      DBUS           : Boolean := False;
      --  unspecified
      Reserved_18_31 : STM32H753.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C2TBR_Register use record
      TSEL           at 0 range 0 .. 5;
      Reserved_6_15  at 0 range 6 .. 15;
      SBUS           at 0 range 16 .. 16;
      DBUS           at 0 range 17 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   --  MDMA channel x interrupt/status register
   type MDMA_C3ISR_Register is record
      --  Read-only. Channel x transfer error interrupt flag This bit is set by
      --  hardware. It is cleared by software writing 1 to the corresponding
      --  bit in the DMA_IFCRy register.
      TEIF3          : Boolean;
      --  Read-only. Channel x Channel Transfer Complete interrupt flag This
      --  bit is set by hardware. It is cleared by software writing 1 to the
      --  corresponding bit in the DMA_IFCRy register. CTC is set when the last
      --  block was transferred and the channel has been automatically
      --  disabled. CTC is also set when the channel is suspended, as a result
      --  of writing EN bit to 0.
      CTCIF3         : Boolean;
      --  Read-only. Channel x block repeat transfer complete interrupt flag
      --  This bit is set by hardware. It is cleared by software writing 1 to
      --  the corresponding bit in the DMA_IFCRy register.
      BRTIF3         : Boolean;
      --  Read-only. Channel x block transfer complete interrupt flag This bit
      --  is set by hardware. It is cleared by software writing 1 to the
      --  corresponding bit in the DMA_IFCRy register.
      BTIF3          : Boolean;
      --  Read-only. channel x buffer transfer complete
      TCIF3          : Boolean;
      --  unspecified
      Reserved_5_15  : STM32H753.UInt11;
      --  Read-only. channel x request active flag
      CRQA3          : Boolean;
      --  unspecified
      Reserved_17_31 : STM32H753.UInt15;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C3ISR_Register use record
      TEIF3          at 0 range 0 .. 0;
      CTCIF3         at 0 range 1 .. 1;
      BRTIF3         at 0 range 2 .. 2;
      BTIF3          at 0 range 3 .. 3;
      TCIF3          at 0 range 4 .. 4;
      Reserved_5_15  at 0 range 5 .. 15;
      CRQA3          at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   --  MDMA channel x interrupt flag clear register
   type MDMA_C3IFCR_Register is record
      --  Write-only. Channel x clear transfer error interrupt flag Writing a 1
      --  into this bit clears TEIFx in the MDMA_ISRy register
      CTEIF3        : Boolean := False;
      --  Write-only. Clear Channel transfer complete interrupt flag for
      --  channel x Writing a 1 into this bit clears CTCIFx in the MDMA_ISRy
      --  register
      CCTCIF3       : Boolean := False;
      --  Write-only. Channel x clear block repeat transfer complete interrupt
      --  flag Writing a 1 into this bit clears BRTIFx in the MDMA_ISRy
      --  register
      CBRTIF3       : Boolean := False;
      --  Write-only. Channel x Clear block transfer complete interrupt flag
      --  Writing a 1 into this bit clears BTIFx in the MDMA_ISRy register
      CBTIF3        : Boolean := False;
      --  Write-only. CLear buffer Transfer Complete Interrupt Flag for channel
      --  x Writing a 1 into this bit clears TCIFx in the MDMA_ISRy register
      CLTCIF3       : Boolean := False;
      --  unspecified
      Reserved_5_31 : STM32H753.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C3IFCR_Register use record
      CTEIF3        at 0 range 0 .. 0;
      CCTCIF3       at 0 range 1 .. 1;
      CBRTIF3       at 0 range 2 .. 2;
      CBTIF3        at 0 range 3 .. 3;
      CLTCIF3       at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   subtype MDMA_C3ESR_TEA_Field is STM32H753.UInt7;

   --  MDMA Channel x error status register
   type MDMA_C3ESR_Register is record
      --  Read-only. Transfer Error Address These bits are set and cleared by
      --  HW, in case of an MDMA data transfer error. It is used in conjunction
      --  with TED. This field indicates the 7 LSBits of the address which
      --  generated a transfer/access error. It may be used by SW to retrieve
      --  the failing address, by adding this value (truncated to the buffer
      --  transfer length size) to the current SAR/DAR value. Note: The SAR/DAR
      --  current value doesnt reflect this last address due to the FIFO
      --  management system. The SAR/DAR are only updated at the end of a
      --  (buffer) transfer (of TLEN+1 bytes). Note: It is not set in case of a
      --  link data error.
      TEA            : MDMA_C3ESR_TEA_Field;
      --  Read-only. Transfer Error Direction These bit is set and cleared by
      --  HW, in case of an MDMA data transfer error.
      TED            : Boolean;
      --  Read-only. Transfer Error Link Data These bit is set by HW, in case
      --  of a transfer error while reading the block link data structure. It
      --  is cleared by software writing 1 to the CTEIFx bit in the DMA_IFCRy
      --  register.
      TELD           : Boolean;
      --  Read-only. Transfer Error Mask Data These bit is set by HW, in case
      --  of a transfer error while writing the Mask Data. It is cleared by
      --  software writing 1 to the CTEIFx bit in the DMA_IFCRy register.
      TEMD           : Boolean;
      --  Read-only. Address/Size Error These bit is set by HW, when the
      --  programmed address is not aligned with the data size. TED will
      --  indicate whether the problem is on the source or destination. It is
      --  cleared by software writing 1 to the CTEIFx bit in the DMA_IFCRy
      --  register.
      ASE            : Boolean;
      --  Read-only. Block Size Error These bit is set by HW, when the block
      --  size is not an integer multiple of the data size either for source or
      --  destination. TED will indicate whether the problem is on the source
      --  or destination. It is cleared by software writing 1 to the CTEIFx bit
      --  in the DMA_IFCRy register.
      BSE            : Boolean;
      --  unspecified
      Reserved_12_31 : STM32H753.UInt20;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C3ESR_Register use record
      TEA            at 0 range 0 .. 6;
      TED            at 0 range 7 .. 7;
      TELD           at 0 range 8 .. 8;
      TEMD           at 0 range 9 .. 9;
      ASE            at 0 range 10 .. 10;
      BSE            at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype MDMA_C3CR_PL_Field is STM32H753.UInt2;

   --  This register is used to control the concerned channel.
   type MDMA_C3CR_Register is record
      --  channel enable
      EN             : Boolean := False;
      --  Transfer error interrupt enable This bit is set and cleared by
      --  software.
      TEIE           : Boolean := False;
      --  Channel Transfer Complete interrupt enable This bit is set and
      --  cleared by software.
      CTCIE          : Boolean := False;
      --  Block Repeat transfer interrupt enable This bit is set and cleared by
      --  software.
      BRTIE          : Boolean := False;
      --  Block Transfer interrupt enable This bit is set and cleared by
      --  software.
      BTIE           : Boolean := False;
      --  buffer Transfer Complete interrupt enable This bit is set and cleared
      --  by software.
      TCIE           : Boolean := False;
      --  Priority level These bits are set and cleared by software. These bits
      --  are protected and can be written only if EN is 0.
      PL             : MDMA_C3CR_PL_Field := 16#0#;
      --  unspecified
      Reserved_8_11  : STM32H753.UInt4 := 16#0#;
      --  byte Endianness exchange
      BEX            : Boolean := False;
      --  Half word Endianes exchange
      HEX            : Boolean := False;
      --  Word Endianness exchange
      WEX            : Boolean := False;
      --  unspecified
      Reserved_15_15 : STM32H753.Bit := 16#0#;
      --  Write-only. SW ReQuest Writing a 1 into this bit sets the CRQAx in
      --  MDMA_ISRy register, activating the request on Channel x Note: Either
      --  the whole CxCR register or the 8-bit/16-bit register @ Address
      --  offset: 0x4E + 0x40 chn may be used for SWRQ activation. In case of a
      --  SW request, acknowledge is not generated (neither HW signal, nor
      --  CxMAR write access).
      SWRQ           : Boolean := False;
      --  unspecified
      Reserved_17_31 : STM32H753.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C3CR_Register use record
      EN             at 0 range 0 .. 0;
      TEIE           at 0 range 1 .. 1;
      CTCIE          at 0 range 2 .. 2;
      BRTIE          at 0 range 3 .. 3;
      BTIE           at 0 range 4 .. 4;
      TCIE           at 0 range 5 .. 5;
      PL             at 0 range 6 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      BEX            at 0 range 12 .. 12;
      HEX            at 0 range 13 .. 13;
      WEX            at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      SWRQ           at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype MDMA_C3TCR_SINC_Field is STM32H753.UInt2;
   subtype MDMA_C3TCR_DINC_Field is STM32H753.UInt2;
   subtype MDMA_C3TCR_SSIZE_Field is STM32H753.UInt2;
   subtype MDMA_C3TCR_DSIZE_Field is STM32H753.UInt2;
   subtype MDMA_C3TCR_SINCOS_Field is STM32H753.UInt2;
   subtype MDMA_C3TCR_DINCOS_Field is STM32H753.UInt2;
   subtype MDMA_C3TCR_SBURST_Field is STM32H753.UInt3;
   subtype MDMA_C3TCR_DBURST_Field is STM32H753.UInt3;
   subtype MDMA_C3TCR_TLEN_Field is STM32H753.UInt7;
   subtype MDMA_C3TCR_PAM_Field is STM32H753.UInt2;
   subtype MDMA_C3TCR_TRGM_Field is STM32H753.UInt2;

   --  This register is used to configure the concerned channel.
   type MDMA_C3TCR_Register is record
      --  Source increment mode These bits are set and cleared by software.
      --  These bits are protected and can be written only if EN is 0 Note:
      --  When source is AHB (SBUS=1), SINC = 00 is forbidden. In Linked List
      --  Mode, at the end of a block (single or last block in repeated block
      --  transfer mode), this register will be loaded from memory (from
      --  address given by current LAR[31:0] + 0x00).
      SINC   : MDMA_C3TCR_SINC_Field := 16#0#;
      --  Destination increment mode These bits are set and cleared by
      --  software. These bits are protected and can be written only if EN is 0
      --  Note: When destination is AHB (DBUS=1), DINC = 00 is forbidden.
      DINC   : MDMA_C3TCR_DINC_Field := 16#0#;
      --  Source data size These bits are set and cleared by software. These
      --  bits are protected and can be written only if EN is 0 Note: If a
      --  value of 11 is programmed for the TCM access/AHB port, a transfer
      --  error will occur (TEIF bit set) If SINCOS &lt; SSIZE and SINC &#8800;
      --  00, the result will be unpredictable. Note: SSIZE = 11 (double-word)
      --  is forbidden when source is TCM/AHB bus (SBUS=1).
      SSIZE  : MDMA_C3TCR_SSIZE_Field := 16#0#;
      --  Destination data size These bits are set and cleared by software.
      --  These bits are protected and can be written only if EN is 0. Note: If
      --  a value of 11 is programmed for the TCM access/AHB port, a transfer
      --  error will occur (TEIF bit set) If DINCOS &lt; DSIZE and DINC &#8800;
      --  00, the result will be unpredictable. Note: DSIZE = 11 (double-word)
      --  is forbidden when destination is TCM/AHB bus (DBUS=1).
      DSIZE  : MDMA_C3TCR_DSIZE_Field := 16#0#;
      --  source increment offset size
      SINCOS : MDMA_C3TCR_SINCOS_Field := 16#0#;
      --  Destination increment offset
      DINCOS : MDMA_C3TCR_DINCOS_Field := 16#0#;
      --  source burst transfer configuration
      SBURST : MDMA_C3TCR_SBURST_Field := 16#0#;
      --  Destination burst transfer configuration
      DBURST : MDMA_C3TCR_DBURST_Field := 16#0#;
      --  buffer transfer lengh
      TLEN   : MDMA_C3TCR_TLEN_Field := 16#0#;
      --  PacK Enable These bit is set and cleared by software. If the Source
      --  Size is smaller than the destination, it will be padded according to
      --  the PAM value. If the Source data size is larger than the destination
      --  one, it will be truncated. The alignment will be done according to
      --  the PAM[0] value. This bit is protected and can be written only if EN
      --  is 0
      PKE    : Boolean := False;
      --  Padding/Alignement Mode These bits are set and cleared by software.
      --  Case 1: Source data size smaller than destination data size - 3
      --  options are valid. Case 2: Source data size larger than destination
      --  data size. The remainder part is discarded. When PKE = 1 or
      --  DSIZE=SSIZE, these bits are ignored. These bits are protected and can
      --  be written only if EN is 0
      PAM    : MDMA_C3TCR_PAM_Field := 16#0#;
      --  Trigger Mode These bits are set and cleared by software. Note: If
      --  TRGM is 11 for the current block, all the values loaded at the end of
      --  the current block through the linked list mechanism must keep the
      --  same value (TRGM=11) and the same SWRM value, otherwise the result is
      --  undefined. These bits are protected and can be written only if EN is
      --  0.
      TRGM   : MDMA_C3TCR_TRGM_Field := 16#0#;
      --  SW Request Mode This bit is set and cleared by software. If a HW or
      --  SW request is currently active, the bit change will be delayed until
      --  the current transfer is completed. If the CxMAR contains a valid
      --  address, the CxMDR value will also be written @ CxMAR address. This
      --  bit is protected and can be written only if EN is 0.
      SWRM   : Boolean := False;
      --  Bufferable Write Mode This bit is set and cleared by software. This
      --  bit is protected and can be written only if EN is 0. Note: All MDMA
      --  destination accesses are non-cacheable.
      BWM    : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C3TCR_Register use record
      SINC   at 0 range 0 .. 1;
      DINC   at 0 range 2 .. 3;
      SSIZE  at 0 range 4 .. 5;
      DSIZE  at 0 range 6 .. 7;
      SINCOS at 0 range 8 .. 9;
      DINCOS at 0 range 10 .. 11;
      SBURST at 0 range 12 .. 14;
      DBURST at 0 range 15 .. 17;
      TLEN   at 0 range 18 .. 24;
      PKE    at 0 range 25 .. 25;
      PAM    at 0 range 26 .. 27;
      TRGM   at 0 range 28 .. 29;
      SWRM   at 0 range 30 .. 30;
      BWM    at 0 range 31 .. 31;
   end record;

   subtype MDMA_C3BNDTR_BNDT_Field is STM32H753.UInt17;
   subtype MDMA_C3BNDTR_BRC_Field is STM32H753.UInt12;

   --  MDMA Channel x block number of data register
   type MDMA_C3BNDTR_Register is record
      --  block number of data to transfer
      BNDT           : MDMA_C3BNDTR_BNDT_Field := 16#0#;
      --  unspecified
      Reserved_17_17 : STM32H753.Bit := 16#0#;
      --  Block Repeat Source address Update Mode These bits are protected and
      --  can be written only if EN is 0.
      BRSUM          : Boolean := False;
      --  Block Repeat Destination address Update Mode These bits are protected
      --  and can be written only if EN is 0.
      BRDUM          : Boolean := False;
      --  Block Repeat Count This field contains the number of repetitions of
      --  the current block (0 to 4095). When the channel is enabled, this
      --  register is read-only, indicating the remaining number of blocks,
      --  excluding the current one. This register decrements after each
      --  complete block transfer. Once the last block transfer has completed,
      --  this register can either stay at zero or be reloaded automatically
      --  from memory (in Linked List mode - i.e. Link Address valid). These
      --  bits are protected and can be written only if EN is 0.
      BRC            : MDMA_C3BNDTR_BRC_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C3BNDTR_Register use record
      BNDT           at 0 range 0 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      BRSUM          at 0 range 18 .. 18;
      BRDUM          at 0 range 19 .. 19;
      BRC            at 0 range 20 .. 31;
   end record;

   subtype MDMA_C3BRUR_SUV_Field is STM32H753.UInt16;
   subtype MDMA_C3BRUR_DUV_Field is STM32H753.UInt16;

   --  MDMA channel x Block Repeat address Update register
   type MDMA_C3BRUR_Register is record
      --  source adresse update value
      SUV : MDMA_C3BRUR_SUV_Field := 16#0#;
      --  destination address update
      DUV : MDMA_C3BRUR_DUV_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C3BRUR_Register use record
      SUV at 0 range 0 .. 15;
      DUV at 0 range 16 .. 31;
   end record;

   subtype MDMA_C3TBR_TSEL_Field is STM32H753.UInt6;

   --  MDMA channel x Trigger and Bus selection Register
   type MDMA_C3TBR_Register is record
      --  Trigger selection
      TSEL           : MDMA_C3TBR_TSEL_Field := 16#0#;
      --  unspecified
      Reserved_6_15  : STM32H753.UInt10 := 16#0#;
      --  Source BUS select This bit is protected and can be written only if EN
      --  is 0.
      SBUS           : Boolean := False;
      --  Destination BUS slect This bit is protected and can be written only
      --  if EN is 0.
      DBUS           : Boolean := False;
      --  unspecified
      Reserved_18_31 : STM32H753.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C3TBR_Register use record
      TSEL           at 0 range 0 .. 5;
      Reserved_6_15  at 0 range 6 .. 15;
      SBUS           at 0 range 16 .. 16;
      DBUS           at 0 range 17 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   --  MDMA channel x interrupt/status register
   type MDMA_C4ISR_Register is record
      --  Read-only. Channel x transfer error interrupt flag This bit is set by
      --  hardware. It is cleared by software writing 1 to the corresponding
      --  bit in the DMA_IFCRy register.
      TEIF4          : Boolean;
      --  Read-only. Channel x Channel Transfer Complete interrupt flag This
      --  bit is set by hardware. It is cleared by software writing 1 to the
      --  corresponding bit in the DMA_IFCRy register. CTC is set when the last
      --  block was transferred and the channel has been automatically
      --  disabled. CTC is also set when the channel is suspended, as a result
      --  of writing EN bit to 0.
      CTCIF4         : Boolean;
      --  Read-only. Channel x block repeat transfer complete interrupt flag
      --  This bit is set by hardware. It is cleared by software writing 1 to
      --  the corresponding bit in the DMA_IFCRy register.
      BRTIF4         : Boolean;
      --  Read-only. Channel x block transfer complete interrupt flag This bit
      --  is set by hardware. It is cleared by software writing 1 to the
      --  corresponding bit in the DMA_IFCRy register.
      BTIF4          : Boolean;
      --  Read-only. channel x buffer transfer complete
      TCIF4          : Boolean;
      --  unspecified
      Reserved_5_15  : STM32H753.UInt11;
      --  Read-only. channel x request active flag
      CRQA4          : Boolean;
      --  unspecified
      Reserved_17_31 : STM32H753.UInt15;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C4ISR_Register use record
      TEIF4          at 0 range 0 .. 0;
      CTCIF4         at 0 range 1 .. 1;
      BRTIF4         at 0 range 2 .. 2;
      BTIF4          at 0 range 3 .. 3;
      TCIF4          at 0 range 4 .. 4;
      Reserved_5_15  at 0 range 5 .. 15;
      CRQA4          at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   --  MDMA channel x interrupt flag clear register
   type MDMA_C4IFCR_Register is record
      --  Write-only. Channel x clear transfer error interrupt flag Writing a 1
      --  into this bit clears TEIFx in the MDMA_ISRy register
      CTEIF4        : Boolean := False;
      --  Write-only. Clear Channel transfer complete interrupt flag for
      --  channel x Writing a 1 into this bit clears CTCIFx in the MDMA_ISRy
      --  register
      CCTCIF4       : Boolean := False;
      --  Write-only. Channel x clear block repeat transfer complete interrupt
      --  flag Writing a 1 into this bit clears BRTIFx in the MDMA_ISRy
      --  register
      CBRTIF4       : Boolean := False;
      --  Write-only. Channel x Clear block transfer complete interrupt flag
      --  Writing a 1 into this bit clears BTIFx in the MDMA_ISRy register
      CBTIF4        : Boolean := False;
      --  Write-only. CLear buffer Transfer Complete Interrupt Flag for channel
      --  x Writing a 1 into this bit clears TCIFx in the MDMA_ISRy register
      CLTCIF4       : Boolean := False;
      --  unspecified
      Reserved_5_31 : STM32H753.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C4IFCR_Register use record
      CTEIF4        at 0 range 0 .. 0;
      CCTCIF4       at 0 range 1 .. 1;
      CBRTIF4       at 0 range 2 .. 2;
      CBTIF4        at 0 range 3 .. 3;
      CLTCIF4       at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   subtype MDMA_C4ESR_TEA_Field is STM32H753.UInt7;

   --  MDMA Channel x error status register
   type MDMA_C4ESR_Register is record
      --  Read-only. Transfer Error Address These bits are set and cleared by
      --  HW, in case of an MDMA data transfer error. It is used in conjunction
      --  with TED. This field indicates the 7 LSBits of the address which
      --  generated a transfer/access error. It may be used by SW to retrieve
      --  the failing address, by adding this value (truncated to the buffer
      --  transfer length size) to the current SAR/DAR value. Note: The SAR/DAR
      --  current value doesnt reflect this last address due to the FIFO
      --  management system. The SAR/DAR are only updated at the end of a
      --  (buffer) transfer (of TLEN+1 bytes). Note: It is not set in case of a
      --  link data error.
      TEA            : MDMA_C4ESR_TEA_Field;
      --  Read-only. Transfer Error Direction These bit is set and cleared by
      --  HW, in case of an MDMA data transfer error.
      TED            : Boolean;
      --  Read-only. Transfer Error Link Data These bit is set by HW, in case
      --  of a transfer error while reading the block link data structure. It
      --  is cleared by software writing 1 to the CTEIFx bit in the DMA_IFCRy
      --  register.
      TELD           : Boolean;
      --  Read-only. Transfer Error Mask Data These bit is set by HW, in case
      --  of a transfer error while writing the Mask Data. It is cleared by
      --  software writing 1 to the CTEIFx bit in the DMA_IFCRy register.
      TEMD           : Boolean;
      --  Read-only. Address/Size Error These bit is set by HW, when the
      --  programmed address is not aligned with the data size. TED will
      --  indicate whether the problem is on the source or destination. It is
      --  cleared by software writing 1 to the CTEIFx bit in the DMA_IFCRy
      --  register.
      ASE            : Boolean;
      --  Read-only. Block Size Error These bit is set by HW, when the block
      --  size is not an integer multiple of the data size either for source or
      --  destination. TED will indicate whether the problem is on the source
      --  or destination. It is cleared by software writing 1 to the CTEIFx bit
      --  in the DMA_IFCRy register.
      BSE            : Boolean;
      --  unspecified
      Reserved_12_31 : STM32H753.UInt20;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C4ESR_Register use record
      TEA            at 0 range 0 .. 6;
      TED            at 0 range 7 .. 7;
      TELD           at 0 range 8 .. 8;
      TEMD           at 0 range 9 .. 9;
      ASE            at 0 range 10 .. 10;
      BSE            at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype MDMA_C4CR_PL_Field is STM32H753.UInt2;

   --  This register is used to control the concerned channel.
   type MDMA_C4CR_Register is record
      --  channel enable
      EN             : Boolean := False;
      --  Transfer error interrupt enable This bit is set and cleared by
      --  software.
      TEIE           : Boolean := False;
      --  Channel Transfer Complete interrupt enable This bit is set and
      --  cleared by software.
      CTCIE          : Boolean := False;
      --  Block Repeat transfer interrupt enable This bit is set and cleared by
      --  software.
      BRTIE          : Boolean := False;
      --  Block Transfer interrupt enable This bit is set and cleared by
      --  software.
      BTIE           : Boolean := False;
      --  buffer Transfer Complete interrupt enable This bit is set and cleared
      --  by software.
      TCIE           : Boolean := False;
      --  Priority level These bits are set and cleared by software. These bits
      --  are protected and can be written only if EN is 0.
      PL             : MDMA_C4CR_PL_Field := 16#0#;
      --  unspecified
      Reserved_8_11  : STM32H753.UInt4 := 16#0#;
      --  byte Endianness exchange
      BEX            : Boolean := False;
      --  Half word Endianes exchange
      HEX            : Boolean := False;
      --  Word Endianness exchange
      WEX            : Boolean := False;
      --  unspecified
      Reserved_15_15 : STM32H753.Bit := 16#0#;
      --  Write-only. SW ReQuest Writing a 1 into this bit sets the CRQAx in
      --  MDMA_ISRy register, activating the request on Channel x Note: Either
      --  the whole CxCR register or the 8-bit/16-bit register @ Address
      --  offset: 0x4E + 0x40 chn may be used for SWRQ activation. In case of a
      --  SW request, acknowledge is not generated (neither HW signal, nor
      --  CxMAR write access).
      SWRQ           : Boolean := False;
      --  unspecified
      Reserved_17_31 : STM32H753.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C4CR_Register use record
      EN             at 0 range 0 .. 0;
      TEIE           at 0 range 1 .. 1;
      CTCIE          at 0 range 2 .. 2;
      BRTIE          at 0 range 3 .. 3;
      BTIE           at 0 range 4 .. 4;
      TCIE           at 0 range 5 .. 5;
      PL             at 0 range 6 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      BEX            at 0 range 12 .. 12;
      HEX            at 0 range 13 .. 13;
      WEX            at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      SWRQ           at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype MDMA_C4TCR_SINC_Field is STM32H753.UInt2;
   subtype MDMA_C4TCR_DINC_Field is STM32H753.UInt2;
   subtype MDMA_C4TCR_SSIZE_Field is STM32H753.UInt2;
   subtype MDMA_C4TCR_DSIZE_Field is STM32H753.UInt2;
   subtype MDMA_C4TCR_SINCOS_Field is STM32H753.UInt2;
   subtype MDMA_C4TCR_DINCOS_Field is STM32H753.UInt2;
   subtype MDMA_C4TCR_SBURST_Field is STM32H753.UInt3;
   subtype MDMA_C4TCR_DBURST_Field is STM32H753.UInt3;
   subtype MDMA_C4TCR_TLEN_Field is STM32H753.UInt7;
   subtype MDMA_C4TCR_PAM_Field is STM32H753.UInt2;
   subtype MDMA_C4TCR_TRGM_Field is STM32H753.UInt2;

   --  This register is used to configure the concerned channel.
   type MDMA_C4TCR_Register is record
      --  Source increment mode These bits are set and cleared by software.
      --  These bits are protected and can be written only if EN is 0 Note:
      --  When source is AHB (SBUS=1), SINC = 00 is forbidden. In Linked List
      --  Mode, at the end of a block (single or last block in repeated block
      --  transfer mode), this register will be loaded from memory (from
      --  address given by current LAR[31:0] + 0x00).
      SINC   : MDMA_C4TCR_SINC_Field := 16#0#;
      --  Destination increment mode These bits are set and cleared by
      --  software. These bits are protected and can be written only if EN is 0
      --  Note: When destination is AHB (DBUS=1), DINC = 00 is forbidden.
      DINC   : MDMA_C4TCR_DINC_Field := 16#0#;
      --  Source data size These bits are set and cleared by software. These
      --  bits are protected and can be written only if EN is 0 Note: If a
      --  value of 11 is programmed for the TCM access/AHB port, a transfer
      --  error will occur (TEIF bit set) If SINCOS &lt; SSIZE and SINC &#8800;
      --  00, the result will be unpredictable. Note: SSIZE = 11 (double-word)
      --  is forbidden when source is TCM/AHB bus (SBUS=1).
      SSIZE  : MDMA_C4TCR_SSIZE_Field := 16#0#;
      --  Destination data size These bits are set and cleared by software.
      --  These bits are protected and can be written only if EN is 0. Note: If
      --  a value of 11 is programmed for the TCM access/AHB port, a transfer
      --  error will occur (TEIF bit set) If DINCOS &lt; DSIZE and DINC &#8800;
      --  00, the result will be unpredictable. Note: DSIZE = 11 (double-word)
      --  is forbidden when destination is TCM/AHB bus (DBUS=1).
      DSIZE  : MDMA_C4TCR_DSIZE_Field := 16#0#;
      --  source increment offset size
      SINCOS : MDMA_C4TCR_SINCOS_Field := 16#0#;
      --  Destination increment offset
      DINCOS : MDMA_C4TCR_DINCOS_Field := 16#0#;
      --  source burst transfer configuration
      SBURST : MDMA_C4TCR_SBURST_Field := 16#0#;
      --  Destination burst transfer configuration
      DBURST : MDMA_C4TCR_DBURST_Field := 16#0#;
      --  buffer transfer lengh
      TLEN   : MDMA_C4TCR_TLEN_Field := 16#0#;
      --  PacK Enable These bit is set and cleared by software. If the Source
      --  Size is smaller than the destination, it will be padded according to
      --  the PAM value. If the Source data size is larger than the destination
      --  one, it will be truncated. The alignment will be done according to
      --  the PAM[0] value. This bit is protected and can be written only if EN
      --  is 0
      PKE    : Boolean := False;
      --  Padding/Alignement Mode These bits are set and cleared by software.
      --  Case 1: Source data size smaller than destination data size - 3
      --  options are valid. Case 2: Source data size larger than destination
      --  data size. The remainder part is discarded. When PKE = 1 or
      --  DSIZE=SSIZE, these bits are ignored. These bits are protected and can
      --  be written only if EN is 0
      PAM    : MDMA_C4TCR_PAM_Field := 16#0#;
      --  Trigger Mode These bits are set and cleared by software. Note: If
      --  TRGM is 11 for the current block, all the values loaded at the end of
      --  the current block through the linked list mechanism must keep the
      --  same value (TRGM=11) and the same SWRM value, otherwise the result is
      --  undefined. These bits are protected and can be written only if EN is
      --  0.
      TRGM   : MDMA_C4TCR_TRGM_Field := 16#0#;
      --  SW Request Mode This bit is set and cleared by software. If a HW or
      --  SW request is currently active, the bit change will be delayed until
      --  the current transfer is completed. If the CxMAR contains a valid
      --  address, the CxMDR value will also be written @ CxMAR address. This
      --  bit is protected and can be written only if EN is 0.
      SWRM   : Boolean := False;
      --  Bufferable Write Mode This bit is set and cleared by software. This
      --  bit is protected and can be written only if EN is 0. Note: All MDMA
      --  destination accesses are non-cacheable.
      BWM    : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C4TCR_Register use record
      SINC   at 0 range 0 .. 1;
      DINC   at 0 range 2 .. 3;
      SSIZE  at 0 range 4 .. 5;
      DSIZE  at 0 range 6 .. 7;
      SINCOS at 0 range 8 .. 9;
      DINCOS at 0 range 10 .. 11;
      SBURST at 0 range 12 .. 14;
      DBURST at 0 range 15 .. 17;
      TLEN   at 0 range 18 .. 24;
      PKE    at 0 range 25 .. 25;
      PAM    at 0 range 26 .. 27;
      TRGM   at 0 range 28 .. 29;
      SWRM   at 0 range 30 .. 30;
      BWM    at 0 range 31 .. 31;
   end record;

   subtype MDMA_C4BNDTR_BNDT_Field is STM32H753.UInt17;
   subtype MDMA_C4BNDTR_BRC_Field is STM32H753.UInt12;

   --  MDMA Channel x block number of data register
   type MDMA_C4BNDTR_Register is record
      --  block number of data to transfer
      BNDT           : MDMA_C4BNDTR_BNDT_Field := 16#0#;
      --  unspecified
      Reserved_17_17 : STM32H753.Bit := 16#0#;
      --  Block Repeat Source address Update Mode These bits are protected and
      --  can be written only if EN is 0.
      BRSUM          : Boolean := False;
      --  Block Repeat Destination address Update Mode These bits are protected
      --  and can be written only if EN is 0.
      BRDUM          : Boolean := False;
      --  Block Repeat Count This field contains the number of repetitions of
      --  the current block (0 to 4095). When the channel is enabled, this
      --  register is read-only, indicating the remaining number of blocks,
      --  excluding the current one. This register decrements after each
      --  complete block transfer. Once the last block transfer has completed,
      --  this register can either stay at zero or be reloaded automatically
      --  from memory (in Linked List mode - i.e. Link Address valid). These
      --  bits are protected and can be written only if EN is 0.
      BRC            : MDMA_C4BNDTR_BRC_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C4BNDTR_Register use record
      BNDT           at 0 range 0 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      BRSUM          at 0 range 18 .. 18;
      BRDUM          at 0 range 19 .. 19;
      BRC            at 0 range 20 .. 31;
   end record;

   subtype MDMA_C4BRUR_SUV_Field is STM32H753.UInt16;
   subtype MDMA_C4BRUR_DUV_Field is STM32H753.UInt16;

   --  MDMA channel x Block Repeat address Update register
   type MDMA_C4BRUR_Register is record
      --  source adresse update value
      SUV : MDMA_C4BRUR_SUV_Field := 16#0#;
      --  destination address update
      DUV : MDMA_C4BRUR_DUV_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C4BRUR_Register use record
      SUV at 0 range 0 .. 15;
      DUV at 0 range 16 .. 31;
   end record;

   subtype MDMA_C4TBR_TSEL_Field is STM32H753.UInt6;

   --  MDMA channel x Trigger and Bus selection Register
   type MDMA_C4TBR_Register is record
      --  Trigger selection
      TSEL           : MDMA_C4TBR_TSEL_Field := 16#0#;
      --  unspecified
      Reserved_6_15  : STM32H753.UInt10 := 16#0#;
      --  Source BUS select This bit is protected and can be written only if EN
      --  is 0.
      SBUS           : Boolean := False;
      --  Destination BUS slect This bit is protected and can be written only
      --  if EN is 0.
      DBUS           : Boolean := False;
      --  unspecified
      Reserved_18_31 : STM32H753.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C4TBR_Register use record
      TSEL           at 0 range 0 .. 5;
      Reserved_6_15  at 0 range 6 .. 15;
      SBUS           at 0 range 16 .. 16;
      DBUS           at 0 range 17 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   --  MDMA channel x interrupt/status register
   type MDMA_C5ISR_Register is record
      --  Read-only. Channel x transfer error interrupt flag This bit is set by
      --  hardware. It is cleared by software writing 1 to the corresponding
      --  bit in the DMA_IFCRy register.
      TEIF5          : Boolean;
      --  Read-only. Channel x Channel Transfer Complete interrupt flag This
      --  bit is set by hardware. It is cleared by software writing 1 to the
      --  corresponding bit in the DMA_IFCRy register. CTC is set when the last
      --  block was transferred and the channel has been automatically
      --  disabled. CTC is also set when the channel is suspended, as a result
      --  of writing EN bit to 0.
      CTCIF5         : Boolean;
      --  Read-only. Channel x block repeat transfer complete interrupt flag
      --  This bit is set by hardware. It is cleared by software writing 1 to
      --  the corresponding bit in the DMA_IFCRy register.
      BRTIF5         : Boolean;
      --  Read-only. Channel x block transfer complete interrupt flag This bit
      --  is set by hardware. It is cleared by software writing 1 to the
      --  corresponding bit in the DMA_IFCRy register.
      BTIF5          : Boolean;
      --  Read-only. channel x buffer transfer complete
      TCIF5          : Boolean;
      --  unspecified
      Reserved_5_15  : STM32H753.UInt11;
      --  Read-only. channel x request active flag
      CRQA5          : Boolean;
      --  unspecified
      Reserved_17_31 : STM32H753.UInt15;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C5ISR_Register use record
      TEIF5          at 0 range 0 .. 0;
      CTCIF5         at 0 range 1 .. 1;
      BRTIF5         at 0 range 2 .. 2;
      BTIF5          at 0 range 3 .. 3;
      TCIF5          at 0 range 4 .. 4;
      Reserved_5_15  at 0 range 5 .. 15;
      CRQA5          at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   --  MDMA channel x interrupt flag clear register
   type MDMA_C5IFCR_Register is record
      --  Write-only. Channel x clear transfer error interrupt flag Writing a 1
      --  into this bit clears TEIFx in the MDMA_ISRy register
      CTEIF5        : Boolean := False;
      --  Write-only. Clear Channel transfer complete interrupt flag for
      --  channel x Writing a 1 into this bit clears CTCIFx in the MDMA_ISRy
      --  register
      CCTCIF5       : Boolean := False;
      --  Write-only. Channel x clear block repeat transfer complete interrupt
      --  flag Writing a 1 into this bit clears BRTIFx in the MDMA_ISRy
      --  register
      CBRTIF5       : Boolean := False;
      --  Write-only. Channel x Clear block transfer complete interrupt flag
      --  Writing a 1 into this bit clears BTIFx in the MDMA_ISRy register
      CBTIF5        : Boolean := False;
      --  Write-only. CLear buffer Transfer Complete Interrupt Flag for channel
      --  x Writing a 1 into this bit clears TCIFx in the MDMA_ISRy register
      CLTCIF5       : Boolean := False;
      --  unspecified
      Reserved_5_31 : STM32H753.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C5IFCR_Register use record
      CTEIF5        at 0 range 0 .. 0;
      CCTCIF5       at 0 range 1 .. 1;
      CBRTIF5       at 0 range 2 .. 2;
      CBTIF5        at 0 range 3 .. 3;
      CLTCIF5       at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   subtype MDMA_C5ESR_TEA_Field is STM32H753.UInt7;

   --  MDMA Channel x error status register
   type MDMA_C5ESR_Register is record
      --  Read-only. Transfer Error Address These bits are set and cleared by
      --  HW, in case of an MDMA data transfer error. It is used in conjunction
      --  with TED. This field indicates the 7 LSBits of the address which
      --  generated a transfer/access error. It may be used by SW to retrieve
      --  the failing address, by adding this value (truncated to the buffer
      --  transfer length size) to the current SAR/DAR value. Note: The SAR/DAR
      --  current value doesnt reflect this last address due to the FIFO
      --  management system. The SAR/DAR are only updated at the end of a
      --  (buffer) transfer (of TLEN+1 bytes). Note: It is not set in case of a
      --  link data error.
      TEA            : MDMA_C5ESR_TEA_Field;
      --  Read-only. Transfer Error Direction These bit is set and cleared by
      --  HW, in case of an MDMA data transfer error.
      TED            : Boolean;
      --  Read-only. Transfer Error Link Data These bit is set by HW, in case
      --  of a transfer error while reading the block link data structure. It
      --  is cleared by software writing 1 to the CTEIFx bit in the DMA_IFCRy
      --  register.
      TELD           : Boolean;
      --  Read-only. Transfer Error Mask Data These bit is set by HW, in case
      --  of a transfer error while writing the Mask Data. It is cleared by
      --  software writing 1 to the CTEIFx bit in the DMA_IFCRy register.
      TEMD           : Boolean;
      --  Read-only. Address/Size Error These bit is set by HW, when the
      --  programmed address is not aligned with the data size. TED will
      --  indicate whether the problem is on the source or destination. It is
      --  cleared by software writing 1 to the CTEIFx bit in the DMA_IFCRy
      --  register.
      ASE            : Boolean;
      --  Read-only. Block Size Error These bit is set by HW, when the block
      --  size is not an integer multiple of the data size either for source or
      --  destination. TED will indicate whether the problem is on the source
      --  or destination. It is cleared by software writing 1 to the CTEIFx bit
      --  in the DMA_IFCRy register.
      BSE            : Boolean;
      --  unspecified
      Reserved_12_31 : STM32H753.UInt20;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C5ESR_Register use record
      TEA            at 0 range 0 .. 6;
      TED            at 0 range 7 .. 7;
      TELD           at 0 range 8 .. 8;
      TEMD           at 0 range 9 .. 9;
      ASE            at 0 range 10 .. 10;
      BSE            at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype MDMA_C5CR_PL_Field is STM32H753.UInt2;

   --  This register is used to control the concerned channel.
   type MDMA_C5CR_Register is record
      --  channel enable
      EN             : Boolean := False;
      --  Transfer error interrupt enable This bit is set and cleared by
      --  software.
      TEIE           : Boolean := False;
      --  Channel Transfer Complete interrupt enable This bit is set and
      --  cleared by software.
      CTCIE          : Boolean := False;
      --  Block Repeat transfer interrupt enable This bit is set and cleared by
      --  software.
      BRTIE          : Boolean := False;
      --  Block Transfer interrupt enable This bit is set and cleared by
      --  software.
      BTIE           : Boolean := False;
      --  buffer Transfer Complete interrupt enable This bit is set and cleared
      --  by software.
      TCIE           : Boolean := False;
      --  Priority level These bits are set and cleared by software. These bits
      --  are protected and can be written only if EN is 0.
      PL             : MDMA_C5CR_PL_Field := 16#0#;
      --  unspecified
      Reserved_8_11  : STM32H753.UInt4 := 16#0#;
      --  byte Endianness exchange
      BEX            : Boolean := False;
      --  Half word Endianes exchange
      HEX            : Boolean := False;
      --  Word Endianness exchange
      WEX            : Boolean := False;
      --  unspecified
      Reserved_15_15 : STM32H753.Bit := 16#0#;
      --  Write-only. SW ReQuest Writing a 1 into this bit sets the CRQAx in
      --  MDMA_ISRy register, activating the request on Channel x Note: Either
      --  the whole CxCR register or the 8-bit/16-bit register @ Address
      --  offset: 0x4E + 0x40 chn may be used for SWRQ activation. In case of a
      --  SW request, acknowledge is not generated (neither HW signal, nor
      --  CxMAR write access).
      SWRQ           : Boolean := False;
      --  unspecified
      Reserved_17_31 : STM32H753.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C5CR_Register use record
      EN             at 0 range 0 .. 0;
      TEIE           at 0 range 1 .. 1;
      CTCIE          at 0 range 2 .. 2;
      BRTIE          at 0 range 3 .. 3;
      BTIE           at 0 range 4 .. 4;
      TCIE           at 0 range 5 .. 5;
      PL             at 0 range 6 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      BEX            at 0 range 12 .. 12;
      HEX            at 0 range 13 .. 13;
      WEX            at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      SWRQ           at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype MDMA_C5TCR_SINC_Field is STM32H753.UInt2;
   subtype MDMA_C5TCR_DINC_Field is STM32H753.UInt2;
   subtype MDMA_C5TCR_SSIZE_Field is STM32H753.UInt2;
   subtype MDMA_C5TCR_DSIZE_Field is STM32H753.UInt2;
   subtype MDMA_C5TCR_SINCOS_Field is STM32H753.UInt2;
   subtype MDMA_C5TCR_DINCOS_Field is STM32H753.UInt2;
   subtype MDMA_C5TCR_SBURST_Field is STM32H753.UInt3;
   subtype MDMA_C5TCR_DBURST_Field is STM32H753.UInt3;
   subtype MDMA_C5TCR_TLEN_Field is STM32H753.UInt7;
   subtype MDMA_C5TCR_PAM_Field is STM32H753.UInt2;
   subtype MDMA_C5TCR_TRGM_Field is STM32H753.UInt2;

   --  This register is used to configure the concerned channel.
   type MDMA_C5TCR_Register is record
      --  Source increment mode These bits are set and cleared by software.
      --  These bits are protected and can be written only if EN is 0 Note:
      --  When source is AHB (SBUS=1), SINC = 00 is forbidden. In Linked List
      --  Mode, at the end of a block (single or last block in repeated block
      --  transfer mode), this register will be loaded from memory (from
      --  address given by current LAR[31:0] + 0x00).
      SINC   : MDMA_C5TCR_SINC_Field := 16#0#;
      --  Destination increment mode These bits are set and cleared by
      --  software. These bits are protected and can be written only if EN is 0
      --  Note: When destination is AHB (DBUS=1), DINC = 00 is forbidden.
      DINC   : MDMA_C5TCR_DINC_Field := 16#0#;
      --  Source data size These bits are set and cleared by software. These
      --  bits are protected and can be written only if EN is 0 Note: If a
      --  value of 11 is programmed for the TCM access/AHB port, a transfer
      --  error will occur (TEIF bit set) If SINCOS &lt; SSIZE and SINC &#8800;
      --  00, the result will be unpredictable. Note: SSIZE = 11 (double-word)
      --  is forbidden when source is TCM/AHB bus (SBUS=1).
      SSIZE  : MDMA_C5TCR_SSIZE_Field := 16#0#;
      --  Destination data size These bits are set and cleared by software.
      --  These bits are protected and can be written only if EN is 0. Note: If
      --  a value of 11 is programmed for the TCM access/AHB port, a transfer
      --  error will occur (TEIF bit set) If DINCOS &lt; DSIZE and DINC &#8800;
      --  00, the result will be unpredictable. Note: DSIZE = 11 (double-word)
      --  is forbidden when destination is TCM/AHB bus (DBUS=1).
      DSIZE  : MDMA_C5TCR_DSIZE_Field := 16#0#;
      --  source increment offset size
      SINCOS : MDMA_C5TCR_SINCOS_Field := 16#0#;
      --  Destination increment offset
      DINCOS : MDMA_C5TCR_DINCOS_Field := 16#0#;
      --  source burst transfer configuration
      SBURST : MDMA_C5TCR_SBURST_Field := 16#0#;
      --  Destination burst transfer configuration
      DBURST : MDMA_C5TCR_DBURST_Field := 16#0#;
      --  buffer transfer lengh
      TLEN   : MDMA_C5TCR_TLEN_Field := 16#0#;
      --  PacK Enable These bit is set and cleared by software. If the Source
      --  Size is smaller than the destination, it will be padded according to
      --  the PAM value. If the Source data size is larger than the destination
      --  one, it will be truncated. The alignment will be done according to
      --  the PAM[0] value. This bit is protected and can be written only if EN
      --  is 0
      PKE    : Boolean := False;
      --  Padding/Alignement Mode These bits are set and cleared by software.
      --  Case 1: Source data size smaller than destination data size - 3
      --  options are valid. Case 2: Source data size larger than destination
      --  data size. The remainder part is discarded. When PKE = 1 or
      --  DSIZE=SSIZE, these bits are ignored. These bits are protected and can
      --  be written only if EN is 0
      PAM    : MDMA_C5TCR_PAM_Field := 16#0#;
      --  Trigger Mode These bits are set and cleared by software. Note: If
      --  TRGM is 11 for the current block, all the values loaded at the end of
      --  the current block through the linked list mechanism must keep the
      --  same value (TRGM=11) and the same SWRM value, otherwise the result is
      --  undefined. These bits are protected and can be written only if EN is
      --  0.
      TRGM   : MDMA_C5TCR_TRGM_Field := 16#0#;
      --  SW Request Mode This bit is set and cleared by software. If a HW or
      --  SW request is currently active, the bit change will be delayed until
      --  the current transfer is completed. If the CxMAR contains a valid
      --  address, the CxMDR value will also be written @ CxMAR address. This
      --  bit is protected and can be written only if EN is 0.
      SWRM   : Boolean := False;
      --  Bufferable Write Mode This bit is set and cleared by software. This
      --  bit is protected and can be written only if EN is 0. Note: All MDMA
      --  destination accesses are non-cacheable.
      BWM    : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C5TCR_Register use record
      SINC   at 0 range 0 .. 1;
      DINC   at 0 range 2 .. 3;
      SSIZE  at 0 range 4 .. 5;
      DSIZE  at 0 range 6 .. 7;
      SINCOS at 0 range 8 .. 9;
      DINCOS at 0 range 10 .. 11;
      SBURST at 0 range 12 .. 14;
      DBURST at 0 range 15 .. 17;
      TLEN   at 0 range 18 .. 24;
      PKE    at 0 range 25 .. 25;
      PAM    at 0 range 26 .. 27;
      TRGM   at 0 range 28 .. 29;
      SWRM   at 0 range 30 .. 30;
      BWM    at 0 range 31 .. 31;
   end record;

   subtype MDMA_C5BNDTR_BNDT_Field is STM32H753.UInt17;
   subtype MDMA_C5BNDTR_BRC_Field is STM32H753.UInt12;

   --  MDMA Channel x block number of data register
   type MDMA_C5BNDTR_Register is record
      --  block number of data to transfer
      BNDT           : MDMA_C5BNDTR_BNDT_Field := 16#0#;
      --  unspecified
      Reserved_17_17 : STM32H753.Bit := 16#0#;
      --  Block Repeat Source address Update Mode These bits are protected and
      --  can be written only if EN is 0.
      BRSUM          : Boolean := False;
      --  Block Repeat Destination address Update Mode These bits are protected
      --  and can be written only if EN is 0.
      BRDUM          : Boolean := False;
      --  Block Repeat Count This field contains the number of repetitions of
      --  the current block (0 to 4095). When the channel is enabled, this
      --  register is read-only, indicating the remaining number of blocks,
      --  excluding the current one. This register decrements after each
      --  complete block transfer. Once the last block transfer has completed,
      --  this register can either stay at zero or be reloaded automatically
      --  from memory (in Linked List mode - i.e. Link Address valid). These
      --  bits are protected and can be written only if EN is 0.
      BRC            : MDMA_C5BNDTR_BRC_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C5BNDTR_Register use record
      BNDT           at 0 range 0 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      BRSUM          at 0 range 18 .. 18;
      BRDUM          at 0 range 19 .. 19;
      BRC            at 0 range 20 .. 31;
   end record;

   subtype MDMA_C5BRUR_SUV_Field is STM32H753.UInt16;
   subtype MDMA_C5BRUR_DUV_Field is STM32H753.UInt16;

   --  MDMA channel x Block Repeat address Update register
   type MDMA_C5BRUR_Register is record
      --  source adresse update value
      SUV : MDMA_C5BRUR_SUV_Field := 16#0#;
      --  destination address update
      DUV : MDMA_C5BRUR_DUV_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C5BRUR_Register use record
      SUV at 0 range 0 .. 15;
      DUV at 0 range 16 .. 31;
   end record;

   subtype MDMA_C5TBR_TSEL_Field is STM32H753.UInt6;

   --  MDMA channel x Trigger and Bus selection Register
   type MDMA_C5TBR_Register is record
      --  Trigger selection
      TSEL           : MDMA_C5TBR_TSEL_Field := 16#0#;
      --  unspecified
      Reserved_6_15  : STM32H753.UInt10 := 16#0#;
      --  Source BUS select This bit is protected and can be written only if EN
      --  is 0.
      SBUS           : Boolean := False;
      --  Destination BUS slect This bit is protected and can be written only
      --  if EN is 0.
      DBUS           : Boolean := False;
      --  unspecified
      Reserved_18_31 : STM32H753.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C5TBR_Register use record
      TSEL           at 0 range 0 .. 5;
      Reserved_6_15  at 0 range 6 .. 15;
      SBUS           at 0 range 16 .. 16;
      DBUS           at 0 range 17 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   --  MDMA channel x interrupt/status register
   type MDMA_C6ISR_Register is record
      --  Read-only. Channel x transfer error interrupt flag This bit is set by
      --  hardware. It is cleared by software writing 1 to the corresponding
      --  bit in the DMA_IFCRy register.
      TEIF6          : Boolean;
      --  Read-only. Channel x Channel Transfer Complete interrupt flag This
      --  bit is set by hardware. It is cleared by software writing 1 to the
      --  corresponding bit in the DMA_IFCRy register. CTC is set when the last
      --  block was transferred and the channel has been automatically
      --  disabled. CTC is also set when the channel is suspended, as a result
      --  of writing EN bit to 0.
      CTCIF6         : Boolean;
      --  Read-only. Channel x block repeat transfer complete interrupt flag
      --  This bit is set by hardware. It is cleared by software writing 1 to
      --  the corresponding bit in the DMA_IFCRy register.
      BRTIF6         : Boolean;
      --  Read-only. Channel x block transfer complete interrupt flag This bit
      --  is set by hardware. It is cleared by software writing 1 to the
      --  corresponding bit in the DMA_IFCRy register.
      BTIF6          : Boolean;
      --  Read-only. channel x buffer transfer complete
      TCIF6          : Boolean;
      --  unspecified
      Reserved_5_15  : STM32H753.UInt11;
      --  Read-only. channel x request active flag
      CRQA6          : Boolean;
      --  unspecified
      Reserved_17_31 : STM32H753.UInt15;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C6ISR_Register use record
      TEIF6          at 0 range 0 .. 0;
      CTCIF6         at 0 range 1 .. 1;
      BRTIF6         at 0 range 2 .. 2;
      BTIF6          at 0 range 3 .. 3;
      TCIF6          at 0 range 4 .. 4;
      Reserved_5_15  at 0 range 5 .. 15;
      CRQA6          at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   --  MDMA channel x interrupt flag clear register
   type MDMA_C6IFCR_Register is record
      --  Write-only. Channel x clear transfer error interrupt flag Writing a 1
      --  into this bit clears TEIFx in the MDMA_ISRy register
      CTEIF6        : Boolean := False;
      --  Write-only. Clear Channel transfer complete interrupt flag for
      --  channel x Writing a 1 into this bit clears CTCIFx in the MDMA_ISRy
      --  register
      CCTCIF6       : Boolean := False;
      --  Write-only. Channel x clear block repeat transfer complete interrupt
      --  flag Writing a 1 into this bit clears BRTIFx in the MDMA_ISRy
      --  register
      CBRTIF6       : Boolean := False;
      --  Write-only. Channel x Clear block transfer complete interrupt flag
      --  Writing a 1 into this bit clears BTIFx in the MDMA_ISRy register
      CBTIF6        : Boolean := False;
      --  Write-only. CLear buffer Transfer Complete Interrupt Flag for channel
      --  x Writing a 1 into this bit clears TCIFx in the MDMA_ISRy register
      CLTCIF6       : Boolean := False;
      --  unspecified
      Reserved_5_31 : STM32H753.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C6IFCR_Register use record
      CTEIF6        at 0 range 0 .. 0;
      CCTCIF6       at 0 range 1 .. 1;
      CBRTIF6       at 0 range 2 .. 2;
      CBTIF6        at 0 range 3 .. 3;
      CLTCIF6       at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   subtype MDMA_C6ESR_TEA_Field is STM32H753.UInt7;

   --  MDMA Channel x error status register
   type MDMA_C6ESR_Register is record
      --  Read-only. Transfer Error Address These bits are set and cleared by
      --  HW, in case of an MDMA data transfer error. It is used in conjunction
      --  with TED. This field indicates the 7 LSBits of the address which
      --  generated a transfer/access error. It may be used by SW to retrieve
      --  the failing address, by adding this value (truncated to the buffer
      --  transfer length size) to the current SAR/DAR value. Note: The SAR/DAR
      --  current value doesnt reflect this last address due to the FIFO
      --  management system. The SAR/DAR are only updated at the end of a
      --  (buffer) transfer (of TLEN+1 bytes). Note: It is not set in case of a
      --  link data error.
      TEA            : MDMA_C6ESR_TEA_Field;
      --  Read-only. Transfer Error Direction These bit is set and cleared by
      --  HW, in case of an MDMA data transfer error.
      TED            : Boolean;
      --  Read-only. Transfer Error Link Data These bit is set by HW, in case
      --  of a transfer error while reading the block link data structure. It
      --  is cleared by software writing 1 to the CTEIFx bit in the DMA_IFCRy
      --  register.
      TELD           : Boolean;
      --  Read-only. Transfer Error Mask Data These bit is set by HW, in case
      --  of a transfer error while writing the Mask Data. It is cleared by
      --  software writing 1 to the CTEIFx bit in the DMA_IFCRy register.
      TEMD           : Boolean;
      --  Read-only. Address/Size Error These bit is set by HW, when the
      --  programmed address is not aligned with the data size. TED will
      --  indicate whether the problem is on the source or destination. It is
      --  cleared by software writing 1 to the CTEIFx bit in the DMA_IFCRy
      --  register.
      ASE            : Boolean;
      --  Read-only. Block Size Error These bit is set by HW, when the block
      --  size is not an integer multiple of the data size either for source or
      --  destination. TED will indicate whether the problem is on the source
      --  or destination. It is cleared by software writing 1 to the CTEIFx bit
      --  in the DMA_IFCRy register.
      BSE            : Boolean;
      --  unspecified
      Reserved_12_31 : STM32H753.UInt20;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C6ESR_Register use record
      TEA            at 0 range 0 .. 6;
      TED            at 0 range 7 .. 7;
      TELD           at 0 range 8 .. 8;
      TEMD           at 0 range 9 .. 9;
      ASE            at 0 range 10 .. 10;
      BSE            at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype MDMA_C6CR_PL_Field is STM32H753.UInt2;

   --  This register is used to control the concerned channel.
   type MDMA_C6CR_Register is record
      --  channel enable
      EN             : Boolean := False;
      --  Transfer error interrupt enable This bit is set and cleared by
      --  software.
      TEIE           : Boolean := False;
      --  Channel Transfer Complete interrupt enable This bit is set and
      --  cleared by software.
      CTCIE          : Boolean := False;
      --  Block Repeat transfer interrupt enable This bit is set and cleared by
      --  software.
      BRTIE          : Boolean := False;
      --  Block Transfer interrupt enable This bit is set and cleared by
      --  software.
      BTIE           : Boolean := False;
      --  buffer Transfer Complete interrupt enable This bit is set and cleared
      --  by software.
      TCIE           : Boolean := False;
      --  Priority level These bits are set and cleared by software. These bits
      --  are protected and can be written only if EN is 0.
      PL             : MDMA_C6CR_PL_Field := 16#0#;
      --  unspecified
      Reserved_8_11  : STM32H753.UInt4 := 16#0#;
      --  byte Endianness exchange
      BEX            : Boolean := False;
      --  Half word Endianes exchange
      HEX            : Boolean := False;
      --  Word Endianness exchange
      WEX            : Boolean := False;
      --  unspecified
      Reserved_15_15 : STM32H753.Bit := 16#0#;
      --  Write-only. SW ReQuest Writing a 1 into this bit sets the CRQAx in
      --  MDMA_ISRy register, activating the request on Channel x Note: Either
      --  the whole CxCR register or the 8-bit/16-bit register @ Address
      --  offset: 0x4E + 0x40 chn may be used for SWRQ activation. In case of a
      --  SW request, acknowledge is not generated (neither HW signal, nor
      --  CxMAR write access).
      SWRQ           : Boolean := False;
      --  unspecified
      Reserved_17_31 : STM32H753.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C6CR_Register use record
      EN             at 0 range 0 .. 0;
      TEIE           at 0 range 1 .. 1;
      CTCIE          at 0 range 2 .. 2;
      BRTIE          at 0 range 3 .. 3;
      BTIE           at 0 range 4 .. 4;
      TCIE           at 0 range 5 .. 5;
      PL             at 0 range 6 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      BEX            at 0 range 12 .. 12;
      HEX            at 0 range 13 .. 13;
      WEX            at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      SWRQ           at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype MDMA_C6TCR_SINC_Field is STM32H753.UInt2;
   subtype MDMA_C6TCR_DINC_Field is STM32H753.UInt2;
   subtype MDMA_C6TCR_SSIZE_Field is STM32H753.UInt2;
   subtype MDMA_C6TCR_DSIZE_Field is STM32H753.UInt2;
   subtype MDMA_C6TCR_SINCOS_Field is STM32H753.UInt2;
   subtype MDMA_C6TCR_DINCOS_Field is STM32H753.UInt2;
   subtype MDMA_C6TCR_SBURST_Field is STM32H753.UInt3;
   subtype MDMA_C6TCR_DBURST_Field is STM32H753.UInt3;
   subtype MDMA_C6TCR_TLEN_Field is STM32H753.UInt7;
   subtype MDMA_C6TCR_PAM_Field is STM32H753.UInt2;
   subtype MDMA_C6TCR_TRGM_Field is STM32H753.UInt2;

   --  This register is used to configure the concerned channel.
   type MDMA_C6TCR_Register is record
      --  Source increment mode These bits are set and cleared by software.
      --  These bits are protected and can be written only if EN is 0 Note:
      --  When source is AHB (SBUS=1), SINC = 00 is forbidden. In Linked List
      --  Mode, at the end of a block (single or last block in repeated block
      --  transfer mode), this register will be loaded from memory (from
      --  address given by current LAR[31:0] + 0x00).
      SINC   : MDMA_C6TCR_SINC_Field := 16#0#;
      --  Destination increment mode These bits are set and cleared by
      --  software. These bits are protected and can be written only if EN is 0
      --  Note: When destination is AHB (DBUS=1), DINC = 00 is forbidden.
      DINC   : MDMA_C6TCR_DINC_Field := 16#0#;
      --  Source data size These bits are set and cleared by software. These
      --  bits are protected and can be written only if EN is 0 Note: If a
      --  value of 11 is programmed for the TCM access/AHB port, a transfer
      --  error will occur (TEIF bit set) If SINCOS &lt; SSIZE and SINC &#8800;
      --  00, the result will be unpredictable. Note: SSIZE = 11 (double-word)
      --  is forbidden when source is TCM/AHB bus (SBUS=1).
      SSIZE  : MDMA_C6TCR_SSIZE_Field := 16#0#;
      --  Destination data size These bits are set and cleared by software.
      --  These bits are protected and can be written only if EN is 0. Note: If
      --  a value of 11 is programmed for the TCM access/AHB port, a transfer
      --  error will occur (TEIF bit set) If DINCOS &lt; DSIZE and DINC &#8800;
      --  00, the result will be unpredictable. Note: DSIZE = 11 (double-word)
      --  is forbidden when destination is TCM/AHB bus (DBUS=1).
      DSIZE  : MDMA_C6TCR_DSIZE_Field := 16#0#;
      --  source increment offset size
      SINCOS : MDMA_C6TCR_SINCOS_Field := 16#0#;
      --  Destination increment offset
      DINCOS : MDMA_C6TCR_DINCOS_Field := 16#0#;
      --  source burst transfer configuration
      SBURST : MDMA_C6TCR_SBURST_Field := 16#0#;
      --  Destination burst transfer configuration
      DBURST : MDMA_C6TCR_DBURST_Field := 16#0#;
      --  buffer transfer lengh
      TLEN   : MDMA_C6TCR_TLEN_Field := 16#0#;
      --  PacK Enable These bit is set and cleared by software. If the Source
      --  Size is smaller than the destination, it will be padded according to
      --  the PAM value. If the Source data size is larger than the destination
      --  one, it will be truncated. The alignment will be done according to
      --  the PAM[0] value. This bit is protected and can be written only if EN
      --  is 0
      PKE    : Boolean := False;
      --  Padding/Alignement Mode These bits are set and cleared by software.
      --  Case 1: Source data size smaller than destination data size - 3
      --  options are valid. Case 2: Source data size larger than destination
      --  data size. The remainder part is discarded. When PKE = 1 or
      --  DSIZE=SSIZE, these bits are ignored. These bits are protected and can
      --  be written only if EN is 0
      PAM    : MDMA_C6TCR_PAM_Field := 16#0#;
      --  Trigger Mode These bits are set and cleared by software. Note: If
      --  TRGM is 11 for the current block, all the values loaded at the end of
      --  the current block through the linked list mechanism must keep the
      --  same value (TRGM=11) and the same SWRM value, otherwise the result is
      --  undefined. These bits are protected and can be written only if EN is
      --  0.
      TRGM   : MDMA_C6TCR_TRGM_Field := 16#0#;
      --  SW Request Mode This bit is set and cleared by software. If a HW or
      --  SW request is currently active, the bit change will be delayed until
      --  the current transfer is completed. If the CxMAR contains a valid
      --  address, the CxMDR value will also be written @ CxMAR address. This
      --  bit is protected and can be written only if EN is 0.
      SWRM   : Boolean := False;
      --  Bufferable Write Mode This bit is set and cleared by software. This
      --  bit is protected and can be written only if EN is 0. Note: All MDMA
      --  destination accesses are non-cacheable.
      BWM    : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C6TCR_Register use record
      SINC   at 0 range 0 .. 1;
      DINC   at 0 range 2 .. 3;
      SSIZE  at 0 range 4 .. 5;
      DSIZE  at 0 range 6 .. 7;
      SINCOS at 0 range 8 .. 9;
      DINCOS at 0 range 10 .. 11;
      SBURST at 0 range 12 .. 14;
      DBURST at 0 range 15 .. 17;
      TLEN   at 0 range 18 .. 24;
      PKE    at 0 range 25 .. 25;
      PAM    at 0 range 26 .. 27;
      TRGM   at 0 range 28 .. 29;
      SWRM   at 0 range 30 .. 30;
      BWM    at 0 range 31 .. 31;
   end record;

   subtype MDMA_C6BNDTR_BNDT_Field is STM32H753.UInt17;
   subtype MDMA_C6BNDTR_BRC_Field is STM32H753.UInt12;

   --  MDMA Channel x block number of data register
   type MDMA_C6BNDTR_Register is record
      --  block number of data to transfer
      BNDT           : MDMA_C6BNDTR_BNDT_Field := 16#0#;
      --  unspecified
      Reserved_17_17 : STM32H753.Bit := 16#0#;
      --  Block Repeat Source address Update Mode These bits are protected and
      --  can be written only if EN is 0.
      BRSUM          : Boolean := False;
      --  Block Repeat Destination address Update Mode These bits are protected
      --  and can be written only if EN is 0.
      BRDUM          : Boolean := False;
      --  Block Repeat Count This field contains the number of repetitions of
      --  the current block (0 to 4095). When the channel is enabled, this
      --  register is read-only, indicating the remaining number of blocks,
      --  excluding the current one. This register decrements after each
      --  complete block transfer. Once the last block transfer has completed,
      --  this register can either stay at zero or be reloaded automatically
      --  from memory (in Linked List mode - i.e. Link Address valid). These
      --  bits are protected and can be written only if EN is 0
      BRC            : MDMA_C6BNDTR_BRC_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C6BNDTR_Register use record
      BNDT           at 0 range 0 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      BRSUM          at 0 range 18 .. 18;
      BRDUM          at 0 range 19 .. 19;
      BRC            at 0 range 20 .. 31;
   end record;

   subtype MDMA_C6BRUR_SUV_Field is STM32H753.UInt16;
   subtype MDMA_C6BRUR_DUV_Field is STM32H753.UInt16;

   --  MDMA channel x Block Repeat address Update register
   type MDMA_C6BRUR_Register is record
      --  source adresse update value
      SUV : MDMA_C6BRUR_SUV_Field := 16#0#;
      --  destination address update
      DUV : MDMA_C6BRUR_DUV_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C6BRUR_Register use record
      SUV at 0 range 0 .. 15;
      DUV at 0 range 16 .. 31;
   end record;

   subtype MDMA_C6TBR_TSEL_Field is STM32H753.UInt6;

   --  MDMA channel x Trigger and Bus selection Register
   type MDMA_C6TBR_Register is record
      --  Trigger selection
      TSEL           : MDMA_C6TBR_TSEL_Field := 16#0#;
      --  unspecified
      Reserved_6_15  : STM32H753.UInt10 := 16#0#;
      --  Source BUS select This bit is protected and can be written only if EN
      --  is 0.
      SBUS           : Boolean := False;
      --  Destination BUS slect This bit is protected and can be written only
      --  if EN is 0.
      DBUS           : Boolean := False;
      --  unspecified
      Reserved_18_31 : STM32H753.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C6TBR_Register use record
      TSEL           at 0 range 0 .. 5;
      Reserved_6_15  at 0 range 6 .. 15;
      SBUS           at 0 range 16 .. 16;
      DBUS           at 0 range 17 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   --  MDMA channel x interrupt/status register
   type MDMA_C7ISR_Register is record
      --  Read-only. Channel x transfer error interrupt flag This bit is set by
      --  hardware. It is cleared by software writing 1 to the corresponding
      --  bit in the DMA_IFCRy register.
      TEIF7          : Boolean;
      --  Read-only. Channel x Channel Transfer Complete interrupt flag This
      --  bit is set by hardware. It is cleared by software writing 1 to the
      --  corresponding bit in the DMA_IFCRy register. CTC is set when the last
      --  block was transferred and the channel has been automatically
      --  disabled. CTC is also set when the channel is suspended, as a result
      --  of writing EN bit to 0.
      CTCIF7         : Boolean;
      --  Read-only. Channel x block repeat transfer complete interrupt flag
      --  This bit is set by hardware. It is cleared by software writing 1 to
      --  the corresponding bit in the DMA_IFCRy register.
      BRTIF7         : Boolean;
      --  Read-only. Channel x block transfer complete interrupt flag This bit
      --  is set by hardware. It is cleared by software writing 1 to the
      --  corresponding bit in the DMA_IFCRy register.
      BTIF7          : Boolean;
      --  Read-only. channel x buffer transfer complete
      TCIF7          : Boolean;
      --  unspecified
      Reserved_5_15  : STM32H753.UInt11;
      --  Read-only. channel x request active flag
      CRQA7          : Boolean;
      --  unspecified
      Reserved_17_31 : STM32H753.UInt15;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C7ISR_Register use record
      TEIF7          at 0 range 0 .. 0;
      CTCIF7         at 0 range 1 .. 1;
      BRTIF7         at 0 range 2 .. 2;
      BTIF7          at 0 range 3 .. 3;
      TCIF7          at 0 range 4 .. 4;
      Reserved_5_15  at 0 range 5 .. 15;
      CRQA7          at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   --  MDMA channel x interrupt flag clear register
   type MDMA_C7IFCR_Register is record
      --  Write-only. Channel x clear transfer error interrupt flag Writing a 1
      --  into this bit clears TEIFx in the MDMA_ISRy register
      CTEIF7        : Boolean := False;
      --  Write-only. Clear Channel transfer complete interrupt flag for
      --  channel x Writing a 1 into this bit clears CTCIFx in the MDMA_ISRy
      --  register
      CCTCIF7       : Boolean := False;
      --  Write-only. Channel x clear block repeat transfer complete interrupt
      --  flag Writing a 1 into this bit clears BRTIFx in the MDMA_ISRy
      --  register
      CBRTIF7       : Boolean := False;
      --  Write-only. Channel x Clear block transfer complete interrupt flag
      --  Writing a 1 into this bit clears BTIFx in the MDMA_ISRy register
      CBTIF7        : Boolean := False;
      --  Write-only. CLear buffer Transfer Complete Interrupt Flag for channel
      --  x Writing a 1 into this bit clears TCIFx in the MDMA_ISRy register
      CLTCIF7       : Boolean := False;
      --  unspecified
      Reserved_5_31 : STM32H753.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C7IFCR_Register use record
      CTEIF7        at 0 range 0 .. 0;
      CCTCIF7       at 0 range 1 .. 1;
      CBRTIF7       at 0 range 2 .. 2;
      CBTIF7        at 0 range 3 .. 3;
      CLTCIF7       at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   subtype MDMA_C7ESR_TEA_Field is STM32H753.UInt7;

   --  MDMA Channel x error status register
   type MDMA_C7ESR_Register is record
      --  Read-only. Transfer Error Address These bits are set and cleared by
      --  HW, in case of an MDMA data transfer error. It is used in conjunction
      --  with TED. This field indicates the 7 LSBits of the address which
      --  generated a transfer/access error. It may be used by SW to retrieve
      --  the failing address, by adding this value (truncated to the buffer
      --  transfer length size) to the current SAR/DAR value. Note: The SAR/DAR
      --  current value doesnt reflect this last address due to the FIFO
      --  management system. The SAR/DAR are only updated at the end of a
      --  (buffer) transfer (of TLEN+1 bytes). Note: It is not set in case of a
      --  link data error.
      TEA            : MDMA_C7ESR_TEA_Field;
      --  Read-only. Transfer Error Direction These bit is set and cleared by
      --  HW, in case of an MDMA data transfer error.
      TED            : Boolean;
      --  Read-only. Transfer Error Link Data These bit is set by HW, in case
      --  of a transfer error while reading the block link data structure. It
      --  is cleared by software writing 1 to the CTEIFx bit in the DMA_IFCRy
      --  register.
      TELD           : Boolean;
      --  Read-only. Transfer Error Mask Data These bit is set by HW, in case
      --  of a transfer error while writing the Mask Data. It is cleared by
      --  software writing 1 to the CTEIFx bit in the DMA_IFCRy register.
      TEMD           : Boolean;
      --  Read-only. Address/Size Error These bit is set by HW, when the
      --  programmed address is not aligned with the data size. TED will
      --  indicate whether the problem is on the source or destination. It is
      --  cleared by software writing 1 to the CTEIFx bit in the DMA_IFCRy
      --  register.
      ASE            : Boolean;
      --  Read-only. Block Size Error These bit is set by HW, when the block
      --  size is not an integer multiple of the data size either for source or
      --  destination. TED will indicate whether the problem is on the source
      --  or destination. It is cleared by software writing 1 to the CTEIFx bit
      --  in the DMA_IFCRy register.
      BSE            : Boolean;
      --  unspecified
      Reserved_12_31 : STM32H753.UInt20;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C7ESR_Register use record
      TEA            at 0 range 0 .. 6;
      TED            at 0 range 7 .. 7;
      TELD           at 0 range 8 .. 8;
      TEMD           at 0 range 9 .. 9;
      ASE            at 0 range 10 .. 10;
      BSE            at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype MDMA_C7CR_PL_Field is STM32H753.UInt2;

   --  This register is used to control the concerned channel.
   type MDMA_C7CR_Register is record
      --  channel enable
      EN             : Boolean := False;
      --  Transfer error interrupt enable This bit is set and cleared by
      --  software.
      TEIE           : Boolean := False;
      --  Channel Transfer Complete interrupt enable This bit is set and
      --  cleared by software.
      CTCIE          : Boolean := False;
      --  Block Repeat transfer interrupt enable This bit is set and cleared by
      --  software.
      BRTIE          : Boolean := False;
      --  Block Transfer interrupt enable This bit is set and cleared by
      --  software.
      BTIE           : Boolean := False;
      --  buffer Transfer Complete interrupt enable This bit is set and cleared
      --  by software.
      TCIE           : Boolean := False;
      --  Priority level These bits are set and cleared by software. These bits
      --  are protected and can be written only if EN is 0.
      PL             : MDMA_C7CR_PL_Field := 16#0#;
      --  unspecified
      Reserved_8_11  : STM32H753.UInt4 := 16#0#;
      --  byte Endianness exchange
      BEX            : Boolean := False;
      --  Half word Endianes exchange
      HEX            : Boolean := False;
      --  Word Endianness exchange
      WEX            : Boolean := False;
      --  unspecified
      Reserved_15_15 : STM32H753.Bit := 16#0#;
      --  Write-only. SW ReQuest Writing a 1 into this bit sets the CRQAx in
      --  MDMA_ISRy register, activating the request on Channel x Note: Either
      --  the whole CxCR register or the 8-bit/16-bit register @ Address
      --  offset: 0x4E + 0x40 chn may be used for SWRQ activation. In case of a
      --  SW request, acknowledge is not generated (neither HW signal, nor
      --  CxMAR write access).
      SWRQ           : Boolean := False;
      --  unspecified
      Reserved_17_31 : STM32H753.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C7CR_Register use record
      EN             at 0 range 0 .. 0;
      TEIE           at 0 range 1 .. 1;
      CTCIE          at 0 range 2 .. 2;
      BRTIE          at 0 range 3 .. 3;
      BTIE           at 0 range 4 .. 4;
      TCIE           at 0 range 5 .. 5;
      PL             at 0 range 6 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      BEX            at 0 range 12 .. 12;
      HEX            at 0 range 13 .. 13;
      WEX            at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      SWRQ           at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype MDMA_C7TCR_SINC_Field is STM32H753.UInt2;
   subtype MDMA_C7TCR_DINC_Field is STM32H753.UInt2;
   subtype MDMA_C7TCR_SSIZE_Field is STM32H753.UInt2;
   subtype MDMA_C7TCR_DSIZE_Field is STM32H753.UInt2;
   subtype MDMA_C7TCR_SINCOS_Field is STM32H753.UInt2;
   subtype MDMA_C7TCR_DINCOS_Field is STM32H753.UInt2;
   subtype MDMA_C7TCR_SBURST_Field is STM32H753.UInt3;
   subtype MDMA_C7TCR_DBURST_Field is STM32H753.UInt3;
   subtype MDMA_C7TCR_TLEN_Field is STM32H753.UInt7;
   subtype MDMA_C7TCR_PAM_Field is STM32H753.UInt2;
   subtype MDMA_C7TCR_TRGM_Field is STM32H753.UInt2;

   --  This register is used to configure the concerned channel.
   type MDMA_C7TCR_Register is record
      --  Source increment mode These bits are set and cleared by software.
      --  These bits are protected and can be written only if EN is 0 Note:
      --  When source is AHB (SBUS=1), SINC = 00 is forbidden. In Linked List
      --  Mode, at the end of a block (single or last block in repeated block
      --  transfer mode), this register will be loaded from memory (from
      --  address given by current LAR[31:0] + 0x00).
      SINC   : MDMA_C7TCR_SINC_Field := 16#0#;
      --  Destination increment mode These bits are set and cleared by
      --  software. These bits are protected and can be written only if EN is 0
      --  Note: When destination is AHB (DBUS=1), DINC = 00 is forbidden.
      DINC   : MDMA_C7TCR_DINC_Field := 16#0#;
      --  Source data size These bits are set and cleared by software. These
      --  bits are protected and can be written only if EN is 0 Note: If a
      --  value of 11 is programmed for the TCM access/AHB port, a transfer
      --  error will occur (TEIF bit set) If SINCOS &lt; SSIZE and SINC &#8800;
      --  00, the result will be unpredictable. Note: SSIZE = 11 (double-word)
      --  is forbidden when source is TCM/AHB bus (SBUS=1).
      SSIZE  : MDMA_C7TCR_SSIZE_Field := 16#0#;
      --  Destination data size These bits are set and cleared by software.
      --  These bits are protected and can be written only if EN is 0. Note: If
      --  a value of 11 is programmed for the TCM access/AHB port, a transfer
      --  error will occur (TEIF bit set) If DINCOS &lt; DSIZE and DINC &#8800;
      --  00, the result will be unpredictable. Note: DSIZE = 11 (double-word)
      --  is forbidden when destination is TCM/AHB bus (DBUS=1).
      DSIZE  : MDMA_C7TCR_DSIZE_Field := 16#0#;
      --  source increment offset size
      SINCOS : MDMA_C7TCR_SINCOS_Field := 16#0#;
      --  Destination increment offset
      DINCOS : MDMA_C7TCR_DINCOS_Field := 16#0#;
      --  source burst transfer configuration
      SBURST : MDMA_C7TCR_SBURST_Field := 16#0#;
      --  Destination burst transfer configuration
      DBURST : MDMA_C7TCR_DBURST_Field := 16#0#;
      --  buffer transfer lengh
      TLEN   : MDMA_C7TCR_TLEN_Field := 16#0#;
      --  PacK Enable These bit is set and cleared by software. If the Source
      --  Size is smaller than the destination, it will be padded according to
      --  the PAM value. If the Source data size is larger than the destination
      --  one, it will be truncated. The alignment will be done according to
      --  the PAM[0] value. This bit is protected and can be written only if EN
      --  is 0
      PKE    : Boolean := False;
      --  Padding/Alignement Mode These bits are set and cleared by software.
      --  Case 1: Source data size smaller than destination data size - 3
      --  options are valid. Case 2: Source data size larger than destination
      --  data size. The remainder part is discarded. When PKE = 1 or
      --  DSIZE=SSIZE, these bits are ignored. These bits are protected and can
      --  be written only if EN is 0
      PAM    : MDMA_C7TCR_PAM_Field := 16#0#;
      --  Trigger Mode These bits are set and cleared by software. Note: If
      --  TRGM is 11 for the current block, all the values loaded at the end of
      --  the current block through the linked list mechanism must keep the
      --  same value (TRGM=11) and the same SWRM value, otherwise the result is
      --  undefined. These bits are protected and can be written only if EN is
      --  0.
      TRGM   : MDMA_C7TCR_TRGM_Field := 16#0#;
      --  SW Request Mode This bit is set and cleared by software. If a HW or
      --  SW request is currently active, the bit change will be delayed until
      --  the current transfer is completed. If the CxMAR contains a valid
      --  address, the CxMDR value will also be written @ CxMAR address. This
      --  bit is protected and can be written only if EN is 0.
      SWRM   : Boolean := False;
      --  Bufferable Write Mode This bit is set and cleared by software. This
      --  bit is protected and can be written only if EN is 0. Note: All MDMA
      --  destination accesses are non-cacheable.
      BWM    : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C7TCR_Register use record
      SINC   at 0 range 0 .. 1;
      DINC   at 0 range 2 .. 3;
      SSIZE  at 0 range 4 .. 5;
      DSIZE  at 0 range 6 .. 7;
      SINCOS at 0 range 8 .. 9;
      DINCOS at 0 range 10 .. 11;
      SBURST at 0 range 12 .. 14;
      DBURST at 0 range 15 .. 17;
      TLEN   at 0 range 18 .. 24;
      PKE    at 0 range 25 .. 25;
      PAM    at 0 range 26 .. 27;
      TRGM   at 0 range 28 .. 29;
      SWRM   at 0 range 30 .. 30;
      BWM    at 0 range 31 .. 31;
   end record;

   subtype MDMA_C7BNDTR_BNDT_Field is STM32H753.UInt17;
   subtype MDMA_C7BNDTR_BRC_Field is STM32H753.UInt12;

   --  MDMA Channel x block number of data register
   type MDMA_C7BNDTR_Register is record
      --  block number of data to transfer
      BNDT           : MDMA_C7BNDTR_BNDT_Field := 16#0#;
      --  unspecified
      Reserved_17_17 : STM32H753.Bit := 16#0#;
      --  Block Repeat Source address Update Mode These bits are protected and
      --  can be written only if EN is 0.
      BRSUM          : Boolean := False;
      --  Block Repeat Destination address Update Mode These bits are protected
      --  and can be written only if EN is 0.
      BRDUM          : Boolean := False;
      --  Block Repeat Count This field contains the number of repetitions of
      --  the current block (0 to 4095). When the channel is enabled, this
      --  register is read-only, indicating the remaining number of blocks,
      --  excluding the current one. This register decrements after each
      --  complete block transfer. Once the last block transfer has completed,
      --  this register can either stay at zero or be reloaded automatically
      --  from memory (in Linked List mode - i.e. Link Address valid). These
      --  bits are protected and can be written only if EN is 0.
      BRC            : MDMA_C7BNDTR_BRC_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C7BNDTR_Register use record
      BNDT           at 0 range 0 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      BRSUM          at 0 range 18 .. 18;
      BRDUM          at 0 range 19 .. 19;
      BRC            at 0 range 20 .. 31;
   end record;

   subtype MDMA_C7BRUR_SUV_Field is STM32H753.UInt16;
   subtype MDMA_C7BRUR_DUV_Field is STM32H753.UInt16;

   --  MDMA channel x Block Repeat address Update register
   type MDMA_C7BRUR_Register is record
      --  source adresse update value
      SUV : MDMA_C7BRUR_SUV_Field := 16#0#;
      --  destination address update
      DUV : MDMA_C7BRUR_DUV_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C7BRUR_Register use record
      SUV at 0 range 0 .. 15;
      DUV at 0 range 16 .. 31;
   end record;

   subtype MDMA_C7TBR_TSEL_Field is STM32H753.UInt6;

   --  MDMA channel x Trigger and Bus selection Register
   type MDMA_C7TBR_Register is record
      --  Trigger selection
      TSEL           : MDMA_C7TBR_TSEL_Field := 16#0#;
      --  unspecified
      Reserved_6_15  : STM32H753.UInt10 := 16#0#;
      --  Source BUS select This bit is protected and can be written only if EN
      --  is 0.
      SBUS           : Boolean := False;
      --  Destination BUS slect This bit is protected and can be written only
      --  if EN is 0.
      DBUS           : Boolean := False;
      --  unspecified
      Reserved_18_31 : STM32H753.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C7TBR_Register use record
      TSEL           at 0 range 0 .. 5;
      Reserved_6_15  at 0 range 6 .. 15;
      SBUS           at 0 range 16 .. 16;
      DBUS           at 0 range 17 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   --  MDMA channel x interrupt/status register
   type MDMA_C8ISR_Register is record
      --  Read-only. Channel x transfer error interrupt flag This bit is set by
      --  hardware. It is cleared by software writing 1 to the corresponding
      --  bit in the DMA_IFCRy register.
      TEIF8          : Boolean;
      --  Read-only. Channel x Channel Transfer Complete interrupt flag This
      --  bit is set by hardware. It is cleared by software writing 1 to the
      --  corresponding bit in the DMA_IFCRy register. CTC is set when the last
      --  block was transferred and the channel has been automatically
      --  disabled. CTC is also set when the channel is suspended, as a result
      --  of writing EN bit to 0.
      CTCIF8         : Boolean;
      --  Read-only. Channel x block repeat transfer complete interrupt flag
      --  This bit is set by hardware. It is cleared by software writing 1 to
      --  the corresponding bit in the DMA_IFCRy register.
      BRTIF8         : Boolean;
      --  Read-only. Channel x block transfer complete interrupt flag This bit
      --  is set by hardware. It is cleared by software writing 1 to the
      --  corresponding bit in the DMA_IFCRy register.
      BTIF8          : Boolean;
      --  Read-only. channel x buffer transfer complete
      TCIF8          : Boolean;
      --  unspecified
      Reserved_5_15  : STM32H753.UInt11;
      --  Read-only. channel x request active flag
      CRQA8          : Boolean;
      --  unspecified
      Reserved_17_31 : STM32H753.UInt15;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C8ISR_Register use record
      TEIF8          at 0 range 0 .. 0;
      CTCIF8         at 0 range 1 .. 1;
      BRTIF8         at 0 range 2 .. 2;
      BTIF8          at 0 range 3 .. 3;
      TCIF8          at 0 range 4 .. 4;
      Reserved_5_15  at 0 range 5 .. 15;
      CRQA8          at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   --  MDMA channel x interrupt flag clear register
   type MDMA_C8IFCR_Register is record
      --  Write-only. Channel x clear transfer error interrupt flag Writing a 1
      --  into this bit clears TEIFx in the MDMA_ISRy register
      CTEIF8        : Boolean := False;
      --  Write-only. Clear Channel transfer complete interrupt flag for
      --  channel x Writing a 1 into this bit clears CTCIFx in the MDMA_ISRy
      --  register
      CCTCIF8       : Boolean := False;
      --  Write-only. Channel x clear block repeat transfer complete interrupt
      --  flag Writing a 1 into this bit clears BRTIFx in the MDMA_ISRy
      --  register
      CBRTIF8       : Boolean := False;
      --  Write-only. Channel x Clear block transfer complete interrupt flag
      --  Writing a 1 into this bit clears BTIFx in the MDMA_ISRy register
      CBTIF8        : Boolean := False;
      --  Write-only. CLear buffer Transfer Complete Interrupt Flag for channel
      --  x Writing a 1 into this bit clears TCIFx in the MDMA_ISRy register
      CLTCIF8       : Boolean := False;
      --  unspecified
      Reserved_5_31 : STM32H753.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C8IFCR_Register use record
      CTEIF8        at 0 range 0 .. 0;
      CCTCIF8       at 0 range 1 .. 1;
      CBRTIF8       at 0 range 2 .. 2;
      CBTIF8        at 0 range 3 .. 3;
      CLTCIF8       at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   subtype MDMA_C8ESR_TEA_Field is STM32H753.UInt7;

   --  MDMA Channel x error status register
   type MDMA_C8ESR_Register is record
      --  Read-only. Transfer Error Address These bits are set and cleared by
      --  HW, in case of an MDMA data transfer error. It is used in conjunction
      --  with TED. This field indicates the 7 LSBits of the address which
      --  generated a transfer/access error. It may be used by SW to retrieve
      --  the failing address, by adding this value (truncated to the buffer
      --  transfer length size) to the current SAR/DAR value. Note: The SAR/DAR
      --  current value doesnt reflect this last address due to the FIFO
      --  management system. The SAR/DAR are only updated at the end of a
      --  (buffer) transfer (of TLEN+1 bytes). Note: It is not set in case of a
      --  link data error.
      TEA            : MDMA_C8ESR_TEA_Field;
      --  Read-only. Transfer Error Direction These bit is set and cleared by
      --  HW, in case of an MDMA data transfer error.
      TED            : Boolean;
      --  Read-only. Transfer Error Link Data These bit is set by HW, in case
      --  of a transfer error while reading the block link data structure. It
      --  is cleared by software writing 1 to the CTEIFx bit in the DMA_IFCRy
      --  register.
      TELD           : Boolean;
      --  Read-only. Transfer Error Mask Data These bit is set by HW, in case
      --  of a transfer error while writing the Mask Data. It is cleared by
      --  software writing 1 to the CTEIFx bit in the DMA_IFCRy register.
      TEMD           : Boolean;
      --  Read-only. Address/Size Error These bit is set by HW, when the
      --  programmed address is not aligned with the data size. TED will
      --  indicate whether the problem is on the source or destination. It is
      --  cleared by software writing 1 to the CTEIFx bit in the DMA_IFCRy
      --  register.
      ASE            : Boolean;
      --  Read-only. Block Size Error These bit is set by HW, when the block
      --  size is not an integer multiple of the data size either for source or
      --  destination. TED will indicate whether the problem is on the source
      --  or destination. It is cleared by software writing 1 to the CTEIFx bit
      --  in the DMA_IFCRy register.
      BSE            : Boolean;
      --  unspecified
      Reserved_12_31 : STM32H753.UInt20;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C8ESR_Register use record
      TEA            at 0 range 0 .. 6;
      TED            at 0 range 7 .. 7;
      TELD           at 0 range 8 .. 8;
      TEMD           at 0 range 9 .. 9;
      ASE            at 0 range 10 .. 10;
      BSE            at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype MDMA_C8CR_PL_Field is STM32H753.UInt2;

   --  This register is used to control the concerned channel.
   type MDMA_C8CR_Register is record
      --  channel enable
      EN             : Boolean := False;
      --  Transfer error interrupt enable This bit is set and cleared by
      --  software.
      TEIE           : Boolean := False;
      --  Channel Transfer Complete interrupt enable This bit is set and
      --  cleared by software.
      CTCIE          : Boolean := False;
      --  Block Repeat transfer interrupt enable This bit is set and cleared by
      --  software.
      BRTIE          : Boolean := False;
      --  Block Transfer interrupt enable This bit is set and cleared by
      --  software.
      BTIE           : Boolean := False;
      --  buffer Transfer Complete interrupt enable This bit is set and cleared
      --  by software.
      TCIE           : Boolean := False;
      --  Priority level These bits are set and cleared by software. These bits
      --  are protected and can be written only if EN is 0.
      PL             : MDMA_C8CR_PL_Field := 16#0#;
      --  unspecified
      Reserved_8_11  : STM32H753.UInt4 := 16#0#;
      --  byte Endianness exchange
      BEX            : Boolean := False;
      --  Half word Endianes exchange
      HEX            : Boolean := False;
      --  Word Endianness exchange
      WEX            : Boolean := False;
      --  unspecified
      Reserved_15_15 : STM32H753.Bit := 16#0#;
      --  Write-only. SW ReQuest Writing a 1 into this bit sets the CRQAx in
      --  MDMA_ISRy register, activating the request on Channel x Note: Either
      --  the whole CxCR register or the 8-bit/16-bit register @ Address
      --  offset: 0x4E + 0x40 chn may be used for SWRQ activation. In case of a
      --  SW request, acknowledge is not generated (neither HW signal, nor
      --  CxMAR write access).
      SWRQ           : Boolean := False;
      --  unspecified
      Reserved_17_31 : STM32H753.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C8CR_Register use record
      EN             at 0 range 0 .. 0;
      TEIE           at 0 range 1 .. 1;
      CTCIE          at 0 range 2 .. 2;
      BRTIE          at 0 range 3 .. 3;
      BTIE           at 0 range 4 .. 4;
      TCIE           at 0 range 5 .. 5;
      PL             at 0 range 6 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      BEX            at 0 range 12 .. 12;
      HEX            at 0 range 13 .. 13;
      WEX            at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      SWRQ           at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype MDMA_C8TCR_SINC_Field is STM32H753.UInt2;
   subtype MDMA_C8TCR_DINC_Field is STM32H753.UInt2;
   subtype MDMA_C8TCR_SSIZE_Field is STM32H753.UInt2;
   subtype MDMA_C8TCR_DSIZE_Field is STM32H753.UInt2;
   subtype MDMA_C8TCR_SINCOS_Field is STM32H753.UInt2;
   subtype MDMA_C8TCR_DINCOS_Field is STM32H753.UInt2;
   subtype MDMA_C8TCR_SBURST_Field is STM32H753.UInt3;
   subtype MDMA_C8TCR_DBURST_Field is STM32H753.UInt3;
   subtype MDMA_C8TCR_TLEN_Field is STM32H753.UInt7;
   subtype MDMA_C8TCR_PAM_Field is STM32H753.UInt2;
   subtype MDMA_C8TCR_TRGM_Field is STM32H753.UInt2;

   --  This register is used to configure the concerned channel.
   type MDMA_C8TCR_Register is record
      --  Source increment mode These bits are set and cleared by software.
      --  These bits are protected and can be written only if EN is 0 Note:
      --  When source is AHB (SBUS=1), SINC = 00 is forbidden. In Linked List
      --  Mode, at the end of a block (single or last block in repeated block
      --  transfer mode), this register will be loaded from memory (from
      --  address given by current LAR[31:0] + 0x00).
      SINC   : MDMA_C8TCR_SINC_Field := 16#0#;
      --  Destination increment mode These bits are set and cleared by
      --  software. These bits are protected and can be written only if EN is 0
      --  Note: When destination is AHB (DBUS=1), DINC = 00 is forbidden.
      DINC   : MDMA_C8TCR_DINC_Field := 16#0#;
      --  Source data size These bits are set and cleared by software. These
      --  bits are protected and can be written only if EN is 0 Note: If a
      --  value of 11 is programmed for the TCM access/AHB port, a transfer
      --  error will occur (TEIF bit set) If SINCOS &lt; SSIZE and SINC &#8800;
      --  00, the result will be unpredictable. Note: SSIZE = 11 (double-word)
      --  is forbidden when source is TCM/AHB bus (SBUS=1).
      SSIZE  : MDMA_C8TCR_SSIZE_Field := 16#0#;
      --  Destination data size These bits are set and cleared by software.
      --  These bits are protected and can be written only if EN is 0. Note: If
      --  a value of 11 is programmed for the TCM access/AHB port, a transfer
      --  error will occur (TEIF bit set) If DINCOS &lt; DSIZE and DINC &#8800;
      --  00, the result will be unpredictable. Note: DSIZE = 11 (double-word)
      --  is forbidden when destination is TCM/AHB bus (DBUS=1).
      DSIZE  : MDMA_C8TCR_DSIZE_Field := 16#0#;
      --  source increment offset size
      SINCOS : MDMA_C8TCR_SINCOS_Field := 16#0#;
      --  Destination increment offset
      DINCOS : MDMA_C8TCR_DINCOS_Field := 16#0#;
      --  source burst transfer configuration
      SBURST : MDMA_C8TCR_SBURST_Field := 16#0#;
      --  Destination burst transfer configuration
      DBURST : MDMA_C8TCR_DBURST_Field := 16#0#;
      --  buffer transfer lengh
      TLEN   : MDMA_C8TCR_TLEN_Field := 16#0#;
      --  PacK Enable These bit is set and cleared by software. If the Source
      --  Size is smaller than the destination, it will be padded according to
      --  the PAM value. If the Source data size is larger than the destination
      --  one, it will be truncated. The alignment will be done according to
      --  the PAM[0] value. This bit is protected and can be written only if EN
      --  is 0
      PKE    : Boolean := False;
      --  Padding/Alignement Mode These bits are set and cleared by software.
      --  Case 1: Source data size smaller than destination data size - 3
      --  options are valid. Case 2: Source data size larger than destination
      --  data size. The remainder part is discarded. When PKE = 1 or
      --  DSIZE=SSIZE, these bits are ignored. These bits are protected and can
      --  be written only if EN is 0
      PAM    : MDMA_C8TCR_PAM_Field := 16#0#;
      --  Trigger Mode These bits are set and cleared by software. Note: If
      --  TRGM is 11 for the current block, all the values loaded at the end of
      --  the current block through the linked list mechanism must keep the
      --  same value (TRGM=11) and the same SWRM value, otherwise the result is
      --  undefined. These bits are protected and can be written only if EN is
      --  0.
      TRGM   : MDMA_C8TCR_TRGM_Field := 16#0#;
      --  SW Request Mode This bit is set and cleared by software. If a HW or
      --  SW request is currently active, the bit change will be delayed until
      --  the current transfer is completed. If the CxMAR contains a valid
      --  address, the CxMDR value will also be written @ CxMAR address. This
      --  bit is protected and can be written only if EN is 0.
      SWRM   : Boolean := False;
      --  Bufferable Write Mode This bit is set and cleared by software. This
      --  bit is protected and can be written only if EN is 0. Note: All MDMA
      --  destination accesses are non-cacheable.
      BWM    : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C8TCR_Register use record
      SINC   at 0 range 0 .. 1;
      DINC   at 0 range 2 .. 3;
      SSIZE  at 0 range 4 .. 5;
      DSIZE  at 0 range 6 .. 7;
      SINCOS at 0 range 8 .. 9;
      DINCOS at 0 range 10 .. 11;
      SBURST at 0 range 12 .. 14;
      DBURST at 0 range 15 .. 17;
      TLEN   at 0 range 18 .. 24;
      PKE    at 0 range 25 .. 25;
      PAM    at 0 range 26 .. 27;
      TRGM   at 0 range 28 .. 29;
      SWRM   at 0 range 30 .. 30;
      BWM    at 0 range 31 .. 31;
   end record;

   subtype MDMA_C8BNDTR_BNDT_Field is STM32H753.UInt17;
   subtype MDMA_C8BNDTR_BRC_Field is STM32H753.UInt12;

   --  MDMA Channel x block number of data register
   type MDMA_C8BNDTR_Register is record
      --  block number of data to transfer
      BNDT           : MDMA_C8BNDTR_BNDT_Field := 16#0#;
      --  unspecified
      Reserved_17_17 : STM32H753.Bit := 16#0#;
      --  Block Repeat Source address Update Mode These bits are protected and
      --  can be written only if EN is 0.
      BRSUM          : Boolean := False;
      --  Block Repeat Destination address Update Mode These bits are protected
      --  and can be written only if EN is 0.
      BRDUM          : Boolean := False;
      --  Block Repeat Count This field contains the number of repetitions of
      --  the current block (0 to 4095). When the channel is enabled, this
      --  register is read-only, indicating the remaining number of blocks,
      --  excluding the current one. This register decrements after each
      --  complete block transfer. Once the last block transfer has completed,
      --  this register can either stay at zero or be reloaded automatically
      --  from memory (in Linked List mode - i.e. Link Address valid). These
      --  bits are protected and can be written only if EN is 0.
      BRC            : MDMA_C8BNDTR_BRC_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C8BNDTR_Register use record
      BNDT           at 0 range 0 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      BRSUM          at 0 range 18 .. 18;
      BRDUM          at 0 range 19 .. 19;
      BRC            at 0 range 20 .. 31;
   end record;

   subtype MDMA_C8BRUR_SUV_Field is STM32H753.UInt16;
   subtype MDMA_C8BRUR_DUV_Field is STM32H753.UInt16;

   --  MDMA channel x Block Repeat address Update register
   type MDMA_C8BRUR_Register is record
      --  source adresse update value
      SUV : MDMA_C8BRUR_SUV_Field := 16#0#;
      --  destination address update
      DUV : MDMA_C8BRUR_DUV_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C8BRUR_Register use record
      SUV at 0 range 0 .. 15;
      DUV at 0 range 16 .. 31;
   end record;

   subtype MDMA_C8TBR_TSEL_Field is STM32H753.UInt6;

   --  MDMA channel x Trigger and Bus selection Register
   type MDMA_C8TBR_Register is record
      --  Trigger selection
      TSEL           : MDMA_C8TBR_TSEL_Field := 16#0#;
      --  unspecified
      Reserved_6_15  : STM32H753.UInt10 := 16#0#;
      --  Source BUS select This bit is protected and can be written only if EN
      --  is 0.
      SBUS           : Boolean := False;
      --  Destination BUS slect This bit is protected and can be written only
      --  if EN is 0.
      DBUS           : Boolean := False;
      --  unspecified
      Reserved_18_31 : STM32H753.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C8TBR_Register use record
      TSEL           at 0 range 0 .. 5;
      Reserved_6_15  at 0 range 6 .. 15;
      SBUS           at 0 range 16 .. 16;
      DBUS           at 0 range 17 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   --  MDMA channel x interrupt/status register
   type MDMA_C9ISR_Register is record
      --  Read-only. Channel x transfer error interrupt flag This bit is set by
      --  hardware. It is cleared by software writing 1 to the corresponding
      --  bit in the DMA_IFCRy register.
      TEIF9          : Boolean;
      --  Read-only. Channel x Channel Transfer Complete interrupt flag This
      --  bit is set by hardware. It is cleared by software writing 1 to the
      --  corresponding bit in the DMA_IFCRy register. CTC is set when the last
      --  block was transferred and the channel has been automatically
      --  disabled. CTC is also set when the channel is suspended, as a result
      --  of writing EN bit to 0.
      CTCIF9         : Boolean;
      --  Read-only. Channel x block repeat transfer complete interrupt flag
      --  This bit is set by hardware. It is cleared by software writing 1 to
      --  the corresponding bit in the DMA_IFCRy register.
      BRTIF9         : Boolean;
      --  Read-only. Channel x block transfer complete interrupt flag This bit
      --  is set by hardware. It is cleared by software writing 1 to the
      --  corresponding bit in the DMA_IFCRy register.
      BTIF9          : Boolean;
      --  Read-only. channel x buffer transfer complete
      TCIF9          : Boolean;
      --  unspecified
      Reserved_5_15  : STM32H753.UInt11;
      --  Read-only. channel x request active flag
      CRQA9          : Boolean;
      --  unspecified
      Reserved_17_31 : STM32H753.UInt15;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C9ISR_Register use record
      TEIF9          at 0 range 0 .. 0;
      CTCIF9         at 0 range 1 .. 1;
      BRTIF9         at 0 range 2 .. 2;
      BTIF9          at 0 range 3 .. 3;
      TCIF9          at 0 range 4 .. 4;
      Reserved_5_15  at 0 range 5 .. 15;
      CRQA9          at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   --  MDMA channel x interrupt flag clear register
   type MDMA_C9IFCR_Register is record
      --  Write-only. Channel x clear transfer error interrupt flag Writing a 1
      --  into this bit clears TEIFx in the MDMA_ISRy register
      CTEIF9        : Boolean := False;
      --  Write-only. Clear Channel transfer complete interrupt flag for
      --  channel x Writing a 1 into this bit clears CTCIFx in the MDMA_ISRy
      --  register
      CCTCIF9       : Boolean := False;
      --  Write-only. Channel x clear block repeat transfer complete interrupt
      --  flag Writing a 1 into this bit clears BRTIFx in the MDMA_ISRy
      --  register
      CBRTIF9       : Boolean := False;
      --  Write-only. Channel x Clear block transfer complete interrupt flag
      --  Writing a 1 into this bit clears BTIFx in the MDMA_ISRy register
      CBTIF9        : Boolean := False;
      --  Write-only. CLear buffer Transfer Complete Interrupt Flag for channel
      --  x Writing a 1 into this bit clears TCIFx in the MDMA_ISRy register
      CLTCIF9       : Boolean := False;
      --  unspecified
      Reserved_5_31 : STM32H753.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C9IFCR_Register use record
      CTEIF9        at 0 range 0 .. 0;
      CCTCIF9       at 0 range 1 .. 1;
      CBRTIF9       at 0 range 2 .. 2;
      CBTIF9        at 0 range 3 .. 3;
      CLTCIF9       at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   subtype MDMA_C9ESR_TEA_Field is STM32H753.UInt7;

   --  MDMA Channel x error status register
   type MDMA_C9ESR_Register is record
      --  Read-only. Transfer Error Address These bits are set and cleared by
      --  HW, in case of an MDMA data transfer error. It is used in conjunction
      --  with TED. This field indicates the 7 LSBits of the address which
      --  generated a transfer/access error. It may be used by SW to retrieve
      --  the failing address, by adding this value (truncated to the buffer
      --  transfer length size) to the current SAR/DAR value. Note: The SAR/DAR
      --  current value doesnt reflect this last address due to the FIFO
      --  management system. The SAR/DAR are only updated at the end of a
      --  (buffer) transfer (of TLEN+1 bytes). Note: It is not set in case of a
      --  link data error.
      TEA            : MDMA_C9ESR_TEA_Field;
      --  Read-only. Transfer Error Direction These bit is set and cleared by
      --  HW, in case of an MDMA data transfer error.
      TED            : Boolean;
      --  Read-only. Transfer Error Link Data These bit is set by HW, in case
      --  of a transfer error while reading the block link data structure. It
      --  is cleared by software writing 1 to the CTEIFx bit in the DMA_IFCRy
      --  register.
      TELD           : Boolean;
      --  Read-only. Transfer Error Mask Data These bit is set by HW, in case
      --  of a transfer error while writing the Mask Data. It is cleared by
      --  software writing 1 to the CTEIFx bit in the DMA_IFCRy register.
      TEMD           : Boolean;
      --  Read-only. Address/Size Error These bit is set by HW, when the
      --  programmed address is not aligned with the data size. TED will
      --  indicate whether the problem is on the source or destination. It is
      --  cleared by software writing 1 to the CTEIFx bit in the DMA_IFCRy
      --  register.
      ASE            : Boolean;
      --  Read-only. Block Size Error These bit is set by HW, when the block
      --  size is not an integer multiple of the data size either for source or
      --  destination. TED will indicate whether the problem is on the source
      --  or destination. It is cleared by software writing 1 to the CTEIFx bit
      --  in the DMA_IFCRy register.
      BSE            : Boolean;
      --  unspecified
      Reserved_12_31 : STM32H753.UInt20;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C9ESR_Register use record
      TEA            at 0 range 0 .. 6;
      TED            at 0 range 7 .. 7;
      TELD           at 0 range 8 .. 8;
      TEMD           at 0 range 9 .. 9;
      ASE            at 0 range 10 .. 10;
      BSE            at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype MDMA_C9CR_PL_Field is STM32H753.UInt2;

   --  This register is used to control the concerned channel.
   type MDMA_C9CR_Register is record
      --  channel enable
      EN             : Boolean := False;
      --  Transfer error interrupt enable This bit is set and cleared by
      --  software.
      TEIE           : Boolean := False;
      --  Channel Transfer Complete interrupt enable This bit is set and
      --  cleared by software.
      CTCIE          : Boolean := False;
      --  Block Repeat transfer interrupt enable This bit is set and cleared by
      --  software.
      BRTIE          : Boolean := False;
      --  Block Transfer interrupt enable This bit is set and cleared by
      --  software.
      BTIE           : Boolean := False;
      --  buffer Transfer Complete interrupt enable This bit is set and cleared
      --  by software.
      TCIE           : Boolean := False;
      --  Priority level These bits are set and cleared by software. These bits
      --  are protected and can be written only if EN is 0.
      PL             : MDMA_C9CR_PL_Field := 16#0#;
      --  unspecified
      Reserved_8_11  : STM32H753.UInt4 := 16#0#;
      --  byte Endianness exchange
      BEX            : Boolean := False;
      --  Half word Endianes exchange
      HEX            : Boolean := False;
      --  Word Endianness exchange
      WEX            : Boolean := False;
      --  unspecified
      Reserved_15_15 : STM32H753.Bit := 16#0#;
      --  Write-only. SW ReQuest Writing a 1 into this bit sets the CRQAx in
      --  MDMA_ISRy register, activating the request on Channel x Note: Either
      --  the whole CxCR register or the 8-bit/16-bit register @ Address
      --  offset: 0x4E + 0x40 chn may be used for SWRQ activation. In case of a
      --  SW request, acknowledge is not generated (neither HW signal, nor
      --  CxMAR write access).
      SWRQ           : Boolean := False;
      --  unspecified
      Reserved_17_31 : STM32H753.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C9CR_Register use record
      EN             at 0 range 0 .. 0;
      TEIE           at 0 range 1 .. 1;
      CTCIE          at 0 range 2 .. 2;
      BRTIE          at 0 range 3 .. 3;
      BTIE           at 0 range 4 .. 4;
      TCIE           at 0 range 5 .. 5;
      PL             at 0 range 6 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      BEX            at 0 range 12 .. 12;
      HEX            at 0 range 13 .. 13;
      WEX            at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      SWRQ           at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype MDMA_C9TCR_SINC_Field is STM32H753.UInt2;
   subtype MDMA_C9TCR_DINC_Field is STM32H753.UInt2;
   subtype MDMA_C9TCR_SSIZE_Field is STM32H753.UInt2;
   subtype MDMA_C9TCR_DSIZE_Field is STM32H753.UInt2;
   subtype MDMA_C9TCR_SINCOS_Field is STM32H753.UInt2;
   subtype MDMA_C9TCR_DINCOS_Field is STM32H753.UInt2;
   subtype MDMA_C9TCR_SBURST_Field is STM32H753.UInt3;
   subtype MDMA_C9TCR_DBURST_Field is STM32H753.UInt3;
   subtype MDMA_C9TCR_TLEN_Field is STM32H753.UInt7;
   subtype MDMA_C9TCR_PAM_Field is STM32H753.UInt2;
   subtype MDMA_C9TCR_TRGM_Field is STM32H753.UInt2;

   --  This register is used to configure the concerned channel.
   type MDMA_C9TCR_Register is record
      --  Source increment mode These bits are set and cleared by software.
      --  These bits are protected and can be written only if EN is 0 Note:
      --  When source is AHB (SBUS=1), SINC = 00 is forbidden. In Linked List
      --  Mode, at the end of a block (single or last block in repeated block
      --  transfer mode), this register will be loaded from memory (from
      --  address given by current LAR[31:0] + 0x00).
      SINC   : MDMA_C9TCR_SINC_Field := 16#0#;
      --  Destination increment mode These bits are set and cleared by
      --  software. These bits are protected and can be written only if EN is 0
      --  Note: When destination is AHB (DBUS=1), DINC = 00 is forbidden.
      DINC   : MDMA_C9TCR_DINC_Field := 16#0#;
      --  Source data size These bits are set and cleared by software. These
      --  bits are protected and can be written only if EN is 0 Note: If a
      --  value of 11 is programmed for the TCM access/AHB port, a transfer
      --  error will occur (TEIF bit set) If SINCOS &lt; SSIZE and SINC &#8800;
      --  00, the result will be unpredictable. Note: SSIZE = 11 (double-word)
      --  is forbidden when source is TCM/AHB bus (SBUS=1).
      SSIZE  : MDMA_C9TCR_SSIZE_Field := 16#0#;
      --  Destination data size These bits are set and cleared by software.
      --  These bits are protected and can be written only if EN is 0. Note: If
      --  a value of 11 is programmed for the TCM access/AHB port, a transfer
      --  error will occur (TEIF bit set) If DINCOS &lt; DSIZE and DINC &#8800;
      --  00, the result will be unpredictable. Note: DSIZE = 11 (double-word)
      --  is forbidden when destination is TCM/AHB bus (DBUS=1).
      DSIZE  : MDMA_C9TCR_DSIZE_Field := 16#0#;
      --  source increment offset size
      SINCOS : MDMA_C9TCR_SINCOS_Field := 16#0#;
      --  Destination increment offset
      DINCOS : MDMA_C9TCR_DINCOS_Field := 16#0#;
      --  source burst transfer configuration
      SBURST : MDMA_C9TCR_SBURST_Field := 16#0#;
      --  Destination burst transfer configuration
      DBURST : MDMA_C9TCR_DBURST_Field := 16#0#;
      --  buffer transfer lengh
      TLEN   : MDMA_C9TCR_TLEN_Field := 16#0#;
      --  PacK Enable These bit is set and cleared by software. If the Source
      --  Size is smaller than the destination, it will be padded according to
      --  the PAM value. If the Source data size is larger than the destination
      --  one, it will be truncated. The alignment will be done according to
      --  the PAM[0] value. This bit is protected and can be written only if EN
      --  is 0
      PKE    : Boolean := False;
      --  Padding/Alignement Mode These bits are set and cleared by software.
      --  Case 1: Source data size smaller than destination data size - 3
      --  options are valid. Case 2: Source data size larger than destination
      --  data size. The remainder part is discarded. When PKE = 1 or
      --  DSIZE=SSIZE, these bits are ignored. These bits are protected and can
      --  be written only if EN is 0
      PAM    : MDMA_C9TCR_PAM_Field := 16#0#;
      --  Trigger Mode These bits are set and cleared by software. Note: If
      --  TRGM is 11 for the current block, all the values loaded at the end of
      --  the current block through the linked list mechanism must keep the
      --  same value (TRGM=11) and the same SWRM value, otherwise the result is
      --  undefined. These bits are protected and can be written only if EN is
      --  0.
      TRGM   : MDMA_C9TCR_TRGM_Field := 16#0#;
      --  SW Request Mode This bit is set and cleared by software. If a HW or
      --  SW request is currently active, the bit change will be delayed until
      --  the current transfer is completed. If the CxMAR contains a valid
      --  address, the CxMDR value will also be written @ CxMAR address. This
      --  bit is protected and can be written only if EN is 0.
      SWRM   : Boolean := False;
      --  Bufferable Write Mode This bit is set and cleared by software. This
      --  bit is protected and can be written only if EN is 0. Note: All MDMA
      --  destination accesses are non-cacheable.
      BWM    : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C9TCR_Register use record
      SINC   at 0 range 0 .. 1;
      DINC   at 0 range 2 .. 3;
      SSIZE  at 0 range 4 .. 5;
      DSIZE  at 0 range 6 .. 7;
      SINCOS at 0 range 8 .. 9;
      DINCOS at 0 range 10 .. 11;
      SBURST at 0 range 12 .. 14;
      DBURST at 0 range 15 .. 17;
      TLEN   at 0 range 18 .. 24;
      PKE    at 0 range 25 .. 25;
      PAM    at 0 range 26 .. 27;
      TRGM   at 0 range 28 .. 29;
      SWRM   at 0 range 30 .. 30;
      BWM    at 0 range 31 .. 31;
   end record;

   subtype MDMA_C9BNDTR_BNDT_Field is STM32H753.UInt17;
   subtype MDMA_C9BNDTR_BRC_Field is STM32H753.UInt12;

   --  MDMA Channel x block number of data register
   type MDMA_C9BNDTR_Register is record
      --  block number of data to transfer
      BNDT           : MDMA_C9BNDTR_BNDT_Field := 16#0#;
      --  unspecified
      Reserved_17_17 : STM32H753.Bit := 16#0#;
      --  Block Repeat Source address Update Mode These bits are protected and
      --  can be written only if EN is 0.
      BRSUM          : Boolean := False;
      --  Block Repeat Destination address Update Mode These bits are protected
      --  and can be written only if EN is 0.
      BRDUM          : Boolean := False;
      --  Block Repeat Count This field contains the number of repetitions of
      --  the current block (0 to 4095). When the channel is enabled, this
      --  register is read-only, indicating the remaining number of blocks,
      --  excluding the current one. This register decrements after each
      --  complete block transfer. Once the last block transfer has completed,
      --  this register can either stay at zero or be reloaded automatically
      --  from memory (in Linked List mode - i.e. Link Address valid). These
      --  bits are protected and can be written only if EN is 0.
      BRC            : MDMA_C9BNDTR_BRC_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C9BNDTR_Register use record
      BNDT           at 0 range 0 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      BRSUM          at 0 range 18 .. 18;
      BRDUM          at 0 range 19 .. 19;
      BRC            at 0 range 20 .. 31;
   end record;

   subtype MDMA_C9BRUR_SUV_Field is STM32H753.UInt16;
   subtype MDMA_C9BRUR_DUV_Field is STM32H753.UInt16;

   --  MDMA channel x Block Repeat address Update register
   type MDMA_C9BRUR_Register is record
      --  source adresse update value
      SUV : MDMA_C9BRUR_SUV_Field := 16#0#;
      --  destination address update
      DUV : MDMA_C9BRUR_DUV_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C9BRUR_Register use record
      SUV at 0 range 0 .. 15;
      DUV at 0 range 16 .. 31;
   end record;

   subtype MDMA_C9TBR_TSEL_Field is STM32H753.UInt6;

   --  MDMA channel x Trigger and Bus selection Register
   type MDMA_C9TBR_Register is record
      --  Trigger selection
      TSEL           : MDMA_C9TBR_TSEL_Field := 16#0#;
      --  unspecified
      Reserved_6_15  : STM32H753.UInt10 := 16#0#;
      --  Source BUS select This bit is protected and can be written only if EN
      --  is 0.
      SBUS           : Boolean := False;
      --  Destination BUS slect This bit is protected and can be written only
      --  if EN is 0.
      DBUS           : Boolean := False;
      --  unspecified
      Reserved_18_31 : STM32H753.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C9TBR_Register use record
      TSEL           at 0 range 0 .. 5;
      Reserved_6_15  at 0 range 6 .. 15;
      SBUS           at 0 range 16 .. 16;
      DBUS           at 0 range 17 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   --  MDMA channel x interrupt/status register
   type MDMA_C10ISR_Register is record
      --  Read-only. Channel x transfer error interrupt flag This bit is set by
      --  hardware. It is cleared by software writing 1 to the corresponding
      --  bit in the DMA_IFCRy register.
      TEIF10         : Boolean;
      --  Read-only. Channel x Channel Transfer Complete interrupt flag This
      --  bit is set by hardware. It is cleared by software writing 1 to the
      --  corresponding bit in the DMA_IFCRy register. CTC is set when the last
      --  block was transferred and the channel has been automatically
      --  disabled. CTC is also set when the channel is suspended, as a result
      --  of writing EN bit to 0.
      CTCIF10        : Boolean;
      --  Read-only. Channel x block repeat transfer complete interrupt flag
      --  This bit is set by hardware. It is cleared by software writing 1 to
      --  the corresponding bit in the DMA_IFCRy register.
      BRTIF10        : Boolean;
      --  Read-only. Channel x block transfer complete interrupt flag This bit
      --  is set by hardware. It is cleared by software writing 1 to the
      --  corresponding bit in the DMA_IFCRy register.
      BTIF10         : Boolean;
      --  Read-only. channel x buffer transfer complete
      TCIF10         : Boolean;
      --  unspecified
      Reserved_5_15  : STM32H753.UInt11;
      --  Read-only. channel x request active flag
      CRQA10         : Boolean;
      --  unspecified
      Reserved_17_31 : STM32H753.UInt15;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C10ISR_Register use record
      TEIF10         at 0 range 0 .. 0;
      CTCIF10        at 0 range 1 .. 1;
      BRTIF10        at 0 range 2 .. 2;
      BTIF10         at 0 range 3 .. 3;
      TCIF10         at 0 range 4 .. 4;
      Reserved_5_15  at 0 range 5 .. 15;
      CRQA10         at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   --  MDMA channel x interrupt flag clear register
   type MDMA_C10IFCR_Register is record
      --  Write-only. Channel x clear transfer error interrupt flag Writing a 1
      --  into this bit clears TEIFx in the MDMA_ISRy register
      CTEIF10       : Boolean := False;
      --  Write-only. Clear Channel transfer complete interrupt flag for
      --  channel x Writing a 1 into this bit clears CTCIFx in the MDMA_ISRy
      --  register
      CCTCIF10      : Boolean := False;
      --  Write-only. Channel x clear block repeat transfer complete interrupt
      --  flag Writing a 1 into this bit clears BRTIFx in the MDMA_ISRy
      --  register
      CBRTIF10      : Boolean := False;
      --  Write-only. Channel x Clear block transfer complete interrupt flag
      --  Writing a 1 into this bit clears BTIFx in the MDMA_ISRy register
      CBTIF10       : Boolean := False;
      --  Write-only. CLear buffer Transfer Complete Interrupt Flag for channel
      --  x Writing a 1 into this bit clears TCIFx in the MDMA_ISRy register
      CLTCIF10      : Boolean := False;
      --  unspecified
      Reserved_5_31 : STM32H753.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C10IFCR_Register use record
      CTEIF10       at 0 range 0 .. 0;
      CCTCIF10      at 0 range 1 .. 1;
      CBRTIF10      at 0 range 2 .. 2;
      CBTIF10       at 0 range 3 .. 3;
      CLTCIF10      at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   subtype MDMA_C10ESR_TEA_Field is STM32H753.UInt7;

   --  MDMA Channel x error status register
   type MDMA_C10ESR_Register is record
      --  Read-only. Transfer Error Address These bits are set and cleared by
      --  HW, in case of an MDMA data transfer error. It is used in conjunction
      --  with TED. This field indicates the 7 LSBits of the address which
      --  generated a transfer/access error. It may be used by SW to retrieve
      --  the failing address, by adding this value (truncated to the buffer
      --  transfer length size) to the current SAR/DAR value. Note: The SAR/DAR
      --  current value doesnt reflect this last address due to the FIFO
      --  management system. The SAR/DAR are only updated at the end of a
      --  (buffer) transfer (of TLEN+1 bytes). Note: It is not set in case of a
      --  link data error.
      TEA            : MDMA_C10ESR_TEA_Field;
      --  Read-only. Transfer Error Direction These bit is set and cleared by
      --  HW, in case of an MDMA data transfer error.
      TED            : Boolean;
      --  Read-only. Transfer Error Link Data These bit is set by HW, in case
      --  of a transfer error while reading the block link data structure. It
      --  is cleared by software writing 1 to the CTEIFx bit in the DMA_IFCRy
      --  register.
      TELD           : Boolean;
      --  Read-only. Transfer Error Mask Data These bit is set by HW, in case
      --  of a transfer error while writing the Mask Data. It is cleared by
      --  software writing 1 to the CTEIFx bit in the DMA_IFCRy register.
      TEMD           : Boolean;
      --  Read-only. Address/Size Error These bit is set by HW, when the
      --  programmed address is not aligned with the data size. TED will
      --  indicate whether the problem is on the source or destination. It is
      --  cleared by software writing 1 to the CTEIFx bit in the DMA_IFCRy
      --  register.
      ASE            : Boolean;
      --  Read-only. Block Size Error These bit is set by HW, when the block
      --  size is not an integer multiple of the data size either for source or
      --  destination. TED will indicate whether the problem is on the source
      --  or destination. It is cleared by software writing 1 to the CTEIFx bit
      --  in the DMA_IFCRy register.
      BSE            : Boolean;
      --  unspecified
      Reserved_12_31 : STM32H753.UInt20;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C10ESR_Register use record
      TEA            at 0 range 0 .. 6;
      TED            at 0 range 7 .. 7;
      TELD           at 0 range 8 .. 8;
      TEMD           at 0 range 9 .. 9;
      ASE            at 0 range 10 .. 10;
      BSE            at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype MDMA_C10CR_PL_Field is STM32H753.UInt2;

   --  This register is used to control the concerned channel.
   type MDMA_C10CR_Register is record
      --  channel enable
      EN             : Boolean := False;
      --  Transfer error interrupt enable This bit is set and cleared by
      --  software.
      TEIE           : Boolean := False;
      --  Channel Transfer Complete interrupt enable This bit is set and
      --  cleared by software.
      CTCIE          : Boolean := False;
      --  Block Repeat transfer interrupt enable This bit is set and cleared by
      --  software.
      BRTIE          : Boolean := False;
      --  Block Transfer interrupt enable This bit is set and cleared by
      --  software.
      BTIE           : Boolean := False;
      --  buffer Transfer Complete interrupt enable This bit is set and cleared
      --  by software.
      TCIE           : Boolean := False;
      --  Priority level These bits are set and cleared by software. These bits
      --  are protected and can be written only if EN is 0.
      PL             : MDMA_C10CR_PL_Field := 16#0#;
      --  unspecified
      Reserved_8_11  : STM32H753.UInt4 := 16#0#;
      --  byte Endianness exchange
      BEX            : Boolean := False;
      --  Half word Endianes exchange
      HEX            : Boolean := False;
      --  Word Endianness exchange
      WEX            : Boolean := False;
      --  unspecified
      Reserved_15_15 : STM32H753.Bit := 16#0#;
      --  Write-only. SW ReQuest Writing a 1 into this bit sets the CRQAx in
      --  MDMA_ISRy register, activating the request on Channel x Note: Either
      --  the whole CxCR register or the 8-bit/16-bit register @ Address
      --  offset: 0x4E + 0x40 chn may be used for SWRQ activation. In case of a
      --  SW request, acknowledge is not generated (neither HW signal, nor
      --  CxMAR write access).
      SWRQ           : Boolean := False;
      --  unspecified
      Reserved_17_31 : STM32H753.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C10CR_Register use record
      EN             at 0 range 0 .. 0;
      TEIE           at 0 range 1 .. 1;
      CTCIE          at 0 range 2 .. 2;
      BRTIE          at 0 range 3 .. 3;
      BTIE           at 0 range 4 .. 4;
      TCIE           at 0 range 5 .. 5;
      PL             at 0 range 6 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      BEX            at 0 range 12 .. 12;
      HEX            at 0 range 13 .. 13;
      WEX            at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      SWRQ           at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype MDMA_C10TCR_SINC_Field is STM32H753.UInt2;
   subtype MDMA_C10TCR_DINC_Field is STM32H753.UInt2;
   subtype MDMA_C10TCR_SSIZE_Field is STM32H753.UInt2;
   subtype MDMA_C10TCR_DSIZE_Field is STM32H753.UInt2;
   subtype MDMA_C10TCR_SINCOS_Field is STM32H753.UInt2;
   subtype MDMA_C10TCR_DINCOS_Field is STM32H753.UInt2;
   subtype MDMA_C10TCR_SBURST_Field is STM32H753.UInt3;
   subtype MDMA_C10TCR_DBURST_Field is STM32H753.UInt3;
   subtype MDMA_C10TCR_TLEN_Field is STM32H753.UInt7;
   subtype MDMA_C10TCR_PAM_Field is STM32H753.UInt2;
   subtype MDMA_C10TCR_TRGM_Field is STM32H753.UInt2;

   --  This register is used to configure the concerned channel.
   type MDMA_C10TCR_Register is record
      --  Source increment mode These bits are set and cleared by software.
      --  These bits are protected and can be written only if EN is 0 Note:
      --  When source is AHB (SBUS=1), SINC = 00 is forbidden. In Linked List
      --  Mode, at the end of a block (single or last block in repeated block
      --  transfer mode), this register will be loaded from memory (from
      --  address given by current LAR[31:0] + 0x00).
      SINC   : MDMA_C10TCR_SINC_Field := 16#0#;
      --  Destination increment mode These bits are set and cleared by
      --  software. These bits are protected and can be written only if EN is 0
      --  Note: When destination is AHB (DBUS=1), DINC = 00 is forbidden.
      DINC   : MDMA_C10TCR_DINC_Field := 16#0#;
      --  Source data size These bits are set and cleared by software. These
      --  bits are protected and can be written only if EN is 0 Note: If a
      --  value of 11 is programmed for the TCM access/AHB port, a transfer
      --  error will occur (TEIF bit set) If SINCOS &lt; SSIZE and SINC &#8800;
      --  00, the result will be unpredictable. Note: SSIZE = 11 (double-word)
      --  is forbidden when source is TCM/AHB bus (SBUS=1).
      SSIZE  : MDMA_C10TCR_SSIZE_Field := 16#0#;
      --  Destination data size These bits are set and cleared by software.
      --  These bits are protected and can be written only if EN is 0. Note: If
      --  a value of 11 is programmed for the TCM access/AHB port, a transfer
      --  error will occur (TEIF bit set) If DINCOS &lt; DSIZE and DINC &#8800;
      --  00, the result will be unpredictable. Note: DSIZE = 11 (double-word)
      --  is forbidden when destination is TCM/AHB bus (DBUS=1).
      DSIZE  : MDMA_C10TCR_DSIZE_Field := 16#0#;
      --  source increment offset size
      SINCOS : MDMA_C10TCR_SINCOS_Field := 16#0#;
      --  Destination increment offset
      DINCOS : MDMA_C10TCR_DINCOS_Field := 16#0#;
      --  source burst transfer configuration
      SBURST : MDMA_C10TCR_SBURST_Field := 16#0#;
      --  Destination burst transfer configuration
      DBURST : MDMA_C10TCR_DBURST_Field := 16#0#;
      --  buffer transfer lengh
      TLEN   : MDMA_C10TCR_TLEN_Field := 16#0#;
      --  PacK Enable These bit is set and cleared by software. If the Source
      --  Size is smaller than the destination, it will be padded according to
      --  the PAM value. If the Source data size is larger than the destination
      --  one, it will be truncated. The alignment will be done according to
      --  the PAM[0] value. This bit is protected and can be written only if EN
      --  is 0
      PKE    : Boolean := False;
      --  Padding/Alignement Mode These bits are set and cleared by software.
      --  Case 1: Source data size smaller than destination data size - 3
      --  options are valid. Case 2: Source data size larger than destination
      --  data size. The remainder part is discarded. When PKE = 1 or
      --  DSIZE=SSIZE, these bits are ignored. These bits are protected and can
      --  be written only if EN is 0
      PAM    : MDMA_C10TCR_PAM_Field := 16#0#;
      --  Trigger Mode These bits are set and cleared by software. Note: If
      --  TRGM is 11 for the current block, all the values loaded at the end of
      --  the current block through the linked list mechanism must keep the
      --  same value (TRGM=11) and the same SWRM value, otherwise the result is
      --  undefined. These bits are protected and can be written only if EN is
      --  0.
      TRGM   : MDMA_C10TCR_TRGM_Field := 16#0#;
      --  SW Request Mode This bit is set and cleared by software. If a HW or
      --  SW request is currently active, the bit change will be delayed until
      --  the current transfer is completed. If the CxMAR contains a valid
      --  address, the CxMDR value will also be written @ CxMAR address. This
      --  bit is protected and can be written only if EN is 0.
      SWRM   : Boolean := False;
      --  Bufferable Write Mode This bit is set and cleared by software. This
      --  bit is protected and can be written only if EN is 0. Note: All MDMA
      --  destination accesses are non-cacheable.
      BWM    : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C10TCR_Register use record
      SINC   at 0 range 0 .. 1;
      DINC   at 0 range 2 .. 3;
      SSIZE  at 0 range 4 .. 5;
      DSIZE  at 0 range 6 .. 7;
      SINCOS at 0 range 8 .. 9;
      DINCOS at 0 range 10 .. 11;
      SBURST at 0 range 12 .. 14;
      DBURST at 0 range 15 .. 17;
      TLEN   at 0 range 18 .. 24;
      PKE    at 0 range 25 .. 25;
      PAM    at 0 range 26 .. 27;
      TRGM   at 0 range 28 .. 29;
      SWRM   at 0 range 30 .. 30;
      BWM    at 0 range 31 .. 31;
   end record;

   subtype MDMA_C10BNDTR_BNDT_Field is STM32H753.UInt17;
   subtype MDMA_C10BNDTR_BRC_Field is STM32H753.UInt12;

   --  MDMA Channel x block number of data register
   type MDMA_C10BNDTR_Register is record
      --  block number of data to transfer
      BNDT           : MDMA_C10BNDTR_BNDT_Field := 16#0#;
      --  unspecified
      Reserved_17_17 : STM32H753.Bit := 16#0#;
      --  Block Repeat Source address Update Mode These bits are protected and
      --  can be written only if EN is 0.
      BRSUM          : Boolean := False;
      --  Block Repeat Destination address Update Mode These bits are protected
      --  and can be written only if EN is 0.
      BRDUM          : Boolean := False;
      --  Block Repeat Count This field contains the number of repetitions of
      --  the current block (0 to 4095). When the channel is enabled, this
      --  register is read-only, indicating the remaining number of blocks,
      --  excluding the current one. This register decrements after each
      --  complete block transfer. Once the last block transfer has completed,
      --  this register can either stay at zero or be reloaded automatically
      --  from memory (in Linked List mode - i.e. Link Address valid). These
      --  bits are protected and can be written only if EN is 0.
      BRC            : MDMA_C10BNDTR_BRC_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C10BNDTR_Register use record
      BNDT           at 0 range 0 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      BRSUM          at 0 range 18 .. 18;
      BRDUM          at 0 range 19 .. 19;
      BRC            at 0 range 20 .. 31;
   end record;

   subtype MDMA_C10BRUR_SUV_Field is STM32H753.UInt16;
   subtype MDMA_C10BRUR_DUV_Field is STM32H753.UInt16;

   --  MDMA channel x Block Repeat address Update register
   type MDMA_C10BRUR_Register is record
      --  source adresse update value
      SUV : MDMA_C10BRUR_SUV_Field := 16#0#;
      --  destination address update
      DUV : MDMA_C10BRUR_DUV_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C10BRUR_Register use record
      SUV at 0 range 0 .. 15;
      DUV at 0 range 16 .. 31;
   end record;

   subtype MDMA_C10TBR_TSEL_Field is STM32H753.UInt6;

   --  MDMA channel x Trigger and Bus selection Register
   type MDMA_C10TBR_Register is record
      --  Trigger selection
      TSEL           : MDMA_C10TBR_TSEL_Field := 16#0#;
      --  unspecified
      Reserved_6_15  : STM32H753.UInt10 := 16#0#;
      --  Source BUS select This bit is protected and can be written only if EN
      --  is 0.
      SBUS           : Boolean := False;
      --  Destination BUS slect This bit is protected and can be written only
      --  if EN is 0.
      DBUS           : Boolean := False;
      --  unspecified
      Reserved_18_31 : STM32H753.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C10TBR_Register use record
      TSEL           at 0 range 0 .. 5;
      Reserved_6_15  at 0 range 6 .. 15;
      SBUS           at 0 range 16 .. 16;
      DBUS           at 0 range 17 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   --  MDMA channel x interrupt/status register
   type MDMA_C11ISR_Register is record
      --  Read-only. Channel x transfer error interrupt flag This bit is set by
      --  hardware. It is cleared by software writing 1 to the corresponding
      --  bit in the DMA_IFCRy register.
      TEIF11         : Boolean;
      --  Read-only. Channel x Channel Transfer Complete interrupt flag This
      --  bit is set by hardware. It is cleared by software writing 1 to the
      --  corresponding bit in the DMA_IFCRy register. CTC is set when the last
      --  block was transferred and the channel has been automatically
      --  disabled. CTC is also set when the channel is suspended, as a result
      --  of writing EN bit to 0.
      CTCIF11        : Boolean;
      --  Read-only. Channel x block repeat transfer complete interrupt flag
      --  This bit is set by hardware. It is cleared by software writing 1 to
      --  the corresponding bit in the DMA_IFCRy register.
      BRTIF11        : Boolean;
      --  Read-only. Channel x block transfer complete interrupt flag This bit
      --  is set by hardware. It is cleared by software writing 1 to the
      --  corresponding bit in the DMA_IFCRy register.
      BTIF11         : Boolean;
      --  Read-only. channel x buffer transfer complete
      TCIF11         : Boolean;
      --  unspecified
      Reserved_5_15  : STM32H753.UInt11;
      --  Read-only. channel x request active flag
      CRQA11         : Boolean;
      --  unspecified
      Reserved_17_31 : STM32H753.UInt15;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C11ISR_Register use record
      TEIF11         at 0 range 0 .. 0;
      CTCIF11        at 0 range 1 .. 1;
      BRTIF11        at 0 range 2 .. 2;
      BTIF11         at 0 range 3 .. 3;
      TCIF11         at 0 range 4 .. 4;
      Reserved_5_15  at 0 range 5 .. 15;
      CRQA11         at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   --  MDMA channel x interrupt flag clear register
   type MDMA_C11IFCR_Register is record
      --  Write-only. Channel x clear transfer error interrupt flag Writing a 1
      --  into this bit clears TEIFx in the MDMA_ISRy register
      CTEIF11       : Boolean := False;
      --  Write-only. Clear Channel transfer complete interrupt flag for
      --  channel x Writing a 1 into this bit clears CTCIFx in the MDMA_ISRy
      --  register
      CCTCIF11      : Boolean := False;
      --  Write-only. Channel x clear block repeat transfer complete interrupt
      --  flag Writing a 1 into this bit clears BRTIFx in the MDMA_ISRy
      --  register
      CBRTIF11      : Boolean := False;
      --  Write-only. Channel x Clear block transfer complete interrupt flag
      --  Writing a 1 into this bit clears BTIFx in the MDMA_ISRy register
      CBTIF11       : Boolean := False;
      --  Write-only. CLear buffer Transfer Complete Interrupt Flag for channel
      --  x Writing a 1 into this bit clears TCIFx in the MDMA_ISRy register
      CLTCIF11      : Boolean := False;
      --  unspecified
      Reserved_5_31 : STM32H753.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C11IFCR_Register use record
      CTEIF11       at 0 range 0 .. 0;
      CCTCIF11      at 0 range 1 .. 1;
      CBRTIF11      at 0 range 2 .. 2;
      CBTIF11       at 0 range 3 .. 3;
      CLTCIF11      at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   subtype MDMA_C11ESR_TEA_Field is STM32H753.UInt7;

   --  MDMA Channel x error status register
   type MDMA_C11ESR_Register is record
      --  Read-only. Transfer Error Address These bits are set and cleared by
      --  HW, in case of an MDMA data transfer error. It is used in conjunction
      --  with TED. This field indicates the 7 LSBits of the address which
      --  generated a transfer/access error. It may be used by SW to retrieve
      --  the failing address, by adding this value (truncated to the buffer
      --  transfer length size) to the current SAR/DAR value. Note: The SAR/DAR
      --  current value doesnt reflect this last address due to the FIFO
      --  management system. The SAR/DAR are only updated at the end of a
      --  (buffer) transfer (of TLEN+1 bytes). Note: It is not set in case of a
      --  link data error.
      TEA            : MDMA_C11ESR_TEA_Field;
      --  Read-only. Transfer Error Direction These bit is set and cleared by
      --  HW, in case of an MDMA data transfer error.
      TED            : Boolean;
      --  Read-only. Transfer Error Link Data These bit is set by HW, in case
      --  of a transfer error while reading the block link data structure. It
      --  is cleared by software writing 1 to the CTEIFx bit in the DMA_IFCRy
      --  register.
      TELD           : Boolean;
      --  Read-only. Transfer Error Mask Data These bit is set by HW, in case
      --  of a transfer error while writing the Mask Data. It is cleared by
      --  software writing 1 to the CTEIFx bit in the DMA_IFCRy register.
      TEMD           : Boolean;
      --  Read-only. Address/Size Error These bit is set by HW, when the
      --  programmed address is not aligned with the data size. TED will
      --  indicate whether the problem is on the source or destination. It is
      --  cleared by software writing 1 to the CTEIFx bit in the DMA_IFCRy
      --  register.
      ASE            : Boolean;
      --  Read-only. Block Size Error These bit is set by HW, when the block
      --  size is not an integer multiple of the data size either for source or
      --  destination. TED will indicate whether the problem is on the source
      --  or destination. It is cleared by software writing 1 to the CTEIFx bit
      --  in the DMA_IFCRy register.
      BSE            : Boolean;
      --  unspecified
      Reserved_12_31 : STM32H753.UInt20;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C11ESR_Register use record
      TEA            at 0 range 0 .. 6;
      TED            at 0 range 7 .. 7;
      TELD           at 0 range 8 .. 8;
      TEMD           at 0 range 9 .. 9;
      ASE            at 0 range 10 .. 10;
      BSE            at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype MDMA_C11CR_PL_Field is STM32H753.UInt2;

   --  This register is used to control the concerned channel.
   type MDMA_C11CR_Register is record
      --  channel enable
      EN             : Boolean := False;
      --  Transfer error interrupt enable This bit is set and cleared by
      --  software.
      TEIE           : Boolean := False;
      --  Channel Transfer Complete interrupt enable This bit is set and
      --  cleared by software.
      CTCIE          : Boolean := False;
      --  Block Repeat transfer interrupt enable This bit is set and cleared by
      --  software.
      BRTIE          : Boolean := False;
      --  Block Transfer interrupt enable This bit is set and cleared by
      --  software.
      BTIE           : Boolean := False;
      --  buffer Transfer Complete interrupt enable This bit is set and cleared
      --  by software.
      TCIE           : Boolean := False;
      --  Priority level These bits are set and cleared by software. These bits
      --  are protected and can be written only if EN is 0.
      PL             : MDMA_C11CR_PL_Field := 16#0#;
      --  unspecified
      Reserved_8_11  : STM32H753.UInt4 := 16#0#;
      --  byte Endianness exchange
      BEX            : Boolean := False;
      --  Half word Endianes exchange
      HEX            : Boolean := False;
      --  Word Endianness exchange
      WEX            : Boolean := False;
      --  unspecified
      Reserved_15_15 : STM32H753.Bit := 16#0#;
      --  Write-only. SW ReQuest Writing a 1 into this bit sets the CRQAx in
      --  MDMA_ISRy register, activating the request on Channel x Note: Either
      --  the whole CxCR register or the 8-bit/16-bit register @ Address
      --  offset: 0x4E + 0x40 chn may be used for SWRQ activation. In case of a
      --  SW request, acknowledge is not generated (neither HW signal, nor
      --  CxMAR write access).
      SWRQ           : Boolean := False;
      --  unspecified
      Reserved_17_31 : STM32H753.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C11CR_Register use record
      EN             at 0 range 0 .. 0;
      TEIE           at 0 range 1 .. 1;
      CTCIE          at 0 range 2 .. 2;
      BRTIE          at 0 range 3 .. 3;
      BTIE           at 0 range 4 .. 4;
      TCIE           at 0 range 5 .. 5;
      PL             at 0 range 6 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      BEX            at 0 range 12 .. 12;
      HEX            at 0 range 13 .. 13;
      WEX            at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      SWRQ           at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype MDMA_C11TCR_SINC_Field is STM32H753.UInt2;
   subtype MDMA_C11TCR_DINC_Field is STM32H753.UInt2;
   subtype MDMA_C11TCR_SSIZE_Field is STM32H753.UInt2;
   subtype MDMA_C11TCR_DSIZE_Field is STM32H753.UInt2;
   subtype MDMA_C11TCR_SINCOS_Field is STM32H753.UInt2;
   subtype MDMA_C11TCR_DINCOS_Field is STM32H753.UInt2;
   subtype MDMA_C11TCR_SBURST_Field is STM32H753.UInt3;
   subtype MDMA_C11TCR_DBURST_Field is STM32H753.UInt3;
   subtype MDMA_C11TCR_TLEN_Field is STM32H753.UInt7;
   subtype MDMA_C11TCR_PAM_Field is STM32H753.UInt2;
   subtype MDMA_C11TCR_TRGM_Field is STM32H753.UInt2;

   --  This register is used to configure the concerned channel.
   type MDMA_C11TCR_Register is record
      --  Source increment mode These bits are set and cleared by software.
      --  These bits are protected and can be written only if EN is 0 Note:
      --  When source is AHB (SBUS=1), SINC = 00 is forbidden. In Linked List
      --  Mode, at the end of a block (single or last block in repeated block
      --  transfer mode), this register will be loaded from memory (from
      --  address given by current LAR[31:0] + 0x00).
      SINC   : MDMA_C11TCR_SINC_Field := 16#0#;
      --  Destination increment mode These bits are set and cleared by
      --  software. These bits are protected and can be written only if EN is 0
      --  Note: When destination is AHB (DBUS=1), DINC = 00 is forbidden.
      DINC   : MDMA_C11TCR_DINC_Field := 16#0#;
      --  Source data size These bits are set and cleared by software. These
      --  bits are protected and can be written only if EN is 0 Note: If a
      --  value of 11 is programmed for the TCM access/AHB port, a transfer
      --  error will occur (TEIF bit set) If SINCOS &lt; SSIZE and SINC &#8800;
      --  00, the result will be unpredictable. Note: SSIZE = 11 (double-word)
      --  is forbidden when source is TCM/AHB bus (SBUS=1).
      SSIZE  : MDMA_C11TCR_SSIZE_Field := 16#0#;
      --  Destination data size These bits are set and cleared by software.
      --  These bits are protected and can be written only if EN is 0. Note: If
      --  a value of 11 is programmed for the TCM access/AHB port, a transfer
      --  error will occur (TEIF bit set) If DINCOS &lt; DSIZE and DINC &#8800;
      --  00, the result will be unpredictable. Note: DSIZE = 11 (double-word)
      --  is forbidden when destination is TCM/AHB bus (DBUS=1).
      DSIZE  : MDMA_C11TCR_DSIZE_Field := 16#0#;
      --  source increment offset size
      SINCOS : MDMA_C11TCR_SINCOS_Field := 16#0#;
      --  Destination increment offset
      DINCOS : MDMA_C11TCR_DINCOS_Field := 16#0#;
      --  source burst transfer configuration
      SBURST : MDMA_C11TCR_SBURST_Field := 16#0#;
      --  Destination burst transfer configuration
      DBURST : MDMA_C11TCR_DBURST_Field := 16#0#;
      --  buffer transfer lengh
      TLEN   : MDMA_C11TCR_TLEN_Field := 16#0#;
      --  PacK Enable These bit is set and cleared by software. If the Source
      --  Size is smaller than the destination, it will be padded according to
      --  the PAM value. If the Source data size is larger than the destination
      --  one, it will be truncated. The alignment will be done according to
      --  the PAM[0] value. This bit is protected and can be written only if EN
      --  is 0
      PKE    : Boolean := False;
      --  Padding/Alignement Mode These bits are set and cleared by software.
      --  Case 1: Source data size smaller than destination data size - 3
      --  options are valid. Case 2: Source data size larger than destination
      --  data size. The remainder part is discarded. When PKE = 1 or
      --  DSIZE=SSIZE, these bits are ignored. These bits are protected and can
      --  be written only if EN is 0
      PAM    : MDMA_C11TCR_PAM_Field := 16#0#;
      --  Trigger Mode These bits are set and cleared by software. Note: If
      --  TRGM is 11 for the current block, all the values loaded at the end of
      --  the current block through the linked list mechanism must keep the
      --  same value (TRGM=11) and the same SWRM value, otherwise the result is
      --  undefined. These bits are protected and can be written only if EN is
      --  0.
      TRGM   : MDMA_C11TCR_TRGM_Field := 16#0#;
      --  SW Request Mode This bit is set and cleared by software. If a HW or
      --  SW request is currently active, the bit change will be delayed until
      --  the current transfer is completed. If the CxMAR contains a valid
      --  address, the CxMDR value will also be written @ CxMAR address. This
      --  bit is protected and can be written only if EN is 0.
      SWRM   : Boolean := False;
      --  Bufferable Write Mode This bit is set and cleared by software. This
      --  bit is protected and can be written only if EN is 0. Note: All MDMA
      --  destination accesses are non-cacheable.
      BWM    : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C11TCR_Register use record
      SINC   at 0 range 0 .. 1;
      DINC   at 0 range 2 .. 3;
      SSIZE  at 0 range 4 .. 5;
      DSIZE  at 0 range 6 .. 7;
      SINCOS at 0 range 8 .. 9;
      DINCOS at 0 range 10 .. 11;
      SBURST at 0 range 12 .. 14;
      DBURST at 0 range 15 .. 17;
      TLEN   at 0 range 18 .. 24;
      PKE    at 0 range 25 .. 25;
      PAM    at 0 range 26 .. 27;
      TRGM   at 0 range 28 .. 29;
      SWRM   at 0 range 30 .. 30;
      BWM    at 0 range 31 .. 31;
   end record;

   subtype MDMA_C11BNDTR_BNDT_Field is STM32H753.UInt17;
   subtype MDMA_C11BNDTR_BRC_Field is STM32H753.UInt12;

   --  MDMA Channel x block number of data register
   type MDMA_C11BNDTR_Register is record
      --  block number of data to transfer
      BNDT           : MDMA_C11BNDTR_BNDT_Field := 16#0#;
      --  unspecified
      Reserved_17_17 : STM32H753.Bit := 16#0#;
      --  Block Repeat Source address Update Mode These bits are protected and
      --  can be written only if EN is 0.
      BRSUM          : Boolean := False;
      --  Block Repeat Destination address Update Mode These bits are protected
      --  and can be written only if EN is 0.
      BRDUM          : Boolean := False;
      --  Block Repeat Count This field contains the number of repetitions of
      --  the current block (0 to 4095). When the channel is enabled, this
      --  register is read-only, indicating the remaining number of blocks,
      --  excluding the current one. This register decrements after each
      --  complete block transfer. Once the last block transfer has completed,
      --  this register can either stay at zero or be reloaded automatically
      --  from memory (in Linked List mode - i.e. Link Address valid). These
      --  bits are protected and can be written only if EN is 0.
      BRC            : MDMA_C11BNDTR_BRC_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C11BNDTR_Register use record
      BNDT           at 0 range 0 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      BRSUM          at 0 range 18 .. 18;
      BRDUM          at 0 range 19 .. 19;
      BRC            at 0 range 20 .. 31;
   end record;

   subtype MDMA_C11BRUR_SUV_Field is STM32H753.UInt16;
   subtype MDMA_C11BRUR_DUV_Field is STM32H753.UInt16;

   --  MDMA channel x Block Repeat address Update register
   type MDMA_C11BRUR_Register is record
      --  source adresse update value
      SUV : MDMA_C11BRUR_SUV_Field := 16#0#;
      --  destination address update
      DUV : MDMA_C11BRUR_DUV_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C11BRUR_Register use record
      SUV at 0 range 0 .. 15;
      DUV at 0 range 16 .. 31;
   end record;

   subtype MDMA_C11TBR_TSEL_Field is STM32H753.UInt6;

   --  MDMA channel x Trigger and Bus selection Register
   type MDMA_C11TBR_Register is record
      --  Trigger selection
      TSEL           : MDMA_C11TBR_TSEL_Field := 16#0#;
      --  unspecified
      Reserved_6_15  : STM32H753.UInt10 := 16#0#;
      --  Source BUS select This bit is protected and can be written only if EN
      --  is 0.
      SBUS           : Boolean := False;
      --  Destination BUS slect This bit is protected and can be written only
      --  if EN is 0.
      DBUS           : Boolean := False;
      --  unspecified
      Reserved_18_31 : STM32H753.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C11TBR_Register use record
      TSEL           at 0 range 0 .. 5;
      Reserved_6_15  at 0 range 6 .. 15;
      SBUS           at 0 range 16 .. 16;
      DBUS           at 0 range 17 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   --  MDMA channel x interrupt/status register
   type MDMA_C12ISR_Register is record
      --  Read-only. Channel x transfer error interrupt flag This bit is set by
      --  hardware. It is cleared by software writing 1 to the corresponding
      --  bit in the DMA_IFCRy register.
      TEIF12         : Boolean;
      --  Read-only. Channel x Channel Transfer Complete interrupt flag This
      --  bit is set by hardware. It is cleared by software writing 1 to the
      --  corresponding bit in the DMA_IFCRy register. CTC is set when the last
      --  block was transferred and the channel has been automatically
      --  disabled. CTC is also set when the channel is suspended, as a result
      --  of writing EN bit to 0.
      CTCIF12        : Boolean;
      --  Read-only. Channel x block repeat transfer complete interrupt flag
      --  This bit is set by hardware. It is cleared by software writing 1 to
      --  the corresponding bit in the DMA_IFCRy register.
      BRTIF12        : Boolean;
      --  Read-only. Channel x block transfer complete interrupt flag This bit
      --  is set by hardware. It is cleared by software writing 1 to the
      --  corresponding bit in the DMA_IFCRy register.
      BTIF12         : Boolean;
      --  Read-only. channel x buffer transfer complete
      TCIF12         : Boolean;
      --  unspecified
      Reserved_5_15  : STM32H753.UInt11;
      --  Read-only. channel x request active flag
      CRQA12         : Boolean;
      --  unspecified
      Reserved_17_31 : STM32H753.UInt15;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C12ISR_Register use record
      TEIF12         at 0 range 0 .. 0;
      CTCIF12        at 0 range 1 .. 1;
      BRTIF12        at 0 range 2 .. 2;
      BTIF12         at 0 range 3 .. 3;
      TCIF12         at 0 range 4 .. 4;
      Reserved_5_15  at 0 range 5 .. 15;
      CRQA12         at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   --  MDMA channel x interrupt flag clear register
   type MDMA_C12IFCR_Register is record
      --  Write-only. Channel x clear transfer error interrupt flag Writing a 1
      --  into this bit clears TEIFx in the MDMA_ISRy register
      CTEIF12       : Boolean := False;
      --  Write-only. Clear Channel transfer complete interrupt flag for
      --  channel x Writing a 1 into this bit clears CTCIFx in the MDMA_ISRy
      --  register
      CCTCIF12      : Boolean := False;
      --  Write-only. Channel x clear block repeat transfer complete interrupt
      --  flag Writing a 1 into this bit clears BRTIFx in the MDMA_ISRy
      --  register
      CBRTIF12      : Boolean := False;
      --  Write-only. Channel x Clear block transfer complete interrupt flag
      --  Writing a 1 into this bit clears BTIFx in the MDMA_ISRy register
      CBTIF12       : Boolean := False;
      --  Write-only. CLear buffer Transfer Complete Interrupt Flag for channel
      --  x Writing a 1 into this bit clears TCIFx in the MDMA_ISRy register
      CLTCIF12      : Boolean := False;
      --  unspecified
      Reserved_5_31 : STM32H753.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C12IFCR_Register use record
      CTEIF12       at 0 range 0 .. 0;
      CCTCIF12      at 0 range 1 .. 1;
      CBRTIF12      at 0 range 2 .. 2;
      CBTIF12       at 0 range 3 .. 3;
      CLTCIF12      at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   subtype MDMA_C12ESR_TEA_Field is STM32H753.UInt7;

   --  MDMA Channel x error status register
   type MDMA_C12ESR_Register is record
      --  Read-only. Transfer Error Address These bits are set and cleared by
      --  HW, in case of an MDMA data transfer error. It is used in conjunction
      --  with TED. This field indicates the 7 LSBits of the address which
      --  generated a transfer/access error. It may be used by SW to retrieve
      --  the failing address, by adding this value (truncated to the buffer
      --  transfer length size) to the current SAR/DAR value. Note: The SAR/DAR
      --  current value doesnt reflect this last address due to the FIFO
      --  management system. The SAR/DAR are only updated at the end of a
      --  (buffer) transfer (of TLEN+1 bytes). Note: It is not set in case of a
      --  link data error.
      TEA            : MDMA_C12ESR_TEA_Field;
      --  Read-only. Transfer Error Direction These bit is set and cleared by
      --  HW, in case of an MDMA data transfer error.
      TED            : Boolean;
      --  Read-only. Transfer Error Link Data These bit is set by HW, in case
      --  of a transfer error while reading the block link data structure. It
      --  is cleared by software writing 1 to the CTEIFx bit in the DMA_IFCRy
      --  register.
      TELD           : Boolean;
      --  Read-only. Transfer Error Mask Data These bit is set by HW, in case
      --  of a transfer error while writing the Mask Data. It is cleared by
      --  software writing 1 to the CTEIFx bit in the DMA_IFCRy register.
      TEMD           : Boolean;
      --  Read-only. Address/Size Error These bit is set by HW, when the
      --  programmed address is not aligned with the data size. TED will
      --  indicate whether the problem is on the source or destination. It is
      --  cleared by software writing 1 to the CTEIFx bit in the DMA_IFCRy
      --  register.
      ASE            : Boolean;
      --  Read-only. Block Size Error These bit is set by HW, when the block
      --  size is not an integer multiple of the data size either for source or
      --  destination. TED will indicate whether the problem is on the source
      --  or destination. It is cleared by software writing 1 to the CTEIFx bit
      --  in the DMA_IFCRy register.
      BSE            : Boolean;
      --  unspecified
      Reserved_12_31 : STM32H753.UInt20;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C12ESR_Register use record
      TEA            at 0 range 0 .. 6;
      TED            at 0 range 7 .. 7;
      TELD           at 0 range 8 .. 8;
      TEMD           at 0 range 9 .. 9;
      ASE            at 0 range 10 .. 10;
      BSE            at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype MDMA_C12CR_PL_Field is STM32H753.UInt2;

   --  This register is used to control the concerned channel.
   type MDMA_C12CR_Register is record
      --  channel enable
      EN             : Boolean := False;
      --  Transfer error interrupt enable This bit is set and cleared by
      --  software.
      TEIE           : Boolean := False;
      --  Channel Transfer Complete interrupt enable This bit is set and
      --  cleared by software.
      CTCIE          : Boolean := False;
      --  Block Repeat transfer interrupt enable This bit is set and cleared by
      --  software.
      BRTIE          : Boolean := False;
      --  Block Transfer interrupt enable This bit is set and cleared by
      --  software.
      BTIE           : Boolean := False;
      --  buffer Transfer Complete interrupt enable This bit is set and cleared
      --  by software.
      TCIE           : Boolean := False;
      --  Priority level These bits are set and cleared by software. These bits
      --  are protected and can be written only if EN is 0.
      PL             : MDMA_C12CR_PL_Field := 16#0#;
      --  unspecified
      Reserved_8_11  : STM32H753.UInt4 := 16#0#;
      --  byte Endianness exchange
      BEX            : Boolean := False;
      --  Half word Endianes exchange
      HEX            : Boolean := False;
      --  Word Endianness exchange
      WEX            : Boolean := False;
      --  unspecified
      Reserved_15_15 : STM32H753.Bit := 16#0#;
      --  Write-only. SW ReQuest Writing a 1 into this bit sets the CRQAx in
      --  MDMA_ISRy register, activating the request on Channel x Note: Either
      --  the whole CxCR register or the 8-bit/16-bit register @ Address
      --  offset: 0x4E + 0x40 chn may be used for SWRQ activation. In case of a
      --  SW request, acknowledge is not generated (neither HW signal, nor
      --  CxMAR write access).
      SWRQ           : Boolean := False;
      --  unspecified
      Reserved_17_31 : STM32H753.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C12CR_Register use record
      EN             at 0 range 0 .. 0;
      TEIE           at 0 range 1 .. 1;
      CTCIE          at 0 range 2 .. 2;
      BRTIE          at 0 range 3 .. 3;
      BTIE           at 0 range 4 .. 4;
      TCIE           at 0 range 5 .. 5;
      PL             at 0 range 6 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      BEX            at 0 range 12 .. 12;
      HEX            at 0 range 13 .. 13;
      WEX            at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      SWRQ           at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype MDMA_C12TCR_SINC_Field is STM32H753.UInt2;
   subtype MDMA_C12TCR_DINC_Field is STM32H753.UInt2;
   subtype MDMA_C12TCR_SSIZE_Field is STM32H753.UInt2;
   subtype MDMA_C12TCR_DSIZE_Field is STM32H753.UInt2;
   subtype MDMA_C12TCR_SINCOS_Field is STM32H753.UInt2;
   subtype MDMA_C12TCR_DINCOS_Field is STM32H753.UInt2;
   subtype MDMA_C12TCR_SBURST_Field is STM32H753.UInt3;
   subtype MDMA_C12TCR_DBURST_Field is STM32H753.UInt3;
   subtype MDMA_C12TCR_TLEN_Field is STM32H753.UInt7;
   subtype MDMA_C12TCR_PAM_Field is STM32H753.UInt2;
   subtype MDMA_C12TCR_TRGM_Field is STM32H753.UInt2;

   --  This register is used to configure the concerned channel.
   type MDMA_C12TCR_Register is record
      --  Source increment mode These bits are set and cleared by software.
      --  These bits are protected and can be written only if EN is 0 Note:
      --  When source is AHB (SBUS=1), SINC = 00 is forbidden. In Linked List
      --  Mode, at the end of a block (single or last block in repeated block
      --  transfer mode), this register will be loaded from memory (from
      --  address given by current LAR[31:0] + 0x00).
      SINC   : MDMA_C12TCR_SINC_Field := 16#0#;
      --  Destination increment mode These bits are set and cleared by
      --  software. These bits are protected and can be written only if EN is 0
      --  Note: When destination is AHB (DBUS=1), DINC = 00 is forbidden.
      DINC   : MDMA_C12TCR_DINC_Field := 16#0#;
      --  Source data size These bits are set and cleared by software. These
      --  bits are protected and can be written only if EN is 0 Note: If a
      --  value of 11 is programmed for the TCM access/AHB port, a transfer
      --  error will occur (TEIF bit set) If SINCOS &lt; SSIZE and SINC &#8800;
      --  00, the result will be unpredictable. Note: SSIZE = 11 (double-word)
      --  is forbidden when source is TCM/AHB bus (SBUS=1).
      SSIZE  : MDMA_C12TCR_SSIZE_Field := 16#0#;
      --  Destination data size These bits are set and cleared by software.
      --  These bits are protected and can be written only if EN is 0. Note: If
      --  a value of 11 is programmed for the TCM access/AHB port, a transfer
      --  error will occur (TEIF bit set) If DINCOS &lt; DSIZE and DINC &#8800;
      --  00, the result will be unpredictable. Note: DSIZE = 11 (double-word)
      --  is forbidden when destination is TCM/AHB bus (DBUS=1).
      DSIZE  : MDMA_C12TCR_DSIZE_Field := 16#0#;
      --  source increment offset size
      SINCOS : MDMA_C12TCR_SINCOS_Field := 16#0#;
      --  Destination increment offset
      DINCOS : MDMA_C12TCR_DINCOS_Field := 16#0#;
      --  source burst transfer configuration
      SBURST : MDMA_C12TCR_SBURST_Field := 16#0#;
      --  Destination burst transfer configuration
      DBURST : MDMA_C12TCR_DBURST_Field := 16#0#;
      --  buffer transfer lengh
      TLEN   : MDMA_C12TCR_TLEN_Field := 16#0#;
      --  PacK Enable These bit is set and cleared by software. If the Source
      --  Size is smaller than the destination, it will be padded according to
      --  the PAM value. If the Source data size is larger than the destination
      --  one, it will be truncated. The alignment will be done according to
      --  the PAM[0] value. This bit is protected and can be written only if EN
      --  is 0
      PKE    : Boolean := False;
      --  Padding/Alignement Mode These bits are set and cleared by software.
      --  Case 1: Source data size smaller than destination data size - 3
      --  options are valid. Case 2: Source data size larger than destination
      --  data size. The remainder part is discarded. When PKE = 1 or
      --  DSIZE=SSIZE, these bits are ignored. These bits are protected and can
      --  be written only if EN is 0
      PAM    : MDMA_C12TCR_PAM_Field := 16#0#;
      --  Trigger Mode These bits are set and cleared by software. Note: If
      --  TRGM is 11 for the current block, all the values loaded at the end of
      --  the current block through the linked list mechanism must keep the
      --  same value (TRGM=11) and the same SWRM value, otherwise the result is
      --  undefined. These bits are protected and can be written only if EN is
      --  0.
      TRGM   : MDMA_C12TCR_TRGM_Field := 16#0#;
      --  SW Request Mode This bit is set and cleared by software. If a HW or
      --  SW request is currently active, the bit change will be delayed until
      --  the current transfer is completed. If the CxMAR contains a valid
      --  address, the CxMDR value will also be written @ CxMAR address. This
      --  bit is protected and can be written only if EN is 0.
      SWRM   : Boolean := False;
      --  Bufferable Write Mode This bit is set and cleared by software. This
      --  bit is protected and can be written only if EN is 0. Note: All MDMA
      --  destination accesses are non-cacheable.
      BWM    : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C12TCR_Register use record
      SINC   at 0 range 0 .. 1;
      DINC   at 0 range 2 .. 3;
      SSIZE  at 0 range 4 .. 5;
      DSIZE  at 0 range 6 .. 7;
      SINCOS at 0 range 8 .. 9;
      DINCOS at 0 range 10 .. 11;
      SBURST at 0 range 12 .. 14;
      DBURST at 0 range 15 .. 17;
      TLEN   at 0 range 18 .. 24;
      PKE    at 0 range 25 .. 25;
      PAM    at 0 range 26 .. 27;
      TRGM   at 0 range 28 .. 29;
      SWRM   at 0 range 30 .. 30;
      BWM    at 0 range 31 .. 31;
   end record;

   subtype MDMA_C12BNDTR_BNDT_Field is STM32H753.UInt17;
   subtype MDMA_C12BNDTR_BRC_Field is STM32H753.UInt12;

   --  MDMA Channel x block number of data register
   type MDMA_C12BNDTR_Register is record
      --  block number of data to transfer
      BNDT           : MDMA_C12BNDTR_BNDT_Field := 16#0#;
      --  unspecified
      Reserved_17_17 : STM32H753.Bit := 16#0#;
      --  Block Repeat Source address Update Mode These bits are protected and
      --  can be written only if EN is 0.
      BRSUM          : Boolean := False;
      --  Block Repeat Destination address Update Mode These bits are protected
      --  and can be written only if EN is 0.
      BRDUM          : Boolean := False;
      --  Block Repeat Count This field contains the number of repetitions of
      --  the current block (0 to 4095). When the channel is enabled, this
      --  register is read-only, indicating the remaining number of blocks,
      --  excluding the current one. This register decrements after each
      --  complete block transfer. Once the last block transfer has completed,
      --  this register can either stay at zero or be reloaded automatically
      --  from memory (in Linked List mode - i.e. Link Address valid). These
      --  bits are protected and can be written only if EN is 0.
      BRC            : MDMA_C12BNDTR_BRC_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C12BNDTR_Register use record
      BNDT           at 0 range 0 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      BRSUM          at 0 range 18 .. 18;
      BRDUM          at 0 range 19 .. 19;
      BRC            at 0 range 20 .. 31;
   end record;

   subtype MDMA_C12BRUR_SUV_Field is STM32H753.UInt16;
   subtype MDMA_C12BRUR_DUV_Field is STM32H753.UInt16;

   --  MDMA channel x Block Repeat address Update register
   type MDMA_C12BRUR_Register is record
      --  source adresse update value
      SUV : MDMA_C12BRUR_SUV_Field := 16#0#;
      --  destination address update
      DUV : MDMA_C12BRUR_DUV_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C12BRUR_Register use record
      SUV at 0 range 0 .. 15;
      DUV at 0 range 16 .. 31;
   end record;

   subtype MDMA_C12TBR_TSEL_Field is STM32H753.UInt6;

   --  MDMA channel x Trigger and Bus selection Register
   type MDMA_C12TBR_Register is record
      --  Trigger selection
      TSEL           : MDMA_C12TBR_TSEL_Field := 16#0#;
      --  unspecified
      Reserved_6_15  : STM32H753.UInt10 := 16#0#;
      --  Source BUS select This bit is protected and can be written only if EN
      --  is 0.
      SBUS           : Boolean := False;
      --  Destination BUS slect This bit is protected and can be written only
      --  if EN is 0.
      DBUS           : Boolean := False;
      --  unspecified
      Reserved_18_31 : STM32H753.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C12TBR_Register use record
      TSEL           at 0 range 0 .. 5;
      Reserved_6_15  at 0 range 6 .. 15;
      SBUS           at 0 range 16 .. 16;
      DBUS           at 0 range 17 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   --  MDMA channel x interrupt/status register
   type MDMA_C13ISR_Register is record
      --  Read-only. Channel x transfer error interrupt flag This bit is set by
      --  hardware. It is cleared by software writing 1 to the corresponding
      --  bit in the DMA_IFCRy register.
      TEIF13         : Boolean;
      --  Read-only. Channel x Channel Transfer Complete interrupt flag This
      --  bit is set by hardware. It is cleared by software writing 1 to the
      --  corresponding bit in the DMA_IFCRy register. CTC is set when the last
      --  block was transferred and the channel has been automatically
      --  disabled. CTC is also set when the channel is suspended, as a result
      --  of writing EN bit to 0.
      CTCIF13        : Boolean;
      --  Read-only. Channel x block repeat transfer complete interrupt flag
      --  This bit is set by hardware. It is cleared by software writing 1 to
      --  the corresponding bit in the DMA_IFCRy register.
      BRTIF13        : Boolean;
      --  Read-only. Channel x block transfer complete interrupt flag This bit
      --  is set by hardware. It is cleared by software writing 1 to the
      --  corresponding bit in the DMA_IFCRy register.
      BTIF13         : Boolean;
      --  Read-only. channel x buffer transfer complete
      TCIF13         : Boolean;
      --  unspecified
      Reserved_5_15  : STM32H753.UInt11;
      --  Read-only. channel x request active flag
      CRQA13         : Boolean;
      --  unspecified
      Reserved_17_31 : STM32H753.UInt15;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C13ISR_Register use record
      TEIF13         at 0 range 0 .. 0;
      CTCIF13        at 0 range 1 .. 1;
      BRTIF13        at 0 range 2 .. 2;
      BTIF13         at 0 range 3 .. 3;
      TCIF13         at 0 range 4 .. 4;
      Reserved_5_15  at 0 range 5 .. 15;
      CRQA13         at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   --  MDMA channel x interrupt flag clear register
   type MDMA_C13IFCR_Register is record
      --  Write-only. Channel x clear transfer error interrupt flag Writing a 1
      --  into this bit clears TEIFx in the MDMA_ISRy register
      CTEIF13       : Boolean := False;
      --  Write-only. Clear Channel transfer complete interrupt flag for
      --  channel x Writing a 1 into this bit clears CTCIFx in the MDMA_ISRy
      --  register
      CCTCIF13      : Boolean := False;
      --  Write-only. Channel x clear block repeat transfer complete interrupt
      --  flag Writing a 1 into this bit clears BRTIFx in the MDMA_ISRy
      --  register
      CBRTIF13      : Boolean := False;
      --  Write-only. Channel x Clear block transfer complete interrupt flag
      --  Writing a 1 into this bit clears BTIFx in the MDMA_ISRy register
      CBTIF13       : Boolean := False;
      --  Write-only. CLear buffer Transfer Complete Interrupt Flag for channel
      --  x Writing a 1 into this bit clears TCIFx in the MDMA_ISRy register
      CLTCIF13      : Boolean := False;
      --  unspecified
      Reserved_5_31 : STM32H753.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C13IFCR_Register use record
      CTEIF13       at 0 range 0 .. 0;
      CCTCIF13      at 0 range 1 .. 1;
      CBRTIF13      at 0 range 2 .. 2;
      CBTIF13       at 0 range 3 .. 3;
      CLTCIF13      at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   subtype MDMA_C13ESR_TEA_Field is STM32H753.UInt7;

   --  MDMA Channel x error status register
   type MDMA_C13ESR_Register is record
      --  Read-only. Transfer Error Address These bits are set and cleared by
      --  HW, in case of an MDMA data transfer error. It is used in conjunction
      --  with TED. This field indicates the 7 LSBits of the address which
      --  generated a transfer/access error. It may be used by SW to retrieve
      --  the failing address, by adding this value (truncated to the buffer
      --  transfer length size) to the current SAR/DAR value. Note: The SAR/DAR
      --  current value doesnt reflect this last address due to the FIFO
      --  management system. The SAR/DAR are only updated at the end of a
      --  (buffer) transfer (of TLEN+1 bytes). Note: It is not set in case of a
      --  link data error.
      TEA            : MDMA_C13ESR_TEA_Field;
      --  Read-only. Transfer Error Direction These bit is set and cleared by
      --  HW, in case of an MDMA data transfer error.
      TED            : Boolean;
      --  Read-only. Transfer Error Link Data These bit is set by HW, in case
      --  of a transfer error while reading the block link data structure. It
      --  is cleared by software writing 1 to the CTEIFx bit in the DMA_IFCRy
      --  register.
      TELD           : Boolean;
      --  Read-only. Transfer Error Mask Data These bit is set by HW, in case
      --  of a transfer error while writing the Mask Data. It is cleared by
      --  software writing 1 to the CTEIFx bit in the DMA_IFCRy register.
      TEMD           : Boolean;
      --  Read-only. Address/Size Error These bit is set by HW, when the
      --  programmed address is not aligned with the data size. TED will
      --  indicate whether the problem is on the source or destination. It is
      --  cleared by software writing 1 to the CTEIFx bit in the DMA_IFCRy
      --  register.
      ASE            : Boolean;
      --  Read-only. Block Size Error These bit is set by HW, when the block
      --  size is not an integer multiple of the data size either for source or
      --  destination. TED will indicate whether the problem is on the source
      --  or destination. It is cleared by software writing 1 to the CTEIFx bit
      --  in the DMA_IFCRy register.
      BSE            : Boolean;
      --  unspecified
      Reserved_12_31 : STM32H753.UInt20;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C13ESR_Register use record
      TEA            at 0 range 0 .. 6;
      TED            at 0 range 7 .. 7;
      TELD           at 0 range 8 .. 8;
      TEMD           at 0 range 9 .. 9;
      ASE            at 0 range 10 .. 10;
      BSE            at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype MDMA_C13CR_PL_Field is STM32H753.UInt2;

   --  This register is used to control the concerned channel.
   type MDMA_C13CR_Register is record
      --  channel enable
      EN             : Boolean := False;
      --  Transfer error interrupt enable This bit is set and cleared by
      --  software.
      TEIE           : Boolean := False;
      --  Channel Transfer Complete interrupt enable This bit is set and
      --  cleared by software.
      CTCIE          : Boolean := False;
      --  Block Repeat transfer interrupt enable This bit is set and cleared by
      --  software.
      BRTIE          : Boolean := False;
      --  Block Transfer interrupt enable This bit is set and cleared by
      --  software.
      BTIE           : Boolean := False;
      --  buffer Transfer Complete interrupt enable This bit is set and cleared
      --  by software.
      TCIE           : Boolean := False;
      --  Priority level These bits are set and cleared by software. These bits
      --  are protected and can be written only if EN is 0.
      PL             : MDMA_C13CR_PL_Field := 16#0#;
      --  unspecified
      Reserved_8_11  : STM32H753.UInt4 := 16#0#;
      --  byte Endianness exchange
      BEX            : Boolean := False;
      --  Half word Endianes exchange
      HEX            : Boolean := False;
      --  Word Endianness exchange
      WEX            : Boolean := False;
      --  unspecified
      Reserved_15_15 : STM32H753.Bit := 16#0#;
      --  Write-only. SW ReQuest Writing a 1 into this bit sets the CRQAx in
      --  MDMA_ISRy register, activating the request on Channel x Note: Either
      --  the whole CxCR register or the 8-bit/16-bit register @ Address
      --  offset: 0x4E + 0x40 chn may be used for SWRQ activation. In case of a
      --  SW request, acknowledge is not generated (neither HW signal, nor
      --  CxMAR write access).
      SWRQ           : Boolean := False;
      --  unspecified
      Reserved_17_31 : STM32H753.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C13CR_Register use record
      EN             at 0 range 0 .. 0;
      TEIE           at 0 range 1 .. 1;
      CTCIE          at 0 range 2 .. 2;
      BRTIE          at 0 range 3 .. 3;
      BTIE           at 0 range 4 .. 4;
      TCIE           at 0 range 5 .. 5;
      PL             at 0 range 6 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      BEX            at 0 range 12 .. 12;
      HEX            at 0 range 13 .. 13;
      WEX            at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      SWRQ           at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype MDMA_C13TCR_SINC_Field is STM32H753.UInt2;
   subtype MDMA_C13TCR_DINC_Field is STM32H753.UInt2;
   subtype MDMA_C13TCR_SSIZE_Field is STM32H753.UInt2;
   subtype MDMA_C13TCR_DSIZE_Field is STM32H753.UInt2;
   subtype MDMA_C13TCR_SINCOS_Field is STM32H753.UInt2;
   subtype MDMA_C13TCR_DINCOS_Field is STM32H753.UInt2;
   subtype MDMA_C13TCR_SBURST_Field is STM32H753.UInt3;
   subtype MDMA_C13TCR_DBURST_Field is STM32H753.UInt3;
   subtype MDMA_C13TCR_TLEN_Field is STM32H753.UInt7;
   subtype MDMA_C13TCR_PAM_Field is STM32H753.UInt2;
   subtype MDMA_C13TCR_TRGM_Field is STM32H753.UInt2;

   --  This register is used to configure the concerned channel.
   type MDMA_C13TCR_Register is record
      --  Source increment mode These bits are set and cleared by software.
      --  These bits are protected and can be written only if EN is 0 Note:
      --  When source is AHB (SBUS=1), SINC = 00 is forbidden. In Linked List
      --  Mode, at the end of a block (single or last block in repeated block
      --  transfer mode), this register will be loaded from memory (from
      --  address given by current LAR[31:0] + 0x00).
      SINC   : MDMA_C13TCR_SINC_Field := 16#0#;
      --  Destination increment mode These bits are set and cleared by
      --  software. These bits are protected and can be written only if EN is 0
      --  Note: When destination is AHB (DBUS=1), DINC = 00 is forbidden.
      DINC   : MDMA_C13TCR_DINC_Field := 16#0#;
      --  Source data size These bits are set and cleared by software. These
      --  bits are protected and can be written only if EN is 0 Note: If a
      --  value of 11 is programmed for the TCM access/AHB port, a transfer
      --  error will occur (TEIF bit set) If SINCOS &lt; SSIZE and SINC &#8800;
      --  00, the result will be unpredictable. Note: SSIZE = 11 (double-word)
      --  is forbidden when source is TCM/AHB bus (SBUS=1).
      SSIZE  : MDMA_C13TCR_SSIZE_Field := 16#0#;
      --  Destination data size These bits are set and cleared by software.
      --  These bits are protected and can be written only if EN is 0. Note: If
      --  a value of 11 is programmed for the TCM access/AHB port, a transfer
      --  error will occur (TEIF bit set) If DINCOS &lt; DSIZE and DINC &#8800;
      --  00, the result will be unpredictable. Note: DSIZE = 11 (double-word)
      --  is forbidden when destination is TCM/AHB bus (DBUS=1).
      DSIZE  : MDMA_C13TCR_DSIZE_Field := 16#0#;
      --  source increment offset size
      SINCOS : MDMA_C13TCR_SINCOS_Field := 16#0#;
      --  Destination increment offset
      DINCOS : MDMA_C13TCR_DINCOS_Field := 16#0#;
      --  source burst transfer configuration
      SBURST : MDMA_C13TCR_SBURST_Field := 16#0#;
      --  Destination burst transfer configuration
      DBURST : MDMA_C13TCR_DBURST_Field := 16#0#;
      --  buffer transfer lengh
      TLEN   : MDMA_C13TCR_TLEN_Field := 16#0#;
      --  PacK Enable These bit is set and cleared by software. If the Source
      --  Size is smaller than the destination, it will be padded according to
      --  the PAM value. If the Source data size is larger than the destination
      --  one, it will be truncated. The alignment will be done according to
      --  the PAM[0] value. This bit is protected and can be written only if EN
      --  is 0
      PKE    : Boolean := False;
      --  Padding/Alignement Mode These bits are set and cleared by software.
      --  Case 1: Source data size smaller than destination data size - 3
      --  options are valid. Case 2: Source data size larger than destination
      --  data size. The remainder part is discarded. When PKE = 1 or
      --  DSIZE=SSIZE, these bits are ignored. These bits are protected and can
      --  be written only if EN is 0
      PAM    : MDMA_C13TCR_PAM_Field := 16#0#;
      --  Trigger Mode These bits are set and cleared by software. Note: If
      --  TRGM is 11 for the current block, all the values loaded at the end of
      --  the current block through the linked list mechanism must keep the
      --  same value (TRGM=11) and the same SWRM value, otherwise the result is
      --  undefined. These bits are protected and can be written only if EN is
      --  0.
      TRGM   : MDMA_C13TCR_TRGM_Field := 16#0#;
      --  SW Request Mode This bit is set and cleared by software. If a HW or
      --  SW request is currently active, the bit change will be delayed until
      --  the current transfer is completed. If the CxMAR contains a valid
      --  address, the CxMDR value will also be written @ CxMAR address. This
      --  bit is protected and can be written only if EN is 0.
      SWRM   : Boolean := False;
      --  Bufferable Write Mode This bit is set and cleared by software. This
      --  bit is protected and can be written only if EN is 0. Note: All MDMA
      --  destination accesses are non-cacheable.
      BWM    : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C13TCR_Register use record
      SINC   at 0 range 0 .. 1;
      DINC   at 0 range 2 .. 3;
      SSIZE  at 0 range 4 .. 5;
      DSIZE  at 0 range 6 .. 7;
      SINCOS at 0 range 8 .. 9;
      DINCOS at 0 range 10 .. 11;
      SBURST at 0 range 12 .. 14;
      DBURST at 0 range 15 .. 17;
      TLEN   at 0 range 18 .. 24;
      PKE    at 0 range 25 .. 25;
      PAM    at 0 range 26 .. 27;
      TRGM   at 0 range 28 .. 29;
      SWRM   at 0 range 30 .. 30;
      BWM    at 0 range 31 .. 31;
   end record;

   subtype MDMA_C13BNDTR_BNDT_Field is STM32H753.UInt17;
   subtype MDMA_C13BNDTR_BRC_Field is STM32H753.UInt12;

   --  MDMA Channel x block number of data register
   type MDMA_C13BNDTR_Register is record
      --  block number of data to transfer
      BNDT           : MDMA_C13BNDTR_BNDT_Field := 16#0#;
      --  unspecified
      Reserved_17_17 : STM32H753.Bit := 16#0#;
      --  Block Repeat Source address Update Mode These bits are protected and
      --  can be written only if EN is 0.
      BRSUM          : Boolean := False;
      --  Block Repeat Destination address Update Mode These bits are protected
      --  and can be written only if EN is 0.
      BRDUM          : Boolean := False;
      --  Block Repeat Count This field contains the number of repetitions of
      --  the current block (0 to 4095). When the channel is enabled, this
      --  register is read-only, indicating the remaining number of blocks,
      --  excluding the current one. This register decrements after each
      --  complete block transfer. Once the last block transfer has completed,
      --  this register can either stay at zero or be reloaded automatically
      --  from memory (in Linked List mode - i.e. Link Address valid). These
      --  bits are protected and can be written only if EN is 0.
      BRC            : MDMA_C13BNDTR_BRC_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C13BNDTR_Register use record
      BNDT           at 0 range 0 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      BRSUM          at 0 range 18 .. 18;
      BRDUM          at 0 range 19 .. 19;
      BRC            at 0 range 20 .. 31;
   end record;

   subtype MDMA_C13BRUR_SUV_Field is STM32H753.UInt16;
   subtype MDMA_C13BRUR_DUV_Field is STM32H753.UInt16;

   --  MDMA channel x Block Repeat address Update register
   type MDMA_C13BRUR_Register is record
      --  source adresse update value
      SUV : MDMA_C13BRUR_SUV_Field := 16#0#;
      --  destination address update
      DUV : MDMA_C13BRUR_DUV_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C13BRUR_Register use record
      SUV at 0 range 0 .. 15;
      DUV at 0 range 16 .. 31;
   end record;

   subtype MDMA_C13TBR_TSEL_Field is STM32H753.UInt6;

   --  MDMA channel x Trigger and Bus selection Register
   type MDMA_C13TBR_Register is record
      --  Trigger selection
      TSEL           : MDMA_C13TBR_TSEL_Field := 16#0#;
      --  unspecified
      Reserved_6_15  : STM32H753.UInt10 := 16#0#;
      --  Source BUS select This bit is protected and can be written only if EN
      --  is 0.
      SBUS           : Boolean := False;
      --  Destination BUS slect This bit is protected and can be written only
      --  if EN is 0.
      DBUS           : Boolean := False;
      --  unspecified
      Reserved_18_31 : STM32H753.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C13TBR_Register use record
      TSEL           at 0 range 0 .. 5;
      Reserved_6_15  at 0 range 6 .. 15;
      SBUS           at 0 range 16 .. 16;
      DBUS           at 0 range 17 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   --  MDMA channel x interrupt/status register
   type MDMA_C14ISR_Register is record
      --  Read-only. Channel x transfer error interrupt flag This bit is set by
      --  hardware. It is cleared by software writing 1 to the corresponding
      --  bit in the DMA_IFCRy register.
      TEIF14         : Boolean;
      --  Read-only. Channel x Channel Transfer Complete interrupt flag This
      --  bit is set by hardware. It is cleared by software writing 1 to the
      --  corresponding bit in the DMA_IFCRy register. CTC is set when the last
      --  block was transferred and the channel has been automatically
      --  disabled. CTC is also set when the channel is suspended, as a result
      --  of writing EN bit to 0.
      CTCIF14        : Boolean;
      --  Read-only. Channel x block repeat transfer complete interrupt flag
      --  This bit is set by hardware. It is cleared by software writing 1 to
      --  the corresponding bit in the DMA_IFCRy register.
      BRTIF14        : Boolean;
      --  Read-only. Channel x block transfer complete interrupt flag This bit
      --  is set by hardware. It is cleared by software writing 1 to the
      --  corresponding bit in the DMA_IFCRy register.
      BTIF14         : Boolean;
      --  Read-only. channel x buffer transfer complete
      TCIF14         : Boolean;
      --  unspecified
      Reserved_5_15  : STM32H753.UInt11;
      --  Read-only. channel x request active flag
      CRQA14         : Boolean;
      --  unspecified
      Reserved_17_31 : STM32H753.UInt15;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C14ISR_Register use record
      TEIF14         at 0 range 0 .. 0;
      CTCIF14        at 0 range 1 .. 1;
      BRTIF14        at 0 range 2 .. 2;
      BTIF14         at 0 range 3 .. 3;
      TCIF14         at 0 range 4 .. 4;
      Reserved_5_15  at 0 range 5 .. 15;
      CRQA14         at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   --  MDMA channel x interrupt flag clear register
   type MDMA_C14IFCR_Register is record
      --  Write-only. Channel x clear transfer error interrupt flag Writing a 1
      --  into this bit clears TEIFx in the MDMA_ISRy register
      CTEIF14       : Boolean := False;
      --  Write-only. Clear Channel transfer complete interrupt flag for
      --  channel x Writing a 1 into this bit clears CTCIFx in the MDMA_ISRy
      --  register
      CCTCIF14      : Boolean := False;
      --  Write-only. Channel x clear block repeat transfer complete interrupt
      --  flag Writing a 1 into this bit clears BRTIFx in the MDMA_ISRy
      --  register
      CBRTIF14      : Boolean := False;
      --  Write-only. Channel x Clear block transfer complete interrupt flag
      --  Writing a 1 into this bit clears BTIFx in the MDMA_ISRy register
      CBTIF14       : Boolean := False;
      --  Write-only. CLear buffer Transfer Complete Interrupt Flag for channel
      --  x Writing a 1 into this bit clears TCIFx in the MDMA_ISRy register
      CLTCIF14      : Boolean := False;
      --  unspecified
      Reserved_5_31 : STM32H753.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C14IFCR_Register use record
      CTEIF14       at 0 range 0 .. 0;
      CCTCIF14      at 0 range 1 .. 1;
      CBRTIF14      at 0 range 2 .. 2;
      CBTIF14       at 0 range 3 .. 3;
      CLTCIF14      at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   subtype MDMA_C14ESR_TEA_Field is STM32H753.UInt7;

   --  MDMA Channel x error status register
   type MDMA_C14ESR_Register is record
      --  Read-only. Transfer Error Address These bits are set and cleared by
      --  HW, in case of an MDMA data transfer error. It is used in conjunction
      --  with TED. This field indicates the 7 LSBits of the address which
      --  generated a transfer/access error. It may be used by SW to retrieve
      --  the failing address, by adding this value (truncated to the buffer
      --  transfer length size) to the current SAR/DAR value. Note: The SAR/DAR
      --  current value doesnt reflect this last address due to the FIFO
      --  management system. The SAR/DAR are only updated at the end of a
      --  (buffer) transfer (of TLEN+1 bytes). Note: It is not set in case of a
      --  link data error.
      TEA            : MDMA_C14ESR_TEA_Field;
      --  Read-only. Transfer Error Direction These bit is set and cleared by
      --  HW, in case of an MDMA data transfer error.
      TED            : Boolean;
      --  Read-only. Transfer Error Link Data These bit is set by HW, in case
      --  of a transfer error while reading the block link data structure. It
      --  is cleared by software writing 1 to the CTEIFx bit in the DMA_IFCRy
      --  register.
      TELD           : Boolean;
      --  Read-only. Transfer Error Mask Data These bit is set by HW, in case
      --  of a transfer error while writing the Mask Data. It is cleared by
      --  software writing 1 to the CTEIFx bit in the DMA_IFCRy register.
      TEMD           : Boolean;
      --  Read-only. Address/Size Error These bit is set by HW, when the
      --  programmed address is not aligned with the data size. TED will
      --  indicate whether the problem is on the source or destination. It is
      --  cleared by software writing 1 to the CTEIFx bit in the DMA_IFCRy
      --  register.
      ASE            : Boolean;
      --  Read-only. Block Size Error These bit is set by HW, when the block
      --  size is not an integer multiple of the data size either for source or
      --  destination. TED will indicate whether the problem is on the source
      --  or destination. It is cleared by software writing 1 to the CTEIFx bit
      --  in the DMA_IFCRy register.
      BSE            : Boolean;
      --  unspecified
      Reserved_12_31 : STM32H753.UInt20;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C14ESR_Register use record
      TEA            at 0 range 0 .. 6;
      TED            at 0 range 7 .. 7;
      TELD           at 0 range 8 .. 8;
      TEMD           at 0 range 9 .. 9;
      ASE            at 0 range 10 .. 10;
      BSE            at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype MDMA_C14CR_PL_Field is STM32H753.UInt2;

   --  This register is used to control the concerned channel.
   type MDMA_C14CR_Register is record
      --  channel enable
      EN             : Boolean := False;
      --  Transfer error interrupt enable This bit is set and cleared by
      --  software.
      TEIE           : Boolean := False;
      --  Channel Transfer Complete interrupt enable This bit is set and
      --  cleared by software.
      CTCIE          : Boolean := False;
      --  Block Repeat transfer interrupt enable This bit is set and cleared by
      --  software.
      BRTIE          : Boolean := False;
      --  Block Transfer interrupt enable This bit is set and cleared by
      --  software.
      BTIE           : Boolean := False;
      --  buffer Transfer Complete interrupt enable This bit is set and cleared
      --  by software.
      TCIE           : Boolean := False;
      --  Priority level These bits are set and cleared by software. These bits
      --  are protected and can be written only if EN is 0.
      PL             : MDMA_C14CR_PL_Field := 16#0#;
      --  unspecified
      Reserved_8_11  : STM32H753.UInt4 := 16#0#;
      --  byte Endianness exchange
      BEX            : Boolean := False;
      --  Half word Endianes exchange
      HEX            : Boolean := False;
      --  Word Endianness exchange
      WEX            : Boolean := False;
      --  unspecified
      Reserved_15_15 : STM32H753.Bit := 16#0#;
      --  Write-only. SW ReQuest Writing a 1 into this bit sets the CRQAx in
      --  MDMA_ISRy register, activating the request on Channel x Note: Either
      --  the whole CxCR register or the 8-bit/16-bit register @ Address
      --  offset: 0x4E + 0x40 chn may be used for SWRQ activation. In case of a
      --  SW request, acknowledge is not generated (neither HW signal, nor
      --  CxMAR write access).
      SWRQ           : Boolean := False;
      --  unspecified
      Reserved_17_31 : STM32H753.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C14CR_Register use record
      EN             at 0 range 0 .. 0;
      TEIE           at 0 range 1 .. 1;
      CTCIE          at 0 range 2 .. 2;
      BRTIE          at 0 range 3 .. 3;
      BTIE           at 0 range 4 .. 4;
      TCIE           at 0 range 5 .. 5;
      PL             at 0 range 6 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      BEX            at 0 range 12 .. 12;
      HEX            at 0 range 13 .. 13;
      WEX            at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      SWRQ           at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype MDMA_C14TCR_SINC_Field is STM32H753.UInt2;
   subtype MDMA_C14TCR_DINC_Field is STM32H753.UInt2;
   subtype MDMA_C14TCR_SSIZE_Field is STM32H753.UInt2;
   subtype MDMA_C14TCR_DSIZE_Field is STM32H753.UInt2;
   subtype MDMA_C14TCR_SINCOS_Field is STM32H753.UInt2;
   subtype MDMA_C14TCR_DINCOS_Field is STM32H753.UInt2;
   subtype MDMA_C14TCR_SBURST_Field is STM32H753.UInt3;
   subtype MDMA_C14TCR_DBURST_Field is STM32H753.UInt3;
   subtype MDMA_C14TCR_TLEN_Field is STM32H753.UInt7;
   subtype MDMA_C14TCR_PAM_Field is STM32H753.UInt2;
   subtype MDMA_C14TCR_TRGM_Field is STM32H753.UInt2;

   --  This register is used to configure the concerned channel.
   type MDMA_C14TCR_Register is record
      --  Source increment mode These bits are set and cleared by software.
      --  These bits are protected and can be written only if EN is 0 Note:
      --  When source is AHB (SBUS=1), SINC = 00 is forbidden. In Linked List
      --  Mode, at the end of a block (single or last block in repeated block
      --  transfer mode), this register will be loaded from memory (from
      --  address given by current LAR[31:0] + 0x00).
      SINC   : MDMA_C14TCR_SINC_Field := 16#0#;
      --  Destination increment mode These bits are set and cleared by
      --  software. These bits are protected and can be written only if EN is 0
      --  Note: When destination is AHB (DBUS=1), DINC = 00 is forbidden.
      DINC   : MDMA_C14TCR_DINC_Field := 16#0#;
      --  Source data size These bits are set and cleared by software. These
      --  bits are protected and can be written only if EN is 0 Note: If a
      --  value of 11 is programmed for the TCM access/AHB port, a transfer
      --  error will occur (TEIF bit set) If SINCOS &lt; SSIZE and SINC &#8800;
      --  00, the result will be unpredictable. Note: SSIZE = 11 (double-word)
      --  is forbidden when source is TCM/AHB bus (SBUS=1).
      SSIZE  : MDMA_C14TCR_SSIZE_Field := 16#0#;
      --  Destination data size These bits are set and cleared by software.
      --  These bits are protected and can be written only if EN is 0. Note: If
      --  a value of 11 is programmed for the TCM access/AHB port, a transfer
      --  error will occur (TEIF bit set) If DINCOS &lt; DSIZE and DINC &#8800;
      --  00, the result will be unpredictable. Note: DSIZE = 11 (double-word)
      --  is forbidden when destination is TCM/AHB bus (DBUS=1).
      DSIZE  : MDMA_C14TCR_DSIZE_Field := 16#0#;
      --  source increment offset size
      SINCOS : MDMA_C14TCR_SINCOS_Field := 16#0#;
      --  Destination increment offset
      DINCOS : MDMA_C14TCR_DINCOS_Field := 16#0#;
      --  source burst transfer configuration
      SBURST : MDMA_C14TCR_SBURST_Field := 16#0#;
      --  Destination burst transfer configuration
      DBURST : MDMA_C14TCR_DBURST_Field := 16#0#;
      --  buffer transfer lengh
      TLEN   : MDMA_C14TCR_TLEN_Field := 16#0#;
      --  PacK Enable These bit is set and cleared by software. If the Source
      --  Size is smaller than the destination, it will be padded according to
      --  the PAM value. If the Source data size is larger than the destination
      --  one, it will be truncated. The alignment will be done according to
      --  the PAM[0] value. This bit is protected and can be written only if EN
      --  is 0
      PKE    : Boolean := False;
      --  Padding/Alignement Mode These bits are set and cleared by software.
      --  Case 1: Source data size smaller than destination data size - 3
      --  options are valid. Case 2: Source data size larger than destination
      --  data size. The remainder part is discarded. When PKE = 1 or
      --  DSIZE=SSIZE, these bits are ignored. These bits are protected and can
      --  be written only if EN is 0
      PAM    : MDMA_C14TCR_PAM_Field := 16#0#;
      --  Trigger Mode These bits are set and cleared by software. Note: If
      --  TRGM is 11 for the current block, all the values loaded at the end of
      --  the current block through the linked list mechanism must keep the
      --  same value (TRGM=11) and the same SWRM value, otherwise the result is
      --  undefined. These bits are protected and can be written only if EN is
      --  0.
      TRGM   : MDMA_C14TCR_TRGM_Field := 16#0#;
      --  SW Request Mode This bit is set and cleared by software. If a HW or
      --  SW request is currently active, the bit change will be delayed until
      --  the current transfer is completed. If the CxMAR contains a valid
      --  address, the CxMDR value will also be written @ CxMAR address. This
      --  bit is protected and can be written only if EN is 0.
      SWRM   : Boolean := False;
      --  Bufferable Write Mode This bit is set and cleared by software. This
      --  bit is protected and can be written only if EN is 0. Note: All MDMA
      --  destination accesses are non-cacheable.
      BWM    : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C14TCR_Register use record
      SINC   at 0 range 0 .. 1;
      DINC   at 0 range 2 .. 3;
      SSIZE  at 0 range 4 .. 5;
      DSIZE  at 0 range 6 .. 7;
      SINCOS at 0 range 8 .. 9;
      DINCOS at 0 range 10 .. 11;
      SBURST at 0 range 12 .. 14;
      DBURST at 0 range 15 .. 17;
      TLEN   at 0 range 18 .. 24;
      PKE    at 0 range 25 .. 25;
      PAM    at 0 range 26 .. 27;
      TRGM   at 0 range 28 .. 29;
      SWRM   at 0 range 30 .. 30;
      BWM    at 0 range 31 .. 31;
   end record;

   subtype MDMA_C14BNDTR_BNDT_Field is STM32H753.UInt17;
   subtype MDMA_C14BNDTR_BRC_Field is STM32H753.UInt12;

   --  MDMA Channel x block number of data register
   type MDMA_C14BNDTR_Register is record
      --  block number of data to transfer
      BNDT           : MDMA_C14BNDTR_BNDT_Field := 16#0#;
      --  unspecified
      Reserved_17_17 : STM32H753.Bit := 16#0#;
      --  Block Repeat Source address Update Mode These bits are protected and
      --  can be written only if EN is 0.
      BRSUM          : Boolean := False;
      --  Block Repeat Destination address Update Mode These bits are protected
      --  and can be written only if EN is 0.
      BRDUM          : Boolean := False;
      --  Block Repeat Count This field contains the number of repetitions of
      --  the current block (0 to 4095). When the channel is enabled, this
      --  register is read-only, indicating the remaining number of blocks,
      --  excluding the current one. This register decrements after each
      --  complete block transfer. Once the last block transfer has completed,
      --  this register can either stay at zero or be reloaded automatically
      --  from memory (in Linked List mode - i.e. Link Address valid). These
      --  bits are protected and can be written only if EN is 0.
      BRC            : MDMA_C14BNDTR_BRC_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C14BNDTR_Register use record
      BNDT           at 0 range 0 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      BRSUM          at 0 range 18 .. 18;
      BRDUM          at 0 range 19 .. 19;
      BRC            at 0 range 20 .. 31;
   end record;

   subtype MDMA_C14BRUR_SUV_Field is STM32H753.UInt16;
   subtype MDMA_C14BRUR_DUV_Field is STM32H753.UInt16;

   --  MDMA channel x Block Repeat address Update register
   type MDMA_C14BRUR_Register is record
      --  source adresse update value
      SUV : MDMA_C14BRUR_SUV_Field := 16#0#;
      --  destination address update
      DUV : MDMA_C14BRUR_DUV_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C14BRUR_Register use record
      SUV at 0 range 0 .. 15;
      DUV at 0 range 16 .. 31;
   end record;

   subtype MDMA_C14TBR_TSEL_Field is STM32H753.UInt6;

   --  MDMA channel x Trigger and Bus selection Register
   type MDMA_C14TBR_Register is record
      --  Trigger selection
      TSEL           : MDMA_C14TBR_TSEL_Field := 16#0#;
      --  unspecified
      Reserved_6_15  : STM32H753.UInt10 := 16#0#;
      --  Source BUS select This bit is protected and can be written only if EN
      --  is 0.
      SBUS           : Boolean := False;
      --  Destination BUS slect This bit is protected and can be written only
      --  if EN is 0.
      DBUS           : Boolean := False;
      --  unspecified
      Reserved_18_31 : STM32H753.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C14TBR_Register use record
      TSEL           at 0 range 0 .. 5;
      Reserved_6_15  at 0 range 6 .. 15;
      SBUS           at 0 range 16 .. 16;
      DBUS           at 0 range 17 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   --  MDMA channel x interrupt/status register
   type MDMA_C15ISR_Register is record
      --  Read-only. Channel x transfer error interrupt flag This bit is set by
      --  hardware. It is cleared by software writing 1 to the corresponding
      --  bit in the DMA_IFCRy register.
      TEIF15         : Boolean;
      --  Read-only. Channel x Channel Transfer Complete interrupt flag This
      --  bit is set by hardware. It is cleared by software writing 1 to the
      --  corresponding bit in the DMA_IFCRy register. CTC is set when the last
      --  block was transferred and the channel has been automatically
      --  disabled. CTC is also set when the channel is suspended, as a result
      --  of writing EN bit to 0.
      CTCIF15        : Boolean;
      --  Read-only. Channel x block repeat transfer complete interrupt flag
      --  This bit is set by hardware. It is cleared by software writing 1 to
      --  the corresponding bit in the DMA_IFCRy register.
      BRTIF15        : Boolean;
      --  Read-only. Channel x block transfer complete interrupt flag This bit
      --  is set by hardware. It is cleared by software writing 1 to the
      --  corresponding bit in the DMA_IFCRy register.
      BTIF15         : Boolean;
      --  Read-only. channel x buffer transfer complete
      TCIF15         : Boolean;
      --  unspecified
      Reserved_5_15  : STM32H753.UInt11;
      --  Read-only. channel x request active flag
      CRQA15         : Boolean;
      --  unspecified
      Reserved_17_31 : STM32H753.UInt15;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C15ISR_Register use record
      TEIF15         at 0 range 0 .. 0;
      CTCIF15        at 0 range 1 .. 1;
      BRTIF15        at 0 range 2 .. 2;
      BTIF15         at 0 range 3 .. 3;
      TCIF15         at 0 range 4 .. 4;
      Reserved_5_15  at 0 range 5 .. 15;
      CRQA15         at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   --  MDMA channel x interrupt flag clear register
   type MDMA_C15IFCR_Register is record
      --  Write-only. Channel x clear transfer error interrupt flag Writing a 1
      --  into this bit clears TEIFx in the MDMA_ISRy register
      CTEIF15       : Boolean := False;
      --  Write-only. Clear Channel transfer complete interrupt flag for
      --  channel x Writing a 1 into this bit clears CTCIFx in the MDMA_ISRy
      --  register
      CCTCIF15      : Boolean := False;
      --  Write-only. Channel x clear block repeat transfer complete interrupt
      --  flag Writing a 1 into this bit clears BRTIFx in the MDMA_ISRy
      --  register
      CBRTIF15      : Boolean := False;
      --  Write-only. Channel x Clear block transfer complete interrupt flag
      --  Writing a 1 into this bit clears BTIFx in the MDMA_ISRy register
      CBTIF15       : Boolean := False;
      --  Write-only. CLear buffer Transfer Complete Interrupt Flag for channel
      --  x Writing a 1 into this bit clears TCIFx in the MDMA_ISRy register
      CLTCIF15      : Boolean := False;
      --  unspecified
      Reserved_5_31 : STM32H753.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C15IFCR_Register use record
      CTEIF15       at 0 range 0 .. 0;
      CCTCIF15      at 0 range 1 .. 1;
      CBRTIF15      at 0 range 2 .. 2;
      CBTIF15       at 0 range 3 .. 3;
      CLTCIF15      at 0 range 4 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   subtype MDMA_C15ESR_TEA_Field is STM32H753.UInt7;

   --  MDMA Channel x error status register
   type MDMA_C15ESR_Register is record
      --  Read-only. Transfer Error Address These bits are set and cleared by
      --  HW, in case of an MDMA data transfer error. It is used in conjunction
      --  with TED. This field indicates the 7 LSBits of the address which
      --  generated a transfer/access error. It may be used by SW to retrieve
      --  the failing address, by adding this value (truncated to the buffer
      --  transfer length size) to the current SAR/DAR value. Note: The SAR/DAR
      --  current value doesnt reflect this last address due to the FIFO
      --  management system. The SAR/DAR are only updated at the end of a
      --  (buffer) transfer (of TLEN+1 bytes). Note: It is not set in case of a
      --  link data error.
      TEA            : MDMA_C15ESR_TEA_Field;
      --  Read-only. Transfer Error Direction These bit is set and cleared by
      --  HW, in case of an MDMA data transfer error.
      TED            : Boolean;
      --  Read-only. Transfer Error Link Data These bit is set by HW, in case
      --  of a transfer error while reading the block link data structure. It
      --  is cleared by software writing 1 to the CTEIFx bit in the DMA_IFCRy
      --  register.
      TELD           : Boolean;
      --  Read-only. Transfer Error Mask Data These bit is set by HW, in case
      --  of a transfer error while writing the Mask Data. It is cleared by
      --  software writing 1 to the CTEIFx bit in the DMA_IFCRy register.
      TEMD           : Boolean;
      --  Read-only. Address/Size Error These bit is set by HW, when the
      --  programmed address is not aligned with the data size. TED will
      --  indicate whether the problem is on the source or destination. It is
      --  cleared by software writing 1 to the CTEIFx bit in the DMA_IFCRy
      --  register.
      ASE            : Boolean;
      --  Read-only. Block Size Error These bit is set by HW, when the block
      --  size is not an integer multiple of the data size either for source or
      --  destination. TED will indicate whether the problem is on the source
      --  or destination. It is cleared by software writing 1 to the CTEIFx bit
      --  in the DMA_IFCRy register.
      BSE            : Boolean;
      --  unspecified
      Reserved_12_31 : STM32H753.UInt20;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C15ESR_Register use record
      TEA            at 0 range 0 .. 6;
      TED            at 0 range 7 .. 7;
      TELD           at 0 range 8 .. 8;
      TEMD           at 0 range 9 .. 9;
      ASE            at 0 range 10 .. 10;
      BSE            at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype MDMA_C15CR_PL_Field is STM32H753.UInt2;

   --  This register is used to control the concerned channel.
   type MDMA_C15CR_Register is record
      --  channel enable
      EN             : Boolean := False;
      --  Transfer error interrupt enable This bit is set and cleared by
      --  software.
      TEIE           : Boolean := False;
      --  Channel Transfer Complete interrupt enable This bit is set and
      --  cleared by software.
      CTCIE          : Boolean := False;
      --  Block Repeat transfer interrupt enable This bit is set and cleared by
      --  software.
      BRTIE          : Boolean := False;
      --  Block Transfer interrupt enable This bit is set and cleared by
      --  software.
      BTIE           : Boolean := False;
      --  buffer Transfer Complete interrupt enable This bit is set and cleared
      --  by software.
      TCIE           : Boolean := False;
      --  Priority level These bits are set and cleared by software. These bits
      --  are protected and can be written only if EN is 0.
      PL             : MDMA_C15CR_PL_Field := 16#0#;
      --  unspecified
      Reserved_8_11  : STM32H753.UInt4 := 16#0#;
      --  byte Endianness exchange
      BEX            : Boolean := False;
      --  Half word Endianes exchange
      HEX            : Boolean := False;
      --  Word Endianness exchange
      WEX            : Boolean := False;
      --  unspecified
      Reserved_15_15 : STM32H753.Bit := 16#0#;
      --  Write-only. SW ReQuest Writing a 1 into this bit sets the CRQAx in
      --  MDMA_ISRy register, activating the request on Channel x Note: Either
      --  the whole CxCR register or the 8-bit/16-bit register @ Address
      --  offset: 0x4E + 0x40 chn may be used for SWRQ activation. In case of a
      --  SW request, acknowledge is not generated (neither HW signal, nor
      --  CxMAR write access).
      SWRQ           : Boolean := False;
      --  unspecified
      Reserved_17_31 : STM32H753.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C15CR_Register use record
      EN             at 0 range 0 .. 0;
      TEIE           at 0 range 1 .. 1;
      CTCIE          at 0 range 2 .. 2;
      BRTIE          at 0 range 3 .. 3;
      BTIE           at 0 range 4 .. 4;
      TCIE           at 0 range 5 .. 5;
      PL             at 0 range 6 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      BEX            at 0 range 12 .. 12;
      HEX            at 0 range 13 .. 13;
      WEX            at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      SWRQ           at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype MDMA_C15TCR_SINC_Field is STM32H753.UInt2;
   subtype MDMA_C15TCR_DINC_Field is STM32H753.UInt2;
   subtype MDMA_C15TCR_SSIZE_Field is STM32H753.UInt2;
   subtype MDMA_C15TCR_DSIZE_Field is STM32H753.UInt2;
   subtype MDMA_C15TCR_SINCOS_Field is STM32H753.UInt2;
   subtype MDMA_C15TCR_DINCOS_Field is STM32H753.UInt2;
   subtype MDMA_C15TCR_SBURST_Field is STM32H753.UInt3;
   subtype MDMA_C15TCR_DBURST_Field is STM32H753.UInt3;
   subtype MDMA_C15TCR_TLEN_Field is STM32H753.UInt7;
   subtype MDMA_C15TCR_PAM_Field is STM32H753.UInt2;
   subtype MDMA_C15TCR_TRGM_Field is STM32H753.UInt2;

   --  This register is used to configure the concerned channel.
   type MDMA_C15TCR_Register is record
      --  Source increment mode These bits are set and cleared by software.
      --  These bits are protected and can be written only if EN is 0 Note:
      --  When source is AHB (SBUS=1), SINC = 00 is forbidden. In Linked List
      --  Mode, at the end of a block (single or last block in repeated block
      --  transfer mode), this register will be loaded from memory (from
      --  address given by current LAR[31:0] + 0x00).
      SINC   : MDMA_C15TCR_SINC_Field := 16#0#;
      --  Destination increment mode These bits are set and cleared by
      --  software. These bits are protected and can be written only if EN is 0
      --  Note: When destination is AHB (DBUS=1), DINC = 00 is forbidden.
      DINC   : MDMA_C15TCR_DINC_Field := 16#0#;
      --  Source data size These bits are set and cleared by software. These
      --  bits are protected and can be written only if EN is 0 Note: If a
      --  value of 11 is programmed for the TCM access/AHB port, a transfer
      --  error will occur (TEIF bit set) If SINCOS &lt; SSIZE and SINC &#8800;
      --  00, the result will be unpredictable. Note: SSIZE = 11 (double-word)
      --  is forbidden when source is TCM/AHB bus (SBUS=1).
      SSIZE  : MDMA_C15TCR_SSIZE_Field := 16#0#;
      --  Destination data size These bits are set and cleared by software.
      --  These bits are protected and can be written only if EN is 0. Note: If
      --  a value of 11 is programmed for the TCM access/AHB port, a transfer
      --  error will occur (TEIF bit set) If DINCOS &lt; DSIZE and DINC &#8800;
      --  00, the result will be unpredictable. Note: DSIZE = 11 (double-word)
      --  is forbidden when destination is TCM/AHB bus (DBUS=1).
      DSIZE  : MDMA_C15TCR_DSIZE_Field := 16#0#;
      --  source increment offset size
      SINCOS : MDMA_C15TCR_SINCOS_Field := 16#0#;
      --  Destination increment offset
      DINCOS : MDMA_C15TCR_DINCOS_Field := 16#0#;
      --  source burst transfer configuration
      SBURST : MDMA_C15TCR_SBURST_Field := 16#0#;
      --  Destination burst transfer configuration
      DBURST : MDMA_C15TCR_DBURST_Field := 16#0#;
      --  buffer transfer lengh
      TLEN   : MDMA_C15TCR_TLEN_Field := 16#0#;
      --  PacK Enable These bit is set and cleared by software. If the Source
      --  Size is smaller than the destination, it will be padded according to
      --  the PAM value. If the Source data size is larger than the destination
      --  one, it will be truncated. The alignment will be done according to
      --  the PAM[0] value. This bit is protected and can be written only if EN
      --  is 0
      PKE    : Boolean := False;
      --  Padding/Alignement Mode These bits are set and cleared by software.
      --  Case 1: Source data size smaller than destination data size - 3
      --  options are valid. Case 2: Source data size larger than destination
      --  data size. The remainder part is discarded. When PKE = 1 or
      --  DSIZE=SSIZE, these bits are ignored. These bits are protected and can
      --  be written only if EN is 0
      PAM    : MDMA_C15TCR_PAM_Field := 16#0#;
      --  Trigger Mode These bits are set and cleared by software. Note: If
      --  TRGM is 11 for the current block, all the values loaded at the end of
      --  the current block through the linked list mechanism must keep the
      --  same value (TRGM=11) and the same SWRM value, otherwise the result is
      --  undefined. These bits are protected and can be written only if EN is
      --  0.
      TRGM   : MDMA_C15TCR_TRGM_Field := 16#0#;
      --  SW Request Mode This bit is set and cleared by software. If a HW or
      --  SW request is currently active, the bit change will be delayed until
      --  the current transfer is completed. If the CxMAR contains a valid
      --  address, the CxMDR value will also be written @ CxMAR address. This
      --  bit is protected and can be written only if EN is 0.
      SWRM   : Boolean := False;
      --  Bufferable Write Mode This bit is set and cleared by software. This
      --  bit is protected and can be written only if EN is 0. Note: All MDMA
      --  destination accesses are non-cacheable.
      BWM    : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C15TCR_Register use record
      SINC   at 0 range 0 .. 1;
      DINC   at 0 range 2 .. 3;
      SSIZE  at 0 range 4 .. 5;
      DSIZE  at 0 range 6 .. 7;
      SINCOS at 0 range 8 .. 9;
      DINCOS at 0 range 10 .. 11;
      SBURST at 0 range 12 .. 14;
      DBURST at 0 range 15 .. 17;
      TLEN   at 0 range 18 .. 24;
      PKE    at 0 range 25 .. 25;
      PAM    at 0 range 26 .. 27;
      TRGM   at 0 range 28 .. 29;
      SWRM   at 0 range 30 .. 30;
      BWM    at 0 range 31 .. 31;
   end record;

   subtype MDMA_C15BNDTR_BNDT_Field is STM32H753.UInt17;
   subtype MDMA_C15BNDTR_BRC_Field is STM32H753.UInt12;

   --  MDMA Channel x block number of data register
   type MDMA_C15BNDTR_Register is record
      --  block number of data to transfer
      BNDT           : MDMA_C15BNDTR_BNDT_Field := 16#0#;
      --  unspecified
      Reserved_17_17 : STM32H753.Bit := 16#0#;
      --  Block Repeat Source address Update Mode These bits are protected and
      --  can be written only if EN is 0.
      BRSUM          : Boolean := False;
      --  Block Repeat Destination address Update Mode These bits are protected
      --  and can be written only if EN is 0.
      BRDUM          : Boolean := False;
      --  Block Repeat Count This field contains the number of repetitions of
      --  the current block (0 to 4095). When the channel is enabled, this
      --  register is read-only, indicating the remaining number of blocks,
      --  excluding the current one. This register decrements after each
      --  complete block transfer. Once the last block transfer has completed,
      --  this register can either stay at zero or be reloaded automatically
      --  from memory (in Linked List mode - i.e. Link Address valid). These
      --  bits are protected and can be written only if EN is 0.
      BRC            : MDMA_C15BNDTR_BRC_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C15BNDTR_Register use record
      BNDT           at 0 range 0 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      BRSUM          at 0 range 18 .. 18;
      BRDUM          at 0 range 19 .. 19;
      BRC            at 0 range 20 .. 31;
   end record;

   subtype MDMA_C15BRUR_SUV_Field is STM32H753.UInt16;
   subtype MDMA_C15BRUR_DUV_Field is STM32H753.UInt16;

   --  MDMA channel x Block Repeat address Update register
   type MDMA_C15BRUR_Register is record
      --  source adresse update value
      SUV : MDMA_C15BRUR_SUV_Field := 16#0#;
      --  destination address update
      DUV : MDMA_C15BRUR_DUV_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C15BRUR_Register use record
      SUV at 0 range 0 .. 15;
      DUV at 0 range 16 .. 31;
   end record;

   subtype MDMA_C15TBR_TSEL_Field is STM32H753.UInt6;

   --  MDMA channel x Trigger and Bus selection Register
   type MDMA_C15TBR_Register is record
      --  Trigger selection
      TSEL           : MDMA_C15TBR_TSEL_Field := 16#0#;
      --  unspecified
      Reserved_6_15  : STM32H753.UInt10 := 16#0#;
      --  Source BUS select This bit is protected and can be written only if EN
      --  is 0.
      SBUS           : Boolean := False;
      --  Destination BUS slect This bit is protected and can be written only
      --  if EN is 0.
      DBUS           : Boolean := False;
      --  unspecified
      Reserved_18_31 : STM32H753.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MDMA_C15TBR_Register use record
      TSEL           at 0 range 0 .. 5;
      Reserved_6_15  at 0 range 6 .. 15;
      SBUS           at 0 range 16 .. 16;
      DBUS           at 0 range 17 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  MDMA
   type MDMA_Peripheral is record
      --  MDMA Global Interrupt/Status Register
      MDMA_GISR0    : aliased MDMA_GISR0_Register;
      --  MDMA channel x interrupt/status register
      MDMA_C0ISR    : aliased MDMA_C0ISR_Register;
      --  MDMA channel x interrupt flag clear register
      MDMA_C0IFCR   : aliased MDMA_C0IFCR_Register;
      --  MDMA Channel x error status register
      MDMA_C0ESR    : aliased MDMA_C0ESR_Register;
      --  This register is used to control the concerned channel.
      MDMA_C0CR     : aliased MDMA_C0CR_Register;
      --  This register is used to configure the concerned channel.
      MDMA_C0TCR    : aliased MDMA_C0TCR_Register;
      --  MDMA Channel x block number of data register
      MDMA_C0BNDTR  : aliased MDMA_C0BNDTR_Register;
      --  MDMA channel x source address register
      MDMA_C0SAR    : aliased STM32H753.UInt32;
      --  MDMA channel x destination address register
      MDMA_C0DAR    : aliased STM32H753.UInt32;
      --  MDMA channel x Block Repeat address Update register
      MDMA_C0BRUR   : aliased MDMA_C0BRUR_Register;
      --  MDMA channel x Link Address register
      MDMA_C0LAR    : aliased STM32H753.UInt32;
      --  MDMA channel x Trigger and Bus selection Register
      MDMA_C0TBR    : aliased MDMA_C0TBR_Register;
      --  MDMA channel x Mask address register
      MDMA_C0MAR    : aliased STM32H753.UInt32;
      --  MDMA channel x Mask Data register
      MDMA_C0MDR    : aliased STM32H753.UInt32;
      --  MDMA channel x interrupt/status register
      MDMA_C1ISR    : aliased MDMA_C1ISR_Register;
      --  MDMA channel x interrupt flag clear register
      MDMA_C1IFCR   : aliased MDMA_C1IFCR_Register;
      --  MDMA Channel x error status register
      MDMA_C1ESR    : aliased MDMA_C1ESR_Register;
      --  This register is used to control the concerned channel.
      MDMA_C1CR     : aliased MDMA_C1CR_Register;
      --  This register is used to configure the concerned channel.
      MDMA_C1TCR    : aliased MDMA_C1TCR_Register;
      --  MDMA Channel x block number of data register
      MDMA_C1BNDTR  : aliased MDMA_C1BNDTR_Register;
      --  MDMA channel x source address register
      MDMA_C1SAR    : aliased STM32H753.UInt32;
      --  MDMA channel x destination address register
      MDMA_C1DAR    : aliased STM32H753.UInt32;
      --  MDMA channel x Block Repeat address Update register
      MDMA_C1BRUR   : aliased MDMA_C1BRUR_Register;
      --  MDMA channel x Link Address register
      MDMA_C1LAR    : aliased STM32H753.UInt32;
      --  MDMA channel x Trigger and Bus selection Register
      MDMA_C1TBR    : aliased MDMA_C1TBR_Register;
      --  MDMA channel x Mask address register
      MDMA_C1MAR    : aliased STM32H753.UInt32;
      --  MDMA channel x Mask Data register
      MDMA_C1MDR    : aliased STM32H753.UInt32;
      --  MDMA channel x interrupt/status register
      MDMA_C2ISR    : aliased MDMA_C2ISR_Register;
      --  MDMA channel x interrupt flag clear register
      MDMA_C2IFCR   : aliased MDMA_C2IFCR_Register;
      --  MDMA Channel x error status register
      MDMA_C2ESR    : aliased MDMA_C2ESR_Register;
      --  This register is used to control the concerned channel.
      MDMA_C2CR     : aliased MDMA_C2CR_Register;
      --  This register is used to configure the concerned channel.
      MDMA_C2TCR    : aliased MDMA_C2TCR_Register;
      --  MDMA Channel x block number of data register
      MDMA_C2BNDTR  : aliased MDMA_C2BNDTR_Register;
      --  MDMA channel x source address register
      MDMA_C2SAR    : aliased STM32H753.UInt32;
      --  MDMA channel x destination address register
      MDMA_C2DAR    : aliased STM32H753.UInt32;
      --  MDMA channel x Block Repeat address Update register
      MDMA_C2BRUR   : aliased MDMA_C2BRUR_Register;
      --  MDMA channel x Link Address register
      MDMA_C2LAR    : aliased STM32H753.UInt32;
      --  MDMA channel x Trigger and Bus selection Register
      MDMA_C2TBR    : aliased MDMA_C2TBR_Register;
      --  MDMA channel x Mask address register
      MDMA_C2MAR    : aliased STM32H753.UInt32;
      --  MDMA channel x Mask Data register
      MDMA_C2MDR    : aliased STM32H753.UInt32;
      --  MDMA channel x interrupt/status register
      MDMA_C3ISR    : aliased MDMA_C3ISR_Register;
      --  MDMA channel x interrupt flag clear register
      MDMA_C3IFCR   : aliased MDMA_C3IFCR_Register;
      --  MDMA Channel x error status register
      MDMA_C3ESR    : aliased MDMA_C3ESR_Register;
      --  This register is used to control the concerned channel.
      MDMA_C3CR     : aliased MDMA_C3CR_Register;
      --  This register is used to configure the concerned channel.
      MDMA_C3TCR    : aliased MDMA_C3TCR_Register;
      --  MDMA Channel x block number of data register
      MDMA_C3BNDTR  : aliased MDMA_C3BNDTR_Register;
      --  MDMA channel x source address register
      MDMA_C3SAR    : aliased STM32H753.UInt32;
      --  MDMA channel x destination address register
      MDMA_C3DAR    : aliased STM32H753.UInt32;
      --  MDMA channel x Block Repeat address Update register
      MDMA_C3BRUR   : aliased MDMA_C3BRUR_Register;
      --  MDMA channel x Link Address register
      MDMA_C3LAR    : aliased STM32H753.UInt32;
      --  MDMA channel x Trigger and Bus selection Register
      MDMA_C3TBR    : aliased MDMA_C3TBR_Register;
      --  MDMA channel x Mask address register
      MDMA_C3MAR    : aliased STM32H753.UInt32;
      --  MDMA channel x Mask Data register
      MDMA_C3MDR    : aliased STM32H753.UInt32;
      --  MDMA channel x interrupt/status register
      MDMA_C4ISR    : aliased MDMA_C4ISR_Register;
      --  MDMA channel x interrupt flag clear register
      MDMA_C4IFCR   : aliased MDMA_C4IFCR_Register;
      --  MDMA Channel x error status register
      MDMA_C4ESR    : aliased MDMA_C4ESR_Register;
      --  This register is used to control the concerned channel.
      MDMA_C4CR     : aliased MDMA_C4CR_Register;
      --  This register is used to configure the concerned channel.
      MDMA_C4TCR    : aliased MDMA_C4TCR_Register;
      --  MDMA Channel x block number of data register
      MDMA_C4BNDTR  : aliased MDMA_C4BNDTR_Register;
      --  MDMA channel x source address register
      MDMA_C4SAR    : aliased STM32H753.UInt32;
      --  MDMA channel x destination address register
      MDMA_C4DAR    : aliased STM32H753.UInt32;
      --  MDMA channel x Block Repeat address Update register
      MDMA_C4BRUR   : aliased MDMA_C4BRUR_Register;
      --  MDMA channel x Link Address register
      MDMA_C4LAR    : aliased STM32H753.UInt32;
      --  MDMA channel x Trigger and Bus selection Register
      MDMA_C4TBR    : aliased MDMA_C4TBR_Register;
      --  MDMA channel x Mask address register
      MDMA_C4MAR    : aliased STM32H753.UInt32;
      --  MDMA channel x Mask Data register
      MDMA_C4MDR    : aliased STM32H753.UInt32;
      --  MDMA channel x interrupt/status register
      MDMA_C5ISR    : aliased MDMA_C5ISR_Register;
      --  MDMA channel x interrupt flag clear register
      MDMA_C5IFCR   : aliased MDMA_C5IFCR_Register;
      --  MDMA Channel x error status register
      MDMA_C5ESR    : aliased MDMA_C5ESR_Register;
      --  This register is used to control the concerned channel.
      MDMA_C5CR     : aliased MDMA_C5CR_Register;
      --  This register is used to configure the concerned channel.
      MDMA_C5TCR    : aliased MDMA_C5TCR_Register;
      --  MDMA Channel x block number of data register
      MDMA_C5BNDTR  : aliased MDMA_C5BNDTR_Register;
      --  MDMA channel x source address register
      MDMA_C5SAR    : aliased STM32H753.UInt32;
      --  MDMA channel x destination address register
      MDMA_C5DAR    : aliased STM32H753.UInt32;
      --  MDMA channel x Block Repeat address Update register
      MDMA_C5BRUR   : aliased MDMA_C5BRUR_Register;
      --  MDMA channel x Link Address register
      MDMA_C5LAR    : aliased STM32H753.UInt32;
      --  MDMA channel x Trigger and Bus selection Register
      MDMA_C5TBR    : aliased MDMA_C5TBR_Register;
      --  MDMA channel x Mask address register
      MDMA_C5MAR    : aliased STM32H753.UInt32;
      --  MDMA channel x Mask Data register
      MDMA_C5MDR    : aliased STM32H753.UInt32;
      --  MDMA channel x interrupt/status register
      MDMA_C6ISR    : aliased MDMA_C6ISR_Register;
      --  MDMA channel x interrupt flag clear register
      MDMA_C6IFCR   : aliased MDMA_C6IFCR_Register;
      --  MDMA Channel x error status register
      MDMA_C6ESR    : aliased MDMA_C6ESR_Register;
      --  This register is used to control the concerned channel.
      MDMA_C6CR     : aliased MDMA_C6CR_Register;
      --  This register is used to configure the concerned channel.
      MDMA_C6TCR    : aliased MDMA_C6TCR_Register;
      --  MDMA Channel x block number of data register
      MDMA_C6BNDTR  : aliased MDMA_C6BNDTR_Register;
      --  MDMA channel x source address register
      MDMA_C6SAR    : aliased STM32H753.UInt32;
      --  MDMA channel x destination address register
      MDMA_C6DAR    : aliased STM32H753.UInt32;
      --  MDMA channel x Block Repeat address Update register
      MDMA_C6BRUR   : aliased MDMA_C6BRUR_Register;
      --  MDMA channel x Link Address register
      MDMA_C6LAR    : aliased STM32H753.UInt32;
      --  MDMA channel x Trigger and Bus selection Register
      MDMA_C6TBR    : aliased MDMA_C6TBR_Register;
      --  MDMA channel x Mask address register
      MDMA_C6MAR    : aliased STM32H753.UInt32;
      --  MDMA channel x Mask Data register
      MDMA_C6MDR    : aliased STM32H753.UInt32;
      --  MDMA channel x interrupt/status register
      MDMA_C7ISR    : aliased MDMA_C7ISR_Register;
      --  MDMA channel x interrupt flag clear register
      MDMA_C7IFCR   : aliased MDMA_C7IFCR_Register;
      --  MDMA Channel x error status register
      MDMA_C7ESR    : aliased MDMA_C7ESR_Register;
      --  This register is used to control the concerned channel.
      MDMA_C7CR     : aliased MDMA_C7CR_Register;
      --  This register is used to configure the concerned channel.
      MDMA_C7TCR    : aliased MDMA_C7TCR_Register;
      --  MDMA Channel x block number of data register
      MDMA_C7BNDTR  : aliased MDMA_C7BNDTR_Register;
      --  MDMA channel x source address register
      MDMA_C7SAR    : aliased STM32H753.UInt32;
      --  MDMA channel x destination address register
      MDMA_C7DAR    : aliased STM32H753.UInt32;
      --  MDMA channel x Block Repeat address Update register
      MDMA_C7BRUR   : aliased MDMA_C7BRUR_Register;
      --  MDMA channel x Link Address register
      MDMA_C7LAR    : aliased STM32H753.UInt32;
      --  MDMA channel x Trigger and Bus selection Register
      MDMA_C7TBR    : aliased MDMA_C7TBR_Register;
      --  MDMA channel x Mask address register
      MDMA_C7MAR    : aliased STM32H753.UInt32;
      --  MDMA channel x Mask Data register
      MDMA_C7MDR    : aliased STM32H753.UInt32;
      --  MDMA channel x interrupt/status register
      MDMA_C8ISR    : aliased MDMA_C8ISR_Register;
      --  MDMA channel x interrupt flag clear register
      MDMA_C8IFCR   : aliased MDMA_C8IFCR_Register;
      --  MDMA Channel x error status register
      MDMA_C8ESR    : aliased MDMA_C8ESR_Register;
      --  This register is used to control the concerned channel.
      MDMA_C8CR     : aliased MDMA_C8CR_Register;
      --  This register is used to configure the concerned channel.
      MDMA_C8TCR    : aliased MDMA_C8TCR_Register;
      --  MDMA Channel x block number of data register
      MDMA_C8BNDTR  : aliased MDMA_C8BNDTR_Register;
      --  MDMA channel x source address register
      MDMA_C8SAR    : aliased STM32H753.UInt32;
      --  MDMA channel x destination address register
      MDMA_C8DAR    : aliased STM32H753.UInt32;
      --  MDMA channel x Block Repeat address Update register
      MDMA_C8BRUR   : aliased MDMA_C8BRUR_Register;
      --  MDMA channel x Link Address register
      MDMA_C8LAR    : aliased STM32H753.UInt32;
      --  MDMA channel x Trigger and Bus selection Register
      MDMA_C8TBR    : aliased MDMA_C8TBR_Register;
      --  MDMA channel x Mask address register
      MDMA_C8MAR    : aliased STM32H753.UInt32;
      --  MDMA channel x Mask Data register
      MDMA_C8MDR    : aliased STM32H753.UInt32;
      --  MDMA channel x interrupt/status register
      MDMA_C9ISR    : aliased MDMA_C9ISR_Register;
      --  MDMA channel x interrupt flag clear register
      MDMA_C9IFCR   : aliased MDMA_C9IFCR_Register;
      --  MDMA Channel x error status register
      MDMA_C9ESR    : aliased MDMA_C9ESR_Register;
      --  This register is used to control the concerned channel.
      MDMA_C9CR     : aliased MDMA_C9CR_Register;
      --  This register is used to configure the concerned channel.
      MDMA_C9TCR    : aliased MDMA_C9TCR_Register;
      --  MDMA Channel x block number of data register
      MDMA_C9BNDTR  : aliased MDMA_C9BNDTR_Register;
      --  MDMA channel x source address register
      MDMA_C9SAR    : aliased STM32H753.UInt32;
      --  MDMA channel x destination address register
      MDMA_C9DAR    : aliased STM32H753.UInt32;
      --  MDMA channel x Block Repeat address Update register
      MDMA_C9BRUR   : aliased MDMA_C9BRUR_Register;
      --  MDMA channel x Link Address register
      MDMA_C9LAR    : aliased STM32H753.UInt32;
      --  MDMA channel x Trigger and Bus selection Register
      MDMA_C9TBR    : aliased MDMA_C9TBR_Register;
      --  MDMA channel x Mask address register
      MDMA_C9MAR    : aliased STM32H753.UInt32;
      --  MDMA channel x Mask Data register
      MDMA_C9MDR    : aliased STM32H753.UInt32;
      --  MDMA channel x interrupt/status register
      MDMA_C10ISR   : aliased MDMA_C10ISR_Register;
      --  MDMA channel x interrupt flag clear register
      MDMA_C10IFCR  : aliased MDMA_C10IFCR_Register;
      --  MDMA Channel x error status register
      MDMA_C10ESR   : aliased MDMA_C10ESR_Register;
      --  This register is used to control the concerned channel.
      MDMA_C10CR    : aliased MDMA_C10CR_Register;
      --  This register is used to configure the concerned channel.
      MDMA_C10TCR   : aliased MDMA_C10TCR_Register;
      --  MDMA Channel x block number of data register
      MDMA_C10BNDTR : aliased MDMA_C10BNDTR_Register;
      --  MDMA channel x source address register
      MDMA_C10SAR   : aliased STM32H753.UInt32;
      --  MDMA channel x destination address register
      MDMA_C10DAR   : aliased STM32H753.UInt32;
      --  MDMA channel x Block Repeat address Update register
      MDMA_C10BRUR  : aliased MDMA_C10BRUR_Register;
      --  MDMA channel x Link Address register
      MDMA_C10LAR   : aliased STM32H753.UInt32;
      --  MDMA channel x Trigger and Bus selection Register
      MDMA_C10TBR   : aliased MDMA_C10TBR_Register;
      --  MDMA channel x Mask address register
      MDMA_C10MAR   : aliased STM32H753.UInt32;
      --  MDMA channel x Mask Data register
      MDMA_C10MDR   : aliased STM32H753.UInt32;
      --  MDMA channel x interrupt/status register
      MDMA_C11ISR   : aliased MDMA_C11ISR_Register;
      --  MDMA channel x interrupt flag clear register
      MDMA_C11IFCR  : aliased MDMA_C11IFCR_Register;
      --  MDMA Channel x error status register
      MDMA_C11ESR   : aliased MDMA_C11ESR_Register;
      --  This register is used to control the concerned channel.
      MDMA_C11CR    : aliased MDMA_C11CR_Register;
      --  This register is used to configure the concerned channel.
      MDMA_C11TCR   : aliased MDMA_C11TCR_Register;
      --  MDMA Channel x block number of data register
      MDMA_C11BNDTR : aliased MDMA_C11BNDTR_Register;
      --  MDMA channel x source address register
      MDMA_C11SAR   : aliased STM32H753.UInt32;
      --  MDMA channel x destination address register
      MDMA_C11DAR   : aliased STM32H753.UInt32;
      --  MDMA channel x Block Repeat address Update register
      MDMA_C11BRUR  : aliased MDMA_C11BRUR_Register;
      --  MDMA channel x Link Address register
      MDMA_C11LAR   : aliased STM32H753.UInt32;
      --  MDMA channel x Trigger and Bus selection Register
      MDMA_C11TBR   : aliased MDMA_C11TBR_Register;
      --  MDMA channel x Mask address register
      MDMA_C11MAR   : aliased STM32H753.UInt32;
      --  MDMA channel x Mask Data register
      MDMA_C11MDR   : aliased STM32H753.UInt32;
      --  MDMA channel x interrupt/status register
      MDMA_C12ISR   : aliased MDMA_C12ISR_Register;
      --  MDMA channel x interrupt flag clear register
      MDMA_C12IFCR  : aliased MDMA_C12IFCR_Register;
      --  MDMA Channel x error status register
      MDMA_C12ESR   : aliased MDMA_C12ESR_Register;
      --  This register is used to control the concerned channel.
      MDMA_C12CR    : aliased MDMA_C12CR_Register;
      --  This register is used to configure the concerned channel.
      MDMA_C12TCR   : aliased MDMA_C12TCR_Register;
      --  MDMA Channel x block number of data register
      MDMA_C12BNDTR : aliased MDMA_C12BNDTR_Register;
      --  MDMA channel x source address register
      MDMA_C12SAR   : aliased STM32H753.UInt32;
      --  MDMA channel x destination address register
      MDMA_C12DAR   : aliased STM32H753.UInt32;
      --  MDMA channel x Block Repeat address Update register
      MDMA_C12BRUR  : aliased MDMA_C12BRUR_Register;
      --  MDMA channel x Link Address register
      MDMA_C12LAR   : aliased STM32H753.UInt32;
      --  MDMA channel x Trigger and Bus selection Register
      MDMA_C12TBR   : aliased MDMA_C12TBR_Register;
      --  MDMA channel x Mask address register
      MDMA_C12MAR   : aliased STM32H753.UInt32;
      --  MDMA channel x Mask Data register
      MDMA_C12MDR   : aliased STM32H753.UInt32;
      --  MDMA channel x interrupt/status register
      MDMA_C13ISR   : aliased MDMA_C13ISR_Register;
      --  MDMA channel x interrupt flag clear register
      MDMA_C13IFCR  : aliased MDMA_C13IFCR_Register;
      --  MDMA Channel x error status register
      MDMA_C13ESR   : aliased MDMA_C13ESR_Register;
      --  This register is used to control the concerned channel.
      MDMA_C13CR    : aliased MDMA_C13CR_Register;
      --  This register is used to configure the concerned channel.
      MDMA_C13TCR   : aliased MDMA_C13TCR_Register;
      --  MDMA Channel x block number of data register
      MDMA_C13BNDTR : aliased MDMA_C13BNDTR_Register;
      --  MDMA channel x source address register
      MDMA_C13SAR   : aliased STM32H753.UInt32;
      --  MDMA channel x destination address register
      MDMA_C13DAR   : aliased STM32H753.UInt32;
      --  MDMA channel x Block Repeat address Update register
      MDMA_C13BRUR  : aliased MDMA_C13BRUR_Register;
      --  MDMA channel x Link Address register
      MDMA_C13LAR   : aliased STM32H753.UInt32;
      --  MDMA channel x Trigger and Bus selection Register
      MDMA_C13TBR   : aliased MDMA_C13TBR_Register;
      --  MDMA channel x Mask address register
      MDMA_C13MAR   : aliased STM32H753.UInt32;
      --  MDMA channel x Mask Data register
      MDMA_C13MDR   : aliased STM32H753.UInt32;
      --  MDMA channel x interrupt/status register
      MDMA_C14ISR   : aliased MDMA_C14ISR_Register;
      --  MDMA channel x interrupt flag clear register
      MDMA_C14IFCR  : aliased MDMA_C14IFCR_Register;
      --  MDMA Channel x error status register
      MDMA_C14ESR   : aliased MDMA_C14ESR_Register;
      --  This register is used to control the concerned channel.
      MDMA_C14CR    : aliased MDMA_C14CR_Register;
      --  This register is used to configure the concerned channel.
      MDMA_C14TCR   : aliased MDMA_C14TCR_Register;
      --  MDMA Channel x block number of data register
      MDMA_C14BNDTR : aliased MDMA_C14BNDTR_Register;
      --  MDMA channel x source address register
      MDMA_C14SAR   : aliased STM32H753.UInt32;
      --  MDMA channel x destination address register
      MDMA_C14DAR   : aliased STM32H753.UInt32;
      --  MDMA channel x Block Repeat address Update register
      MDMA_C14BRUR  : aliased MDMA_C14BRUR_Register;
      --  MDMA channel x Link Address register
      MDMA_C14LAR   : aliased STM32H753.UInt32;
      --  MDMA channel x Trigger and Bus selection Register
      MDMA_C14TBR   : aliased MDMA_C14TBR_Register;
      --  MDMA channel x Mask address register
      MDMA_C14MAR   : aliased STM32H753.UInt32;
      --  MDMA channel x Mask Data register
      MDMA_C14MDR   : aliased STM32H753.UInt32;
      --  MDMA channel x interrupt/status register
      MDMA_C15ISR   : aliased MDMA_C15ISR_Register;
      --  MDMA channel x interrupt flag clear register
      MDMA_C15IFCR  : aliased MDMA_C15IFCR_Register;
      --  MDMA Channel x error status register
      MDMA_C15ESR   : aliased MDMA_C15ESR_Register;
      --  This register is used to control the concerned channel.
      MDMA_C15CR    : aliased MDMA_C15CR_Register;
      --  This register is used to configure the concerned channel.
      MDMA_C15TCR   : aliased MDMA_C15TCR_Register;
      --  MDMA Channel x block number of data register
      MDMA_C15BNDTR : aliased MDMA_C15BNDTR_Register;
      --  MDMA channel x source address register
      MDMA_C15SAR   : aliased STM32H753.UInt32;
      --  MDMA channel x destination address register
      MDMA_C15DAR   : aliased STM32H753.UInt32;
      --  MDMA channel x Block Repeat address Update register
      MDMA_C15BRUR  : aliased MDMA_C15BRUR_Register;
      --  MDMA channel x Link Address register
      MDMA_C15LAR   : aliased STM32H753.UInt32;
      --  MDMA channel x Trigger and Bus selection Register
      MDMA_C15TBR   : aliased MDMA_C15TBR_Register;
      --  MDMA channel x Mask address register
      MDMA_C15MAR   : aliased STM32H753.UInt32;
      --  MDMA channel x Mask Data register
      MDMA_C15MDR   : aliased STM32H753.UInt32;
   end record
     with Volatile;

   for MDMA_Peripheral use record
      MDMA_GISR0    at 16#0# range 0 .. 31;
      MDMA_C0ISR    at 16#40# range 0 .. 31;
      MDMA_C0IFCR   at 16#44# range 0 .. 31;
      MDMA_C0ESR    at 16#48# range 0 .. 31;
      MDMA_C0CR     at 16#4C# range 0 .. 31;
      MDMA_C0TCR    at 16#50# range 0 .. 31;
      MDMA_C0BNDTR  at 16#54# range 0 .. 31;
      MDMA_C0SAR    at 16#58# range 0 .. 31;
      MDMA_C0DAR    at 16#5C# range 0 .. 31;
      MDMA_C0BRUR   at 16#60# range 0 .. 31;
      MDMA_C0LAR    at 16#64# range 0 .. 31;
      MDMA_C0TBR    at 16#68# range 0 .. 31;
      MDMA_C0MAR    at 16#70# range 0 .. 31;
      MDMA_C0MDR    at 16#74# range 0 .. 31;
      MDMA_C1ISR    at 16#80# range 0 .. 31;
      MDMA_C1IFCR   at 16#84# range 0 .. 31;
      MDMA_C1ESR    at 16#88# range 0 .. 31;
      MDMA_C1CR     at 16#8C# range 0 .. 31;
      MDMA_C1TCR    at 16#90# range 0 .. 31;
      MDMA_C1BNDTR  at 16#94# range 0 .. 31;
      MDMA_C1SAR    at 16#98# range 0 .. 31;
      MDMA_C1DAR    at 16#9C# range 0 .. 31;
      MDMA_C1BRUR   at 16#A0# range 0 .. 31;
      MDMA_C1LAR    at 16#A4# range 0 .. 31;
      MDMA_C1TBR    at 16#A8# range 0 .. 31;
      MDMA_C1MAR    at 16#B0# range 0 .. 31;
      MDMA_C1MDR    at 16#B4# range 0 .. 31;
      MDMA_C2ISR    at 16#C0# range 0 .. 31;
      MDMA_C2IFCR   at 16#C4# range 0 .. 31;
      MDMA_C2ESR    at 16#C8# range 0 .. 31;
      MDMA_C2CR     at 16#CC# range 0 .. 31;
      MDMA_C2TCR    at 16#D0# range 0 .. 31;
      MDMA_C2BNDTR  at 16#D4# range 0 .. 31;
      MDMA_C2SAR    at 16#D8# range 0 .. 31;
      MDMA_C2DAR    at 16#DC# range 0 .. 31;
      MDMA_C2BRUR   at 16#E0# range 0 .. 31;
      MDMA_C2LAR    at 16#E4# range 0 .. 31;
      MDMA_C2TBR    at 16#E8# range 0 .. 31;
      MDMA_C2MAR    at 16#F0# range 0 .. 31;
      MDMA_C2MDR    at 16#F4# range 0 .. 31;
      MDMA_C3ISR    at 16#100# range 0 .. 31;
      MDMA_C3IFCR   at 16#104# range 0 .. 31;
      MDMA_C3ESR    at 16#108# range 0 .. 31;
      MDMA_C3CR     at 16#10C# range 0 .. 31;
      MDMA_C3TCR    at 16#110# range 0 .. 31;
      MDMA_C3BNDTR  at 16#114# range 0 .. 31;
      MDMA_C3SAR    at 16#118# range 0 .. 31;
      MDMA_C3DAR    at 16#11C# range 0 .. 31;
      MDMA_C3BRUR   at 16#120# range 0 .. 31;
      MDMA_C3LAR    at 16#124# range 0 .. 31;
      MDMA_C3TBR    at 16#128# range 0 .. 31;
      MDMA_C3MAR    at 16#130# range 0 .. 31;
      MDMA_C3MDR    at 16#134# range 0 .. 31;
      MDMA_C4ISR    at 16#140# range 0 .. 31;
      MDMA_C4IFCR   at 16#144# range 0 .. 31;
      MDMA_C4ESR    at 16#148# range 0 .. 31;
      MDMA_C4CR     at 16#14C# range 0 .. 31;
      MDMA_C4TCR    at 16#150# range 0 .. 31;
      MDMA_C4BNDTR  at 16#154# range 0 .. 31;
      MDMA_C4SAR    at 16#158# range 0 .. 31;
      MDMA_C4DAR    at 16#15C# range 0 .. 31;
      MDMA_C4BRUR   at 16#160# range 0 .. 31;
      MDMA_C4LAR    at 16#164# range 0 .. 31;
      MDMA_C4TBR    at 16#168# range 0 .. 31;
      MDMA_C4MAR    at 16#170# range 0 .. 31;
      MDMA_C4MDR    at 16#174# range 0 .. 31;
      MDMA_C5ISR    at 16#180# range 0 .. 31;
      MDMA_C5IFCR   at 16#184# range 0 .. 31;
      MDMA_C5ESR    at 16#188# range 0 .. 31;
      MDMA_C5CR     at 16#18C# range 0 .. 31;
      MDMA_C5TCR    at 16#190# range 0 .. 31;
      MDMA_C5BNDTR  at 16#194# range 0 .. 31;
      MDMA_C5SAR    at 16#198# range 0 .. 31;
      MDMA_C5DAR    at 16#19C# range 0 .. 31;
      MDMA_C5BRUR   at 16#1A0# range 0 .. 31;
      MDMA_C5LAR    at 16#1A4# range 0 .. 31;
      MDMA_C5TBR    at 16#1A8# range 0 .. 31;
      MDMA_C5MAR    at 16#1B0# range 0 .. 31;
      MDMA_C5MDR    at 16#1B4# range 0 .. 31;
      MDMA_C6ISR    at 16#1C0# range 0 .. 31;
      MDMA_C6IFCR   at 16#1C4# range 0 .. 31;
      MDMA_C6ESR    at 16#1C8# range 0 .. 31;
      MDMA_C6CR     at 16#1CC# range 0 .. 31;
      MDMA_C6TCR    at 16#1D0# range 0 .. 31;
      MDMA_C6BNDTR  at 16#1D4# range 0 .. 31;
      MDMA_C6SAR    at 16#1D8# range 0 .. 31;
      MDMA_C6DAR    at 16#1DC# range 0 .. 31;
      MDMA_C6BRUR   at 16#1E0# range 0 .. 31;
      MDMA_C6LAR    at 16#1E4# range 0 .. 31;
      MDMA_C6TBR    at 16#1E8# range 0 .. 31;
      MDMA_C6MAR    at 16#1F0# range 0 .. 31;
      MDMA_C6MDR    at 16#1F4# range 0 .. 31;
      MDMA_C7ISR    at 16#200# range 0 .. 31;
      MDMA_C7IFCR   at 16#204# range 0 .. 31;
      MDMA_C7ESR    at 16#208# range 0 .. 31;
      MDMA_C7CR     at 16#20C# range 0 .. 31;
      MDMA_C7TCR    at 16#210# range 0 .. 31;
      MDMA_C7BNDTR  at 16#214# range 0 .. 31;
      MDMA_C7SAR    at 16#218# range 0 .. 31;
      MDMA_C7DAR    at 16#21C# range 0 .. 31;
      MDMA_C7BRUR   at 16#220# range 0 .. 31;
      MDMA_C7LAR    at 16#224# range 0 .. 31;
      MDMA_C7TBR    at 16#228# range 0 .. 31;
      MDMA_C7MAR    at 16#230# range 0 .. 31;
      MDMA_C7MDR    at 16#234# range 0 .. 31;
      MDMA_C8ISR    at 16#240# range 0 .. 31;
      MDMA_C8IFCR   at 16#244# range 0 .. 31;
      MDMA_C8ESR    at 16#248# range 0 .. 31;
      MDMA_C8CR     at 16#24C# range 0 .. 31;
      MDMA_C8TCR    at 16#250# range 0 .. 31;
      MDMA_C8BNDTR  at 16#254# range 0 .. 31;
      MDMA_C8SAR    at 16#258# range 0 .. 31;
      MDMA_C8DAR    at 16#25C# range 0 .. 31;
      MDMA_C8BRUR   at 16#260# range 0 .. 31;
      MDMA_C8LAR    at 16#264# range 0 .. 31;
      MDMA_C8TBR    at 16#268# range 0 .. 31;
      MDMA_C8MAR    at 16#270# range 0 .. 31;
      MDMA_C8MDR    at 16#274# range 0 .. 31;
      MDMA_C9ISR    at 16#280# range 0 .. 31;
      MDMA_C9IFCR   at 16#284# range 0 .. 31;
      MDMA_C9ESR    at 16#288# range 0 .. 31;
      MDMA_C9CR     at 16#28C# range 0 .. 31;
      MDMA_C9TCR    at 16#290# range 0 .. 31;
      MDMA_C9BNDTR  at 16#294# range 0 .. 31;
      MDMA_C9SAR    at 16#298# range 0 .. 31;
      MDMA_C9DAR    at 16#29C# range 0 .. 31;
      MDMA_C9BRUR   at 16#2A0# range 0 .. 31;
      MDMA_C9LAR    at 16#2A4# range 0 .. 31;
      MDMA_C9TBR    at 16#2A8# range 0 .. 31;
      MDMA_C9MAR    at 16#2B0# range 0 .. 31;
      MDMA_C9MDR    at 16#2B4# range 0 .. 31;
      MDMA_C10ISR   at 16#2C0# range 0 .. 31;
      MDMA_C10IFCR  at 16#2C4# range 0 .. 31;
      MDMA_C10ESR   at 16#2C8# range 0 .. 31;
      MDMA_C10CR    at 16#2CC# range 0 .. 31;
      MDMA_C10TCR   at 16#2D0# range 0 .. 31;
      MDMA_C10BNDTR at 16#2D4# range 0 .. 31;
      MDMA_C10SAR   at 16#2D8# range 0 .. 31;
      MDMA_C10DAR   at 16#2DC# range 0 .. 31;
      MDMA_C10BRUR  at 16#2E0# range 0 .. 31;
      MDMA_C10LAR   at 16#2E4# range 0 .. 31;
      MDMA_C10TBR   at 16#2E8# range 0 .. 31;
      MDMA_C10MAR   at 16#2F0# range 0 .. 31;
      MDMA_C10MDR   at 16#2F4# range 0 .. 31;
      MDMA_C11ISR   at 16#300# range 0 .. 31;
      MDMA_C11IFCR  at 16#304# range 0 .. 31;
      MDMA_C11ESR   at 16#308# range 0 .. 31;
      MDMA_C11CR    at 16#30C# range 0 .. 31;
      MDMA_C11TCR   at 16#310# range 0 .. 31;
      MDMA_C11BNDTR at 16#314# range 0 .. 31;
      MDMA_C11SAR   at 16#318# range 0 .. 31;
      MDMA_C11DAR   at 16#31C# range 0 .. 31;
      MDMA_C11BRUR  at 16#320# range 0 .. 31;
      MDMA_C11LAR   at 16#324# range 0 .. 31;
      MDMA_C11TBR   at 16#328# range 0 .. 31;
      MDMA_C11MAR   at 16#330# range 0 .. 31;
      MDMA_C11MDR   at 16#334# range 0 .. 31;
      MDMA_C12ISR   at 16#340# range 0 .. 31;
      MDMA_C12IFCR  at 16#344# range 0 .. 31;
      MDMA_C12ESR   at 16#348# range 0 .. 31;
      MDMA_C12CR    at 16#34C# range 0 .. 31;
      MDMA_C12TCR   at 16#350# range 0 .. 31;
      MDMA_C12BNDTR at 16#354# range 0 .. 31;
      MDMA_C12SAR   at 16#358# range 0 .. 31;
      MDMA_C12DAR   at 16#35C# range 0 .. 31;
      MDMA_C12BRUR  at 16#360# range 0 .. 31;
      MDMA_C12LAR   at 16#364# range 0 .. 31;
      MDMA_C12TBR   at 16#368# range 0 .. 31;
      MDMA_C12MAR   at 16#370# range 0 .. 31;
      MDMA_C12MDR   at 16#374# range 0 .. 31;
      MDMA_C13ISR   at 16#380# range 0 .. 31;
      MDMA_C13IFCR  at 16#384# range 0 .. 31;
      MDMA_C13ESR   at 16#388# range 0 .. 31;
      MDMA_C13CR    at 16#38C# range 0 .. 31;
      MDMA_C13TCR   at 16#390# range 0 .. 31;
      MDMA_C13BNDTR at 16#394# range 0 .. 31;
      MDMA_C13SAR   at 16#398# range 0 .. 31;
      MDMA_C13DAR   at 16#39C# range 0 .. 31;
      MDMA_C13BRUR  at 16#3A0# range 0 .. 31;
      MDMA_C13LAR   at 16#3A4# range 0 .. 31;
      MDMA_C13TBR   at 16#3A8# range 0 .. 31;
      MDMA_C13MAR   at 16#3B0# range 0 .. 31;
      MDMA_C13MDR   at 16#3B4# range 0 .. 31;
      MDMA_C14ISR   at 16#3C0# range 0 .. 31;
      MDMA_C14IFCR  at 16#3C4# range 0 .. 31;
      MDMA_C14ESR   at 16#3C8# range 0 .. 31;
      MDMA_C14CR    at 16#3CC# range 0 .. 31;
      MDMA_C14TCR   at 16#3D0# range 0 .. 31;
      MDMA_C14BNDTR at 16#3D4# range 0 .. 31;
      MDMA_C14SAR   at 16#3D8# range 0 .. 31;
      MDMA_C14DAR   at 16#3DC# range 0 .. 31;
      MDMA_C14BRUR  at 16#3E0# range 0 .. 31;
      MDMA_C14LAR   at 16#3E4# range 0 .. 31;
      MDMA_C14TBR   at 16#3E8# range 0 .. 31;
      MDMA_C14MAR   at 16#3F0# range 0 .. 31;
      MDMA_C14MDR   at 16#3F4# range 0 .. 31;
      MDMA_C15ISR   at 16#400# range 0 .. 31;
      MDMA_C15IFCR  at 16#404# range 0 .. 31;
      MDMA_C15ESR   at 16#408# range 0 .. 31;
      MDMA_C15CR    at 16#40C# range 0 .. 31;
      MDMA_C15TCR   at 16#410# range 0 .. 31;
      MDMA_C15BNDTR at 16#414# range 0 .. 31;
      MDMA_C15SAR   at 16#418# range 0 .. 31;
      MDMA_C15DAR   at 16#41C# range 0 .. 31;
      MDMA_C15BRUR  at 16#420# range 0 .. 31;
      MDMA_C15LAR   at 16#424# range 0 .. 31;
      MDMA_C15TBR   at 16#428# range 0 .. 31;
      MDMA_C15MAR   at 16#430# range 0 .. 31;
      MDMA_C15MDR   at 16#434# range 0 .. 31;
   end record;

   --  MDMA
   MDMA_Periph : aliased MDMA_Peripheral
     with Import, Address => MDMA_Base;

end STM32H753.MDMA;
