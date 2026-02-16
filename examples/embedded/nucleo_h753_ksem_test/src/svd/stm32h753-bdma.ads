pragma Style_Checks (Off);

--  This spec has been automatically generated from STM32H753.svd


with System;

package STM32H753.BDMA is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   --  global interrupt flag for channel 0
   type BDMA_ISR_GIF0_Field is
     (--  no TE, HT or TC event
      B_0x0,
      --  a TE, HT or TC event occurred
      B_0x1)
     with Size => 1;
   for BDMA_ISR_GIF0_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  transfer complete (TC) flag for channel 0
   type BDMA_ISR_TCIF0_Field is
     (--  no TC event
      B_0x0,
      --  a TC event occurred
      B_0x1)
     with Size => 1;
   for BDMA_ISR_TCIF0_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  half transfer (HT) flag for channel 0
   type BDMA_ISR_HTIF0_Field is
     (--  no HT event
      B_0x0,
      --  a HT event occurred
      B_0x1)
     with Size => 1;
   for BDMA_ISR_HTIF0_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  transfer error (TE) flag for channel 0
   type BDMA_ISR_TEIF0_Field is
     (--  no TE event
      B_0x0,
      --  a TE event occurred
      B_0x1)
     with Size => 1;
   for BDMA_ISR_TEIF0_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  global interrupt flag for channel 1
   type BDMA_ISR_GIF1_Field is
     (--  no TE, HT or TC event
      B_0x0,
      --  a TE, HT or TC event occurred
      B_0x1)
     with Size => 1;
   for BDMA_ISR_GIF1_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  transfer complete (TC) flag for channel 1
   type BDMA_ISR_TCIF1_Field is
     (--  no TC event
      B_0x0,
      --  a TC event occurred
      B_0x1)
     with Size => 1;
   for BDMA_ISR_TCIF1_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  half transfer (HT) flag for channel 1
   type BDMA_ISR_HTIF1_Field is
     (--  no HT event
      B_0x0,
      --  a HT event occurred
      B_0x1)
     with Size => 1;
   for BDMA_ISR_HTIF1_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  transfer error (TE) flag for channel 1
   type BDMA_ISR_TEIF1_Field is
     (--  no TE event
      B_0x0,
      --  a TE event occurred
      B_0x1)
     with Size => 1;
   for BDMA_ISR_TEIF1_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  global interrupt flag for channel 2
   type BDMA_ISR_GIF2_Field is
     (--  no TE, HT or TC event
      B_0x0,
      --  a TE, HT or TC event occurred
      B_0x1)
     with Size => 1;
   for BDMA_ISR_GIF2_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  transfer complete (TC) flag for channel 2
   type BDMA_ISR_TCIF2_Field is
     (--  no TC event
      B_0x0,
      --  a TC event occurred
      B_0x1)
     with Size => 1;
   for BDMA_ISR_TCIF2_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  half transfer (HT) flag for channel 2
   type BDMA_ISR_HTIF2_Field is
     (--  no HT event
      B_0x0,
      --  a HT event occurred
      B_0x1)
     with Size => 1;
   for BDMA_ISR_HTIF2_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  transfer error (TE) flag for channel 2
   type BDMA_ISR_TEIF2_Field is
     (--  no TE event
      B_0x0,
      --  a TE event occurred
      B_0x1)
     with Size => 1;
   for BDMA_ISR_TEIF2_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  global interrupt flag for channel 3
   type BDMA_ISR_GIF3_Field is
     (--  no TE, HT or TC event
      B_0x0,
      --  a TE, HT or TC event occurred
      B_0x1)
     with Size => 1;
   for BDMA_ISR_GIF3_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  transfer complete (TC) flag for channel 3
   type BDMA_ISR_TCIF3_Field is
     (--  no TC event
      B_0x0,
      --  a TC event occurred
      B_0x1)
     with Size => 1;
   for BDMA_ISR_TCIF3_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  half transfer (HT) flag for channel 3
   type BDMA_ISR_HTIF3_Field is
     (--  no HT event
      B_0x0,
      --  a HT event occurred
      B_0x1)
     with Size => 1;
   for BDMA_ISR_HTIF3_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  transfer error (TE) flag for channel 3
   type BDMA_ISR_TEIF3_Field is
     (--  no TE event
      B_0x0,
      --  a TE event occurred
      B_0x1)
     with Size => 1;
   for BDMA_ISR_TEIF3_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  global interrupt flag for channel 4
   type BDMA_ISR_GIF4_Field is
     (--  no TE, HT or TC event
      B_0x0,
      --  a TE, HT or TC event occurred
      B_0x1)
     with Size => 1;
   for BDMA_ISR_GIF4_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  transfer complete (TC) flag for channel 4
   type BDMA_ISR_TCIF4_Field is
     (--  no TC event
      B_0x0,
      --  a TC event occurred
      B_0x1)
     with Size => 1;
   for BDMA_ISR_TCIF4_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  half transfer (HT) flag for channel 4
   type BDMA_ISR_HTIF4_Field is
     (--  no HT event
      B_0x0,
      --  a HT event occurred
      B_0x1)
     with Size => 1;
   for BDMA_ISR_HTIF4_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  transfer error (TE) flag for channel 4
   type BDMA_ISR_TEIF4_Field is
     (--  no TE event
      B_0x0,
      --  a TE event occurred
      B_0x1)
     with Size => 1;
   for BDMA_ISR_TEIF4_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  global interrupt flag for channel 5
   type BDMA_ISR_GIF5_Field is
     (--  no TE, HT or TC event
      B_0x0,
      --  a TE, HT or TC event occurred
      B_0x1)
     with Size => 1;
   for BDMA_ISR_GIF5_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  transfer complete (TC) flag for channel 5
   type BDMA_ISR_TCIF5_Field is
     (--  no TC event
      B_0x0,
      --  a TC event occurred
      B_0x1)
     with Size => 1;
   for BDMA_ISR_TCIF5_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  half transfer (HT) flag for channel 5
   type BDMA_ISR_HTIF5_Field is
     (--  no HT event
      B_0x0,
      --  a HT event occurred
      B_0x1)
     with Size => 1;
   for BDMA_ISR_HTIF5_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  transfer error (TE) flag for channel 5
   type BDMA_ISR_TEIF5_Field is
     (--  no TE event
      B_0x0,
      --  a TE event occurred
      B_0x1)
     with Size => 1;
   for BDMA_ISR_TEIF5_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  global interrupt flag for channel 6
   type BDMA_ISR_GIF6_Field is
     (--  no TE, HT or TC event
      B_0x0,
      --  a TE, HT or TC event occurred
      B_0x1)
     with Size => 1;
   for BDMA_ISR_GIF6_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  transfer complete (TC) flag for channel 6
   type BDMA_ISR_TCIF6_Field is
     (--  no TC event
      B_0x0,
      --  a TC event occurred
      B_0x1)
     with Size => 1;
   for BDMA_ISR_TCIF6_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  half transfer (HT) flag for channel 6
   type BDMA_ISR_HTIF6_Field is
     (--  no HT event
      B_0x0,
      --  a HT event occurred
      B_0x1)
     with Size => 1;
   for BDMA_ISR_HTIF6_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  transfer error (TE) flag for channel 6
   type BDMA_ISR_TEIF6_Field is
     (--  no TE event
      B_0x0,
      --  a TE event occurred
      B_0x1)
     with Size => 1;
   for BDMA_ISR_TEIF6_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  global interrupt flag for channel 7
   type BDMA_ISR_GIF7_Field is
     (--  no TE, HT or TC event
      B_0x0,
      --  a TE, HT or TC event occurred
      B_0x1)
     with Size => 1;
   for BDMA_ISR_GIF7_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  transfer complete (TC) flag for channel 7
   type BDMA_ISR_TCIF7_Field is
     (--  no TC event
      B_0x0,
      --  a TC event occurred
      B_0x1)
     with Size => 1;
   for BDMA_ISR_TCIF7_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  half transfer (HT) flag for channel 7
   type BDMA_ISR_HTIF7_Field is
     (--  no HT event
      B_0x0,
      --  a HT event occurred
      B_0x1)
     with Size => 1;
   for BDMA_ISR_HTIF7_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  transfer error (TE) flag for channel 7
   type BDMA_ISR_TEIF7_Field is
     (--  no TE event
      B_0x0,
      --  a TE event occurred
      B_0x1)
     with Size => 1;
   for BDMA_ISR_TEIF7_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  BDMA interrupt status register
   type BDMA_ISR_Register is record
      --  Read-only. global interrupt flag for channel 0
      GIF0  : BDMA_ISR_GIF0_Field;
      --  Read-only. transfer complete (TC) flag for channel 0
      TCIF0 : BDMA_ISR_TCIF0_Field;
      --  Read-only. half transfer (HT) flag for channel 0
      HTIF0 : BDMA_ISR_HTIF0_Field;
      --  Read-only. transfer error (TE) flag for channel 0
      TEIF0 : BDMA_ISR_TEIF0_Field;
      --  Read-only. global interrupt flag for channel 1
      GIF1  : BDMA_ISR_GIF1_Field;
      --  Read-only. transfer complete (TC) flag for channel 1
      TCIF1 : BDMA_ISR_TCIF1_Field;
      --  Read-only. half transfer (HT) flag for channel 1
      HTIF1 : BDMA_ISR_HTIF1_Field;
      --  Read-only. transfer error (TE) flag for channel 1
      TEIF1 : BDMA_ISR_TEIF1_Field;
      --  Read-only. global interrupt flag for channel 2
      GIF2  : BDMA_ISR_GIF2_Field;
      --  Read-only. transfer complete (TC) flag for channel 2
      TCIF2 : BDMA_ISR_TCIF2_Field;
      --  Read-only. half transfer (HT) flag for channel 2
      HTIF2 : BDMA_ISR_HTIF2_Field;
      --  Read-only. transfer error (TE) flag for channel 2
      TEIF2 : BDMA_ISR_TEIF2_Field;
      --  Read-only. global interrupt flag for channel 3
      GIF3  : BDMA_ISR_GIF3_Field;
      --  Read-only. transfer complete (TC) flag for channel 3
      TCIF3 : BDMA_ISR_TCIF3_Field;
      --  Read-only. half transfer (HT) flag for channel 3
      HTIF3 : BDMA_ISR_HTIF3_Field;
      --  Read-only. transfer error (TE) flag for channel 3
      TEIF3 : BDMA_ISR_TEIF3_Field;
      --  Read-only. global interrupt flag for channel 4
      GIF4  : BDMA_ISR_GIF4_Field;
      --  Read-only. transfer complete (TC) flag for channel 4
      TCIF4 : BDMA_ISR_TCIF4_Field;
      --  Read-only. half transfer (HT) flag for channel 4
      HTIF4 : BDMA_ISR_HTIF4_Field;
      --  Read-only. transfer error (TE) flag for channel 4
      TEIF4 : BDMA_ISR_TEIF4_Field;
      --  Read-only. global interrupt flag for channel 5
      GIF5  : BDMA_ISR_GIF5_Field;
      --  Read-only. transfer complete (TC) flag for channel 5
      TCIF5 : BDMA_ISR_TCIF5_Field;
      --  Read-only. half transfer (HT) flag for channel 5
      HTIF5 : BDMA_ISR_HTIF5_Field;
      --  Read-only. transfer error (TE) flag for channel 5
      TEIF5 : BDMA_ISR_TEIF5_Field;
      --  Read-only. global interrupt flag for channel 6
      GIF6  : BDMA_ISR_GIF6_Field;
      --  Read-only. transfer complete (TC) flag for channel 6
      TCIF6 : BDMA_ISR_TCIF6_Field;
      --  Read-only. half transfer (HT) flag for channel 6
      HTIF6 : BDMA_ISR_HTIF6_Field;
      --  Read-only. transfer error (TE) flag for channel 6
      TEIF6 : BDMA_ISR_TEIF6_Field;
      --  Read-only. global interrupt flag for channel 7
      GIF7  : BDMA_ISR_GIF7_Field;
      --  Read-only. transfer complete (TC) flag for channel 7
      TCIF7 : BDMA_ISR_TCIF7_Field;
      --  Read-only. half transfer (HT) flag for channel 7
      HTIF7 : BDMA_ISR_HTIF7_Field;
      --  Read-only. transfer error (TE) flag for channel 7
      TEIF7 : BDMA_ISR_TEIF7_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for BDMA_ISR_Register use record
      GIF0  at 0 range 0 .. 0;
      TCIF0 at 0 range 1 .. 1;
      HTIF0 at 0 range 2 .. 2;
      TEIF0 at 0 range 3 .. 3;
      GIF1  at 0 range 4 .. 4;
      TCIF1 at 0 range 5 .. 5;
      HTIF1 at 0 range 6 .. 6;
      TEIF1 at 0 range 7 .. 7;
      GIF2  at 0 range 8 .. 8;
      TCIF2 at 0 range 9 .. 9;
      HTIF2 at 0 range 10 .. 10;
      TEIF2 at 0 range 11 .. 11;
      GIF3  at 0 range 12 .. 12;
      TCIF3 at 0 range 13 .. 13;
      HTIF3 at 0 range 14 .. 14;
      TEIF3 at 0 range 15 .. 15;
      GIF4  at 0 range 16 .. 16;
      TCIF4 at 0 range 17 .. 17;
      HTIF4 at 0 range 18 .. 18;
      TEIF4 at 0 range 19 .. 19;
      GIF5  at 0 range 20 .. 20;
      TCIF5 at 0 range 21 .. 21;
      HTIF5 at 0 range 22 .. 22;
      TEIF5 at 0 range 23 .. 23;
      GIF6  at 0 range 24 .. 24;
      TCIF6 at 0 range 25 .. 25;
      HTIF6 at 0 range 26 .. 26;
      TEIF6 at 0 range 27 .. 27;
      GIF7  at 0 range 28 .. 28;
      TCIF7 at 0 range 29 .. 29;
      HTIF7 at 0 range 30 .. 30;
      TEIF7 at 0 range 31 .. 31;
   end record;

   --  BDMA interrupt flag clear register
   type BDMA_IFCR_Register is record
      --  Write-only. global interrupt flag clear for channel 0
      CGIF0  : Boolean := False;
      --  Write-only. transfer complete flag clear for channel 0
      CTCIF0 : Boolean := False;
      --  Write-only. half transfer flag clear for channel 0
      CHTIF0 : Boolean := False;
      --  Write-only. transfer error flag clear for channel 0
      CTEIF0 : Boolean := False;
      --  Write-only. global interrupt flag clear for channel 0
      CGIF1  : Boolean := False;
      --  Write-only. transfer complete flag clear for channel 1
      CTCIF1 : Boolean := False;
      --  Write-only. half transfer flag clear for channel 1
      CHTIF1 : Boolean := False;
      --  Write-only. transfer error flag clear for channel 1
      CTEIF1 : Boolean := False;
      --  Write-only. global interrupt flag clear for channel 2
      CGIF2  : Boolean := False;
      --  Write-only. transfer complete flag clear for channel 2
      CTCIF2 : Boolean := False;
      --  Write-only. half transfer flag clear for channe2
      CHTIF2 : Boolean := False;
      --  Write-only. transfer error flag clear for channel 2
      CTEIF2 : Boolean := False;
      --  Write-only. global interrupt flag clear for channel 3
      CGIF3  : Boolean := False;
      --  Write-only. transfer complete flag clear for channel 3
      CTCIF3 : Boolean := False;
      --  Write-only. half transfer flag clear for channel 3
      CHTIF3 : Boolean := False;
      --  Write-only. transfer error flag clear for channel 3
      CTEIF3 : Boolean := False;
      --  Write-only. global interrupt flag clear for channel 4
      CGIF4  : Boolean := False;
      --  Write-only. transfer complete flag clear for channel 4
      CTCIF4 : Boolean := False;
      --  Write-only. half transfer flag clear for channel 4
      CHTIF4 : Boolean := False;
      --  Write-only. transfer error flag clear for channel 4
      CTEIF4 : Boolean := False;
      --  Write-only. global interrupt flag clear for channel 5
      CGIF5  : Boolean := False;
      --  Write-only. transfer complete flag clear for channel 5
      CTCIF5 : Boolean := False;
      --  Write-only. half transfer flag clear for channel 5
      CHTIF5 : Boolean := False;
      --  Write-only. transfer error flag clear for channel 5
      CTEIF5 : Boolean := False;
      --  Write-only. global interrupt flag clear for channel 6
      CGIF6  : Boolean := False;
      --  Write-only. transfer complete flag clear for channel 6
      CTCIF6 : Boolean := False;
      --  Write-only. half transfer flag clear for channel 6
      CHTIF6 : Boolean := False;
      --  Write-only. transfer error flag clear for channel 6
      CTEIF6 : Boolean := False;
      --  Write-only. global interrupt flag clear for channel 7
      CGIF7  : Boolean := False;
      --  Write-only. transfer complete flag clear for channel 7
      CTCIF7 : Boolean := False;
      --  Write-only. half transfer flag clear for channel 7
      CHTIF7 : Boolean := False;
      --  Write-only. transfer error flag clear for channel 7
      CTEIF7 : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for BDMA_IFCR_Register use record
      CGIF0  at 0 range 0 .. 0;
      CTCIF0 at 0 range 1 .. 1;
      CHTIF0 at 0 range 2 .. 2;
      CTEIF0 at 0 range 3 .. 3;
      CGIF1  at 0 range 4 .. 4;
      CTCIF1 at 0 range 5 .. 5;
      CHTIF1 at 0 range 6 .. 6;
      CTEIF1 at 0 range 7 .. 7;
      CGIF2  at 0 range 8 .. 8;
      CTCIF2 at 0 range 9 .. 9;
      CHTIF2 at 0 range 10 .. 10;
      CTEIF2 at 0 range 11 .. 11;
      CGIF3  at 0 range 12 .. 12;
      CTCIF3 at 0 range 13 .. 13;
      CHTIF3 at 0 range 14 .. 14;
      CTEIF3 at 0 range 15 .. 15;
      CGIF4  at 0 range 16 .. 16;
      CTCIF4 at 0 range 17 .. 17;
      CHTIF4 at 0 range 18 .. 18;
      CTEIF4 at 0 range 19 .. 19;
      CGIF5  at 0 range 20 .. 20;
      CTCIF5 at 0 range 21 .. 21;
      CHTIF5 at 0 range 22 .. 22;
      CTEIF5 at 0 range 23 .. 23;
      CGIF6  at 0 range 24 .. 24;
      CTCIF6 at 0 range 25 .. 25;
      CHTIF6 at 0 range 26 .. 26;
      CTEIF6 at 0 range 27 .. 27;
      CGIF7  at 0 range 28 .. 28;
      CTCIF7 at 0 range 29 .. 29;
      CHTIF7 at 0 range 30 .. 30;
      CTEIF7 at 0 range 31 .. 31;
   end record;

   --  channel enable When a channel transfer error occurs, this bit is cleared
   --  by hardware. It can not be set again by software (channel x
   --  re-activated) until the TEIFx bit of the BDMA_ISR register is cleared
   --  (by setting the CTEIFx bit of the BDMA_IFCR register). Note: this bit is
   --  set and cleared by software.
   type BDMA_CCR0_EN_Field is
     (--  disabled
      B_0x0,
      --  enabled
      B_0x1)
     with Size => 1;
   for BDMA_CCR0_EN_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  transfer complete interrupt enable Note: this bit is set and cleared by
   --  software. It must not be written when the channel is enabled (EN = 1).
   --  It is not read-only when the channel is enabled (EN = 1).
   type BDMA_CCR0_TCIE_Field is
     (--  disabled
      B_0x0,
      --  enabled
      B_0x1)
     with Size => 1;
   for BDMA_CCR0_TCIE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  half transfer interrupt enable Note: this bit is set and cleared by
   --  software. It must not be written when the channel is enabled (EN = 1).
   --  It is not read-only when the channel is enabled (EN = 1).
   type BDMA_CCR0_HTIE_Field is
     (--  disabled
      B_0x0,
      --  enabled
      B_0x1)
     with Size => 1;
   for BDMA_CCR0_HTIE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  transfer error interrupt enable Note: this bit is set and cleared by
   --  software. It must not be written when the channel is enabled (EN = 1).
   --  It is not read-only when the channel is enabled (EN = 1).
   type BDMA_CCR0_TEIE_Field is
     (--  disabled
      B_0x0,
      --  enabled
      B_0x1)
     with Size => 1;
   for BDMA_CCR0_TEIE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  data transfer direction This bit must be set only in
   --  memory-to-peripheral and peripheral-to-memory modes. Source attributes
   --  are defined by PSIZE and PINC, plus the BDMA_CPARx register. This is
   --  still valid in a memory-to-memory mode. Destination attributes are
   --  defined by MSIZE and MINC, plus the BDMA_CM0/1ARx register. This is
   --  still valid in a peripheral-to-peripheral mode. Destination attributes
   --  are defined by PSIZE and PINC, plus the BDMA_CPARx register. This is
   --  still valid in a memory-to-memory mode. Source attributes are defined by
   --  MSIZE and MINC, plus the BDMA_CM0/1ARx register. This is still valid in
   --  a peripheral-to-peripheral mode. Note: this bit is set and cleared by
   --  software. It must not be written when the channel is enabled (EN = 1).
   --  It is read-only when the channel is enabled (EN = 1).
   type BDMA_CCR0_DIR_Field is
     (--  read from peripheral
      B_0x0,
      --  read from memory
      B_0x1)
     with Size => 1;
   for BDMA_CCR0_DIR_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  circular mode Note: this bit is set and cleared by software. It must not
   --  be written when the channel is enabled (EN = 1). It is not read-only
   --  when the channel is enabled (EN = 1).
   type BDMA_CCR0_CIRC_Field is
     (--  disabled
      B_0x0,
      --  enabled
      B_0x1)
     with Size => 1;
   for BDMA_CCR0_CIRC_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  peripheral increment mode Defines the increment mode for each DMA
   --  transfer to the identified peripheral. n memory-to-memory mode, this
   --  field identifies the memory destination if DIR = 1 and the memory source
   --  if DIR = 0. In peripheral-to-peripheral mode, this field identifies the
   --  peripheral destination if DIR = 1 and the peripheral source if DIR = 0.
   --  Note: this bit is set and cleared by software. It must not be written
   --  when the channel is enabled (EN = 1). It is read-only when the channel
   --  is enabled (EN = 1).
   type BDMA_CCR0_PINC_Field is
     (--  disabled
      B_0x0,
      --  enabled
      B_0x1)
     with Size => 1;
   for BDMA_CCR0_PINC_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  memory increment mode Defines the increment mode for each DMA transfer
   --  to the identified memory. In memory-to-memory mode, this field
   --  identifies the memory source if DIR = 1 and the memory destination if
   --  DIR = 0. In peripheral-to-peripheral mode, this field identifies the
   --  peripheral source if DIR = 1 and the peripheral destination if DIR = 0.
   --  Note: this bit is set and cleared by software. It must not be written
   --  when the channel is enabled (EN = 1). It is read-only when the channel
   --  is enabled (EN = 1).
   type BDMA_CCR0_MINC_Field is
     (--  disabled
      B_0x0,
      --  enabled
      B_0x1)
     with Size => 1;
   for BDMA_CCR0_MINC_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  peripheral size Defines the data size of each DMA transfer to the
   --  identified peripheral. In memory-to-memory mode, this field identifies
   --  the memory destination if DIR = 1 and the memory source if DIR = 0. In
   --  peripheral-to-peripheral mode, this field identifies the peripheral
   --  destination if DIR = 1 and the peripheral source if DIR = 0. Note: this
   --  field is set and cleared by software. It must not be written when the
   --  channel is enabled (EN = 1). It is read-only when the channel is enabled
   --  (EN = 1).
   type BDMA_CCR0_PSIZE_Field is
     (--  8 bits
      B_0x0,
      --  16 bits
      B_0x1,
      --  32 bits
      B_0x2)
     with Size => 2;
   for BDMA_CCR0_PSIZE_Field use
     (B_0x0 => 0,
      B_0x1 => 1,
      B_0x2 => 2);

   --  memory size Defines the data size of each DMA transfer to the identified
   --  memory. In memory-to-memory mode, this field identifies the memory
   --  source if DIR = 1 and the memory destination if DIR = 0. In
   --  peripheral-to-peripheral mode, this field identifies the peripheral
   --  source if DIR = 1 and the peripheral destination if DIR = 0. Note: this
   --  field is set and cleared by software. It must not be written when the
   --  channel is enabled (EN = 1). It is read-only when the channel is enabled
   --  (EN = 1).
   type BDMA_CCR0_MSIZE_Field is
     (--  8 bits
      B_0x0,
      --  16 bits
      B_0x1,
      --  32 bits
      B_0x2)
     with Size => 2;
   for BDMA_CCR0_MSIZE_Field use
     (B_0x0 => 0,
      B_0x1 => 1,
      B_0x2 => 2);

   --  priority level Note: this field is set and cleared by software. It must
   --  not be written when the channel is enabled (EN = 1). It is read-only
   --  when the channel is enabled (EN = 1).
   type BDMA_CCR0_PL_Field is
     (--  low
      B_0x0,
      --  medium
      B_0x1,
      --  high
      B_0x2,
      --  very high
      B_0x3)
     with Size => 2;
   for BDMA_CCR0_PL_Field use
     (B_0x0 => 0,
      B_0x1 => 1,
      B_0x2 => 2,
      B_0x3 => 3);

   --  memory-to-memory mode Note: this bit is set and cleared by software. It
   --  must not be written when the channel is enabled (EN = 1). It is
   --  read-only when the channel is enabled (EN = 1).
   type BDMA_CCR0_MEM2MEM_Field is
     (--  disabled
      B_0x0,
      --  enabled
      B_0x1)
     with Size => 1;
   for BDMA_CCR0_MEM2MEM_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  double-buffer mode This bit must be set only in memory-to-peripheral and
   --  peripheral-to-memory transfers (MEM2MEM=0). The CIRC bit must also be
   --  set in double buffer mode. Note: this bit is set and cleared by
   --  software. It must not be written when the channel is enabled (EN = 1).
   --  It is not read-only when the channel is enabled (EN = 1).
   type BDMA_CCR0_DBM_Field is
     (--  disabled (no memory address switch at the end of the BDMA transfer)
      B_0x0,
      --  enabled (memory address switched at the end of the BDMA transfer)
      B_0x1)
     with Size => 1;
   for BDMA_CCR0_DBM_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  current target memory of DMA transfer in double-buffer mode This bit is
   --  toggled by hardware at the end of each channel transfer in double-buffer
   --  mode. Note: this bit is set and cleared by software. It must not be
   --  written when the channel is enabled (EN = 1). It is read-only when the
   --  channel is enabled (EN = 1).
   type BDMA_CCR0_CT_Field is
     (--  memory 0 (addressed by the BDMA_CM0AR pointer)
      B_0x0,
      --  memory 1 (addressed by the BDMA_CM1AR pointer)
      B_0x1)
     with Size => 1;
   for BDMA_CCR0_CT_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  BDMA channel 0 configuration register
   type BDMA_CCR_Register is record
      --  channel enable When a channel transfer error occurs, this bit is
      --  cleared by hardware. It can not be set again by software (channel x
      --  re-activated) until the TEIFx bit of the BDMA_ISR register is cleared
      --  (by setting the CTEIFx bit of the BDMA_IFCR register). Note: this bit
      --  is set and cleared by software.
      EN             : BDMA_CCR0_EN_Field := STM32H753.BDMA.B_0x0;
      --  transfer complete interrupt enable Note: this bit is set and cleared
      --  by software. It must not be written when the channel is enabled (EN =
      --  1). It is not read-only when the channel is enabled (EN = 1).
      TCIE           : BDMA_CCR0_TCIE_Field := STM32H753.BDMA.B_0x0;
      --  half transfer interrupt enable Note: this bit is set and cleared by
      --  software. It must not be written when the channel is enabled (EN =
      --  1). It is not read-only when the channel is enabled (EN = 1).
      HTIE           : BDMA_CCR0_HTIE_Field := STM32H753.BDMA.B_0x0;
      --  transfer error interrupt enable Note: this bit is set and cleared by
      --  software. It must not be written when the channel is enabled (EN =
      --  1). It is not read-only when the channel is enabled (EN = 1).
      TEIE           : BDMA_CCR0_TEIE_Field := STM32H753.BDMA.B_0x0;
      --  data transfer direction This bit must be set only in
      --  memory-to-peripheral and peripheral-to-memory modes. Source
      --  attributes are defined by PSIZE and PINC, plus the BDMA_CPARx
      --  register. This is still valid in a memory-to-memory mode. Destination
      --  attributes are defined by MSIZE and MINC, plus the BDMA_CM0/1ARx
      --  register. This is still valid in a peripheral-to-peripheral mode.
      --  Destination attributes are defined by PSIZE and PINC, plus the
      --  BDMA_CPARx register. This is still valid in a memory-to-memory mode.
      --  Source attributes are defined by MSIZE and MINC, plus the
      --  BDMA_CM0/1ARx register. This is still valid in a
      --  peripheral-to-peripheral mode. Note: this bit is set and cleared by
      --  software. It must not be written when the channel is enabled (EN =
      --  1). It is read-only when the channel is enabled (EN = 1).
      DIR            : BDMA_CCR0_DIR_Field := STM32H753.BDMA.B_0x0;
      --  circular mode Note: this bit is set and cleared by software. It must
      --  not be written when the channel is enabled (EN = 1). It is not
      --  read-only when the channel is enabled (EN = 1).
      CIRC           : BDMA_CCR0_CIRC_Field := STM32H753.BDMA.B_0x0;
      --  peripheral increment mode Defines the increment mode for each DMA
      --  transfer to the identified peripheral. n memory-to-memory mode, this
      --  field identifies the memory destination if DIR = 1 and the memory
      --  source if DIR = 0. In peripheral-to-peripheral mode, this field
      --  identifies the peripheral destination if DIR = 1 and the peripheral
      --  source if DIR = 0. Note: this bit is set and cleared by software. It
      --  must not be written when the channel is enabled (EN = 1). It is
      --  read-only when the channel is enabled (EN = 1).
      PINC           : BDMA_CCR0_PINC_Field := STM32H753.BDMA.B_0x0;
      --  memory increment mode Defines the increment mode for each DMA
      --  transfer to the identified memory. In memory-to-memory mode, this
      --  field identifies the memory source if DIR = 1 and the memory
      --  destination if DIR = 0. In peripheral-to-peripheral mode, this field
      --  identifies the peripheral source if DIR = 1 and the peripheral
      --  destination if DIR = 0. Note: this bit is set and cleared by
      --  software. It must not be written when the channel is enabled (EN =
      --  1). It is read-only when the channel is enabled (EN = 1).
      MINC           : BDMA_CCR0_MINC_Field := STM32H753.BDMA.B_0x0;
      --  peripheral size Defines the data size of each DMA transfer to the
      --  identified peripheral. In memory-to-memory mode, this field
      --  identifies the memory destination if DIR = 1 and the memory source if
      --  DIR = 0. In peripheral-to-peripheral mode, this field identifies the
      --  peripheral destination if DIR = 1 and the peripheral source if DIR =
      --  0. Note: this field is set and cleared by software. It must not be
      --  written when the channel is enabled (EN = 1). It is read-only when
      --  the channel is enabled (EN = 1).
      PSIZE          : BDMA_CCR0_PSIZE_Field := STM32H753.BDMA.B_0x0;
      --  memory size Defines the data size of each DMA transfer to the
      --  identified memory. In memory-to-memory mode, this field identifies
      --  the memory source if DIR = 1 and the memory destination if DIR = 0.
      --  In peripheral-to-peripheral mode, this field identifies the
      --  peripheral source if DIR = 1 and the peripheral destination if DIR =
      --  0. Note: this field is set and cleared by software. It must not be
      --  written when the channel is enabled (EN = 1). It is read-only when
      --  the channel is enabled (EN = 1).
      MSIZE          : BDMA_CCR0_MSIZE_Field := STM32H753.BDMA.B_0x0;
      --  priority level Note: this field is set and cleared by software. It
      --  must not be written when the channel is enabled (EN = 1). It is
      --  read-only when the channel is enabled (EN = 1).
      PL             : BDMA_CCR0_PL_Field := STM32H753.BDMA.B_0x0;
      --  memory-to-memory mode Note: this bit is set and cleared by software.
      --  It must not be written when the channel is enabled (EN = 1). It is
      --  read-only when the channel is enabled (EN = 1).
      MEM2MEM        : BDMA_CCR0_MEM2MEM_Field := STM32H753.BDMA.B_0x0;
      --  double-buffer mode This bit must be set only in memory-to-peripheral
      --  and peripheral-to-memory transfers (MEM2MEM=0). The CIRC bit must
      --  also be set in double buffer mode. Note: this bit is set and cleared
      --  by software. It must not be written when the channel is enabled (EN =
      --  1). It is not read-only when the channel is enabled (EN = 1).
      DBM            : BDMA_CCR0_DBM_Field := STM32H753.BDMA.B_0x0;
      --  current target memory of DMA transfer in double-buffer mode This bit
      --  is toggled by hardware at the end of each channel transfer in
      --  double-buffer mode. Note: this bit is set and cleared by software. It
      --  must not be written when the channel is enabled (EN = 1). It is
      --  read-only when the channel is enabled (EN = 1).
      CT             : BDMA_CCR0_CT_Field := STM32H753.BDMA.B_0x0;
      --  unspecified
      Reserved_17_31 : STM32H753.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for BDMA_CCR_Register use record
      EN             at 0 range 0 .. 0;
      TCIE           at 0 range 1 .. 1;
      HTIE           at 0 range 2 .. 2;
      TEIE           at 0 range 3 .. 3;
      DIR            at 0 range 4 .. 4;
      CIRC           at 0 range 5 .. 5;
      PINC           at 0 range 6 .. 6;
      MINC           at 0 range 7 .. 7;
      PSIZE          at 0 range 8 .. 9;
      MSIZE          at 0 range 10 .. 11;
      PL             at 0 range 12 .. 13;
      MEM2MEM        at 0 range 14 .. 14;
      DBM            at 0 range 15 .. 15;
      CT             at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype BDMA_CNDTR_NDT_Field is STM32H753.UInt16;

   type BDMA_CNDTR_Register is record
      --  number of data to transfer (0 to 216 - 1) This field is updated by
      --  hardware when the channel is enabled: It is decremented after each
      --  single BDMA 'read followed by write' transfer, indicating the
      --  remaining amount of data items to transfer. It is kept at zero when
      --  the programmed amount of data to transfer is reached, if the channel
      --  is not in circular mode (CIRC = 0 in the BDMA_CCRx register). It is
      --  reloaded automatically by the previously programmed value, when the
      --  transfer is complete, if the channel is in circular mode (CIRC = 1).
      --  If this field is zero, no transfer can be served whatever the channel
      --  status (enabled or not). Note: this field is set and cleared by
      --  software. It must not be written when the channel is enabled (EN =
      --  1). It is read-only when the channel is enabled (EN = 1).
      NDT            : BDMA_CNDTR_NDT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for BDMA_CNDTR_Register use record
      NDT            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  BDMA
   type BDMA_Peripheral is record
      --  BDMA interrupt status register
      BDMA_ISR    : aliased BDMA_ISR_Register;
      --  BDMA interrupt flag clear register
      BDMA_IFCR   : aliased BDMA_IFCR_Register;
      --  BDMA channel 0 configuration register
      BDMA_CCR0   : aliased BDMA_CCR_Register;
      BDMA_CNDTR0 : aliased BDMA_CNDTR_Register;
      BDMA_CPAR0  : aliased STM32H753.UInt32;
      BDMA_CM0AR0 : aliased STM32H753.UInt32;
      BDMA_CM1AR0 : aliased STM32H753.UInt32;
      --  BDMA channel 1 configuration register
      BDMA_CCR1   : aliased BDMA_CCR_Register;
      BDMA_CNDTR1 : aliased BDMA_CNDTR_Register;
      BDMA_CPAR1  : aliased STM32H753.UInt32;
      BDMA_CM0AR1 : aliased STM32H753.UInt32;
      BDMA_CM1AR1 : aliased STM32H753.UInt32;
      --  BDMA channel 2 configuration register
      BDMA_CCR2   : aliased BDMA_CCR_Register;
      BDMA_CNDTR2 : aliased BDMA_CNDTR_Register;
      BDMA_CPAR2  : aliased STM32H753.UInt32;
      BDMA_CM0AR2 : aliased STM32H753.UInt32;
      BDMA_CM1AR2 : aliased STM32H753.UInt32;
      --  BDMA channel 3 configuration register
      BDMA_CCR3   : aliased BDMA_CCR_Register;
      BDMA_CNDTR3 : aliased BDMA_CNDTR_Register;
      BDMA_CPAR3  : aliased STM32H753.UInt32;
      BDMA_CM0AR3 : aliased STM32H753.UInt32;
      BDMA_CM1AR3 : aliased STM32H753.UInt32;
      --  BDMA channel 4 configuration register
      BDMA_CCR4   : aliased BDMA_CCR_Register;
      BDMA_CNDTR4 : aliased BDMA_CNDTR_Register;
      BDMA_CPAR4  : aliased STM32H753.UInt32;
      BDMA_CM0AR4 : aliased STM32H753.UInt32;
      BDMA_CM1AR4 : aliased STM32H753.UInt32;
      --  BDMA channel 5 configuration register
      BDMA_CCR5   : aliased BDMA_CCR_Register;
      BDMA_CNDTR5 : aliased BDMA_CNDTR_Register;
      BDMA_CPAR5  : aliased STM32H753.UInt32;
      BDMA_CM0AR5 : aliased STM32H753.UInt32;
      BDMA_CM1AR5 : aliased STM32H753.UInt32;
      --  BDMA channel 6 configuration register
      BDMA_CCR6   : aliased BDMA_CCR_Register;
      BDMA_CNDTR6 : aliased BDMA_CNDTR_Register;
      BDMA_CPAR6  : aliased STM32H753.UInt32;
      BDMA_CM0AR6 : aliased STM32H753.UInt32;
      BDMA_CM1AR6 : aliased STM32H753.UInt32;
      --  BDMA channel 7 configuration register
      BDMA_CCR7   : aliased BDMA_CCR_Register;
      BDMA_CNDTR7 : aliased BDMA_CNDTR_Register;
      BDMA_CPAR7  : aliased STM32H753.UInt32;
      BDMA_CM0AR7 : aliased STM32H753.UInt32;
      BDMA_CM1AR7 : aliased STM32H753.UInt32;
   end record
     with Volatile;

   for BDMA_Peripheral use record
      BDMA_ISR    at 16#0# range 0 .. 31;
      BDMA_IFCR   at 16#4# range 0 .. 31;
      BDMA_CCR0   at 16#8# range 0 .. 31;
      BDMA_CNDTR0 at 16#C# range 0 .. 31;
      BDMA_CPAR0  at 16#10# range 0 .. 31;
      BDMA_CM0AR0 at 16#14# range 0 .. 31;
      BDMA_CM1AR0 at 16#18# range 0 .. 31;
      BDMA_CCR1   at 16#1C# range 0 .. 31;
      BDMA_CNDTR1 at 16#20# range 0 .. 31;
      BDMA_CPAR1  at 16#24# range 0 .. 31;
      BDMA_CM0AR1 at 16#28# range 0 .. 31;
      BDMA_CM1AR1 at 16#2C# range 0 .. 31;
      BDMA_CCR2   at 16#30# range 0 .. 31;
      BDMA_CNDTR2 at 16#34# range 0 .. 31;
      BDMA_CPAR2  at 16#38# range 0 .. 31;
      BDMA_CM0AR2 at 16#3C# range 0 .. 31;
      BDMA_CM1AR2 at 16#40# range 0 .. 31;
      BDMA_CCR3   at 16#44# range 0 .. 31;
      BDMA_CNDTR3 at 16#48# range 0 .. 31;
      BDMA_CPAR3  at 16#4C# range 0 .. 31;
      BDMA_CM0AR3 at 16#50# range 0 .. 31;
      BDMA_CM1AR3 at 16#54# range 0 .. 31;
      BDMA_CCR4   at 16#58# range 0 .. 31;
      BDMA_CNDTR4 at 16#5C# range 0 .. 31;
      BDMA_CPAR4  at 16#60# range 0 .. 31;
      BDMA_CM0AR4 at 16#64# range 0 .. 31;
      BDMA_CM1AR4 at 16#68# range 0 .. 31;
      BDMA_CCR5   at 16#6C# range 0 .. 31;
      BDMA_CNDTR5 at 16#70# range 0 .. 31;
      BDMA_CPAR5  at 16#74# range 0 .. 31;
      BDMA_CM0AR5 at 16#78# range 0 .. 31;
      BDMA_CM1AR5 at 16#7C# range 0 .. 31;
      BDMA_CCR6   at 16#80# range 0 .. 31;
      BDMA_CNDTR6 at 16#84# range 0 .. 31;
      BDMA_CPAR6  at 16#88# range 0 .. 31;
      BDMA_CM0AR6 at 16#8C# range 0 .. 31;
      BDMA_CM1AR6 at 16#90# range 0 .. 31;
      BDMA_CCR7   at 16#94# range 0 .. 31;
      BDMA_CNDTR7 at 16#98# range 0 .. 31;
      BDMA_CPAR7  at 16#9C# range 0 .. 31;
      BDMA_CM0AR7 at 16#A0# range 0 .. 31;
      BDMA_CM1AR7 at 16#A4# range 0 .. 31;
   end record;

   --  BDMA
   BDMA_Periph : aliased BDMA_Peripheral
     with Import, Address => BDMA_Base;

end STM32H753.BDMA;
