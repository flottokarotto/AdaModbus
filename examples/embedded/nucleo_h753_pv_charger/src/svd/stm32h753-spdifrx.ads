pragma Style_Checks (Off);

--  This spec has been automatically generated from STM32H753.svd


with System;

package STM32H753.SPDIFRX is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   subtype CR_SPDIFRXEN_Field is STM32H753.UInt2;
   subtype CR_DRFMT_Field is STM32H753.UInt2;
   subtype CR_NBTR_Field is STM32H753.UInt2;
   subtype CR_INSEL_Field is STM32H753.UInt3;

   --  Control register
   type CR_Register is record
      --  Peripheral Block Enable
      SPDIFRXEN      : CR_SPDIFRXEN_Field := 16#0#;
      --  Receiver DMA ENable for data flow
      RXDMAEN        : Boolean := False;
      --  STerEO Mode
      RXSTEO         : Boolean := False;
      --  RX Data format
      DRFMT          : CR_DRFMT_Field := 16#0#;
      --  Mask Parity error bit
      PMSK           : Boolean := False;
      --  Mask of Validity bit
      VMSK           : Boolean := False;
      --  Mask of channel status and user bits
      CUMSK          : Boolean := False;
      --  Mask of Preamble Type bits
      PTMSK          : Boolean := False;
      --  Control Buffer DMA ENable for control flow
      CBDMAEN        : Boolean := False;
      --  Channel Selection
      CHSEL          : Boolean := False;
      --  Maximum allowed re-tries during synchronization phase
      NBTR           : CR_NBTR_Field := 16#0#;
      --  Wait For Activity
      WFA            : Boolean := False;
      --  unspecified
      Reserved_15_15 : STM32H753.Bit := 16#0#;
      --  input selection
      INSEL          : CR_INSEL_Field := 16#0#;
      --  unspecified
      Reserved_19_19 : STM32H753.Bit := 16#0#;
      --  Symbol Clock Enable
      CKSEN          : Boolean := False;
      --  Backup Symbol Clock Enable
      CKSBKPEN       : Boolean := False;
      --  unspecified
      Reserved_22_31 : STM32H753.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      SPDIFRXEN      at 0 range 0 .. 1;
      RXDMAEN        at 0 range 2 .. 2;
      RXSTEO         at 0 range 3 .. 3;
      DRFMT          at 0 range 4 .. 5;
      PMSK           at 0 range 6 .. 6;
      VMSK           at 0 range 7 .. 7;
      CUMSK          at 0 range 8 .. 8;
      PTMSK          at 0 range 9 .. 9;
      CBDMAEN        at 0 range 10 .. 10;
      CHSEL          at 0 range 11 .. 11;
      NBTR           at 0 range 12 .. 13;
      WFA            at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      INSEL          at 0 range 16 .. 18;
      Reserved_19_19 at 0 range 19 .. 19;
      CKSEN          at 0 range 20 .. 20;
      CKSBKPEN       at 0 range 21 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  Interrupt mask register
   type IMR_Register is record
      --  RXNE interrupt enable
      RXNEIE        : Boolean := False;
      --  Control Buffer Ready Interrupt Enable
      CSRNEIE       : Boolean := False;
      --  Parity error interrupt enable
      PERRIE        : Boolean := False;
      --  Overrun error Interrupt Enable
      OVRIE         : Boolean := False;
      --  Synchronization Block Detected Interrupt Enable
      SBLKIE        : Boolean := False;
      --  Synchronization Done
      SYNCDIE       : Boolean := False;
      --  Serial Interface Error Interrupt Enable
      IFEIE         : Boolean := False;
      --  unspecified
      Reserved_7_31 : STM32H753.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IMR_Register use record
      RXNEIE        at 0 range 0 .. 0;
      CSRNEIE       at 0 range 1 .. 1;
      PERRIE        at 0 range 2 .. 2;
      OVRIE         at 0 range 3 .. 3;
      SBLKIE        at 0 range 4 .. 4;
      SYNCDIE       at 0 range 5 .. 5;
      IFEIE         at 0 range 6 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   subtype SR_WIDTH5_Field is STM32H753.UInt15;

   --  Status register
   type SR_Register is record
      --  Read-only. Read data register not empty
      RXNE           : Boolean;
      --  Read-only. Control Buffer register is not empty
      CSRNE          : Boolean;
      --  Read-only. Parity error
      PERR           : Boolean;
      --  Read-only. Overrun error
      OVR            : Boolean;
      --  Read-only. Synchronization Block Detected
      SBD            : Boolean;
      --  Read-only. Synchronization Done
      SYNCD          : Boolean;
      --  Read-only. Framing error
      FERR           : Boolean;
      --  Read-only. Synchronization error
      SERR           : Boolean;
      --  Read-only. Time-out error
      TERR           : Boolean;
      --  unspecified
      Reserved_9_15  : STM32H753.UInt7;
      --  Read-only. Duration of 5 symbols counted with SPDIF_CLK
      WIDTH5         : SR_WIDTH5_Field;
      --  unspecified
      Reserved_31_31 : STM32H753.Bit;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register use record
      RXNE           at 0 range 0 .. 0;
      CSRNE          at 0 range 1 .. 1;
      PERR           at 0 range 2 .. 2;
      OVR            at 0 range 3 .. 3;
      SBD            at 0 range 4 .. 4;
      SYNCD          at 0 range 5 .. 5;
      FERR           at 0 range 6 .. 6;
      SERR           at 0 range 7 .. 7;
      TERR           at 0 range 8 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      WIDTH5         at 0 range 16 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   --  Interrupt Flag Clear register
   type IFCR_Register is record
      --  unspecified
      Reserved_0_1  : STM32H753.UInt2 := 16#0#;
      --  Write-only. Clears the Parity error flag
      PERRCF        : Boolean := False;
      --  Write-only. Clears the Overrun error flag
      OVRCF         : Boolean := False;
      --  Write-only. Clears the Synchronization Block Detected flag
      SBDCF         : Boolean := False;
      --  Write-only. Clears the Synchronization Done flag
      SYNCDCF       : Boolean := False;
      --  unspecified
      Reserved_6_31 : STM32H753.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IFCR_Register use record
      Reserved_0_1  at 0 range 0 .. 1;
      PERRCF        at 0 range 2 .. 2;
      OVRCF         at 0 range 3 .. 3;
      SBDCF         at 0 range 4 .. 4;
      SYNCDCF       at 0 range 5 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   subtype DR_00_DR_Field is STM32H753.UInt24;
   subtype DR_00_PT_Field is STM32H753.UInt2;

   --  Data input register
   type DR_00_Register is record
      --  Read-only. Parity Error bit
      DR             : DR_00_DR_Field;
      --  Read-only. Parity Error bit
      PE             : Boolean;
      --  Read-only. Validity bit
      V              : Boolean;
      --  Read-only. User bit
      U              : Boolean;
      --  Read-only. Channel Status bit
      C              : Boolean;
      --  Read-only. Preamble Type
      PT             : DR_00_PT_Field;
      --  unspecified
      Reserved_30_31 : STM32H753.UInt2;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR_00_Register use record
      DR             at 0 range 0 .. 23;
      PE             at 0 range 24 .. 24;
      V              at 0 range 25 .. 25;
      U              at 0 range 26 .. 26;
      C              at 0 range 27 .. 27;
      PT             at 0 range 28 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype DR_01_PT_Field is STM32H753.UInt2;
   subtype DR_01_DR_Field is STM32H753.UInt24;

   --  Data input register
   type DR_01_Register is record
      --  Read-only. Parity Error bit
      PE           : Boolean;
      --  Read-only. Validity bit
      V            : Boolean;
      --  Read-only. User bit
      U            : Boolean;
      --  Read-only. Channel Status bit
      C            : Boolean;
      --  Read-only. Preamble Type
      PT           : DR_01_PT_Field;
      --  unspecified
      Reserved_6_7 : STM32H753.UInt2;
      --  Read-only. Data value
      DR           : DR_01_DR_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR_01_Register use record
      PE           at 0 range 0 .. 0;
      V            at 0 range 1 .. 1;
      U            at 0 range 2 .. 2;
      C            at 0 range 3 .. 3;
      PT           at 0 range 4 .. 5;
      Reserved_6_7 at 0 range 6 .. 7;
      DR           at 0 range 8 .. 31;
   end record;

   --  DR_10_DRNL array element
   subtype DR_10_DRNL_Element is STM32H753.UInt16;

   --  DR_10_DRNL array
   type DR_10_DRNL_Field_Array is array (1 .. 2) of DR_10_DRNL_Element
     with Component_Size => 16, Size => 32;

   --  Data input register
   type DR_10_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DRNL as a value
            Val : STM32H753.UInt32;
         when True =>
            --  DRNL as an array
            Arr : DR_10_DRNL_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR_10_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   subtype CSR_USR_Field is STM32H753.UInt16;
   subtype CSR_CS_Field is STM32H753.Byte;

   --  Channel Status register
   type CSR_Register is record
      --  Read-only. User data information
      USR            : CSR_USR_Field;
      --  Read-only. Channel A status information
      CS             : CSR_CS_Field;
      --  Read-only. Start Of Block
      SOB            : Boolean;
      --  unspecified
      Reserved_25_31 : STM32H753.UInt7;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CSR_Register use record
      USR            at 0 range 0 .. 15;
      CS             at 0 range 16 .. 23;
      SOB            at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   subtype DIR_THI_Field is STM32H753.UInt13;
   subtype DIR_TLO_Field is STM32H753.UInt13;

   --  Debug Information register
   type DIR_Register is record
      --  Read-only. Threshold HIGH
      THI            : DIR_THI_Field;
      --  unspecified
      Reserved_13_15 : STM32H753.UInt3;
      --  Read-only. Threshold LOW
      TLO            : DIR_TLO_Field;
      --  unspecified
      Reserved_29_31 : STM32H753.UInt3;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DIR_Register use record
      THI            at 0 range 0 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      TLO            at 0 range 16 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype VERR_MINREV_Field is STM32H753.UInt4;
   subtype VERR_MAJREV_Field is STM32H753.UInt4;

   --  SPDIFRX version register
   type VERR_Register is record
      --  Read-only. Minor revision
      MINREV        : VERR_MINREV_Field;
      --  Read-only. Major revision
      MAJREV        : VERR_MAJREV_Field;
      --  unspecified
      Reserved_8_31 : STM32H753.UInt24;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for VERR_Register use record
      MINREV        at 0 range 0 .. 3;
      MAJREV        at 0 range 4 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   type SPDIFRX_Disc is
     (Val_00,
      Val_01,
      Val_10);

   --  Receiver Interface
   type SPDIFRX_Peripheral
     (Discriminent : SPDIFRX_Disc := Val_00)
   is record
      --  Control register
      CR    : aliased CR_Register;
      --  Interrupt mask register
      IMR   : aliased IMR_Register;
      --  Status register
      SR    : aliased SR_Register;
      --  Interrupt Flag Clear register
      IFCR  : aliased IFCR_Register;
      --  Channel Status register
      CSR   : aliased CSR_Register;
      --  Debug Information register
      DIR   : aliased DIR_Register;
      --  SPDIFRX version register
      VERR  : aliased VERR_Register;
      --  SPDIFRX identification register
      IDR   : aliased STM32H753.UInt32;
      --  SPDIFRX size identification register
      SIDR  : aliased STM32H753.UInt32;
      case Discriminent is
         when Val_00 =>
            --  Data input register
            DR_00 : aliased DR_00_Register;
         when Val_01 =>
            --  Data input register
            DR_01 : aliased DR_01_Register;
         when Val_10 =>
            --  Data input register
            DR_10 : aliased DR_10_Register;
      end case;
   end record
     with Unchecked_Union, Volatile;

   for SPDIFRX_Peripheral use record
      CR    at 16#0# range 0 .. 31;
      IMR   at 16#4# range 0 .. 31;
      SR    at 16#8# range 0 .. 31;
      IFCR  at 16#C# range 0 .. 31;
      CSR   at 16#14# range 0 .. 31;
      DIR   at 16#18# range 0 .. 31;
      VERR  at 16#3F4# range 0 .. 31;
      IDR   at 16#3F8# range 0 .. 31;
      SIDR  at 16#3FC# range 0 .. 31;
      DR_00 at 16#10# range 0 .. 31;
      DR_01 at 16#10# range 0 .. 31;
      DR_10 at 16#10# range 0 .. 31;
   end record;

   --  Receiver Interface
   SPDIFRX_Periph : aliased SPDIFRX_Peripheral
     with Import, Address => SPDIFRX_Base;

end STM32H753.SPDIFRX;
