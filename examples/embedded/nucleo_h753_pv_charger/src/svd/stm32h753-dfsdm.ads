pragma Style_Checks (Off);

--  This spec has been automatically generated from STM32H753.svd


with System;

package STM32H753.DFSDM is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   subtype CH0CFGR1_SITP_Field is STM32H753.UInt2;
   subtype CH0CFGR1_SPICKSEL_Field is STM32H753.UInt2;
   subtype CH0CFGR1_DATMPX_Field is STM32H753.UInt2;
   subtype CH0CFGR1_DATPACK_Field is STM32H753.UInt2;
   subtype CH0CFGR1_CKOUTDIV_Field is STM32H753.Byte;

   --  channel configuration y register
   type CH0CFGR1_Register is record
      --  SITP
      SITP           : CH0CFGR1_SITP_Field := 16#0#;
      --  SPICKSEL
      SPICKSEL       : CH0CFGR1_SPICKSEL_Field := 16#0#;
      --  unspecified
      Reserved_4_4   : STM32H753.Bit := 16#0#;
      --  SCDEN
      SCDEN          : Boolean := False;
      --  CKABEN
      CKABEN         : Boolean := False;
      --  CHEN
      CHEN           : Boolean := False;
      --  CHINSEL
      CHINSEL        : Boolean := False;
      --  unspecified
      Reserved_9_11  : STM32H753.UInt3 := 16#0#;
      --  DATMPX
      DATMPX         : CH0CFGR1_DATMPX_Field := 16#0#;
      --  DATPACK
      DATPACK        : CH0CFGR1_DATPACK_Field := 16#0#;
      --  CKOUTDIV
      CKOUTDIV       : CH0CFGR1_CKOUTDIV_Field := 16#0#;
      --  unspecified
      Reserved_24_29 : STM32H753.UInt6 := 16#0#;
      --  CKOUTSRC
      CKOUTSRC       : Boolean := False;
      --  DFSDMEN
      DFSDMEN        : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH0CFGR1_Register use record
      SITP           at 0 range 0 .. 1;
      SPICKSEL       at 0 range 2 .. 3;
      Reserved_4_4   at 0 range 4 .. 4;
      SCDEN          at 0 range 5 .. 5;
      CKABEN         at 0 range 6 .. 6;
      CHEN           at 0 range 7 .. 7;
      CHINSEL        at 0 range 8 .. 8;
      Reserved_9_11  at 0 range 9 .. 11;
      DATMPX         at 0 range 12 .. 13;
      DATPACK        at 0 range 14 .. 15;
      CKOUTDIV       at 0 range 16 .. 23;
      Reserved_24_29 at 0 range 24 .. 29;
      CKOUTSRC       at 0 range 30 .. 30;
      DFSDMEN        at 0 range 31 .. 31;
   end record;

   subtype CH0CFGR2_DTRBS_Field is STM32H753.UInt5;
   subtype CH0CFGR2_OFFSET_Field is STM32H753.UInt24;

   --  channel configuration y register
   type CH0CFGR2_Register is record
      --  unspecified
      Reserved_0_2 : STM32H753.UInt3 := 16#0#;
      --  DTRBS
      DTRBS        : CH0CFGR2_DTRBS_Field := 16#0#;
      --  OFFSET
      OFFSET       : CH0CFGR2_OFFSET_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH0CFGR2_Register use record
      Reserved_0_2 at 0 range 0 .. 2;
      DTRBS        at 0 range 3 .. 7;
      OFFSET       at 0 range 8 .. 31;
   end record;

   subtype CH0AWSCDR_SCDT_Field is STM32H753.Byte;
   subtype CH0AWSCDR_BKSCD_Field is STM32H753.UInt4;
   subtype CH0AWSCDR_AWFOSR_Field is STM32H753.UInt5;
   subtype CH0AWSCDR_AWFORD_Field is STM32H753.UInt2;

   --  analog watchdog and short-circuit detector register
   type CH0AWSCDR_Register is record
      --  SCDT
      SCDT           : CH0AWSCDR_SCDT_Field := 16#0#;
      --  unspecified
      Reserved_8_11  : STM32H753.UInt4 := 16#0#;
      --  BKSCD
      BKSCD          : CH0AWSCDR_BKSCD_Field := 16#0#;
      --  AWFOSR
      AWFOSR         : CH0AWSCDR_AWFOSR_Field := 16#0#;
      --  unspecified
      Reserved_21_21 : STM32H753.Bit := 16#0#;
      --  AWFORD
      AWFORD         : CH0AWSCDR_AWFORD_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : STM32H753.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH0AWSCDR_Register use record
      SCDT           at 0 range 0 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      BKSCD          at 0 range 12 .. 15;
      AWFOSR         at 0 range 16 .. 20;
      Reserved_21_21 at 0 range 21 .. 21;
      AWFORD         at 0 range 22 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype CH0WDATR_WDATA_Field is STM32H753.UInt16;

   --  channel watchdog filter data register
   type CH0WDATR_Register is record
      --  WDATA
      WDATA          : CH0WDATR_WDATA_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH0WDATR_Register use record
      WDATA          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  CH0DATINR_INDAT array element
   subtype CH0DATINR_INDAT_Element is STM32H753.UInt16;

   --  CH0DATINR_INDAT array
   type CH0DATINR_INDAT_Field_Array is array (0 .. 1)
     of CH0DATINR_INDAT_Element
     with Component_Size => 16, Size => 32;

   --  channel data input register
   type CH0DATINR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  INDAT as a value
            Val : STM32H753.UInt32;
         when True =>
            --  INDAT as an array
            Arr : CH0DATINR_INDAT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH0DATINR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   subtype CH0DLYR_PLSSKP_Field is STM32H753.UInt6;

   --  channel y delay register
   type CH0DLYR_Register is record
      --  PLSSKP
      PLSSKP        : CH0DLYR_PLSSKP_Field := 16#0#;
      --  unspecified
      Reserved_6_31 : STM32H753.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH0DLYR_Register use record
      PLSSKP        at 0 range 0 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   subtype CH1CFGR1_SITP_Field is STM32H753.UInt2;
   subtype CH1CFGR1_SPICKSEL_Field is STM32H753.UInt2;
   subtype CH1CFGR1_DATMPX_Field is STM32H753.UInt2;
   subtype CH1CFGR1_DATPACK_Field is STM32H753.UInt2;

   --  CH1CFGR1
   type CH1CFGR1_Register is record
      --  SITP
      SITP           : CH1CFGR1_SITP_Field := 16#0#;
      --  SPICKSEL
      SPICKSEL       : CH1CFGR1_SPICKSEL_Field := 16#0#;
      --  unspecified
      Reserved_4_4   : STM32H753.Bit := 16#0#;
      --  SCDEN
      SCDEN          : Boolean := False;
      --  CKABEN
      CKABEN         : Boolean := False;
      --  CHEN
      CHEN           : Boolean := False;
      --  CHINSEL
      CHINSEL        : Boolean := False;
      --  unspecified
      Reserved_9_11  : STM32H753.UInt3 := 16#0#;
      --  DATMPX
      DATMPX         : CH1CFGR1_DATMPX_Field := 16#0#;
      --  DATPACK
      DATPACK        : CH1CFGR1_DATPACK_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH1CFGR1_Register use record
      SITP           at 0 range 0 .. 1;
      SPICKSEL       at 0 range 2 .. 3;
      Reserved_4_4   at 0 range 4 .. 4;
      SCDEN          at 0 range 5 .. 5;
      CKABEN         at 0 range 6 .. 6;
      CHEN           at 0 range 7 .. 7;
      CHINSEL        at 0 range 8 .. 8;
      Reserved_9_11  at 0 range 9 .. 11;
      DATMPX         at 0 range 12 .. 13;
      DATPACK        at 0 range 14 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype CH1CFGR2_DTRBS_Field is STM32H753.UInt5;
   subtype CH1CFGR2_OFFSET_Field is STM32H753.UInt24;

   --  CH1CFGR2
   type CH1CFGR2_Register is record
      --  unspecified
      Reserved_0_2 : STM32H753.UInt3 := 16#0#;
      --  DTRBS
      DTRBS        : CH1CFGR2_DTRBS_Field := 16#0#;
      --  OFFSET
      OFFSET       : CH1CFGR2_OFFSET_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH1CFGR2_Register use record
      Reserved_0_2 at 0 range 0 .. 2;
      DTRBS        at 0 range 3 .. 7;
      OFFSET       at 0 range 8 .. 31;
   end record;

   subtype CH1AWSCDR_SCDT_Field is STM32H753.Byte;
   subtype CH1AWSCDR_BKSCD_Field is STM32H753.UInt4;
   subtype CH1AWSCDR_AWFOSR_Field is STM32H753.UInt5;
   subtype CH1AWSCDR_AWFORD_Field is STM32H753.UInt2;

   --  CH1AWSCDR
   type CH1AWSCDR_Register is record
      --  SCDT
      SCDT           : CH1AWSCDR_SCDT_Field := 16#0#;
      --  unspecified
      Reserved_8_11  : STM32H753.UInt4 := 16#0#;
      --  BKSCD
      BKSCD          : CH1AWSCDR_BKSCD_Field := 16#0#;
      --  AWFOSR
      AWFOSR         : CH1AWSCDR_AWFOSR_Field := 16#0#;
      --  unspecified
      Reserved_21_21 : STM32H753.Bit := 16#0#;
      --  AWFORD
      AWFORD         : CH1AWSCDR_AWFORD_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : STM32H753.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH1AWSCDR_Register use record
      SCDT           at 0 range 0 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      BKSCD          at 0 range 12 .. 15;
      AWFOSR         at 0 range 16 .. 20;
      Reserved_21_21 at 0 range 21 .. 21;
      AWFORD         at 0 range 22 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype CH1WDATR_WDATA_Field is STM32H753.UInt16;

   --  CH1WDATR
   type CH1WDATR_Register is record
      --  WDATA
      WDATA          : CH1WDATR_WDATA_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH1WDATR_Register use record
      WDATA          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  CH1DATINR_INDAT array element
   subtype CH1DATINR_INDAT_Element is STM32H753.UInt16;

   --  CH1DATINR_INDAT array
   type CH1DATINR_INDAT_Field_Array is array (0 .. 1)
     of CH1DATINR_INDAT_Element
     with Component_Size => 16, Size => 32;

   --  CH1DATINR
   type CH1DATINR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  INDAT as a value
            Val : STM32H753.UInt32;
         when True =>
            --  INDAT as an array
            Arr : CH1DATINR_INDAT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH1DATINR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   subtype CH1DLYR_PLSSKP_Field is STM32H753.UInt6;

   --  channel y delay register
   type CH1DLYR_Register is record
      --  PLSSKP
      PLSSKP        : CH1DLYR_PLSSKP_Field := 16#0#;
      --  unspecified
      Reserved_6_31 : STM32H753.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH1DLYR_Register use record
      PLSSKP        at 0 range 0 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   subtype CH2CFGR1_SITP_Field is STM32H753.UInt2;
   subtype CH2CFGR1_SPICKSEL_Field is STM32H753.UInt2;
   subtype CH2CFGR1_DATMPX_Field is STM32H753.UInt2;
   subtype CH2CFGR1_DATPACK_Field is STM32H753.UInt2;

   --  CH2CFGR1
   type CH2CFGR1_Register is record
      --  SITP
      SITP           : CH2CFGR1_SITP_Field := 16#0#;
      --  SPICKSEL
      SPICKSEL       : CH2CFGR1_SPICKSEL_Field := 16#0#;
      --  unspecified
      Reserved_4_4   : STM32H753.Bit := 16#0#;
      --  SCDEN
      SCDEN          : Boolean := False;
      --  CKABEN
      CKABEN         : Boolean := False;
      --  CHEN
      CHEN           : Boolean := False;
      --  CHINSEL
      CHINSEL        : Boolean := False;
      --  unspecified
      Reserved_9_11  : STM32H753.UInt3 := 16#0#;
      --  DATMPX
      DATMPX         : CH2CFGR1_DATMPX_Field := 16#0#;
      --  DATPACK
      DATPACK        : CH2CFGR1_DATPACK_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH2CFGR1_Register use record
      SITP           at 0 range 0 .. 1;
      SPICKSEL       at 0 range 2 .. 3;
      Reserved_4_4   at 0 range 4 .. 4;
      SCDEN          at 0 range 5 .. 5;
      CKABEN         at 0 range 6 .. 6;
      CHEN           at 0 range 7 .. 7;
      CHINSEL        at 0 range 8 .. 8;
      Reserved_9_11  at 0 range 9 .. 11;
      DATMPX         at 0 range 12 .. 13;
      DATPACK        at 0 range 14 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype CH2CFGR2_DTRBS_Field is STM32H753.UInt5;
   subtype CH2CFGR2_OFFSET_Field is STM32H753.UInt24;

   --  CH2CFGR2
   type CH2CFGR2_Register is record
      --  unspecified
      Reserved_0_2 : STM32H753.UInt3 := 16#0#;
      --  DTRBS
      DTRBS        : CH2CFGR2_DTRBS_Field := 16#0#;
      --  OFFSET
      OFFSET       : CH2CFGR2_OFFSET_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH2CFGR2_Register use record
      Reserved_0_2 at 0 range 0 .. 2;
      DTRBS        at 0 range 3 .. 7;
      OFFSET       at 0 range 8 .. 31;
   end record;

   subtype CH2AWSCDR_SCDT_Field is STM32H753.Byte;
   subtype CH2AWSCDR_BKSCD_Field is STM32H753.UInt4;
   subtype CH2AWSCDR_AWFOSR_Field is STM32H753.UInt5;
   subtype CH2AWSCDR_AWFORD_Field is STM32H753.UInt2;

   --  CH2AWSCDR
   type CH2AWSCDR_Register is record
      --  SCDT
      SCDT           : CH2AWSCDR_SCDT_Field := 16#0#;
      --  unspecified
      Reserved_8_11  : STM32H753.UInt4 := 16#0#;
      --  BKSCD
      BKSCD          : CH2AWSCDR_BKSCD_Field := 16#0#;
      --  AWFOSR
      AWFOSR         : CH2AWSCDR_AWFOSR_Field := 16#0#;
      --  unspecified
      Reserved_21_21 : STM32H753.Bit := 16#0#;
      --  AWFORD
      AWFORD         : CH2AWSCDR_AWFORD_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : STM32H753.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH2AWSCDR_Register use record
      SCDT           at 0 range 0 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      BKSCD          at 0 range 12 .. 15;
      AWFOSR         at 0 range 16 .. 20;
      Reserved_21_21 at 0 range 21 .. 21;
      AWFORD         at 0 range 22 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype CH2WDATR_WDATA_Field is STM32H753.UInt16;

   --  CH2WDATR
   type CH2WDATR_Register is record
      --  WDATA
      WDATA          : CH2WDATR_WDATA_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH2WDATR_Register use record
      WDATA          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  CH2DATINR_INDAT array element
   subtype CH2DATINR_INDAT_Element is STM32H753.UInt16;

   --  CH2DATINR_INDAT array
   type CH2DATINR_INDAT_Field_Array is array (0 .. 1)
     of CH2DATINR_INDAT_Element
     with Component_Size => 16, Size => 32;

   --  CH2DATINR
   type CH2DATINR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  INDAT as a value
            Val : STM32H753.UInt32;
         when True =>
            --  INDAT as an array
            Arr : CH2DATINR_INDAT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH2DATINR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   subtype CH2DLYR_PLSSKP_Field is STM32H753.UInt6;

   --  channel y delay register
   type CH2DLYR_Register is record
      --  PLSSKP
      PLSSKP        : CH2DLYR_PLSSKP_Field := 16#0#;
      --  unspecified
      Reserved_6_31 : STM32H753.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH2DLYR_Register use record
      PLSSKP        at 0 range 0 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   subtype CH3CFGR1_SITP_Field is STM32H753.UInt2;
   subtype CH3CFGR1_SPICKSEL_Field is STM32H753.UInt2;
   subtype CH3CFGR1_DATMPX_Field is STM32H753.UInt2;
   subtype CH3CFGR1_DATPACK_Field is STM32H753.UInt2;

   --  CH3CFGR1
   type CH3CFGR1_Register is record
      --  SITP
      SITP           : CH3CFGR1_SITP_Field := 16#0#;
      --  SPICKSEL
      SPICKSEL       : CH3CFGR1_SPICKSEL_Field := 16#0#;
      --  unspecified
      Reserved_4_4   : STM32H753.Bit := 16#0#;
      --  SCDEN
      SCDEN          : Boolean := False;
      --  CKABEN
      CKABEN         : Boolean := False;
      --  CHEN
      CHEN           : Boolean := False;
      --  CHINSEL
      CHINSEL        : Boolean := False;
      --  unspecified
      Reserved_9_11  : STM32H753.UInt3 := 16#0#;
      --  DATMPX
      DATMPX         : CH3CFGR1_DATMPX_Field := 16#0#;
      --  DATPACK
      DATPACK        : CH3CFGR1_DATPACK_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH3CFGR1_Register use record
      SITP           at 0 range 0 .. 1;
      SPICKSEL       at 0 range 2 .. 3;
      Reserved_4_4   at 0 range 4 .. 4;
      SCDEN          at 0 range 5 .. 5;
      CKABEN         at 0 range 6 .. 6;
      CHEN           at 0 range 7 .. 7;
      CHINSEL        at 0 range 8 .. 8;
      Reserved_9_11  at 0 range 9 .. 11;
      DATMPX         at 0 range 12 .. 13;
      DATPACK        at 0 range 14 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype CH3CFGR2_DTRBS_Field is STM32H753.UInt5;
   subtype CH3CFGR2_OFFSET_Field is STM32H753.UInt24;

   --  CH3CFGR2
   type CH3CFGR2_Register is record
      --  unspecified
      Reserved_0_2 : STM32H753.UInt3 := 16#0#;
      --  DTRBS
      DTRBS        : CH3CFGR2_DTRBS_Field := 16#0#;
      --  OFFSET
      OFFSET       : CH3CFGR2_OFFSET_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH3CFGR2_Register use record
      Reserved_0_2 at 0 range 0 .. 2;
      DTRBS        at 0 range 3 .. 7;
      OFFSET       at 0 range 8 .. 31;
   end record;

   subtype CH3AWSCDR_SCDT_Field is STM32H753.Byte;
   subtype CH3AWSCDR_BKSCD_Field is STM32H753.UInt4;
   subtype CH3AWSCDR_AWFOSR_Field is STM32H753.UInt5;
   subtype CH3AWSCDR_AWFORD_Field is STM32H753.UInt2;

   --  CH3AWSCDR
   type CH3AWSCDR_Register is record
      --  SCDT
      SCDT           : CH3AWSCDR_SCDT_Field := 16#0#;
      --  unspecified
      Reserved_8_11  : STM32H753.UInt4 := 16#0#;
      --  BKSCD
      BKSCD          : CH3AWSCDR_BKSCD_Field := 16#0#;
      --  AWFOSR
      AWFOSR         : CH3AWSCDR_AWFOSR_Field := 16#0#;
      --  unspecified
      Reserved_21_21 : STM32H753.Bit := 16#0#;
      --  AWFORD
      AWFORD         : CH3AWSCDR_AWFORD_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : STM32H753.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH3AWSCDR_Register use record
      SCDT           at 0 range 0 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      BKSCD          at 0 range 12 .. 15;
      AWFOSR         at 0 range 16 .. 20;
      Reserved_21_21 at 0 range 21 .. 21;
      AWFORD         at 0 range 22 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype CH3WDATR_WDATA_Field is STM32H753.UInt16;

   --  CH3WDATR
   type CH3WDATR_Register is record
      --  WDATA
      WDATA          : CH3WDATR_WDATA_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH3WDATR_Register use record
      WDATA          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  CH3DATINR_INDAT array element
   subtype CH3DATINR_INDAT_Element is STM32H753.UInt16;

   --  CH3DATINR_INDAT array
   type CH3DATINR_INDAT_Field_Array is array (0 .. 1)
     of CH3DATINR_INDAT_Element
     with Component_Size => 16, Size => 32;

   --  CH3DATINR
   type CH3DATINR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  INDAT as a value
            Val : STM32H753.UInt32;
         when True =>
            --  INDAT as an array
            Arr : CH3DATINR_INDAT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH3DATINR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   subtype CH3DLYR_PLSSKP_Field is STM32H753.UInt6;

   --  channel y delay register
   type CH3DLYR_Register is record
      --  PLSSKP
      PLSSKP        : CH3DLYR_PLSSKP_Field := 16#0#;
      --  unspecified
      Reserved_6_31 : STM32H753.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH3DLYR_Register use record
      PLSSKP        at 0 range 0 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   subtype CH4CFGR1_SITP_Field is STM32H753.UInt2;
   subtype CH4CFGR1_SPICKSEL_Field is STM32H753.UInt2;
   subtype CH4CFGR1_DATMPX_Field is STM32H753.UInt2;
   subtype CH4CFGR1_DATPACK_Field is STM32H753.UInt2;

   --  CH4CFGR1
   type CH4CFGR1_Register is record
      --  SITP
      SITP           : CH4CFGR1_SITP_Field := 16#0#;
      --  SPICKSEL
      SPICKSEL       : CH4CFGR1_SPICKSEL_Field := 16#0#;
      --  unspecified
      Reserved_4_4   : STM32H753.Bit := 16#0#;
      --  SCDEN
      SCDEN          : Boolean := False;
      --  CKABEN
      CKABEN         : Boolean := False;
      --  CHEN
      CHEN           : Boolean := False;
      --  CHINSEL
      CHINSEL        : Boolean := False;
      --  unspecified
      Reserved_9_11  : STM32H753.UInt3 := 16#0#;
      --  DATMPX
      DATMPX         : CH4CFGR1_DATMPX_Field := 16#0#;
      --  DATPACK
      DATPACK        : CH4CFGR1_DATPACK_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH4CFGR1_Register use record
      SITP           at 0 range 0 .. 1;
      SPICKSEL       at 0 range 2 .. 3;
      Reserved_4_4   at 0 range 4 .. 4;
      SCDEN          at 0 range 5 .. 5;
      CKABEN         at 0 range 6 .. 6;
      CHEN           at 0 range 7 .. 7;
      CHINSEL        at 0 range 8 .. 8;
      Reserved_9_11  at 0 range 9 .. 11;
      DATMPX         at 0 range 12 .. 13;
      DATPACK        at 0 range 14 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype CH4CFGR2_DTRBS_Field is STM32H753.UInt5;
   subtype CH4CFGR2_OFFSET_Field is STM32H753.UInt24;

   --  CH4CFGR2
   type CH4CFGR2_Register is record
      --  unspecified
      Reserved_0_2 : STM32H753.UInt3 := 16#0#;
      --  DTRBS
      DTRBS        : CH4CFGR2_DTRBS_Field := 16#0#;
      --  OFFSET
      OFFSET       : CH4CFGR2_OFFSET_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH4CFGR2_Register use record
      Reserved_0_2 at 0 range 0 .. 2;
      DTRBS        at 0 range 3 .. 7;
      OFFSET       at 0 range 8 .. 31;
   end record;

   subtype CH4AWSCDR_SCDT_Field is STM32H753.Byte;
   subtype CH4AWSCDR_BKSCD_Field is STM32H753.UInt4;
   subtype CH4AWSCDR_AWFOSR_Field is STM32H753.UInt5;
   subtype CH4AWSCDR_AWFORD_Field is STM32H753.UInt2;

   --  CH4AWSCDR
   type CH4AWSCDR_Register is record
      --  SCDT
      SCDT           : CH4AWSCDR_SCDT_Field := 16#0#;
      --  unspecified
      Reserved_8_11  : STM32H753.UInt4 := 16#0#;
      --  BKSCD
      BKSCD          : CH4AWSCDR_BKSCD_Field := 16#0#;
      --  AWFOSR
      AWFOSR         : CH4AWSCDR_AWFOSR_Field := 16#0#;
      --  unspecified
      Reserved_21_21 : STM32H753.Bit := 16#0#;
      --  AWFORD
      AWFORD         : CH4AWSCDR_AWFORD_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : STM32H753.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH4AWSCDR_Register use record
      SCDT           at 0 range 0 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      BKSCD          at 0 range 12 .. 15;
      AWFOSR         at 0 range 16 .. 20;
      Reserved_21_21 at 0 range 21 .. 21;
      AWFORD         at 0 range 22 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype CH4WDATR_WDATA_Field is STM32H753.UInt16;

   --  CH4WDATR
   type CH4WDATR_Register is record
      --  WDATA
      WDATA          : CH4WDATR_WDATA_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH4WDATR_Register use record
      WDATA          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  CH4DATINR_INDAT array element
   subtype CH4DATINR_INDAT_Element is STM32H753.UInt16;

   --  CH4DATINR_INDAT array
   type CH4DATINR_INDAT_Field_Array is array (0 .. 1)
     of CH4DATINR_INDAT_Element
     with Component_Size => 16, Size => 32;

   --  CH4DATINR
   type CH4DATINR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  INDAT as a value
            Val : STM32H753.UInt32;
         when True =>
            --  INDAT as an array
            Arr : CH4DATINR_INDAT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH4DATINR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   subtype CH4DLYR_PLSSKP_Field is STM32H753.UInt6;

   --  channel y delay register
   type CH4DLYR_Register is record
      --  PLSSKP
      PLSSKP        : CH4DLYR_PLSSKP_Field := 16#0#;
      --  unspecified
      Reserved_6_31 : STM32H753.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH4DLYR_Register use record
      PLSSKP        at 0 range 0 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   subtype CH5CFGR1_SITP_Field is STM32H753.UInt2;
   subtype CH5CFGR1_SPICKSEL_Field is STM32H753.UInt2;
   subtype CH5CFGR1_DATMPX_Field is STM32H753.UInt2;
   subtype CH5CFGR1_DATPACK_Field is STM32H753.UInt2;

   --  CH5CFGR1
   type CH5CFGR1_Register is record
      --  SITP
      SITP           : CH5CFGR1_SITP_Field := 16#0#;
      --  SPICKSEL
      SPICKSEL       : CH5CFGR1_SPICKSEL_Field := 16#0#;
      --  unspecified
      Reserved_4_4   : STM32H753.Bit := 16#0#;
      --  SCDEN
      SCDEN          : Boolean := False;
      --  CKABEN
      CKABEN         : Boolean := False;
      --  CHEN
      CHEN           : Boolean := False;
      --  CHINSEL
      CHINSEL        : Boolean := False;
      --  unspecified
      Reserved_9_11  : STM32H753.UInt3 := 16#0#;
      --  DATMPX
      DATMPX         : CH5CFGR1_DATMPX_Field := 16#0#;
      --  DATPACK
      DATPACK        : CH5CFGR1_DATPACK_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH5CFGR1_Register use record
      SITP           at 0 range 0 .. 1;
      SPICKSEL       at 0 range 2 .. 3;
      Reserved_4_4   at 0 range 4 .. 4;
      SCDEN          at 0 range 5 .. 5;
      CKABEN         at 0 range 6 .. 6;
      CHEN           at 0 range 7 .. 7;
      CHINSEL        at 0 range 8 .. 8;
      Reserved_9_11  at 0 range 9 .. 11;
      DATMPX         at 0 range 12 .. 13;
      DATPACK        at 0 range 14 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype CH5CFGR2_DTRBS_Field is STM32H753.UInt5;
   subtype CH5CFGR2_OFFSET_Field is STM32H753.UInt24;

   --  CH5CFGR2
   type CH5CFGR2_Register is record
      --  unspecified
      Reserved_0_2 : STM32H753.UInt3 := 16#0#;
      --  DTRBS
      DTRBS        : CH5CFGR2_DTRBS_Field := 16#0#;
      --  OFFSET
      OFFSET       : CH5CFGR2_OFFSET_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH5CFGR2_Register use record
      Reserved_0_2 at 0 range 0 .. 2;
      DTRBS        at 0 range 3 .. 7;
      OFFSET       at 0 range 8 .. 31;
   end record;

   subtype CH5AWSCDR_SCDT_Field is STM32H753.Byte;
   subtype CH5AWSCDR_BKSCD_Field is STM32H753.UInt4;
   subtype CH5AWSCDR_AWFOSR_Field is STM32H753.UInt5;
   subtype CH5AWSCDR_AWFORD_Field is STM32H753.UInt2;

   --  CH5AWSCDR
   type CH5AWSCDR_Register is record
      --  SCDT
      SCDT           : CH5AWSCDR_SCDT_Field := 16#0#;
      --  unspecified
      Reserved_8_11  : STM32H753.UInt4 := 16#0#;
      --  BKSCD
      BKSCD          : CH5AWSCDR_BKSCD_Field := 16#0#;
      --  AWFOSR
      AWFOSR         : CH5AWSCDR_AWFOSR_Field := 16#0#;
      --  unspecified
      Reserved_21_21 : STM32H753.Bit := 16#0#;
      --  AWFORD
      AWFORD         : CH5AWSCDR_AWFORD_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : STM32H753.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH5AWSCDR_Register use record
      SCDT           at 0 range 0 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      BKSCD          at 0 range 12 .. 15;
      AWFOSR         at 0 range 16 .. 20;
      Reserved_21_21 at 0 range 21 .. 21;
      AWFORD         at 0 range 22 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype CH5WDATR_WDATA_Field is STM32H753.UInt16;

   --  CH5WDATR
   type CH5WDATR_Register is record
      --  WDATA
      WDATA          : CH5WDATR_WDATA_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH5WDATR_Register use record
      WDATA          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  CH5DATINR_INDAT array element
   subtype CH5DATINR_INDAT_Element is STM32H753.UInt16;

   --  CH5DATINR_INDAT array
   type CH5DATINR_INDAT_Field_Array is array (0 .. 1)
     of CH5DATINR_INDAT_Element
     with Component_Size => 16, Size => 32;

   --  CH5DATINR
   type CH5DATINR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  INDAT as a value
            Val : STM32H753.UInt32;
         when True =>
            --  INDAT as an array
            Arr : CH5DATINR_INDAT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH5DATINR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   subtype CH5DLYR_PLSSKP_Field is STM32H753.UInt6;

   --  channel y delay register
   type CH5DLYR_Register is record
      --  PLSSKP
      PLSSKP        : CH5DLYR_PLSSKP_Field := 16#0#;
      --  unspecified
      Reserved_6_31 : STM32H753.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH5DLYR_Register use record
      PLSSKP        at 0 range 0 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   subtype CH6CFGR1_SITP_Field is STM32H753.UInt2;
   subtype CH6CFGR1_SPICKSEL_Field is STM32H753.UInt2;
   subtype CH6CFGR1_DATMPX_Field is STM32H753.UInt2;
   subtype CH6CFGR1_DATPACK_Field is STM32H753.UInt2;

   --  CH6CFGR1
   type CH6CFGR1_Register is record
      --  SITP
      SITP           : CH6CFGR1_SITP_Field := 16#0#;
      --  SPICKSEL
      SPICKSEL       : CH6CFGR1_SPICKSEL_Field := 16#0#;
      --  unspecified
      Reserved_4_4   : STM32H753.Bit := 16#0#;
      --  SCDEN
      SCDEN          : Boolean := False;
      --  CKABEN
      CKABEN         : Boolean := False;
      --  CHEN
      CHEN           : Boolean := False;
      --  CHINSEL
      CHINSEL        : Boolean := False;
      --  unspecified
      Reserved_9_11  : STM32H753.UInt3 := 16#0#;
      --  DATMPX
      DATMPX         : CH6CFGR1_DATMPX_Field := 16#0#;
      --  DATPACK
      DATPACK        : CH6CFGR1_DATPACK_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH6CFGR1_Register use record
      SITP           at 0 range 0 .. 1;
      SPICKSEL       at 0 range 2 .. 3;
      Reserved_4_4   at 0 range 4 .. 4;
      SCDEN          at 0 range 5 .. 5;
      CKABEN         at 0 range 6 .. 6;
      CHEN           at 0 range 7 .. 7;
      CHINSEL        at 0 range 8 .. 8;
      Reserved_9_11  at 0 range 9 .. 11;
      DATMPX         at 0 range 12 .. 13;
      DATPACK        at 0 range 14 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype CH6CFGR2_DTRBS_Field is STM32H753.UInt5;
   subtype CH6CFGR2_OFFSET_Field is STM32H753.UInt24;

   --  CH6CFGR2
   type CH6CFGR2_Register is record
      --  unspecified
      Reserved_0_2 : STM32H753.UInt3 := 16#0#;
      --  DTRBS
      DTRBS        : CH6CFGR2_DTRBS_Field := 16#0#;
      --  OFFSET
      OFFSET       : CH6CFGR2_OFFSET_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH6CFGR2_Register use record
      Reserved_0_2 at 0 range 0 .. 2;
      DTRBS        at 0 range 3 .. 7;
      OFFSET       at 0 range 8 .. 31;
   end record;

   subtype CH6AWSCDR_SCDT_Field is STM32H753.Byte;
   subtype CH6AWSCDR_BKSCD_Field is STM32H753.UInt4;
   subtype CH6AWSCDR_AWFOSR_Field is STM32H753.UInt5;
   subtype CH6AWSCDR_AWFORD_Field is STM32H753.UInt2;

   --  CH6AWSCDR
   type CH6AWSCDR_Register is record
      --  SCDT
      SCDT           : CH6AWSCDR_SCDT_Field := 16#0#;
      --  unspecified
      Reserved_8_11  : STM32H753.UInt4 := 16#0#;
      --  BKSCD
      BKSCD          : CH6AWSCDR_BKSCD_Field := 16#0#;
      --  AWFOSR
      AWFOSR         : CH6AWSCDR_AWFOSR_Field := 16#0#;
      --  unspecified
      Reserved_21_21 : STM32H753.Bit := 16#0#;
      --  AWFORD
      AWFORD         : CH6AWSCDR_AWFORD_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : STM32H753.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH6AWSCDR_Register use record
      SCDT           at 0 range 0 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      BKSCD          at 0 range 12 .. 15;
      AWFOSR         at 0 range 16 .. 20;
      Reserved_21_21 at 0 range 21 .. 21;
      AWFORD         at 0 range 22 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype CH6WDATR_WDATA_Field is STM32H753.UInt16;

   --  CH6WDATR
   type CH6WDATR_Register is record
      --  WDATA
      WDATA          : CH6WDATR_WDATA_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH6WDATR_Register use record
      WDATA          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  CH6DATINR_INDAT array element
   subtype CH6DATINR_INDAT_Element is STM32H753.UInt16;

   --  CH6DATINR_INDAT array
   type CH6DATINR_INDAT_Field_Array is array (0 .. 1)
     of CH6DATINR_INDAT_Element
     with Component_Size => 16, Size => 32;

   --  CH6DATINR
   type CH6DATINR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  INDAT as a value
            Val : STM32H753.UInt32;
         when True =>
            --  INDAT as an array
            Arr : CH6DATINR_INDAT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH6DATINR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   subtype CH6DLYR_PLSSKP_Field is STM32H753.UInt6;

   --  channel y delay register
   type CH6DLYR_Register is record
      --  PLSSKP
      PLSSKP        : CH6DLYR_PLSSKP_Field := 16#0#;
      --  unspecified
      Reserved_6_31 : STM32H753.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH6DLYR_Register use record
      PLSSKP        at 0 range 0 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   subtype CH7CFGR1_SITP_Field is STM32H753.UInt2;
   subtype CH7CFGR1_SPICKSEL_Field is STM32H753.UInt2;
   subtype CH7CFGR1_DATMPX_Field is STM32H753.UInt2;
   subtype CH7CFGR1_DATPACK_Field is STM32H753.UInt2;

   --  CH7CFGR1
   type CH7CFGR1_Register is record
      --  SITP
      SITP           : CH7CFGR1_SITP_Field := 16#0#;
      --  SPICKSEL
      SPICKSEL       : CH7CFGR1_SPICKSEL_Field := 16#0#;
      --  unspecified
      Reserved_4_4   : STM32H753.Bit := 16#0#;
      --  SCDEN
      SCDEN          : Boolean := False;
      --  CKABEN
      CKABEN         : Boolean := False;
      --  CHEN
      CHEN           : Boolean := False;
      --  CHINSEL
      CHINSEL        : Boolean := False;
      --  unspecified
      Reserved_9_11  : STM32H753.UInt3 := 16#0#;
      --  DATMPX
      DATMPX         : CH7CFGR1_DATMPX_Field := 16#0#;
      --  DATPACK
      DATPACK        : CH7CFGR1_DATPACK_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH7CFGR1_Register use record
      SITP           at 0 range 0 .. 1;
      SPICKSEL       at 0 range 2 .. 3;
      Reserved_4_4   at 0 range 4 .. 4;
      SCDEN          at 0 range 5 .. 5;
      CKABEN         at 0 range 6 .. 6;
      CHEN           at 0 range 7 .. 7;
      CHINSEL        at 0 range 8 .. 8;
      Reserved_9_11  at 0 range 9 .. 11;
      DATMPX         at 0 range 12 .. 13;
      DATPACK        at 0 range 14 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype CH7CFGR2_DTRBS_Field is STM32H753.UInt5;
   subtype CH7CFGR2_OFFSET_Field is STM32H753.UInt24;

   --  CH7CFGR2
   type CH7CFGR2_Register is record
      --  unspecified
      Reserved_0_2 : STM32H753.UInt3 := 16#0#;
      --  DTRBS
      DTRBS        : CH7CFGR2_DTRBS_Field := 16#0#;
      --  OFFSET
      OFFSET       : CH7CFGR2_OFFSET_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH7CFGR2_Register use record
      Reserved_0_2 at 0 range 0 .. 2;
      DTRBS        at 0 range 3 .. 7;
      OFFSET       at 0 range 8 .. 31;
   end record;

   subtype CH7AWSCDR_SCDT_Field is STM32H753.Byte;
   subtype CH7AWSCDR_BKSCD_Field is STM32H753.UInt4;
   subtype CH7AWSCDR_AWFOSR_Field is STM32H753.UInt5;
   subtype CH7AWSCDR_AWFORD_Field is STM32H753.UInt2;

   --  CH7AWSCDR
   type CH7AWSCDR_Register is record
      --  SCDT
      SCDT           : CH7AWSCDR_SCDT_Field := 16#0#;
      --  unspecified
      Reserved_8_11  : STM32H753.UInt4 := 16#0#;
      --  BKSCD
      BKSCD          : CH7AWSCDR_BKSCD_Field := 16#0#;
      --  AWFOSR
      AWFOSR         : CH7AWSCDR_AWFOSR_Field := 16#0#;
      --  unspecified
      Reserved_21_21 : STM32H753.Bit := 16#0#;
      --  AWFORD
      AWFORD         : CH7AWSCDR_AWFORD_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : STM32H753.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH7AWSCDR_Register use record
      SCDT           at 0 range 0 .. 7;
      Reserved_8_11  at 0 range 8 .. 11;
      BKSCD          at 0 range 12 .. 15;
      AWFOSR         at 0 range 16 .. 20;
      Reserved_21_21 at 0 range 21 .. 21;
      AWFORD         at 0 range 22 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype CH7WDATR_WDATA_Field is STM32H753.UInt16;

   --  CH7WDATR
   type CH7WDATR_Register is record
      --  WDATA
      WDATA          : CH7WDATR_WDATA_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH7WDATR_Register use record
      WDATA          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  CH7DATINR_INDAT array element
   subtype CH7DATINR_INDAT_Element is STM32H753.UInt16;

   --  CH7DATINR_INDAT array
   type CH7DATINR_INDAT_Field_Array is array (0 .. 1)
     of CH7DATINR_INDAT_Element
     with Component_Size => 16, Size => 32;

   --  CH7DATINR
   type CH7DATINR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  INDAT as a value
            Val : STM32H753.UInt32;
         when True =>
            --  INDAT as an array
            Arr : CH7DATINR_INDAT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH7DATINR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   subtype CH7DLYR_PLSSKP_Field is STM32H753.UInt6;

   --  channel y delay register
   type CH7DLYR_Register is record
      --  PLSSKP
      PLSSKP        : CH7DLYR_PLSSKP_Field := 16#0#;
      --  unspecified
      Reserved_6_31 : STM32H753.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CH7DLYR_Register use record
      PLSSKP        at 0 range 0 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   subtype DFSDM_FLT0CR1_JEXTSEL_Field is STM32H753.UInt3;
   subtype DFSDM_FLT0CR1_JEXTEN_Field is STM32H753.UInt2;
   subtype DFSDM_FLT0CR1_RCH_Field is STM32H753.UInt3;

   --  control register 1
   type DFSDM_FLT0CR1_Register is record
      --  DFSDM enable
      DFEN           : Boolean := False;
      --  Start a conversion of the injected group of channels
      JSWSTART       : Boolean := False;
      --  unspecified
      Reserved_2_2   : STM32H753.Bit := 16#0#;
      --  Launch an injected conversion synchronously with the DFSDM0 JSWSTART
      --  trigger
      JSYNC          : Boolean := False;
      --  Scanning conversion mode for injected conversions
      JSCAN          : Boolean := False;
      --  DMA channel enabled to read data for the injected channel group
      JDMAEN         : Boolean := False;
      --  unspecified
      Reserved_6_7   : STM32H753.UInt2 := 16#0#;
      --  Trigger signal selection for launching injected conversions
      JEXTSEL        : DFSDM_FLT0CR1_JEXTSEL_Field := 16#0#;
      --  unspecified
      Reserved_11_12 : STM32H753.UInt2 := 16#0#;
      --  Trigger enable and trigger edge selection for injected conversions
      JEXTEN         : DFSDM_FLT0CR1_JEXTEN_Field := 16#0#;
      --  unspecified
      Reserved_15_16 : STM32H753.UInt2 := 16#0#;
      --  Software start of a conversion on the regular channel
      RSWSTART       : Boolean := False;
      --  Continuous mode selection for regular conversions
      RCONT          : Boolean := False;
      --  Launch regular conversion synchronously with DFSDM0
      RSYNC          : Boolean := False;
      --  unspecified
      Reserved_20_20 : STM32H753.Bit := 16#0#;
      --  DMA channel enabled to read data for the regular conversion
      RDMAEN         : Boolean := False;
      --  unspecified
      Reserved_22_23 : STM32H753.UInt2 := 16#0#;
      --  Regular channel selection
      RCH            : DFSDM_FLT0CR1_RCH_Field := 16#0#;
      --  unspecified
      Reserved_27_28 : STM32H753.UInt2 := 16#0#;
      --  Fast conversion mode selection for regular conversions
      FAST           : Boolean := False;
      --  Analog watchdog fast mode select
      AWFSEL         : Boolean := False;
      --  unspecified
      Reserved_31_31 : STM32H753.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT0CR1_Register use record
      DFEN           at 0 range 0 .. 0;
      JSWSTART       at 0 range 1 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      JSYNC          at 0 range 3 .. 3;
      JSCAN          at 0 range 4 .. 4;
      JDMAEN         at 0 range 5 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      JEXTSEL        at 0 range 8 .. 10;
      Reserved_11_12 at 0 range 11 .. 12;
      JEXTEN         at 0 range 13 .. 14;
      Reserved_15_16 at 0 range 15 .. 16;
      RSWSTART       at 0 range 17 .. 17;
      RCONT          at 0 range 18 .. 18;
      RSYNC          at 0 range 19 .. 19;
      Reserved_20_20 at 0 range 20 .. 20;
      RDMAEN         at 0 range 21 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      RCH            at 0 range 24 .. 26;
      Reserved_27_28 at 0 range 27 .. 28;
      FAST           at 0 range 29 .. 29;
      AWFSEL         at 0 range 30 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype DFSDM_FLT0CR2_EXCH_Field is STM32H753.Byte;
   subtype DFSDM_FLT0CR2_AWDCH_Field is STM32H753.Byte;

   --  control register 2
   type DFSDM_FLT0CR2_Register is record
      --  Injected end of conversion interrupt enable
      JEOCIE         : Boolean := False;
      --  Regular end of conversion interrupt enable
      REOCIE         : Boolean := False;
      --  Injected data overrun interrupt enable
      JOVRIE         : Boolean := False;
      --  Regular data overrun interrupt enable
      ROVRIE         : Boolean := False;
      --  Analog watchdog interrupt enable
      AWDIE          : Boolean := False;
      --  Short-circuit detector interrupt enable
      SCDIE          : Boolean := False;
      --  Clock absence interrupt enable
      CKABIE         : Boolean := False;
      --  unspecified
      Reserved_7_7   : STM32H753.Bit := 16#0#;
      --  Extremes detector channel selection
      EXCH           : DFSDM_FLT0CR2_EXCH_Field := 16#0#;
      --  Analog watchdog channel selection
      AWDCH          : DFSDM_FLT0CR2_AWDCH_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : STM32H753.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT0CR2_Register use record
      JEOCIE         at 0 range 0 .. 0;
      REOCIE         at 0 range 1 .. 1;
      JOVRIE         at 0 range 2 .. 2;
      ROVRIE         at 0 range 3 .. 3;
      AWDIE          at 0 range 4 .. 4;
      SCDIE          at 0 range 5 .. 5;
      CKABIE         at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      EXCH           at 0 range 8 .. 15;
      AWDCH          at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype DFSDM_FLT0ISR_CKABF_Field is STM32H753.Byte;
   subtype DFSDM_FLT0ISR_SCDF_Field is STM32H753.Byte;

   --  interrupt and status register
   type DFSDM_FLT0ISR_Register is record
      --  Read-only. End of injected conversion flag
      JEOCF          : Boolean;
      --  Read-only. End of regular conversion flag
      REOCF          : Boolean;
      --  Read-only. Injected conversion overrun flag
      JOVRF          : Boolean;
      --  Read-only. Regular conversion overrun flag
      ROVRF          : Boolean;
      --  Read-only. Analog watchdog
      AWDF           : Boolean;
      --  unspecified
      Reserved_5_12  : STM32H753.Byte;
      --  Read-only. Injected conversion in progress status
      JCIP           : Boolean;
      --  Read-only. Regular conversion in progress status
      RCIP           : Boolean;
      --  unspecified
      Reserved_15_15 : STM32H753.Bit;
      --  Read-only. Clock absence flag
      CKABF          : DFSDM_FLT0ISR_CKABF_Field;
      --  Read-only. short-circuit detector flag
      SCDF           : DFSDM_FLT0ISR_SCDF_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT0ISR_Register use record
      JEOCF          at 0 range 0 .. 0;
      REOCF          at 0 range 1 .. 1;
      JOVRF          at 0 range 2 .. 2;
      ROVRF          at 0 range 3 .. 3;
      AWDF           at 0 range 4 .. 4;
      Reserved_5_12  at 0 range 5 .. 12;
      JCIP           at 0 range 13 .. 13;
      RCIP           at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      CKABF          at 0 range 16 .. 23;
      SCDF           at 0 range 24 .. 31;
   end record;

   subtype DFSDM_FLT0ICR_CLRCKABF_Field is STM32H753.Byte;
   subtype DFSDM_FLT0ICR_CLRSCDF_Field is STM32H753.Byte;

   --  interrupt flag clear register
   type DFSDM_FLT0ICR_Register is record
      --  unspecified
      Reserved_0_1  : STM32H753.UInt2 := 16#0#;
      --  Clear the injected conversion overrun flag
      CLRJOVRF      : Boolean := False;
      --  Clear the regular conversion overrun flag
      CLRROVRF      : Boolean := False;
      --  unspecified
      Reserved_4_15 : STM32H753.UInt12 := 16#0#;
      --  Clear the clock absence flag
      CLRCKABF      : DFSDM_FLT0ICR_CLRCKABF_Field := 16#0#;
      --  Clear the short-circuit detector flag
      CLRSCDF       : DFSDM_FLT0ICR_CLRSCDF_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT0ICR_Register use record
      Reserved_0_1  at 0 range 0 .. 1;
      CLRJOVRF      at 0 range 2 .. 2;
      CLRROVRF      at 0 range 3 .. 3;
      Reserved_4_15 at 0 range 4 .. 15;
      CLRCKABF      at 0 range 16 .. 23;
      CLRSCDF       at 0 range 24 .. 31;
   end record;

   subtype DFSDM_FLT0JCHGR_JCHG_Field is STM32H753.Byte;

   --  injected channel group selection register
   type DFSDM_FLT0JCHGR_Register is record
      --  Injected channel group selection
      JCHG          : DFSDM_FLT0JCHGR_JCHG_Field := 16#1#;
      --  unspecified
      Reserved_8_31 : STM32H753.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT0JCHGR_Register use record
      JCHG          at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype DFSDM_FLT0FCR_IOSR_Field is STM32H753.Byte;
   subtype DFSDM_FLT0FCR_FOSR_Field is STM32H753.UInt10;
   subtype DFSDM_FLT0FCR_FORD_Field is STM32H753.UInt3;

   --  filter control register
   type DFSDM_FLT0FCR_Register is record
      --  Integrator oversampling ratio (averaging length)
      IOSR           : DFSDM_FLT0FCR_IOSR_Field := 16#0#;
      --  unspecified
      Reserved_8_15  : STM32H753.Byte := 16#0#;
      --  Sinc filter oversampling ratio (decimation rate)
      FOSR           : DFSDM_FLT0FCR_FOSR_Field := 16#0#;
      --  unspecified
      Reserved_26_28 : STM32H753.UInt3 := 16#0#;
      --  Sinc filter order
      FORD           : DFSDM_FLT0FCR_FORD_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT0FCR_Register use record
      IOSR           at 0 range 0 .. 7;
      Reserved_8_15  at 0 range 8 .. 15;
      FOSR           at 0 range 16 .. 25;
      Reserved_26_28 at 0 range 26 .. 28;
      FORD           at 0 range 29 .. 31;
   end record;

   subtype DFSDM_FLT0JDATAR_JDATACH_Field is STM32H753.UInt3;
   subtype DFSDM_FLT0JDATAR_JDATA_Field is STM32H753.UInt24;

   --  data register for injected group
   type DFSDM_FLT0JDATAR_Register is record
      --  Read-only. Injected channel most recently converted
      JDATACH      : DFSDM_FLT0JDATAR_JDATACH_Field;
      --  unspecified
      Reserved_3_7 : STM32H753.UInt5;
      --  Read-only. Injected group conversion data
      JDATA        : DFSDM_FLT0JDATAR_JDATA_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT0JDATAR_Register use record
      JDATACH      at 0 range 0 .. 2;
      Reserved_3_7 at 0 range 3 .. 7;
      JDATA        at 0 range 8 .. 31;
   end record;

   subtype DFSDM_FLT0RDATAR_RDATACH_Field is STM32H753.UInt3;
   subtype DFSDM_FLT0RDATAR_RDATA_Field is STM32H753.UInt24;

   --  data register for the regular channel
   type DFSDM_FLT0RDATAR_Register is record
      --  Read-only. Regular channel most recently converted
      RDATACH      : DFSDM_FLT0RDATAR_RDATACH_Field;
      --  unspecified
      Reserved_3_3 : STM32H753.Bit;
      --  Read-only. Regular channel pending data
      RPEND        : Boolean;
      --  unspecified
      Reserved_5_7 : STM32H753.UInt3;
      --  Read-only. Regular channel conversion data
      RDATA        : DFSDM_FLT0RDATAR_RDATA_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT0RDATAR_Register use record
      RDATACH      at 0 range 0 .. 2;
      Reserved_3_3 at 0 range 3 .. 3;
      RPEND        at 0 range 4 .. 4;
      Reserved_5_7 at 0 range 5 .. 7;
      RDATA        at 0 range 8 .. 31;
   end record;

   subtype DFSDM_FLT0AWHTR_BKAWH_Field is STM32H753.UInt4;
   subtype DFSDM_FLT0AWHTR_AWHT_Field is STM32H753.UInt24;

   --  analog watchdog high threshold register
   type DFSDM_FLT0AWHTR_Register is record
      --  Break signal assignment to analog watchdog high threshold event
      BKAWH        : DFSDM_FLT0AWHTR_BKAWH_Field := 16#0#;
      --  unspecified
      Reserved_4_7 : STM32H753.UInt4 := 16#0#;
      --  Analog watchdog high threshold
      AWHT         : DFSDM_FLT0AWHTR_AWHT_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT0AWHTR_Register use record
      BKAWH        at 0 range 0 .. 3;
      Reserved_4_7 at 0 range 4 .. 7;
      AWHT         at 0 range 8 .. 31;
   end record;

   subtype DFSDM_FLT0AWLTR_BKAWL_Field is STM32H753.UInt4;
   subtype DFSDM_FLT0AWLTR_AWLT_Field is STM32H753.UInt24;

   --  analog watchdog low threshold register
   type DFSDM_FLT0AWLTR_Register is record
      --  Break signal assignment to analog watchdog low threshold event
      BKAWL        : DFSDM_FLT0AWLTR_BKAWL_Field := 16#0#;
      --  unspecified
      Reserved_4_7 : STM32H753.UInt4 := 16#0#;
      --  Analog watchdog low threshold
      AWLT         : DFSDM_FLT0AWLTR_AWLT_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT0AWLTR_Register use record
      BKAWL        at 0 range 0 .. 3;
      Reserved_4_7 at 0 range 4 .. 7;
      AWLT         at 0 range 8 .. 31;
   end record;

   subtype DFSDM_FLT0AWSR_AWLTF_Field is STM32H753.Byte;
   subtype DFSDM_FLT0AWSR_AWHTF_Field is STM32H753.Byte;

   --  analog watchdog status register
   type DFSDM_FLT0AWSR_Register is record
      --  Read-only. Analog watchdog low threshold flag
      AWLTF          : DFSDM_FLT0AWSR_AWLTF_Field;
      --  Read-only. Analog watchdog high threshold flag
      AWHTF          : DFSDM_FLT0AWSR_AWHTF_Field;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT0AWSR_Register use record
      AWLTF          at 0 range 0 .. 7;
      AWHTF          at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DFSDM_FLT0AWCFR_CLRAWLTF_Field is STM32H753.Byte;
   subtype DFSDM_FLT0AWCFR_CLRAWHTF_Field is STM32H753.Byte;

   --  analog watchdog clear flag register
   type DFSDM_FLT0AWCFR_Register is record
      --  Clear the analog watchdog low threshold flag
      CLRAWLTF       : DFSDM_FLT0AWCFR_CLRAWLTF_Field := 16#0#;
      --  Clear the analog watchdog high threshold flag
      CLRAWHTF       : DFSDM_FLT0AWCFR_CLRAWHTF_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT0AWCFR_Register use record
      CLRAWLTF       at 0 range 0 .. 7;
      CLRAWHTF       at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DFSDM_FLT0EXMAX_EXMAXCH_Field is STM32H753.UInt3;
   subtype DFSDM_FLT0EXMAX_EXMAX_Field is STM32H753.UInt24;

   --  Extremes detector maximum register
   type DFSDM_FLT0EXMAX_Register is record
      --  Read-only. Extremes detector maximum data channel
      EXMAXCH      : DFSDM_FLT0EXMAX_EXMAXCH_Field;
      --  unspecified
      Reserved_3_7 : STM32H753.UInt5;
      --  Read-only. Extremes detector maximum value
      EXMAX        : DFSDM_FLT0EXMAX_EXMAX_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT0EXMAX_Register use record
      EXMAXCH      at 0 range 0 .. 2;
      Reserved_3_7 at 0 range 3 .. 7;
      EXMAX        at 0 range 8 .. 31;
   end record;

   subtype DFSDM_FLT0EXMIN_EXMINCH_Field is STM32H753.UInt3;
   subtype DFSDM_FLT0EXMIN_EXMIN_Field is STM32H753.UInt24;

   --  Extremes detector minimum register
   type DFSDM_FLT0EXMIN_Register is record
      --  Read-only. Extremes detector minimum data channel
      EXMINCH      : DFSDM_FLT0EXMIN_EXMINCH_Field;
      --  unspecified
      Reserved_3_7 : STM32H753.UInt5;
      --  Read-only. EXMIN
      EXMIN        : DFSDM_FLT0EXMIN_EXMIN_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT0EXMIN_Register use record
      EXMINCH      at 0 range 0 .. 2;
      Reserved_3_7 at 0 range 3 .. 7;
      EXMIN        at 0 range 8 .. 31;
   end record;

   subtype DFSDM_FLT0CNVTIMR_CNVCNT_Field is STM32H753.UInt28;

   --  conversion timer register
   type DFSDM_FLT0CNVTIMR_Register is record
      --  unspecified
      Reserved_0_3 : STM32H753.UInt4;
      --  Read-only. 28-bit timer counting conversion time t = CNVCNT[27:0] /
      --  fDFSDM_CKIN
      CNVCNT       : DFSDM_FLT0CNVTIMR_CNVCNT_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT0CNVTIMR_Register use record
      Reserved_0_3 at 0 range 0 .. 3;
      CNVCNT       at 0 range 4 .. 31;
   end record;

   subtype DFSDM_FLT1CR1_JEXTSEL_Field is STM32H753.UInt3;
   subtype DFSDM_FLT1CR1_JEXTEN_Field is STM32H753.UInt2;
   subtype DFSDM_FLT1CR1_RCH_Field is STM32H753.UInt3;

   --  control register 1
   type DFSDM_FLT1CR1_Register is record
      --  DFSDM enable
      DFEN           : Boolean := False;
      --  Start a conversion of the injected group of channels
      JSWSTART       : Boolean := False;
      --  unspecified
      Reserved_2_2   : STM32H753.Bit := 16#0#;
      --  Launch an injected conversion synchronously with the DFSDM0 JSWSTART
      --  trigger
      JSYNC          : Boolean := False;
      --  Scanning conversion mode for injected conversions
      JSCAN          : Boolean := False;
      --  DMA channel enabled to read data for the injected channel group
      JDMAEN         : Boolean := False;
      --  unspecified
      Reserved_6_7   : STM32H753.UInt2 := 16#0#;
      --  Trigger signal selection for launching injected conversions
      JEXTSEL        : DFSDM_FLT1CR1_JEXTSEL_Field := 16#0#;
      --  unspecified
      Reserved_11_12 : STM32H753.UInt2 := 16#0#;
      --  Trigger enable and trigger edge selection for injected conversions
      JEXTEN         : DFSDM_FLT1CR1_JEXTEN_Field := 16#0#;
      --  unspecified
      Reserved_15_16 : STM32H753.UInt2 := 16#0#;
      --  Software start of a conversion on the regular channel
      RSWSTART       : Boolean := False;
      --  Continuous mode selection for regular conversions
      RCONT          : Boolean := False;
      --  Launch regular conversion synchronously with DFSDM0
      RSYNC          : Boolean := False;
      --  unspecified
      Reserved_20_20 : STM32H753.Bit := 16#0#;
      --  DMA channel enabled to read data for the regular conversion
      RDMAEN         : Boolean := False;
      --  unspecified
      Reserved_22_23 : STM32H753.UInt2 := 16#0#;
      --  Regular channel selection
      RCH            : DFSDM_FLT1CR1_RCH_Field := 16#0#;
      --  unspecified
      Reserved_27_28 : STM32H753.UInt2 := 16#0#;
      --  Fast conversion mode selection for regular conversions
      FAST           : Boolean := False;
      --  Analog watchdog fast mode select
      AWFSEL         : Boolean := False;
      --  unspecified
      Reserved_31_31 : STM32H753.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT1CR1_Register use record
      DFEN           at 0 range 0 .. 0;
      JSWSTART       at 0 range 1 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      JSYNC          at 0 range 3 .. 3;
      JSCAN          at 0 range 4 .. 4;
      JDMAEN         at 0 range 5 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      JEXTSEL        at 0 range 8 .. 10;
      Reserved_11_12 at 0 range 11 .. 12;
      JEXTEN         at 0 range 13 .. 14;
      Reserved_15_16 at 0 range 15 .. 16;
      RSWSTART       at 0 range 17 .. 17;
      RCONT          at 0 range 18 .. 18;
      RSYNC          at 0 range 19 .. 19;
      Reserved_20_20 at 0 range 20 .. 20;
      RDMAEN         at 0 range 21 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      RCH            at 0 range 24 .. 26;
      Reserved_27_28 at 0 range 27 .. 28;
      FAST           at 0 range 29 .. 29;
      AWFSEL         at 0 range 30 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype DFSDM_FLT1CR2_EXCH_Field is STM32H753.Byte;
   subtype DFSDM_FLT1CR2_AWDCH_Field is STM32H753.Byte;

   --  control register 2
   type DFSDM_FLT1CR2_Register is record
      --  Injected end of conversion interrupt enable
      JEOCIE         : Boolean := False;
      --  Regular end of conversion interrupt enable
      REOCIE         : Boolean := False;
      --  Injected data overrun interrupt enable
      JOVRIE         : Boolean := False;
      --  Regular data overrun interrupt enable
      ROVRIE         : Boolean := False;
      --  Analog watchdog interrupt enable
      AWDIE          : Boolean := False;
      --  Short-circuit detector interrupt enable
      SCDIE          : Boolean := False;
      --  Clock absence interrupt enable
      CKABIE         : Boolean := False;
      --  unspecified
      Reserved_7_7   : STM32H753.Bit := 16#0#;
      --  Extremes detector channel selection
      EXCH           : DFSDM_FLT1CR2_EXCH_Field := 16#0#;
      --  Analog watchdog channel selection
      AWDCH          : DFSDM_FLT1CR2_AWDCH_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : STM32H753.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT1CR2_Register use record
      JEOCIE         at 0 range 0 .. 0;
      REOCIE         at 0 range 1 .. 1;
      JOVRIE         at 0 range 2 .. 2;
      ROVRIE         at 0 range 3 .. 3;
      AWDIE          at 0 range 4 .. 4;
      SCDIE          at 0 range 5 .. 5;
      CKABIE         at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      EXCH           at 0 range 8 .. 15;
      AWDCH          at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype DFSDM_FLT1ISR_CKABF_Field is STM32H753.Byte;
   subtype DFSDM_FLT1ISR_SCDF_Field is STM32H753.Byte;

   --  interrupt and status register
   type DFSDM_FLT1ISR_Register is record
      --  Read-only. End of injected conversion flag
      JEOCF          : Boolean;
      --  Read-only. End of regular conversion flag
      REOCF          : Boolean;
      --  Read-only. Injected conversion overrun flag
      JOVRF          : Boolean;
      --  Read-only. Regular conversion overrun flag
      ROVRF          : Boolean;
      --  Read-only. Analog watchdog
      AWDF           : Boolean;
      --  unspecified
      Reserved_5_12  : STM32H753.Byte;
      --  Read-only. Injected conversion in progress status
      JCIP           : Boolean;
      --  Read-only. Regular conversion in progress status
      RCIP           : Boolean;
      --  unspecified
      Reserved_15_15 : STM32H753.Bit;
      --  Read-only. Clock absence flag
      CKABF          : DFSDM_FLT1ISR_CKABF_Field;
      --  Read-only. short-circuit detector flag
      SCDF           : DFSDM_FLT1ISR_SCDF_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT1ISR_Register use record
      JEOCF          at 0 range 0 .. 0;
      REOCF          at 0 range 1 .. 1;
      JOVRF          at 0 range 2 .. 2;
      ROVRF          at 0 range 3 .. 3;
      AWDF           at 0 range 4 .. 4;
      Reserved_5_12  at 0 range 5 .. 12;
      JCIP           at 0 range 13 .. 13;
      RCIP           at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      CKABF          at 0 range 16 .. 23;
      SCDF           at 0 range 24 .. 31;
   end record;

   subtype DFSDM_FLT1ICR_CLRCKABF_Field is STM32H753.Byte;
   subtype DFSDM_FLT1ICR_CLRSCDF_Field is STM32H753.Byte;

   --  interrupt flag clear register
   type DFSDM_FLT1ICR_Register is record
      --  unspecified
      Reserved_0_1  : STM32H753.UInt2 := 16#0#;
      --  Clear the injected conversion overrun flag
      CLRJOVRF      : Boolean := False;
      --  Clear the regular conversion overrun flag
      CLRROVRF      : Boolean := False;
      --  unspecified
      Reserved_4_15 : STM32H753.UInt12 := 16#0#;
      --  Clear the clock absence flag
      CLRCKABF      : DFSDM_FLT1ICR_CLRCKABF_Field := 16#0#;
      --  Clear the short-circuit detector flag
      CLRSCDF       : DFSDM_FLT1ICR_CLRSCDF_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT1ICR_Register use record
      Reserved_0_1  at 0 range 0 .. 1;
      CLRJOVRF      at 0 range 2 .. 2;
      CLRROVRF      at 0 range 3 .. 3;
      Reserved_4_15 at 0 range 4 .. 15;
      CLRCKABF      at 0 range 16 .. 23;
      CLRSCDF       at 0 range 24 .. 31;
   end record;

   subtype DFSDM_FLT1CHGR_JCHG_Field is STM32H753.Byte;

   --  injected channel group selection register
   type DFSDM_FLT1CHGR_Register is record
      --  Injected channel group selection
      JCHG          : DFSDM_FLT1CHGR_JCHG_Field := 16#1#;
      --  unspecified
      Reserved_8_31 : STM32H753.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT1CHGR_Register use record
      JCHG          at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype DFSDM_FLT1FCR_IOSR_Field is STM32H753.Byte;
   subtype DFSDM_FLT1FCR_FOSR_Field is STM32H753.UInt10;
   subtype DFSDM_FLT1FCR_FORD_Field is STM32H753.UInt3;

   --  filter control register
   type DFSDM_FLT1FCR_Register is record
      --  Integrator oversampling ratio (averaging length)
      IOSR           : DFSDM_FLT1FCR_IOSR_Field := 16#0#;
      --  unspecified
      Reserved_8_15  : STM32H753.Byte := 16#0#;
      --  Sinc filter oversampling ratio (decimation rate)
      FOSR           : DFSDM_FLT1FCR_FOSR_Field := 16#0#;
      --  unspecified
      Reserved_26_28 : STM32H753.UInt3 := 16#0#;
      --  Sinc filter order
      FORD           : DFSDM_FLT1FCR_FORD_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT1FCR_Register use record
      IOSR           at 0 range 0 .. 7;
      Reserved_8_15  at 0 range 8 .. 15;
      FOSR           at 0 range 16 .. 25;
      Reserved_26_28 at 0 range 26 .. 28;
      FORD           at 0 range 29 .. 31;
   end record;

   subtype DFSDM_FLT1JDATAR_JDATACH_Field is STM32H753.UInt3;
   subtype DFSDM_FLT1JDATAR_JDATA_Field is STM32H753.UInt24;

   --  data register for injected group
   type DFSDM_FLT1JDATAR_Register is record
      --  Read-only. Injected channel most recently converted
      JDATACH      : DFSDM_FLT1JDATAR_JDATACH_Field;
      --  unspecified
      Reserved_3_7 : STM32H753.UInt5;
      --  Read-only. Injected group conversion data
      JDATA        : DFSDM_FLT1JDATAR_JDATA_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT1JDATAR_Register use record
      JDATACH      at 0 range 0 .. 2;
      Reserved_3_7 at 0 range 3 .. 7;
      JDATA        at 0 range 8 .. 31;
   end record;

   subtype DFSDM_FLT1RDATAR_RDATACH_Field is STM32H753.UInt3;
   subtype DFSDM_FLT1RDATAR_RDATA_Field is STM32H753.UInt24;

   --  data register for the regular channel
   type DFSDM_FLT1RDATAR_Register is record
      --  Read-only. Regular channel most recently converted
      RDATACH      : DFSDM_FLT1RDATAR_RDATACH_Field;
      --  unspecified
      Reserved_3_3 : STM32H753.Bit;
      --  Read-only. Regular channel pending data
      RPEND        : Boolean;
      --  unspecified
      Reserved_5_7 : STM32H753.UInt3;
      --  Read-only. Regular channel conversion data
      RDATA        : DFSDM_FLT1RDATAR_RDATA_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT1RDATAR_Register use record
      RDATACH      at 0 range 0 .. 2;
      Reserved_3_3 at 0 range 3 .. 3;
      RPEND        at 0 range 4 .. 4;
      Reserved_5_7 at 0 range 5 .. 7;
      RDATA        at 0 range 8 .. 31;
   end record;

   subtype DFSDM_FLT1AWHTR_BKAWH_Field is STM32H753.UInt4;
   subtype DFSDM_FLT1AWHTR_AWHT_Field is STM32H753.UInt24;

   --  analog watchdog high threshold register
   type DFSDM_FLT1AWHTR_Register is record
      --  Break signal assignment to analog watchdog high threshold event
      BKAWH        : DFSDM_FLT1AWHTR_BKAWH_Field := 16#0#;
      --  unspecified
      Reserved_4_7 : STM32H753.UInt4 := 16#0#;
      --  Analog watchdog high threshold
      AWHT         : DFSDM_FLT1AWHTR_AWHT_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT1AWHTR_Register use record
      BKAWH        at 0 range 0 .. 3;
      Reserved_4_7 at 0 range 4 .. 7;
      AWHT         at 0 range 8 .. 31;
   end record;

   subtype DFSDM_FLT1AWLTR_BKAWL_Field is STM32H753.UInt4;
   subtype DFSDM_FLT1AWLTR_AWLT_Field is STM32H753.UInt24;

   --  analog watchdog low threshold register
   type DFSDM_FLT1AWLTR_Register is record
      --  Break signal assignment to analog watchdog low threshold event
      BKAWL        : DFSDM_FLT1AWLTR_BKAWL_Field := 16#0#;
      --  unspecified
      Reserved_4_7 : STM32H753.UInt4 := 16#0#;
      --  Analog watchdog low threshold
      AWLT         : DFSDM_FLT1AWLTR_AWLT_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT1AWLTR_Register use record
      BKAWL        at 0 range 0 .. 3;
      Reserved_4_7 at 0 range 4 .. 7;
      AWLT         at 0 range 8 .. 31;
   end record;

   subtype DFSDM_FLT1AWSR_AWLTF_Field is STM32H753.Byte;
   subtype DFSDM_FLT1AWSR_AWHTF_Field is STM32H753.Byte;

   --  analog watchdog status register
   type DFSDM_FLT1AWSR_Register is record
      --  Read-only. Analog watchdog low threshold flag
      AWLTF          : DFSDM_FLT1AWSR_AWLTF_Field;
      --  Read-only. Analog watchdog high threshold flag
      AWHTF          : DFSDM_FLT1AWSR_AWHTF_Field;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT1AWSR_Register use record
      AWLTF          at 0 range 0 .. 7;
      AWHTF          at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DFSDM_FLT1AWCFR_CLRAWLTF_Field is STM32H753.Byte;
   subtype DFSDM_FLT1AWCFR_CLRAWHTF_Field is STM32H753.Byte;

   --  analog watchdog clear flag register
   type DFSDM_FLT1AWCFR_Register is record
      --  Clear the analog watchdog low threshold flag
      CLRAWLTF       : DFSDM_FLT1AWCFR_CLRAWLTF_Field := 16#0#;
      --  Clear the analog watchdog high threshold flag
      CLRAWHTF       : DFSDM_FLT1AWCFR_CLRAWHTF_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT1AWCFR_Register use record
      CLRAWLTF       at 0 range 0 .. 7;
      CLRAWHTF       at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DFSDM_FLT1EXMAX_EXMAXCH_Field is STM32H753.UInt3;
   subtype DFSDM_FLT1EXMAX_EXMAX_Field is STM32H753.UInt24;

   --  Extremes detector maximum register
   type DFSDM_FLT1EXMAX_Register is record
      --  Read-only. Extremes detector maximum data channel
      EXMAXCH      : DFSDM_FLT1EXMAX_EXMAXCH_Field;
      --  unspecified
      Reserved_3_7 : STM32H753.UInt5;
      --  Read-only. Extremes detector maximum value
      EXMAX        : DFSDM_FLT1EXMAX_EXMAX_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT1EXMAX_Register use record
      EXMAXCH      at 0 range 0 .. 2;
      Reserved_3_7 at 0 range 3 .. 7;
      EXMAX        at 0 range 8 .. 31;
   end record;

   subtype DFSDM_FLT1EXMIN_EXMINCH_Field is STM32H753.UInt3;
   subtype DFSDM_FLT1EXMIN_EXMIN_Field is STM32H753.UInt24;

   --  Extremes detector minimum register
   type DFSDM_FLT1EXMIN_Register is record
      --  Read-only. Extremes detector minimum data channel
      EXMINCH      : DFSDM_FLT1EXMIN_EXMINCH_Field;
      --  unspecified
      Reserved_3_7 : STM32H753.UInt5;
      --  Read-only. EXMIN
      EXMIN        : DFSDM_FLT1EXMIN_EXMIN_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT1EXMIN_Register use record
      EXMINCH      at 0 range 0 .. 2;
      Reserved_3_7 at 0 range 3 .. 7;
      EXMIN        at 0 range 8 .. 31;
   end record;

   subtype DFSDM_FLT1CNVTIMR_CNVCNT_Field is STM32H753.UInt28;

   --  conversion timer register
   type DFSDM_FLT1CNVTIMR_Register is record
      --  unspecified
      Reserved_0_3 : STM32H753.UInt4;
      --  Read-only. 28-bit timer counting conversion time t = CNVCNT[27:0] /
      --  fDFSDM_CKIN
      CNVCNT       : DFSDM_FLT1CNVTIMR_CNVCNT_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT1CNVTIMR_Register use record
      Reserved_0_3 at 0 range 0 .. 3;
      CNVCNT       at 0 range 4 .. 31;
   end record;

   subtype DFSDM_FLT2CR1_JEXTSEL_Field is STM32H753.UInt3;
   subtype DFSDM_FLT2CR1_JEXTEN_Field is STM32H753.UInt2;
   subtype DFSDM_FLT2CR1_RCH_Field is STM32H753.UInt3;

   --  control register 1
   type DFSDM_FLT2CR1_Register is record
      --  DFSDM enable
      DFEN           : Boolean := False;
      --  Start a conversion of the injected group of channels
      JSWSTART       : Boolean := False;
      --  unspecified
      Reserved_2_2   : STM32H753.Bit := 16#0#;
      --  Launch an injected conversion synchronously with the DFSDM0 JSWSTART
      --  trigger
      JSYNC          : Boolean := False;
      --  Scanning conversion mode for injected conversions
      JSCAN          : Boolean := False;
      --  DMA channel enabled to read data for the injected channel group
      JDMAEN         : Boolean := False;
      --  unspecified
      Reserved_6_7   : STM32H753.UInt2 := 16#0#;
      --  Trigger signal selection for launching injected conversions
      JEXTSEL        : DFSDM_FLT2CR1_JEXTSEL_Field := 16#0#;
      --  unspecified
      Reserved_11_12 : STM32H753.UInt2 := 16#0#;
      --  Trigger enable and trigger edge selection for injected conversions
      JEXTEN         : DFSDM_FLT2CR1_JEXTEN_Field := 16#0#;
      --  unspecified
      Reserved_15_16 : STM32H753.UInt2 := 16#0#;
      --  Software start of a conversion on the regular channel
      RSWSTART       : Boolean := False;
      --  Continuous mode selection for regular conversions
      RCONT          : Boolean := False;
      --  Launch regular conversion synchronously with DFSDM0
      RSYNC          : Boolean := False;
      --  unspecified
      Reserved_20_20 : STM32H753.Bit := 16#0#;
      --  DMA channel enabled to read data for the regular conversion
      RDMAEN         : Boolean := False;
      --  unspecified
      Reserved_22_23 : STM32H753.UInt2 := 16#0#;
      --  Regular channel selection
      RCH            : DFSDM_FLT2CR1_RCH_Field := 16#0#;
      --  unspecified
      Reserved_27_28 : STM32H753.UInt2 := 16#0#;
      --  Fast conversion mode selection for regular conversions
      FAST           : Boolean := False;
      --  Analog watchdog fast mode select
      AWFSEL         : Boolean := False;
      --  unspecified
      Reserved_31_31 : STM32H753.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT2CR1_Register use record
      DFEN           at 0 range 0 .. 0;
      JSWSTART       at 0 range 1 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      JSYNC          at 0 range 3 .. 3;
      JSCAN          at 0 range 4 .. 4;
      JDMAEN         at 0 range 5 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      JEXTSEL        at 0 range 8 .. 10;
      Reserved_11_12 at 0 range 11 .. 12;
      JEXTEN         at 0 range 13 .. 14;
      Reserved_15_16 at 0 range 15 .. 16;
      RSWSTART       at 0 range 17 .. 17;
      RCONT          at 0 range 18 .. 18;
      RSYNC          at 0 range 19 .. 19;
      Reserved_20_20 at 0 range 20 .. 20;
      RDMAEN         at 0 range 21 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      RCH            at 0 range 24 .. 26;
      Reserved_27_28 at 0 range 27 .. 28;
      FAST           at 0 range 29 .. 29;
      AWFSEL         at 0 range 30 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype DFSDM_FLT2CR2_EXCH_Field is STM32H753.Byte;
   subtype DFSDM_FLT2CR2_AWDCH_Field is STM32H753.Byte;

   --  control register 2
   type DFSDM_FLT2CR2_Register is record
      --  Injected end of conversion interrupt enable
      JEOCIE         : Boolean := False;
      --  Regular end of conversion interrupt enable
      REOCIE         : Boolean := False;
      --  Injected data overrun interrupt enable
      JOVRIE         : Boolean := False;
      --  Regular data overrun interrupt enable
      ROVRIE         : Boolean := False;
      --  Analog watchdog interrupt enable
      AWDIE          : Boolean := False;
      --  Short-circuit detector interrupt enable
      SCDIE          : Boolean := False;
      --  Clock absence interrupt enable
      CKABIE         : Boolean := False;
      --  unspecified
      Reserved_7_7   : STM32H753.Bit := 16#0#;
      --  Extremes detector channel selection
      EXCH           : DFSDM_FLT2CR2_EXCH_Field := 16#0#;
      --  Analog watchdog channel selection
      AWDCH          : DFSDM_FLT2CR2_AWDCH_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : STM32H753.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT2CR2_Register use record
      JEOCIE         at 0 range 0 .. 0;
      REOCIE         at 0 range 1 .. 1;
      JOVRIE         at 0 range 2 .. 2;
      ROVRIE         at 0 range 3 .. 3;
      AWDIE          at 0 range 4 .. 4;
      SCDIE          at 0 range 5 .. 5;
      CKABIE         at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      EXCH           at 0 range 8 .. 15;
      AWDCH          at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype DFSDM_FLT2ISR_CKABF_Field is STM32H753.Byte;
   subtype DFSDM_FLT2ISR_SCDF_Field is STM32H753.Byte;

   --  interrupt and status register
   type DFSDM_FLT2ISR_Register is record
      --  Read-only. End of injected conversion flag
      JEOCF          : Boolean;
      --  Read-only. End of regular conversion flag
      REOCF          : Boolean;
      --  Read-only. Injected conversion overrun flag
      JOVRF          : Boolean;
      --  Read-only. Regular conversion overrun flag
      ROVRF          : Boolean;
      --  Read-only. Analog watchdog
      AWDF           : Boolean;
      --  unspecified
      Reserved_5_12  : STM32H753.Byte;
      --  Read-only. Injected conversion in progress status
      JCIP           : Boolean;
      --  Read-only. Regular conversion in progress status
      RCIP           : Boolean;
      --  unspecified
      Reserved_15_15 : STM32H753.Bit;
      --  Read-only. Clock absence flag
      CKABF          : DFSDM_FLT2ISR_CKABF_Field;
      --  Read-only. short-circuit detector flag
      SCDF           : DFSDM_FLT2ISR_SCDF_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT2ISR_Register use record
      JEOCF          at 0 range 0 .. 0;
      REOCF          at 0 range 1 .. 1;
      JOVRF          at 0 range 2 .. 2;
      ROVRF          at 0 range 3 .. 3;
      AWDF           at 0 range 4 .. 4;
      Reserved_5_12  at 0 range 5 .. 12;
      JCIP           at 0 range 13 .. 13;
      RCIP           at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      CKABF          at 0 range 16 .. 23;
      SCDF           at 0 range 24 .. 31;
   end record;

   subtype DFSDM_FLT2ICR_CLRCKABF_Field is STM32H753.Byte;
   subtype DFSDM_FLT2ICR_CLRSCDF_Field is STM32H753.Byte;

   --  interrupt flag clear register
   type DFSDM_FLT2ICR_Register is record
      --  unspecified
      Reserved_0_1  : STM32H753.UInt2 := 16#0#;
      --  Clear the injected conversion overrun flag
      CLRJOVRF      : Boolean := False;
      --  Clear the regular conversion overrun flag
      CLRROVRF      : Boolean := False;
      --  unspecified
      Reserved_4_15 : STM32H753.UInt12 := 16#0#;
      --  Clear the clock absence flag
      CLRCKABF      : DFSDM_FLT2ICR_CLRCKABF_Field := 16#0#;
      --  Clear the short-circuit detector flag
      CLRSCDF       : DFSDM_FLT2ICR_CLRSCDF_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT2ICR_Register use record
      Reserved_0_1  at 0 range 0 .. 1;
      CLRJOVRF      at 0 range 2 .. 2;
      CLRROVRF      at 0 range 3 .. 3;
      Reserved_4_15 at 0 range 4 .. 15;
      CLRCKABF      at 0 range 16 .. 23;
      CLRSCDF       at 0 range 24 .. 31;
   end record;

   subtype DFSDM_FLT2JCHGR_JCHG_Field is STM32H753.Byte;

   --  injected channel group selection register
   type DFSDM_FLT2JCHGR_Register is record
      --  Injected channel group selection
      JCHG          : DFSDM_FLT2JCHGR_JCHG_Field := 16#1#;
      --  unspecified
      Reserved_8_31 : STM32H753.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT2JCHGR_Register use record
      JCHG          at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype DFSDM_FLT2FCR_IOSR_Field is STM32H753.Byte;
   subtype DFSDM_FLT2FCR_FOSR_Field is STM32H753.UInt10;
   subtype DFSDM_FLT2FCR_FORD_Field is STM32H753.UInt3;

   --  filter control register
   type DFSDM_FLT2FCR_Register is record
      --  Integrator oversampling ratio (averaging length)
      IOSR           : DFSDM_FLT2FCR_IOSR_Field := 16#0#;
      --  unspecified
      Reserved_8_15  : STM32H753.Byte := 16#0#;
      --  Sinc filter oversampling ratio (decimation rate)
      FOSR           : DFSDM_FLT2FCR_FOSR_Field := 16#0#;
      --  unspecified
      Reserved_26_28 : STM32H753.UInt3 := 16#0#;
      --  Sinc filter order
      FORD           : DFSDM_FLT2FCR_FORD_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT2FCR_Register use record
      IOSR           at 0 range 0 .. 7;
      Reserved_8_15  at 0 range 8 .. 15;
      FOSR           at 0 range 16 .. 25;
      Reserved_26_28 at 0 range 26 .. 28;
      FORD           at 0 range 29 .. 31;
   end record;

   subtype DFSDM_FLT2JDATAR_JDATACH_Field is STM32H753.UInt3;
   subtype DFSDM_FLT2JDATAR_JDATA_Field is STM32H753.UInt24;

   --  data register for injected group
   type DFSDM_FLT2JDATAR_Register is record
      --  Read-only. Injected channel most recently converted
      JDATACH      : DFSDM_FLT2JDATAR_JDATACH_Field;
      --  unspecified
      Reserved_3_7 : STM32H753.UInt5;
      --  Read-only. Injected group conversion data
      JDATA        : DFSDM_FLT2JDATAR_JDATA_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT2JDATAR_Register use record
      JDATACH      at 0 range 0 .. 2;
      Reserved_3_7 at 0 range 3 .. 7;
      JDATA        at 0 range 8 .. 31;
   end record;

   subtype DFSDM_FLT2RDATAR_RDATACH_Field is STM32H753.UInt3;
   subtype DFSDM_FLT2RDATAR_RDATA_Field is STM32H753.UInt24;

   --  data register for the regular channel
   type DFSDM_FLT2RDATAR_Register is record
      --  Read-only. Regular channel most recently converted
      RDATACH      : DFSDM_FLT2RDATAR_RDATACH_Field;
      --  unspecified
      Reserved_3_3 : STM32H753.Bit;
      --  Read-only. Regular channel pending data
      RPEND        : Boolean;
      --  unspecified
      Reserved_5_7 : STM32H753.UInt3;
      --  Read-only. Regular channel conversion data
      RDATA        : DFSDM_FLT2RDATAR_RDATA_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT2RDATAR_Register use record
      RDATACH      at 0 range 0 .. 2;
      Reserved_3_3 at 0 range 3 .. 3;
      RPEND        at 0 range 4 .. 4;
      Reserved_5_7 at 0 range 5 .. 7;
      RDATA        at 0 range 8 .. 31;
   end record;

   subtype DFSDM_FLT2AWHTR_BKAWH_Field is STM32H753.UInt4;
   subtype DFSDM_FLT2AWHTR_AWHT_Field is STM32H753.UInt24;

   --  analog watchdog high threshold register
   type DFSDM_FLT2AWHTR_Register is record
      --  Break signal assignment to analog watchdog high threshold event
      BKAWH        : DFSDM_FLT2AWHTR_BKAWH_Field := 16#0#;
      --  unspecified
      Reserved_4_7 : STM32H753.UInt4 := 16#0#;
      --  Analog watchdog high threshold
      AWHT         : DFSDM_FLT2AWHTR_AWHT_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT2AWHTR_Register use record
      BKAWH        at 0 range 0 .. 3;
      Reserved_4_7 at 0 range 4 .. 7;
      AWHT         at 0 range 8 .. 31;
   end record;

   subtype DFSDM_FLT2AWLTR_BKAWL_Field is STM32H753.UInt4;
   subtype DFSDM_FLT2AWLTR_AWLT_Field is STM32H753.UInt24;

   --  analog watchdog low threshold register
   type DFSDM_FLT2AWLTR_Register is record
      --  Break signal assignment to analog watchdog low threshold event
      BKAWL        : DFSDM_FLT2AWLTR_BKAWL_Field := 16#0#;
      --  unspecified
      Reserved_4_7 : STM32H753.UInt4 := 16#0#;
      --  Analog watchdog low threshold
      AWLT         : DFSDM_FLT2AWLTR_AWLT_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT2AWLTR_Register use record
      BKAWL        at 0 range 0 .. 3;
      Reserved_4_7 at 0 range 4 .. 7;
      AWLT         at 0 range 8 .. 31;
   end record;

   subtype DFSDM_FLT2AWSR_AWLTF_Field is STM32H753.Byte;
   subtype DFSDM_FLT2AWSR_AWHTF_Field is STM32H753.Byte;

   --  analog watchdog status register
   type DFSDM_FLT2AWSR_Register is record
      --  Read-only. Analog watchdog low threshold flag
      AWLTF          : DFSDM_FLT2AWSR_AWLTF_Field;
      --  Read-only. Analog watchdog high threshold flag
      AWHTF          : DFSDM_FLT2AWSR_AWHTF_Field;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT2AWSR_Register use record
      AWLTF          at 0 range 0 .. 7;
      AWHTF          at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DFSDM_FLT2AWCFR_CLRAWLTF_Field is STM32H753.Byte;
   subtype DFSDM_FLT2AWCFR_CLRAWHTF_Field is STM32H753.Byte;

   --  analog watchdog clear flag register
   type DFSDM_FLT2AWCFR_Register is record
      --  Clear the analog watchdog low threshold flag
      CLRAWLTF       : DFSDM_FLT2AWCFR_CLRAWLTF_Field := 16#0#;
      --  Clear the analog watchdog high threshold flag
      CLRAWHTF       : DFSDM_FLT2AWCFR_CLRAWHTF_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT2AWCFR_Register use record
      CLRAWLTF       at 0 range 0 .. 7;
      CLRAWHTF       at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DFSDM_FLT2EXMAX_EXMAXCH_Field is STM32H753.UInt3;
   subtype DFSDM_FLT2EXMAX_EXMAX_Field is STM32H753.UInt24;

   --  Extremes detector maximum register
   type DFSDM_FLT2EXMAX_Register is record
      --  Read-only. Extremes detector maximum data channel
      EXMAXCH      : DFSDM_FLT2EXMAX_EXMAXCH_Field;
      --  unspecified
      Reserved_3_7 : STM32H753.UInt5;
      --  Read-only. Extremes detector maximum value
      EXMAX        : DFSDM_FLT2EXMAX_EXMAX_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT2EXMAX_Register use record
      EXMAXCH      at 0 range 0 .. 2;
      Reserved_3_7 at 0 range 3 .. 7;
      EXMAX        at 0 range 8 .. 31;
   end record;

   subtype DFSDM_FLT2EXMIN_EXMINCH_Field is STM32H753.UInt3;
   subtype DFSDM_FLT2EXMIN_EXMIN_Field is STM32H753.UInt24;

   --  Extremes detector minimum register
   type DFSDM_FLT2EXMIN_Register is record
      --  Read-only. Extremes detector minimum data channel
      EXMINCH      : DFSDM_FLT2EXMIN_EXMINCH_Field;
      --  unspecified
      Reserved_3_7 : STM32H753.UInt5;
      --  Read-only. EXMIN
      EXMIN        : DFSDM_FLT2EXMIN_EXMIN_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT2EXMIN_Register use record
      EXMINCH      at 0 range 0 .. 2;
      Reserved_3_7 at 0 range 3 .. 7;
      EXMIN        at 0 range 8 .. 31;
   end record;

   subtype DFSDM_FLT2CNVTIMR_CNVCNT_Field is STM32H753.UInt28;

   --  conversion timer register
   type DFSDM_FLT2CNVTIMR_Register is record
      --  unspecified
      Reserved_0_3 : STM32H753.UInt4;
      --  Read-only. 28-bit timer counting conversion time t = CNVCNT[27:0] /
      --  fDFSDM_CKIN
      CNVCNT       : DFSDM_FLT2CNVTIMR_CNVCNT_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT2CNVTIMR_Register use record
      Reserved_0_3 at 0 range 0 .. 3;
      CNVCNT       at 0 range 4 .. 31;
   end record;

   subtype DFSDM_FLT3CR1_JEXTSEL_Field is STM32H753.UInt3;
   subtype DFSDM_FLT3CR1_JEXTEN_Field is STM32H753.UInt2;
   subtype DFSDM_FLT3CR1_RCH_Field is STM32H753.UInt3;

   --  control register 1
   type DFSDM_FLT3CR1_Register is record
      --  DFSDM enable
      DFEN           : Boolean := False;
      --  Start a conversion of the injected group of channels
      JSWSTART       : Boolean := False;
      --  unspecified
      Reserved_2_2   : STM32H753.Bit := 16#0#;
      --  Launch an injected conversion synchronously with the DFSDM0 JSWSTART
      --  trigger
      JSYNC          : Boolean := False;
      --  Scanning conversion mode for injected conversions
      JSCAN          : Boolean := False;
      --  DMA channel enabled to read data for the injected channel group
      JDMAEN         : Boolean := False;
      --  unspecified
      Reserved_6_7   : STM32H753.UInt2 := 16#0#;
      --  Trigger signal selection for launching injected conversions
      JEXTSEL        : DFSDM_FLT3CR1_JEXTSEL_Field := 16#0#;
      --  unspecified
      Reserved_11_12 : STM32H753.UInt2 := 16#0#;
      --  Trigger enable and trigger edge selection for injected conversions
      JEXTEN         : DFSDM_FLT3CR1_JEXTEN_Field := 16#0#;
      --  unspecified
      Reserved_15_16 : STM32H753.UInt2 := 16#0#;
      --  Software start of a conversion on the regular channel
      RSWSTART       : Boolean := False;
      --  Continuous mode selection for regular conversions
      RCONT          : Boolean := False;
      --  Launch regular conversion synchronously with DFSDM0
      RSYNC          : Boolean := False;
      --  unspecified
      Reserved_20_20 : STM32H753.Bit := 16#0#;
      --  DMA channel enabled to read data for the regular conversion
      RDMAEN         : Boolean := False;
      --  unspecified
      Reserved_22_23 : STM32H753.UInt2 := 16#0#;
      --  Regular channel selection
      RCH            : DFSDM_FLT3CR1_RCH_Field := 16#0#;
      --  unspecified
      Reserved_27_28 : STM32H753.UInt2 := 16#0#;
      --  Fast conversion mode selection for regular conversions
      FAST           : Boolean := False;
      --  Analog watchdog fast mode select
      AWFSEL         : Boolean := False;
      --  unspecified
      Reserved_31_31 : STM32H753.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT3CR1_Register use record
      DFEN           at 0 range 0 .. 0;
      JSWSTART       at 0 range 1 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      JSYNC          at 0 range 3 .. 3;
      JSCAN          at 0 range 4 .. 4;
      JDMAEN         at 0 range 5 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      JEXTSEL        at 0 range 8 .. 10;
      Reserved_11_12 at 0 range 11 .. 12;
      JEXTEN         at 0 range 13 .. 14;
      Reserved_15_16 at 0 range 15 .. 16;
      RSWSTART       at 0 range 17 .. 17;
      RCONT          at 0 range 18 .. 18;
      RSYNC          at 0 range 19 .. 19;
      Reserved_20_20 at 0 range 20 .. 20;
      RDMAEN         at 0 range 21 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      RCH            at 0 range 24 .. 26;
      Reserved_27_28 at 0 range 27 .. 28;
      FAST           at 0 range 29 .. 29;
      AWFSEL         at 0 range 30 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype DFSDM_FLT3CR2_EXCH_Field is STM32H753.Byte;
   subtype DFSDM_FLT3CR2_AWDCH_Field is STM32H753.Byte;

   --  control register 2
   type DFSDM_FLT3CR2_Register is record
      --  Injected end of conversion interrupt enable
      JEOCIE         : Boolean := False;
      --  Regular end of conversion interrupt enable
      REOCIE         : Boolean := False;
      --  Injected data overrun interrupt enable
      JOVRIE         : Boolean := False;
      --  Regular data overrun interrupt enable
      ROVRIE         : Boolean := False;
      --  Analog watchdog interrupt enable
      AWDIE          : Boolean := False;
      --  Short-circuit detector interrupt enable
      SCDIE          : Boolean := False;
      --  Clock absence interrupt enable
      CKABIE         : Boolean := False;
      --  unspecified
      Reserved_7_7   : STM32H753.Bit := 16#0#;
      --  Extremes detector channel selection
      EXCH           : DFSDM_FLT3CR2_EXCH_Field := 16#0#;
      --  Analog watchdog channel selection
      AWDCH          : DFSDM_FLT3CR2_AWDCH_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : STM32H753.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT3CR2_Register use record
      JEOCIE         at 0 range 0 .. 0;
      REOCIE         at 0 range 1 .. 1;
      JOVRIE         at 0 range 2 .. 2;
      ROVRIE         at 0 range 3 .. 3;
      AWDIE          at 0 range 4 .. 4;
      SCDIE          at 0 range 5 .. 5;
      CKABIE         at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      EXCH           at 0 range 8 .. 15;
      AWDCH          at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype DFSDM_FLT3ISR_CKABF_Field is STM32H753.Byte;
   subtype DFSDM_FLT3ISR_SCDF_Field is STM32H753.Byte;

   --  interrupt and status register
   type DFSDM_FLT3ISR_Register is record
      --  Read-only. End of injected conversion flag
      JEOCF          : Boolean;
      --  Read-only. End of regular conversion flag
      REOCF          : Boolean;
      --  Read-only. Injected conversion overrun flag
      JOVRF          : Boolean;
      --  Read-only. Regular conversion overrun flag
      ROVRF          : Boolean;
      --  Read-only. Analog watchdog
      AWDF           : Boolean;
      --  unspecified
      Reserved_5_12  : STM32H753.Byte;
      --  Read-only. Injected conversion in progress status
      JCIP           : Boolean;
      --  Read-only. Regular conversion in progress status
      RCIP           : Boolean;
      --  unspecified
      Reserved_15_15 : STM32H753.Bit;
      --  Read-only. Clock absence flag
      CKABF          : DFSDM_FLT3ISR_CKABF_Field;
      --  Read-only. short-circuit detector flag
      SCDF           : DFSDM_FLT3ISR_SCDF_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT3ISR_Register use record
      JEOCF          at 0 range 0 .. 0;
      REOCF          at 0 range 1 .. 1;
      JOVRF          at 0 range 2 .. 2;
      ROVRF          at 0 range 3 .. 3;
      AWDF           at 0 range 4 .. 4;
      Reserved_5_12  at 0 range 5 .. 12;
      JCIP           at 0 range 13 .. 13;
      RCIP           at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      CKABF          at 0 range 16 .. 23;
      SCDF           at 0 range 24 .. 31;
   end record;

   subtype DFSDM_FLT3ICR_CLRCKABF_Field is STM32H753.Byte;
   subtype DFSDM_FLT3ICR_CLRSCDF_Field is STM32H753.Byte;

   --  interrupt flag clear register
   type DFSDM_FLT3ICR_Register is record
      --  unspecified
      Reserved_0_1  : STM32H753.UInt2 := 16#0#;
      --  Clear the injected conversion overrun flag
      CLRJOVRF      : Boolean := False;
      --  Clear the regular conversion overrun flag
      CLRROVRF      : Boolean := False;
      --  unspecified
      Reserved_4_15 : STM32H753.UInt12 := 16#0#;
      --  Clear the clock absence flag
      CLRCKABF      : DFSDM_FLT3ICR_CLRCKABF_Field := 16#0#;
      --  Clear the short-circuit detector flag
      CLRSCDF       : DFSDM_FLT3ICR_CLRSCDF_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT3ICR_Register use record
      Reserved_0_1  at 0 range 0 .. 1;
      CLRJOVRF      at 0 range 2 .. 2;
      CLRROVRF      at 0 range 3 .. 3;
      Reserved_4_15 at 0 range 4 .. 15;
      CLRCKABF      at 0 range 16 .. 23;
      CLRSCDF       at 0 range 24 .. 31;
   end record;

   subtype DFSDM_FLT3JCHGR_JCHG_Field is STM32H753.Byte;

   --  injected channel group selection register
   type DFSDM_FLT3JCHGR_Register is record
      --  Injected channel group selection
      JCHG          : DFSDM_FLT3JCHGR_JCHG_Field := 16#1#;
      --  unspecified
      Reserved_8_31 : STM32H753.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT3JCHGR_Register use record
      JCHG          at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype DFSDM_FLT3FCR_IOSR_Field is STM32H753.Byte;
   subtype DFSDM_FLT3FCR_FOSR_Field is STM32H753.UInt10;
   subtype DFSDM_FLT3FCR_FORD_Field is STM32H753.UInt3;

   --  filter control register
   type DFSDM_FLT3FCR_Register is record
      --  Integrator oversampling ratio (averaging length)
      IOSR           : DFSDM_FLT3FCR_IOSR_Field := 16#0#;
      --  unspecified
      Reserved_8_15  : STM32H753.Byte := 16#0#;
      --  Sinc filter oversampling ratio (decimation rate)
      FOSR           : DFSDM_FLT3FCR_FOSR_Field := 16#0#;
      --  unspecified
      Reserved_26_28 : STM32H753.UInt3 := 16#0#;
      --  Sinc filter order
      FORD           : DFSDM_FLT3FCR_FORD_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT3FCR_Register use record
      IOSR           at 0 range 0 .. 7;
      Reserved_8_15  at 0 range 8 .. 15;
      FOSR           at 0 range 16 .. 25;
      Reserved_26_28 at 0 range 26 .. 28;
      FORD           at 0 range 29 .. 31;
   end record;

   subtype DFSDM_FLT3JDATAR_JDATACH_Field is STM32H753.UInt3;
   subtype DFSDM_FLT3JDATAR_JDATA_Field is STM32H753.UInt24;

   --  data register for injected group
   type DFSDM_FLT3JDATAR_Register is record
      --  Read-only. Injected channel most recently converted
      JDATACH      : DFSDM_FLT3JDATAR_JDATACH_Field;
      --  unspecified
      Reserved_3_7 : STM32H753.UInt5;
      --  Read-only. Injected group conversion data
      JDATA        : DFSDM_FLT3JDATAR_JDATA_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT3JDATAR_Register use record
      JDATACH      at 0 range 0 .. 2;
      Reserved_3_7 at 0 range 3 .. 7;
      JDATA        at 0 range 8 .. 31;
   end record;

   subtype DFSDM_FLT3RDATAR_RDATACH_Field is STM32H753.UInt3;
   subtype DFSDM_FLT3RDATAR_RDATA_Field is STM32H753.UInt24;

   --  data register for the regular channel
   type DFSDM_FLT3RDATAR_Register is record
      --  Read-only. Regular channel most recently converted
      RDATACH      : DFSDM_FLT3RDATAR_RDATACH_Field;
      --  unspecified
      Reserved_3_3 : STM32H753.Bit;
      --  Read-only. Regular channel pending data
      RPEND        : Boolean;
      --  unspecified
      Reserved_5_7 : STM32H753.UInt3;
      --  Read-only. Regular channel conversion data
      RDATA        : DFSDM_FLT3RDATAR_RDATA_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT3RDATAR_Register use record
      RDATACH      at 0 range 0 .. 2;
      Reserved_3_3 at 0 range 3 .. 3;
      RPEND        at 0 range 4 .. 4;
      Reserved_5_7 at 0 range 5 .. 7;
      RDATA        at 0 range 8 .. 31;
   end record;

   subtype DFSDM_FLT3AWHTR_BKAWH_Field is STM32H753.UInt4;
   subtype DFSDM_FLT3AWHTR_AWHT_Field is STM32H753.UInt24;

   --  analog watchdog high threshold register
   type DFSDM_FLT3AWHTR_Register is record
      --  Break signal assignment to analog watchdog high threshold event
      BKAWH        : DFSDM_FLT3AWHTR_BKAWH_Field := 16#0#;
      --  unspecified
      Reserved_4_7 : STM32H753.UInt4 := 16#0#;
      --  Analog watchdog high threshold
      AWHT         : DFSDM_FLT3AWHTR_AWHT_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT3AWHTR_Register use record
      BKAWH        at 0 range 0 .. 3;
      Reserved_4_7 at 0 range 4 .. 7;
      AWHT         at 0 range 8 .. 31;
   end record;

   subtype DFSDM_FLT3AWLTR_BKAWL_Field is STM32H753.UInt4;
   subtype DFSDM_FLT3AWLTR_AWLT_Field is STM32H753.UInt24;

   --  analog watchdog low threshold register
   type DFSDM_FLT3AWLTR_Register is record
      --  Break signal assignment to analog watchdog low threshold event
      BKAWL        : DFSDM_FLT3AWLTR_BKAWL_Field := 16#0#;
      --  unspecified
      Reserved_4_7 : STM32H753.UInt4 := 16#0#;
      --  Analog watchdog low threshold
      AWLT         : DFSDM_FLT3AWLTR_AWLT_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT3AWLTR_Register use record
      BKAWL        at 0 range 0 .. 3;
      Reserved_4_7 at 0 range 4 .. 7;
      AWLT         at 0 range 8 .. 31;
   end record;

   subtype DFSDM_FLT3AWSR_AWLTF_Field is STM32H753.Byte;
   subtype DFSDM_FLT3AWSR_AWHTF_Field is STM32H753.Byte;

   --  analog watchdog status register
   type DFSDM_FLT3AWSR_Register is record
      --  Read-only. Analog watchdog low threshold flag
      AWLTF          : DFSDM_FLT3AWSR_AWLTF_Field;
      --  Read-only. Analog watchdog high threshold flag
      AWHTF          : DFSDM_FLT3AWSR_AWHTF_Field;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT3AWSR_Register use record
      AWLTF          at 0 range 0 .. 7;
      AWHTF          at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DFSDM_FLT3AWCFR_CLRAWLTF_Field is STM32H753.Byte;
   subtype DFSDM_FLT3AWCFR_CLRAWHTF_Field is STM32H753.Byte;

   --  analog watchdog clear flag register
   type DFSDM_FLT3AWCFR_Register is record
      --  Clear the analog watchdog low threshold flag
      CLRAWLTF       : DFSDM_FLT3AWCFR_CLRAWLTF_Field := 16#0#;
      --  Clear the analog watchdog high threshold flag
      CLRAWHTF       : DFSDM_FLT3AWCFR_CLRAWHTF_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT3AWCFR_Register use record
      CLRAWLTF       at 0 range 0 .. 7;
      CLRAWHTF       at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DFSDM_FLT3EXMAX_EXMAXCH_Field is STM32H753.UInt3;
   subtype DFSDM_FLT3EXMAX_EXMAX_Field is STM32H753.UInt24;

   --  Extremes detector maximum register
   type DFSDM_FLT3EXMAX_Register is record
      --  Read-only. Extremes detector maximum data channel
      EXMAXCH      : DFSDM_FLT3EXMAX_EXMAXCH_Field;
      --  unspecified
      Reserved_3_7 : STM32H753.UInt5;
      --  Read-only. Extremes detector maximum value
      EXMAX        : DFSDM_FLT3EXMAX_EXMAX_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT3EXMAX_Register use record
      EXMAXCH      at 0 range 0 .. 2;
      Reserved_3_7 at 0 range 3 .. 7;
      EXMAX        at 0 range 8 .. 31;
   end record;

   subtype DFSDM_FLT3EXMIN_EXMINCH_Field is STM32H753.UInt3;
   subtype DFSDM_FLT3EXMIN_EXMIN_Field is STM32H753.UInt24;

   --  Extremes detector minimum register
   type DFSDM_FLT3EXMIN_Register is record
      --  Read-only. Extremes detector minimum data channel
      EXMINCH      : DFSDM_FLT3EXMIN_EXMINCH_Field;
      --  unspecified
      Reserved_3_7 : STM32H753.UInt5;
      --  Read-only. EXMIN
      EXMIN        : DFSDM_FLT3EXMIN_EXMIN_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT3EXMIN_Register use record
      EXMINCH      at 0 range 0 .. 2;
      Reserved_3_7 at 0 range 3 .. 7;
      EXMIN        at 0 range 8 .. 31;
   end record;

   subtype DFSDM_FLT3CNVTIMR_CNVCNT_Field is STM32H753.UInt28;

   --  conversion timer register
   type DFSDM_FLT3CNVTIMR_Register is record
      --  unspecified
      Reserved_0_3 : STM32H753.UInt4;
      --  Read-only. 28-bit timer counting conversion time t = CNVCNT[27:0] /
      --  fDFSDM_CKIN
      CNVCNT       : DFSDM_FLT3CNVTIMR_CNVCNT_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DFSDM_FLT3CNVTIMR_Register use record
      Reserved_0_3 at 0 range 0 .. 3;
      CNVCNT       at 0 range 4 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Digital filter for sigma delta modulators
   type DFSDM_Peripheral is record
      --  channel configuration y register
      CH0CFGR1          : aliased CH0CFGR1_Register;
      --  channel configuration y register
      CH0CFGR2          : aliased CH0CFGR2_Register;
      --  analog watchdog and short-circuit detector register
      CH0AWSCDR         : aliased CH0AWSCDR_Register;
      --  channel watchdog filter data register
      CH0WDATR          : aliased CH0WDATR_Register;
      --  channel data input register
      CH0DATINR         : aliased CH0DATINR_Register;
      --  channel y delay register
      CH0DLYR           : aliased CH0DLYR_Register;
      --  CH1CFGR1
      CH1CFGR1          : aliased CH1CFGR1_Register;
      --  CH1CFGR2
      CH1CFGR2          : aliased CH1CFGR2_Register;
      --  CH1AWSCDR
      CH1AWSCDR         : aliased CH1AWSCDR_Register;
      --  CH1WDATR
      CH1WDATR          : aliased CH1WDATR_Register;
      --  CH1DATINR
      CH1DATINR         : aliased CH1DATINR_Register;
      --  channel y delay register
      CH1DLYR           : aliased CH1DLYR_Register;
      --  CH2CFGR1
      CH2CFGR1          : aliased CH2CFGR1_Register;
      --  CH2CFGR2
      CH2CFGR2          : aliased CH2CFGR2_Register;
      --  CH2AWSCDR
      CH2AWSCDR         : aliased CH2AWSCDR_Register;
      --  CH2WDATR
      CH2WDATR          : aliased CH2WDATR_Register;
      --  CH2DATINR
      CH2DATINR         : aliased CH2DATINR_Register;
      --  channel y delay register
      CH2DLYR           : aliased CH2DLYR_Register;
      --  CH3CFGR1
      CH3CFGR1          : aliased CH3CFGR1_Register;
      --  CH3CFGR2
      CH3CFGR2          : aliased CH3CFGR2_Register;
      --  CH3AWSCDR
      CH3AWSCDR         : aliased CH3AWSCDR_Register;
      --  CH3WDATR
      CH3WDATR          : aliased CH3WDATR_Register;
      --  CH3DATINR
      CH3DATINR         : aliased CH3DATINR_Register;
      --  channel y delay register
      CH3DLYR           : aliased CH3DLYR_Register;
      --  CH4CFGR1
      CH4CFGR1          : aliased CH4CFGR1_Register;
      --  CH4CFGR2
      CH4CFGR2          : aliased CH4CFGR2_Register;
      --  CH4AWSCDR
      CH4AWSCDR         : aliased CH4AWSCDR_Register;
      --  CH4WDATR
      CH4WDATR          : aliased CH4WDATR_Register;
      --  CH4DATINR
      CH4DATINR         : aliased CH4DATINR_Register;
      --  channel y delay register
      CH4DLYR           : aliased CH4DLYR_Register;
      --  CH5CFGR1
      CH5CFGR1          : aliased CH5CFGR1_Register;
      --  CH5CFGR2
      CH5CFGR2          : aliased CH5CFGR2_Register;
      --  CH5AWSCDR
      CH5AWSCDR         : aliased CH5AWSCDR_Register;
      --  CH5WDATR
      CH5WDATR          : aliased CH5WDATR_Register;
      --  CH5DATINR
      CH5DATINR         : aliased CH5DATINR_Register;
      --  channel y delay register
      CH5DLYR           : aliased CH5DLYR_Register;
      --  CH6CFGR1
      CH6CFGR1          : aliased CH6CFGR1_Register;
      --  CH6CFGR2
      CH6CFGR2          : aliased CH6CFGR2_Register;
      --  CH6AWSCDR
      CH6AWSCDR         : aliased CH6AWSCDR_Register;
      --  CH6WDATR
      CH6WDATR          : aliased CH6WDATR_Register;
      --  CH6DATINR
      CH6DATINR         : aliased CH6DATINR_Register;
      --  channel y delay register
      CH6DLYR           : aliased CH6DLYR_Register;
      --  CH7CFGR1
      CH7CFGR1          : aliased CH7CFGR1_Register;
      --  CH7CFGR2
      CH7CFGR2          : aliased CH7CFGR2_Register;
      --  CH7AWSCDR
      CH7AWSCDR         : aliased CH7AWSCDR_Register;
      --  CH7WDATR
      CH7WDATR          : aliased CH7WDATR_Register;
      --  CH7DATINR
      CH7DATINR         : aliased CH7DATINR_Register;
      --  channel y delay register
      CH7DLYR           : aliased CH7DLYR_Register;
      --  control register 1
      DFSDM_FLT0CR1     : aliased DFSDM_FLT0CR1_Register;
      --  control register 2
      DFSDM_FLT0CR2     : aliased DFSDM_FLT0CR2_Register;
      --  interrupt and status register
      DFSDM_FLT0ISR     : aliased DFSDM_FLT0ISR_Register;
      --  interrupt flag clear register
      DFSDM_FLT0ICR     : aliased DFSDM_FLT0ICR_Register;
      --  injected channel group selection register
      DFSDM_FLT0JCHGR   : aliased DFSDM_FLT0JCHGR_Register;
      --  filter control register
      DFSDM_FLT0FCR     : aliased DFSDM_FLT0FCR_Register;
      --  data register for injected group
      DFSDM_FLT0JDATAR  : aliased DFSDM_FLT0JDATAR_Register;
      --  data register for the regular channel
      DFSDM_FLT0RDATAR  : aliased DFSDM_FLT0RDATAR_Register;
      --  analog watchdog high threshold register
      DFSDM_FLT0AWHTR   : aliased DFSDM_FLT0AWHTR_Register;
      --  analog watchdog low threshold register
      DFSDM_FLT0AWLTR   : aliased DFSDM_FLT0AWLTR_Register;
      --  analog watchdog status register
      DFSDM_FLT0AWSR    : aliased DFSDM_FLT0AWSR_Register;
      --  analog watchdog clear flag register
      DFSDM_FLT0AWCFR   : aliased DFSDM_FLT0AWCFR_Register;
      --  Extremes detector maximum register
      DFSDM_FLT0EXMAX   : aliased DFSDM_FLT0EXMAX_Register;
      --  Extremes detector minimum register
      DFSDM_FLT0EXMIN   : aliased DFSDM_FLT0EXMIN_Register;
      --  conversion timer register
      DFSDM_FLT0CNVTIMR : aliased DFSDM_FLT0CNVTIMR_Register;
      --  control register 1
      DFSDM_FLT1CR1     : aliased DFSDM_FLT1CR1_Register;
      --  control register 2
      DFSDM_FLT1CR2     : aliased DFSDM_FLT1CR2_Register;
      --  interrupt and status register
      DFSDM_FLT1ISR     : aliased DFSDM_FLT1ISR_Register;
      --  interrupt flag clear register
      DFSDM_FLT1ICR     : aliased DFSDM_FLT1ICR_Register;
      --  injected channel group selection register
      DFSDM_FLT1CHGR    : aliased DFSDM_FLT1CHGR_Register;
      --  filter control register
      DFSDM_FLT1FCR     : aliased DFSDM_FLT1FCR_Register;
      --  data register for injected group
      DFSDM_FLT1JDATAR  : aliased DFSDM_FLT1JDATAR_Register;
      --  data register for the regular channel
      DFSDM_FLT1RDATAR  : aliased DFSDM_FLT1RDATAR_Register;
      --  analog watchdog high threshold register
      DFSDM_FLT1AWHTR   : aliased DFSDM_FLT1AWHTR_Register;
      --  analog watchdog low threshold register
      DFSDM_FLT1AWLTR   : aliased DFSDM_FLT1AWLTR_Register;
      --  analog watchdog status register
      DFSDM_FLT1AWSR    : aliased DFSDM_FLT1AWSR_Register;
      --  analog watchdog clear flag register
      DFSDM_FLT1AWCFR   : aliased DFSDM_FLT1AWCFR_Register;
      --  Extremes detector maximum register
      DFSDM_FLT1EXMAX   : aliased DFSDM_FLT1EXMAX_Register;
      --  Extremes detector minimum register
      DFSDM_FLT1EXMIN   : aliased DFSDM_FLT1EXMIN_Register;
      --  conversion timer register
      DFSDM_FLT1CNVTIMR : aliased DFSDM_FLT1CNVTIMR_Register;
      --  control register 1
      DFSDM_FLT2CR1     : aliased DFSDM_FLT2CR1_Register;
      --  control register 2
      DFSDM_FLT2CR2     : aliased DFSDM_FLT2CR2_Register;
      --  interrupt and status register
      DFSDM_FLT2ISR     : aliased DFSDM_FLT2ISR_Register;
      --  interrupt flag clear register
      DFSDM_FLT2ICR     : aliased DFSDM_FLT2ICR_Register;
      --  injected channel group selection register
      DFSDM_FLT2JCHGR   : aliased DFSDM_FLT2JCHGR_Register;
      --  filter control register
      DFSDM_FLT2FCR     : aliased DFSDM_FLT2FCR_Register;
      --  data register for injected group
      DFSDM_FLT2JDATAR  : aliased DFSDM_FLT2JDATAR_Register;
      --  data register for the regular channel
      DFSDM_FLT2RDATAR  : aliased DFSDM_FLT2RDATAR_Register;
      --  analog watchdog high threshold register
      DFSDM_FLT2AWHTR   : aliased DFSDM_FLT2AWHTR_Register;
      --  analog watchdog low threshold register
      DFSDM_FLT2AWLTR   : aliased DFSDM_FLT2AWLTR_Register;
      --  analog watchdog status register
      DFSDM_FLT2AWSR    : aliased DFSDM_FLT2AWSR_Register;
      --  analog watchdog clear flag register
      DFSDM_FLT2AWCFR   : aliased DFSDM_FLT2AWCFR_Register;
      --  Extremes detector maximum register
      DFSDM_FLT2EXMAX   : aliased DFSDM_FLT2EXMAX_Register;
      --  Extremes detector minimum register
      DFSDM_FLT2EXMIN   : aliased DFSDM_FLT2EXMIN_Register;
      --  conversion timer register
      DFSDM_FLT2CNVTIMR : aliased DFSDM_FLT2CNVTIMR_Register;
      --  control register 1
      DFSDM_FLT3CR1     : aliased DFSDM_FLT3CR1_Register;
      --  control register 2
      DFSDM_FLT3CR2     : aliased DFSDM_FLT3CR2_Register;
      --  interrupt and status register
      DFSDM_FLT3ISR     : aliased DFSDM_FLT3ISR_Register;
      --  interrupt flag clear register
      DFSDM_FLT3ICR     : aliased DFSDM_FLT3ICR_Register;
      --  injected channel group selection register
      DFSDM_FLT3JCHGR   : aliased DFSDM_FLT3JCHGR_Register;
      --  filter control register
      DFSDM_FLT3FCR     : aliased DFSDM_FLT3FCR_Register;
      --  data register for injected group
      DFSDM_FLT3JDATAR  : aliased DFSDM_FLT3JDATAR_Register;
      --  data register for the regular channel
      DFSDM_FLT3RDATAR  : aliased DFSDM_FLT3RDATAR_Register;
      --  analog watchdog high threshold register
      DFSDM_FLT3AWHTR   : aliased DFSDM_FLT3AWHTR_Register;
      --  analog watchdog low threshold register
      DFSDM_FLT3AWLTR   : aliased DFSDM_FLT3AWLTR_Register;
      --  analog watchdog status register
      DFSDM_FLT3AWSR    : aliased DFSDM_FLT3AWSR_Register;
      --  analog watchdog clear flag register
      DFSDM_FLT3AWCFR   : aliased DFSDM_FLT3AWCFR_Register;
      --  Extremes detector maximum register
      DFSDM_FLT3EXMAX   : aliased DFSDM_FLT3EXMAX_Register;
      --  Extremes detector minimum register
      DFSDM_FLT3EXMIN   : aliased DFSDM_FLT3EXMIN_Register;
      --  conversion timer register
      DFSDM_FLT3CNVTIMR : aliased DFSDM_FLT3CNVTIMR_Register;
   end record
     with Volatile;

   for DFSDM_Peripheral use record
      CH0CFGR1          at 16#0# range 0 .. 31;
      CH0CFGR2          at 16#4# range 0 .. 31;
      CH0AWSCDR         at 16#8# range 0 .. 31;
      CH0WDATR          at 16#C# range 0 .. 31;
      CH0DATINR         at 16#10# range 0 .. 31;
      CH0DLYR           at 16#14# range 0 .. 31;
      CH1CFGR1          at 16#20# range 0 .. 31;
      CH1CFGR2          at 16#24# range 0 .. 31;
      CH1AWSCDR         at 16#28# range 0 .. 31;
      CH1WDATR          at 16#2C# range 0 .. 31;
      CH1DATINR         at 16#30# range 0 .. 31;
      CH1DLYR           at 16#34# range 0 .. 31;
      CH2CFGR1          at 16#40# range 0 .. 31;
      CH2CFGR2          at 16#44# range 0 .. 31;
      CH2AWSCDR         at 16#48# range 0 .. 31;
      CH2WDATR          at 16#4C# range 0 .. 31;
      CH2DATINR         at 16#50# range 0 .. 31;
      CH2DLYR           at 16#54# range 0 .. 31;
      CH3CFGR1          at 16#60# range 0 .. 31;
      CH3CFGR2          at 16#64# range 0 .. 31;
      CH3AWSCDR         at 16#68# range 0 .. 31;
      CH3WDATR          at 16#6C# range 0 .. 31;
      CH3DATINR         at 16#70# range 0 .. 31;
      CH3DLYR           at 16#74# range 0 .. 31;
      CH4CFGR1          at 16#80# range 0 .. 31;
      CH4CFGR2          at 16#84# range 0 .. 31;
      CH4AWSCDR         at 16#88# range 0 .. 31;
      CH4WDATR          at 16#8C# range 0 .. 31;
      CH4DATINR         at 16#90# range 0 .. 31;
      CH4DLYR           at 16#94# range 0 .. 31;
      CH5CFGR1          at 16#A0# range 0 .. 31;
      CH5CFGR2          at 16#A4# range 0 .. 31;
      CH5AWSCDR         at 16#A8# range 0 .. 31;
      CH5WDATR          at 16#AC# range 0 .. 31;
      CH5DATINR         at 16#B0# range 0 .. 31;
      CH5DLYR           at 16#B4# range 0 .. 31;
      CH6CFGR1          at 16#C0# range 0 .. 31;
      CH6CFGR2          at 16#C4# range 0 .. 31;
      CH6AWSCDR         at 16#C8# range 0 .. 31;
      CH6WDATR          at 16#CC# range 0 .. 31;
      CH6DATINR         at 16#D0# range 0 .. 31;
      CH6DLYR           at 16#D4# range 0 .. 31;
      CH7CFGR1          at 16#E0# range 0 .. 31;
      CH7CFGR2          at 16#E4# range 0 .. 31;
      CH7AWSCDR         at 16#E8# range 0 .. 31;
      CH7WDATR          at 16#EC# range 0 .. 31;
      CH7DATINR         at 16#F0# range 0 .. 31;
      CH7DLYR           at 16#F4# range 0 .. 31;
      DFSDM_FLT0CR1     at 16#100# range 0 .. 31;
      DFSDM_FLT0CR2     at 16#104# range 0 .. 31;
      DFSDM_FLT0ISR     at 16#108# range 0 .. 31;
      DFSDM_FLT0ICR     at 16#10C# range 0 .. 31;
      DFSDM_FLT0JCHGR   at 16#110# range 0 .. 31;
      DFSDM_FLT0FCR     at 16#114# range 0 .. 31;
      DFSDM_FLT0JDATAR  at 16#118# range 0 .. 31;
      DFSDM_FLT0RDATAR  at 16#11C# range 0 .. 31;
      DFSDM_FLT0AWHTR   at 16#120# range 0 .. 31;
      DFSDM_FLT0AWLTR   at 16#124# range 0 .. 31;
      DFSDM_FLT0AWSR    at 16#128# range 0 .. 31;
      DFSDM_FLT0AWCFR   at 16#12C# range 0 .. 31;
      DFSDM_FLT0EXMAX   at 16#130# range 0 .. 31;
      DFSDM_FLT0EXMIN   at 16#134# range 0 .. 31;
      DFSDM_FLT0CNVTIMR at 16#138# range 0 .. 31;
      DFSDM_FLT1CR1     at 16#180# range 0 .. 31;
      DFSDM_FLT1CR2     at 16#184# range 0 .. 31;
      DFSDM_FLT1ISR     at 16#188# range 0 .. 31;
      DFSDM_FLT1ICR     at 16#18C# range 0 .. 31;
      DFSDM_FLT1CHGR    at 16#190# range 0 .. 31;
      DFSDM_FLT1FCR     at 16#194# range 0 .. 31;
      DFSDM_FLT1JDATAR  at 16#198# range 0 .. 31;
      DFSDM_FLT1RDATAR  at 16#19C# range 0 .. 31;
      DFSDM_FLT1AWHTR   at 16#1A0# range 0 .. 31;
      DFSDM_FLT1AWLTR   at 16#1A4# range 0 .. 31;
      DFSDM_FLT1AWSR    at 16#1A8# range 0 .. 31;
      DFSDM_FLT1AWCFR   at 16#1AC# range 0 .. 31;
      DFSDM_FLT1EXMAX   at 16#1B0# range 0 .. 31;
      DFSDM_FLT1EXMIN   at 16#1B4# range 0 .. 31;
      DFSDM_FLT1CNVTIMR at 16#1B8# range 0 .. 31;
      DFSDM_FLT2CR1     at 16#200# range 0 .. 31;
      DFSDM_FLT2CR2     at 16#204# range 0 .. 31;
      DFSDM_FLT2ISR     at 16#208# range 0 .. 31;
      DFSDM_FLT2ICR     at 16#20C# range 0 .. 31;
      DFSDM_FLT2JCHGR   at 16#210# range 0 .. 31;
      DFSDM_FLT2FCR     at 16#214# range 0 .. 31;
      DFSDM_FLT2JDATAR  at 16#218# range 0 .. 31;
      DFSDM_FLT2RDATAR  at 16#21C# range 0 .. 31;
      DFSDM_FLT2AWHTR   at 16#220# range 0 .. 31;
      DFSDM_FLT2AWLTR   at 16#224# range 0 .. 31;
      DFSDM_FLT2AWSR    at 16#228# range 0 .. 31;
      DFSDM_FLT2AWCFR   at 16#22C# range 0 .. 31;
      DFSDM_FLT2EXMAX   at 16#230# range 0 .. 31;
      DFSDM_FLT2EXMIN   at 16#234# range 0 .. 31;
      DFSDM_FLT2CNVTIMR at 16#238# range 0 .. 31;
      DFSDM_FLT3CR1     at 16#280# range 0 .. 31;
      DFSDM_FLT3CR2     at 16#284# range 0 .. 31;
      DFSDM_FLT3ISR     at 16#288# range 0 .. 31;
      DFSDM_FLT3ICR     at 16#28C# range 0 .. 31;
      DFSDM_FLT3JCHGR   at 16#290# range 0 .. 31;
      DFSDM_FLT3FCR     at 16#294# range 0 .. 31;
      DFSDM_FLT3JDATAR  at 16#298# range 0 .. 31;
      DFSDM_FLT3RDATAR  at 16#29C# range 0 .. 31;
      DFSDM_FLT3AWHTR   at 16#2A0# range 0 .. 31;
      DFSDM_FLT3AWLTR   at 16#2A4# range 0 .. 31;
      DFSDM_FLT3AWSR    at 16#2A8# range 0 .. 31;
      DFSDM_FLT3AWCFR   at 16#2AC# range 0 .. 31;
      DFSDM_FLT3EXMAX   at 16#2B0# range 0 .. 31;
      DFSDM_FLT3EXMIN   at 16#2B4# range 0 .. 31;
      DFSDM_FLT3CNVTIMR at 16#2B8# range 0 .. 31;
   end record;

   --  Digital filter for sigma delta modulators
   DFSDM_Periph : aliased DFSDM_Peripheral
     with Import, Address => DFSDM_Base;

end STM32H753.DFSDM;
