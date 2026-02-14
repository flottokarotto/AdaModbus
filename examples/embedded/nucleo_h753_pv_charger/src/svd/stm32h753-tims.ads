pragma Style_Checks (Off);

--  This spec has been automatically generated from STM32H753.svd


with System;

package STM32H753.TIMs is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   subtype CR1_CKD_Field is STM32H753.UInt2;

   --  control register 1
   type CR1_Register is record
      --  Counter enable
      CEN            : Boolean := False;
      --  Update disable
      UDIS           : Boolean := False;
      --  Update request source
      URS            : Boolean := False;
      --  One-pulse mode
      OPM            : Boolean := False;
      --  unspecified
      Reserved_4_6   : STM32H753.UInt3 := 16#0#;
      --  Auto-reload preload enable
      ARPE           : Boolean := False;
      --  Clock division
      CKD            : CR1_CKD_Field := 16#0#;
      --  unspecified
      Reserved_10_10 : STM32H753.Bit := 16#0#;
      --  UIF status bit remapping
      UIFREMAP       : Boolean := False;
      --  unspecified
      Reserved_12_31 : STM32H753.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR1_Register use record
      CEN            at 0 range 0 .. 0;
      UDIS           at 0 range 1 .. 1;
      URS            at 0 range 2 .. 2;
      OPM            at 0 range 3 .. 3;
      Reserved_4_6   at 0 range 4 .. 6;
      ARPE           at 0 range 7 .. 7;
      CKD            at 0 range 8 .. 9;
      Reserved_10_10 at 0 range 10 .. 10;
      UIFREMAP       at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype CR2_MMS_Field is STM32H753.UInt3;

   --  control register 2
   type CR2_Register is record
      --  Capture/compare preloaded control
      CCPC           : Boolean := False;
      --  unspecified
      Reserved_1_1   : STM32H753.Bit := 16#0#;
      --  Capture/compare control update selection
      CCUS           : Boolean := False;
      --  Capture/compare DMA selection
      CCDS           : Boolean := False;
      --  Master mode selection
      MMS            : CR2_MMS_Field := 16#0#;
      --  TI1 selection
      TI1S           : Boolean := False;
      --  Output Idle state 1
      OIS1           : Boolean := False;
      --  Output Idle state 1
      OIS1N          : Boolean := False;
      --  Output Idle state 2
      OIS2           : Boolean := False;
      --  unspecified
      Reserved_11_31 : STM32H753.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR2_Register use record
      CCPC           at 0 range 0 .. 0;
      Reserved_1_1   at 0 range 1 .. 1;
      CCUS           at 0 range 2 .. 2;
      CCDS           at 0 range 3 .. 3;
      MMS            at 0 range 4 .. 6;
      TI1S           at 0 range 7 .. 7;
      OIS1           at 0 range 8 .. 8;
      OIS1N          at 0 range 9 .. 9;
      OIS2           at 0 range 10 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   subtype SMCR_SMS_Field is STM32H753.UInt3;
   subtype SMCR_TS_2_0_Field is STM32H753.UInt3;
   subtype SMCR_TS_4_3_Field is STM32H753.UInt2;

   --  slave mode control register
   type SMCR_Register is record
      --  Slave mode selection
      SMS            : SMCR_SMS_Field := 16#0#;
      --  unspecified
      Reserved_3_3   : STM32H753.Bit := 16#0#;
      --  Trigger selection
      TS_2_0         : SMCR_TS_2_0_Field := 16#0#;
      --  Master/Slave mode
      MSM            : Boolean := False;
      --  unspecified
      Reserved_8_15  : STM32H753.Byte := 16#0#;
      --  Slave mode selection bit 3
      SMS_3          : Boolean := False;
      --  unspecified
      Reserved_17_19 : STM32H753.UInt3 := 16#0#;
      --  Trigger selection - bit 4:3
      TS_4_3         : SMCR_TS_4_3_Field := 16#0#;
      --  unspecified
      Reserved_22_31 : STM32H753.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SMCR_Register use record
      SMS            at 0 range 0 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      TS_2_0         at 0 range 4 .. 6;
      MSM            at 0 range 7 .. 7;
      Reserved_8_15  at 0 range 8 .. 15;
      SMS_3          at 0 range 16 .. 16;
      Reserved_17_19 at 0 range 17 .. 19;
      TS_4_3         at 0 range 20 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  DMA/Interrupt enable register
   type DIER_Register is record
      --  Update interrupt enable
      UIE            : Boolean := False;
      --  Capture/Compare 1 interrupt enable
      CC1IE          : Boolean := False;
      --  Capture/Compare 2 interrupt enable
      CC2IE          : Boolean := False;
      --  unspecified
      Reserved_3_4   : STM32H753.UInt2 := 16#0#;
      --  COM interrupt enable
      COMIE          : Boolean := False;
      --  Trigger interrupt enable
      TIE            : Boolean := False;
      --  Break interrupt enable
      BIE            : Boolean := False;
      --  Update DMA request enable
      UDE            : Boolean := False;
      --  Capture/Compare 1 DMA request enable
      CC1DE          : Boolean := False;
      --  Capture/Compare 2 DMA request enable
      CC2DE          : Boolean := False;
      --  unspecified
      Reserved_11_12 : STM32H753.UInt2 := 16#0#;
      --  COM DMA request enable
      COMDE          : Boolean := False;
      --  Trigger DMA request enable
      TDE            : Boolean := False;
      --  unspecified
      Reserved_15_31 : STM32H753.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DIER_Register use record
      UIE            at 0 range 0 .. 0;
      CC1IE          at 0 range 1 .. 1;
      CC2IE          at 0 range 2 .. 2;
      Reserved_3_4   at 0 range 3 .. 4;
      COMIE          at 0 range 5 .. 5;
      TIE            at 0 range 6 .. 6;
      BIE            at 0 range 7 .. 7;
      UDE            at 0 range 8 .. 8;
      CC1DE          at 0 range 9 .. 9;
      CC2DE          at 0 range 10 .. 10;
      Reserved_11_12 at 0 range 11 .. 12;
      COMDE          at 0 range 13 .. 13;
      TDE            at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   --  status register
   type SR_Register is record
      --  Update interrupt flag
      UIF            : Boolean := False;
      --  Capture/compare 1 interrupt flag
      CC1IF          : Boolean := False;
      --  Capture/Compare 2 interrupt flag
      CC2IF          : Boolean := False;
      --  unspecified
      Reserved_3_4   : STM32H753.UInt2 := 16#0#;
      --  COM interrupt flag
      COMIF          : Boolean := False;
      --  Trigger interrupt flag
      TIF            : Boolean := False;
      --  Break interrupt flag
      BIF            : Boolean := False;
      --  unspecified
      Reserved_8_8   : STM32H753.Bit := 16#0#;
      --  Capture/Compare 1 overcapture flag
      CC1OF          : Boolean := False;
      --  Capture/compare 2 overcapture flag
      CC2OF          : Boolean := False;
      --  unspecified
      Reserved_11_31 : STM32H753.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register use record
      UIF            at 0 range 0 .. 0;
      CC1IF          at 0 range 1 .. 1;
      CC2IF          at 0 range 2 .. 2;
      Reserved_3_4   at 0 range 3 .. 4;
      COMIF          at 0 range 5 .. 5;
      TIF            at 0 range 6 .. 6;
      BIF            at 0 range 7 .. 7;
      Reserved_8_8   at 0 range 8 .. 8;
      CC1OF          at 0 range 9 .. 9;
      CC2OF          at 0 range 10 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   --  event generation register
   type EGR_Register is record
      --  Write-only. Update generation
      UG            : Boolean := False;
      --  Write-only. Capture/compare 1 generation
      CC1G          : Boolean := False;
      --  Write-only. Capture/compare 2 generation
      CC2G          : Boolean := False;
      --  unspecified
      Reserved_3_4  : STM32H753.UInt2 := 16#0#;
      --  Write-only. Capture/Compare control update generation
      COMG          : Boolean := False;
      --  Write-only. Trigger generation
      TG            : Boolean := False;
      --  Write-only. Break generation
      BG            : Boolean := False;
      --  unspecified
      Reserved_8_31 : STM32H753.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EGR_Register use record
      UG            at 0 range 0 .. 0;
      CC1G          at 0 range 1 .. 1;
      CC2G          at 0 range 2 .. 2;
      Reserved_3_4  at 0 range 3 .. 4;
      COMG          at 0 range 5 .. 5;
      TG            at 0 range 6 .. 6;
      BG            at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype CCMR1_Output_CC1S_Field is STM32H753.UInt2;
   subtype CCMR1_Output_OC1M_Field is STM32H753.UInt3;
   subtype CCMR1_Output_CC2S_Field is STM32H753.UInt2;
   subtype CCMR1_Output_OC2M_Field is STM32H753.UInt3;

   --  capture/compare mode register (output mode)
   type CCMR1_Output_Register is record
      --  Capture/Compare 1 selection
      CC1S           : CCMR1_Output_CC1S_Field := 16#0#;
      --  Output Compare 1 fast enable
      OC1FE          : Boolean := False;
      --  Output Compare 1 preload enable
      OC1PE          : Boolean := False;
      --  Output Compare 1 mode
      OC1M           : CCMR1_Output_OC1M_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : STM32H753.Bit := 16#0#;
      --  Capture/Compare 2 selection
      CC2S           : CCMR1_Output_CC2S_Field := 16#0#;
      --  Output Compare 2 fast enable
      OC2FE          : Boolean := False;
      --  Output Compare 2 preload enable
      OC2PE          : Boolean := False;
      --  Output Compare 2 mode
      OC2M           : CCMR1_Output_OC2M_Field := 16#0#;
      --  unspecified
      Reserved_15_15 : STM32H753.Bit := 16#0#;
      --  Output Compare 1 mode bit 3
      OC1M_3         : Boolean := False;
      --  unspecified
      Reserved_17_23 : STM32H753.UInt7 := 16#0#;
      --  Output Compare 2 mode bit 3
      OC2M_3         : Boolean := False;
      --  unspecified
      Reserved_25_31 : STM32H753.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCMR1_Output_Register use record
      CC1S           at 0 range 0 .. 1;
      OC1FE          at 0 range 2 .. 2;
      OC1PE          at 0 range 3 .. 3;
      OC1M           at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      CC2S           at 0 range 8 .. 9;
      OC2FE          at 0 range 10 .. 10;
      OC2PE          at 0 range 11 .. 11;
      OC2M           at 0 range 12 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      OC1M_3         at 0 range 16 .. 16;
      Reserved_17_23 at 0 range 17 .. 23;
      OC2M_3         at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   subtype CCMR1_Input_CC1S_Field is STM32H753.UInt2;
   subtype CCMR1_Input_IC1PSC_Field is STM32H753.UInt2;
   subtype CCMR1_Input_IC1F_Field is STM32H753.UInt4;
   subtype CCMR1_Input_CC2S_Field is STM32H753.UInt2;
   subtype CCMR1_Input_IC2PSC_Field is STM32H753.UInt2;
   subtype CCMR1_Input_IC2F_Field is STM32H753.UInt4;

   --  capture/compare mode register 1 (input mode)
   type CCMR1_Input_Register is record
      --  Capture/Compare 1 selection
      CC1S           : CCMR1_Input_CC1S_Field := 16#0#;
      --  Input capture 1 prescaler
      IC1PSC         : CCMR1_Input_IC1PSC_Field := 16#0#;
      --  Input capture 1 filter
      IC1F           : CCMR1_Input_IC1F_Field := 16#0#;
      --  Capture/Compare 2 selection
      CC2S           : CCMR1_Input_CC2S_Field := 16#0#;
      --  Input capture 2 prescaler
      IC2PSC         : CCMR1_Input_IC2PSC_Field := 16#0#;
      --  Input capture 2 filter
      IC2F           : CCMR1_Input_IC2F_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCMR1_Input_Register use record
      CC1S           at 0 range 0 .. 1;
      IC1PSC         at 0 range 2 .. 3;
      IC1F           at 0 range 4 .. 7;
      CC2S           at 0 range 8 .. 9;
      IC2PSC         at 0 range 10 .. 11;
      IC2F           at 0 range 12 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  capture/compare enable register
   type CCER_Register is record
      --  Capture/Compare 1 output enable
      CC1E          : Boolean := False;
      --  Capture/Compare 1 output Polarity
      CC1P          : Boolean := False;
      --  Capture/Compare 1 complementary output enable
      CC1NE         : Boolean := False;
      --  Capture/Compare 1 output Polarity
      CC1NP         : Boolean := False;
      --  Capture/Compare 2 output enable
      CC2E          : Boolean := False;
      --  Capture/Compare 2 output Polarity
      CC2P          : Boolean := False;
      --  unspecified
      Reserved_6_6  : STM32H753.Bit := 16#0#;
      --  Capture/Compare 2 output Polarity
      CC2NP         : Boolean := False;
      --  unspecified
      Reserved_8_31 : STM32H753.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCER_Register use record
      CC1E          at 0 range 0 .. 0;
      CC1P          at 0 range 1 .. 1;
      CC1NE         at 0 range 2 .. 2;
      CC1NP         at 0 range 3 .. 3;
      CC2E          at 0 range 4 .. 4;
      CC2P          at 0 range 5 .. 5;
      Reserved_6_6  at 0 range 6 .. 6;
      CC2NP         at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype CNT_CNT_Field is STM32H753.UInt16;

   --  counter
   type CNT_Register is record
      --  counter value
      CNT            : CNT_CNT_Field := 16#0#;
      --  unspecified
      Reserved_16_30 : STM32H753.UInt15 := 16#0#;
      --  Read-only. UIF copy
      UIFCPY         : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CNT_Register use record
      CNT            at 0 range 0 .. 15;
      Reserved_16_30 at 0 range 16 .. 30;
      UIFCPY         at 0 range 31 .. 31;
   end record;

   subtype PSC_PSC_Field is STM32H753.UInt16;

   --  prescaler
   type PSC_Register is record
      --  Prescaler value
      PSC            : PSC_PSC_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PSC_Register use record
      PSC            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype ARR_ARR_Field is STM32H753.UInt16;

   --  auto-reload register
   type ARR_Register is record
      --  Auto-reload value
      ARR            : ARR_ARR_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ARR_Register use record
      ARR            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype RCR_REP_Field is STM32H753.Byte;

   --  repetition counter register
   type RCR_Register is record
      --  Repetition counter value
      REP           : RCR_REP_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : STM32H753.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RCR_Register use record
      REP           at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype CCR1_CCR1_Field is STM32H753.UInt16;

   --  capture/compare register 1
   type CCR1_Register is record
      --  Capture/Compare 1 value
      CCR1           : CCR1_CCR1_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCR1_Register use record
      CCR1           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype CCR2_CCR2_Field is STM32H753.UInt16;

   --  capture/compare register 2
   type CCR2_Register is record
      --  Capture/Compare 2 value
      CCR2           : CCR2_CCR2_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCR2_Register use record
      CCR2           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype BDTR_DTG_Field is STM32H753.Byte;
   subtype BDTR_LOCK_Field is STM32H753.UInt2;
   subtype BDTR_BKF_Field is STM32H753.UInt4;

   --  break and dead-time register
   type BDTR_Register is record
      --  Dead-time generator setup
      DTG            : BDTR_DTG_Field := 16#0#;
      --  Lock configuration
      LOCK           : BDTR_LOCK_Field := 16#0#;
      --  Off-state selection for Idle mode
      OSSI           : Boolean := False;
      --  Off-state selection for Run mode
      OSSR           : Boolean := False;
      --  Break enable
      BKE            : Boolean := False;
      --  Break polarity
      BKP            : Boolean := False;
      --  Automatic output enable
      AOE            : Boolean := False;
      --  Main output enable
      MOE            : Boolean := False;
      --  Break filter
      BKF            : BDTR_BKF_Field := 16#0#;
      --  unspecified
      Reserved_20_31 : STM32H753.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for BDTR_Register use record
      DTG            at 0 range 0 .. 7;
      LOCK           at 0 range 8 .. 9;
      OSSI           at 0 range 10 .. 10;
      OSSR           at 0 range 11 .. 11;
      BKE            at 0 range 12 .. 12;
      BKP            at 0 range 13 .. 13;
      AOE            at 0 range 14 .. 14;
      MOE            at 0 range 15 .. 15;
      BKF            at 0 range 16 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   subtype DCR_DBA_Field is STM32H753.UInt5;
   subtype DCR_DBL_Field is STM32H753.UInt5;

   --  DMA control register
   type DCR_Register is record
      --  DMA base address
      DBA            : DCR_DBA_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : STM32H753.UInt3 := 16#0#;
      --  DMA burst length
      DBL            : DCR_DBL_Field := 16#0#;
      --  unspecified
      Reserved_13_31 : STM32H753.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DCR_Register use record
      DBA            at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      DBL            at 0 range 8 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   subtype DMAR_DMAB_Field is STM32H753.UInt16;

   --  DMA address for full transfer
   type DMAR_Register is record
      --  DMA register for burst accesses
      DMAB           : DMAR_DMAB_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAR_Register use record
      DMAB           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  TIM15 alternate fdfsdm1_breakon register 1
   type AF1_Register is record
      --  BRK BKIN input enable
      BKINE          : Boolean := False;
      --  BRK COMP1 enable
      BKCMP1E        : Boolean := False;
      --  BRK COMP2 enable
      BKCMP2E        : Boolean := False;
      --  unspecified
      Reserved_3_7   : STM32H753.UInt5 := 16#0#;
      --  BRK dfsdm1_break[0] enable
      BKDF1BK0E      : Boolean := False;
      --  BRK BKIN input polarity
      BKINP          : Boolean := False;
      --  BRK COMP1 input polarity
      BKCMP1P        : Boolean := False;
      --  BRK COMP2 input polarity
      BKCMP2P        : Boolean := False;
      --  unspecified
      Reserved_12_31 : STM32H753.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AF1_Register use record
      BKINE          at 0 range 0 .. 0;
      BKCMP1E        at 0 range 1 .. 1;
      BKCMP2E        at 0 range 2 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      BKDF1BK0E      at 0 range 8 .. 8;
      BKINP          at 0 range 9 .. 9;
      BKCMP1P        at 0 range 10 .. 10;
      BKCMP2P        at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype TISEL_TI1SEL_Field is STM32H753.UInt4;
   subtype TISEL_TI2SEL_Field is STM32H753.UInt4;

   --  TIM15 input selection register
   type TISEL_Register is record
      --  selects TI1[0] to TI1[15] input
      TI1SEL         : TISEL_TI1SEL_Field := 16#0#;
      --  unspecified
      Reserved_4_7   : STM32H753.UInt4 := 16#0#;
      --  selects TI2[0] to TI2[15] input
      TI2SEL         : TISEL_TI2SEL_Field := 16#0#;
      --  unspecified
      Reserved_12_31 : STM32H753.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TISEL_Register use record
      TI1SEL         at 0 range 0 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      TI2SEL         at 0 range 8 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   --  control register 2
   type CR2_Register_1 is record
      --  Capture/compare preloaded control
      CCPC           : Boolean := False;
      --  unspecified
      Reserved_1_1   : STM32H753.Bit := 16#0#;
      --  Capture/compare control update selection
      CCUS           : Boolean := False;
      --  Capture/compare DMA selection
      CCDS           : Boolean := False;
      --  unspecified
      Reserved_4_7   : STM32H753.UInt4 := 16#0#;
      --  Output Idle state 1
      OIS1           : Boolean := False;
      --  Output Idle state 1
      OIS1N          : Boolean := False;
      --  unspecified
      Reserved_10_31 : STM32H753.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR2_Register_1 use record
      CCPC           at 0 range 0 .. 0;
      Reserved_1_1   at 0 range 1 .. 1;
      CCUS           at 0 range 2 .. 2;
      CCDS           at 0 range 3 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      OIS1           at 0 range 8 .. 8;
      OIS1N          at 0 range 9 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   --  DMA/Interrupt enable register
   type DIER_Register_1 is record
      --  Update interrupt enable
      UIE            : Boolean := False;
      --  Capture/Compare 1 interrupt enable
      CC1IE          : Boolean := False;
      --  unspecified
      Reserved_2_4   : STM32H753.UInt3 := 16#0#;
      --  COM interrupt enable
      COMIE          : Boolean := False;
      --  unspecified
      Reserved_6_6   : STM32H753.Bit := 16#0#;
      --  Break interrupt enable
      BIE            : Boolean := False;
      --  Update DMA request enable
      UDE            : Boolean := False;
      --  Capture/Compare 1 DMA request enable
      CC1DE          : Boolean := False;
      --  unspecified
      Reserved_10_12 : STM32H753.UInt3 := 16#0#;
      --  COM DMA request enable
      COMDE          : Boolean := False;
      --  unspecified
      Reserved_14_31 : STM32H753.UInt18 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DIER_Register_1 use record
      UIE            at 0 range 0 .. 0;
      CC1IE          at 0 range 1 .. 1;
      Reserved_2_4   at 0 range 2 .. 4;
      COMIE          at 0 range 5 .. 5;
      Reserved_6_6   at 0 range 6 .. 6;
      BIE            at 0 range 7 .. 7;
      UDE            at 0 range 8 .. 8;
      CC1DE          at 0 range 9 .. 9;
      Reserved_10_12 at 0 range 10 .. 12;
      COMDE          at 0 range 13 .. 13;
      Reserved_14_31 at 0 range 14 .. 31;
   end record;

   --  status register
   type SR_Register_1 is record
      --  Update interrupt flag
      UIF            : Boolean := False;
      --  Capture/compare 1 interrupt flag
      CC1IF          : Boolean := False;
      --  unspecified
      Reserved_2_4   : STM32H753.UInt3 := 16#0#;
      --  COM interrupt flag
      COMIF          : Boolean := False;
      --  unspecified
      Reserved_6_6   : STM32H753.Bit := 16#0#;
      --  Break interrupt flag
      BIF            : Boolean := False;
      --  unspecified
      Reserved_8_8   : STM32H753.Bit := 16#0#;
      --  Capture/Compare 1 overcapture flag
      CC1OF          : Boolean := False;
      --  unspecified
      Reserved_10_31 : STM32H753.UInt22 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register_1 use record
      UIF            at 0 range 0 .. 0;
      CC1IF          at 0 range 1 .. 1;
      Reserved_2_4   at 0 range 2 .. 4;
      COMIF          at 0 range 5 .. 5;
      Reserved_6_6   at 0 range 6 .. 6;
      BIF            at 0 range 7 .. 7;
      Reserved_8_8   at 0 range 8 .. 8;
      CC1OF          at 0 range 9 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   --  event generation register
   type EGR_Register_1 is record
      --  Write-only. Update generation
      UG            : Boolean := False;
      --  Write-only. Capture/compare 1 generation
      CC1G          : Boolean := False;
      --  unspecified
      Reserved_2_4  : STM32H753.UInt3 := 16#0#;
      --  Write-only. Capture/Compare control update generation
      COMG          : Boolean := False;
      --  unspecified
      Reserved_6_6  : STM32H753.Bit := 16#0#;
      --  Write-only. Break generation
      BG            : Boolean := False;
      --  unspecified
      Reserved_8_31 : STM32H753.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EGR_Register_1 use record
      UG            at 0 range 0 .. 0;
      CC1G          at 0 range 1 .. 1;
      Reserved_2_4  at 0 range 2 .. 4;
      COMG          at 0 range 5 .. 5;
      Reserved_6_6  at 0 range 6 .. 6;
      BG            at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  capture/compare mode register (output mode)
   type CCMR1_Output_Register_1 is record
      --  Capture/Compare 1 selection
      CC1S           : CCMR1_Output_CC1S_Field := 16#0#;
      --  Output Compare 1 fast enable
      OC1FE          : Boolean := False;
      --  Output Compare 1 preload enable
      OC1PE          : Boolean := False;
      --  Output Compare 1 mode
      OC1M           : CCMR1_Output_OC1M_Field := 16#0#;
      --  unspecified
      Reserved_7_15  : STM32H753.UInt9 := 16#0#;
      --  Output Compare 1 mode
      OC1M_3         : Boolean := False;
      --  unspecified
      Reserved_17_31 : STM32H753.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCMR1_Output_Register_1 use record
      CC1S           at 0 range 0 .. 1;
      OC1FE          at 0 range 2 .. 2;
      OC1PE          at 0 range 3 .. 3;
      OC1M           at 0 range 4 .. 6;
      Reserved_7_15  at 0 range 7 .. 15;
      OC1M_3         at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   --  capture/compare mode register 1 (input mode)
   type CCMR1_Input_Register_1 is record
      --  Capture/Compare 1 selection
      CC1S          : CCMR1_Input_CC1S_Field := 16#0#;
      --  Input capture 1 prescaler
      IC1PSC        : CCMR1_Input_IC1PSC_Field := 16#0#;
      --  Input capture 1 filter
      IC1F          : CCMR1_Input_IC1F_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : STM32H753.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCMR1_Input_Register_1 use record
      CC1S          at 0 range 0 .. 1;
      IC1PSC        at 0 range 2 .. 3;
      IC1F          at 0 range 4 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  capture/compare enable register
   type CCER_Register_1 is record
      --  Capture/Compare 1 output enable
      CC1E          : Boolean := False;
      --  Capture/Compare 1 output Polarity
      CC1P          : Boolean := False;
      --  Capture/Compare 1 complementary output enable
      CC1NE         : Boolean := False;
      --  Capture/Compare 1 output Polarity
      CC1NP         : Boolean := False;
      --  unspecified
      Reserved_4_31 : STM32H753.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCER_Register_1 use record
      CC1E          at 0 range 0 .. 0;
      CC1P          at 0 range 1 .. 1;
      CC1NE         at 0 range 2 .. 2;
      CC1NP         at 0 range 3 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  TIM16 alternate function register 1
   type TIM16_AF1_Register is record
      --  BRK BKIN input enable
      BKINE          : Boolean := False;
      --  BRK COMP1 enable
      BKCMP1E        : Boolean := False;
      --  BRK COMP2 enable
      BKCMP2E        : Boolean := False;
      --  unspecified
      Reserved_3_7   : STM32H753.UInt5 := 16#0#;
      --  BRK dfsdm1_break[1] enable
      BKDFBK1E       : Boolean := False;
      --  BRK BKIN input polarity
      BKINP          : Boolean := False;
      --  BRK COMP1 input polarity
      BKCMP1P        : Boolean := False;
      --  BRK COMP2 input polarity
      BKCMP2P        : Boolean := False;
      --  unspecified
      Reserved_12_31 : STM32H753.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIM16_AF1_Register use record
      BKINE          at 0 range 0 .. 0;
      BKCMP1E        at 0 range 1 .. 1;
      BKCMP2E        at 0 range 2 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      BKDFBK1E       at 0 range 8 .. 8;
      BKINP          at 0 range 9 .. 9;
      BKCMP1P        at 0 range 10 .. 10;
      BKCMP2P        at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype TIM16_TISEL_TI1SEL_Field is STM32H753.UInt4;

   --  TIM16 input selection register
   type TIM16_TISEL_Register is record
      --  selects TI1[0] to TI1[15] input
      TI1SEL        : TIM16_TISEL_TI1SEL_Field := 16#0#;
      --  unspecified
      Reserved_4_31 : STM32H753.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIM16_TISEL_Register use record
      TI1SEL        at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  TIM17 alternate function register 1
   type TIM17_AF1_Register is record
      --  BRK BKIN input enable
      BKINE          : Boolean := False;
      --  BRK COMP1 enable
      BKCMP1E        : Boolean := False;
      --  BRK COMP2 enable
      BKCMP2E        : Boolean := False;
      --  unspecified
      Reserved_3_7   : STM32H753.UInt5 := 16#0#;
      --  BRK dfsdm1_break[1] enable
      BKDFBK1E       : Boolean := False;
      --  BRK BKIN input polarity
      BKINP          : Boolean := False;
      --  BRK COMP1 input polarity
      BKCMP1P        : Boolean := False;
      --  BRK COMP2 input polarity
      BKCMP2P        : Boolean := False;
      --  unspecified
      Reserved_12_31 : STM32H753.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIM17_AF1_Register use record
      BKINE          at 0 range 0 .. 0;
      BKCMP1E        at 0 range 1 .. 1;
      BKCMP2E        at 0 range 2 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      BKDFBK1E       at 0 range 8 .. 8;
      BKINP          at 0 range 9 .. 9;
      BKCMP1P        at 0 range 10 .. 10;
      BKCMP2P        at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype TIM17_TISEL_TI1SEL_Field is STM32H753.UInt4;

   --  TIM17 input selection register
   type TIM17_TISEL_Register is record
      --  selects TI1[0] to TI1[15] input
      TI1SEL        : TIM17_TISEL_TI1SEL_Field := 16#0#;
      --  unspecified
      Reserved_4_31 : STM32H753.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for TIM17_TISEL_Register use record
      TI1SEL        at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   type TIM15_Disc is
     (Output,
      Input);

   --  General purpose timers
   type TIM15_Peripheral
     (Discriminent : TIM15_Disc := Output)
   is record
      --  control register 1
      CR1          : aliased CR1_Register;
      --  control register 2
      CR2          : aliased CR2_Register;
      --  slave mode control register
      SMCR         : aliased SMCR_Register;
      --  DMA/Interrupt enable register
      DIER         : aliased DIER_Register;
      --  status register
      SR           : aliased SR_Register;
      --  event generation register
      EGR          : aliased EGR_Register;
      --  capture/compare enable register
      CCER         : aliased CCER_Register;
      --  counter
      CNT          : aliased CNT_Register;
      --  prescaler
      PSC          : aliased PSC_Register;
      --  auto-reload register
      ARR          : aliased ARR_Register;
      --  repetition counter register
      RCR          : aliased RCR_Register;
      --  capture/compare register 1
      CCR1         : aliased CCR1_Register;
      --  capture/compare register 2
      CCR2         : aliased CCR2_Register;
      --  break and dead-time register
      BDTR         : aliased BDTR_Register;
      --  DMA control register
      DCR          : aliased DCR_Register;
      --  DMA address for full transfer
      DMAR         : aliased DMAR_Register;
      --  TIM15 alternate fdfsdm1_breakon register 1
      AF1          : aliased AF1_Register;
      --  TIM15 input selection register
      TISEL        : aliased TISEL_Register;
      case Discriminent is
         when Output =>
            --  capture/compare mode register (output mode)
            CCMR1_Output : aliased CCMR1_Output_Register;
         when Input =>
            --  capture/compare mode register 1 (input mode)
            CCMR1_Input : aliased CCMR1_Input_Register;
      end case;
   end record
     with Unchecked_Union, Volatile;

   for TIM15_Peripheral use record
      CR1          at 16#0# range 0 .. 31;
      CR2          at 16#4# range 0 .. 31;
      SMCR         at 16#8# range 0 .. 31;
      DIER         at 16#C# range 0 .. 31;
      SR           at 16#10# range 0 .. 31;
      EGR          at 16#14# range 0 .. 31;
      CCER         at 16#20# range 0 .. 31;
      CNT          at 16#24# range 0 .. 31;
      PSC          at 16#28# range 0 .. 31;
      ARR          at 16#2C# range 0 .. 31;
      RCR          at 16#30# range 0 .. 31;
      CCR1         at 16#34# range 0 .. 31;
      CCR2         at 16#38# range 0 .. 31;
      BDTR         at 16#44# range 0 .. 31;
      DCR          at 16#48# range 0 .. 31;
      DMAR         at 16#4C# range 0 .. 31;
      AF1          at 16#60# range 0 .. 31;
      TISEL        at 16#68# range 0 .. 31;
      CCMR1_Output at 16#18# range 0 .. 31;
      CCMR1_Input  at 16#18# range 0 .. 31;
   end record;

   --  General purpose timers
   TIM15_Periph : aliased TIM15_Peripheral
     with Import, Address => TIM15_Base;

   type TIM16_Disc is
     (Output,
      Input);

   --  General-purpose-timers
   type TIM16_Peripheral
     (Discriminent : TIM16_Disc := Output)
   is record
      --  control register 1
      CR1          : aliased CR1_Register;
      --  control register 2
      CR2          : aliased CR2_Register_1;
      --  DMA/Interrupt enable register
      DIER         : aliased DIER_Register_1;
      --  status register
      SR           : aliased SR_Register_1;
      --  event generation register
      EGR          : aliased EGR_Register_1;
      --  capture/compare enable register
      CCER         : aliased CCER_Register_1;
      --  counter
      CNT          : aliased CNT_Register;
      --  prescaler
      PSC          : aliased PSC_Register;
      --  auto-reload register
      ARR          : aliased ARR_Register;
      --  repetition counter register
      RCR          : aliased RCR_Register;
      --  capture/compare register 1
      CCR1         : aliased CCR1_Register;
      --  break and dead-time register
      BDTR         : aliased BDTR_Register;
      --  DMA control register
      DCR          : aliased DCR_Register;
      --  DMA address for full transfer
      DMAR         : aliased DMAR_Register;
      --  TIM16 alternate function register 1
      TIM16_AF1    : aliased TIM16_AF1_Register;
      --  TIM16 input selection register
      TIM16_TISEL  : aliased TIM16_TISEL_Register;
      case Discriminent is
         when Output =>
            --  capture/compare mode register (output mode)
            CCMR1_Output : aliased CCMR1_Output_Register_1;
         when Input =>
            --  capture/compare mode register 1 (input mode)
            CCMR1_Input : aliased CCMR1_Input_Register_1;
      end case;
   end record
     with Unchecked_Union, Volatile;

   for TIM16_Peripheral use record
      CR1          at 16#0# range 0 .. 31;
      CR2          at 16#4# range 0 .. 31;
      DIER         at 16#C# range 0 .. 31;
      SR           at 16#10# range 0 .. 31;
      EGR          at 16#14# range 0 .. 31;
      CCER         at 16#20# range 0 .. 31;
      CNT          at 16#24# range 0 .. 31;
      PSC          at 16#28# range 0 .. 31;
      ARR          at 16#2C# range 0 .. 31;
      RCR          at 16#30# range 0 .. 31;
      CCR1         at 16#34# range 0 .. 31;
      BDTR         at 16#44# range 0 .. 31;
      DCR          at 16#48# range 0 .. 31;
      DMAR         at 16#4C# range 0 .. 31;
      TIM16_AF1    at 16#60# range 0 .. 31;
      TIM16_TISEL  at 16#68# range 0 .. 31;
      CCMR1_Output at 16#18# range 0 .. 31;
      CCMR1_Input  at 16#18# range 0 .. 31;
   end record;

   --  General-purpose-timers
   TIM16_Periph : aliased TIM16_Peripheral
     with Import, Address => TIM16_Base;

   type TIM17_Disc is
     (Output,
      Input);

   --  General-purpose-timers
   type TIM17_Peripheral
     (Discriminent : TIM17_Disc := Output)
   is record
      --  control register 1
      CR1          : aliased CR1_Register;
      --  control register 2
      CR2          : aliased CR2_Register_1;
      --  DMA/Interrupt enable register
      DIER         : aliased DIER_Register_1;
      --  status register
      SR           : aliased SR_Register_1;
      --  event generation register
      EGR          : aliased EGR_Register_1;
      --  capture/compare enable register
      CCER         : aliased CCER_Register_1;
      --  counter
      CNT          : aliased CNT_Register;
      --  prescaler
      PSC          : aliased PSC_Register;
      --  auto-reload register
      ARR          : aliased ARR_Register;
      --  repetition counter register
      RCR          : aliased RCR_Register;
      --  capture/compare register 1
      CCR1         : aliased CCR1_Register;
      --  break and dead-time register
      BDTR         : aliased BDTR_Register;
      --  DMA control register
      DCR          : aliased DCR_Register;
      --  DMA address for full transfer
      DMAR         : aliased DMAR_Register;
      --  TIM17 alternate function register 1
      TIM17_AF1    : aliased TIM17_AF1_Register;
      --  TIM17 input selection register
      TIM17_TISEL  : aliased TIM17_TISEL_Register;
      case Discriminent is
         when Output =>
            --  capture/compare mode register (output mode)
            CCMR1_Output : aliased CCMR1_Output_Register_1;
         when Input =>
            --  capture/compare mode register 1 (input mode)
            CCMR1_Input : aliased CCMR1_Input_Register_1;
      end case;
   end record
     with Unchecked_Union, Volatile;

   for TIM17_Peripheral use record
      CR1          at 16#0# range 0 .. 31;
      CR2          at 16#4# range 0 .. 31;
      DIER         at 16#C# range 0 .. 31;
      SR           at 16#10# range 0 .. 31;
      EGR          at 16#14# range 0 .. 31;
      CCER         at 16#20# range 0 .. 31;
      CNT          at 16#24# range 0 .. 31;
      PSC          at 16#28# range 0 .. 31;
      ARR          at 16#2C# range 0 .. 31;
      RCR          at 16#30# range 0 .. 31;
      CCR1         at 16#34# range 0 .. 31;
      BDTR         at 16#44# range 0 .. 31;
      DCR          at 16#48# range 0 .. 31;
      DMAR         at 16#4C# range 0 .. 31;
      TIM17_AF1    at 16#60# range 0 .. 31;
      TIM17_TISEL  at 16#68# range 0 .. 31;
      CCMR1_Output at 16#18# range 0 .. 31;
      CCMR1_Input  at 16#18# range 0 .. 31;
   end record;

   --  General-purpose-timers
   TIM17_Periph : aliased TIM17_Peripheral
     with Import, Address => TIM17_Base;

end STM32H753.TIMs;
