pragma Style_Checks (Off);

--  This spec has been automatically generated from STM32H753.svd


with System;

package STM32H753.ADC is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   --  ISR_AWD array
   type ISR_AWD_Field_Array is array (1 .. 3) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for ISR_AWD
   type ISR_AWD_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  AWD as a value
            Val : STM32H753.UInt3;
         when True =>
            --  AWD as an array
            Arr : ISR_AWD_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for ISR_AWD_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  ADC interrupt and status register
   type ISR_Register is record
      --  ADC ready flag
      ADRDY          : Boolean := False;
      --  ADC group regular end of sampling flag
      EOSMP          : Boolean := False;
      --  ADC group regular end of unitary conversion flag
      EOC            : Boolean := False;
      --  ADC group regular end of sequence conversions flag
      EOS            : Boolean := False;
      --  ADC group regular overrun flag
      OVR            : Boolean := False;
      --  ADC group injected end of unitary conversion flag
      JEOC           : Boolean := False;
      --  ADC group injected end of sequence conversions flag
      JEOS           : Boolean := False;
      --  ADC analog watchdog 1 flag
      AWD            : ISR_AWD_Field := (As_Array => False, Val => 16#0#);
      --  ADC group injected contexts queue overflow flag
      JQOVF          : Boolean := False;
      --  unspecified
      Reserved_11_31 : STM32H753.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ISR_Register use record
      ADRDY          at 0 range 0 .. 0;
      EOSMP          at 0 range 1 .. 1;
      EOC            at 0 range 2 .. 2;
      EOS            at 0 range 3 .. 3;
      OVR            at 0 range 4 .. 4;
      JEOC           at 0 range 5 .. 5;
      JEOS           at 0 range 6 .. 6;
      AWD            at 0 range 7 .. 9;
      JQOVF          at 0 range 10 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   --  ADC interrupt enable register
   type IER_Register is record
      --  ADC ready interrupt
      ADRDYIE        : Boolean := False;
      --  ADC group regular end of sampling interrupt
      EOSMPIE        : Boolean := False;
      --  ADC group regular end of unitary conversion interrupt
      EOCIE          : Boolean := False;
      --  ADC group regular end of sequence conversions interrupt
      EOSIE          : Boolean := False;
      --  ADC group regular overrun interrupt
      OVRIE          : Boolean := False;
      --  ADC group injected end of unitary conversion interrupt
      JEOCIE         : Boolean := False;
      --  ADC group injected end of sequence conversions interrupt
      JEOSIE         : Boolean := False;
      --  ADC analog watchdog 1 interrupt
      AWD1IE         : Boolean := False;
      --  ADC analog watchdog 2 interrupt
      AWD2IE         : Boolean := False;
      --  ADC analog watchdog 3 interrupt
      AWD3IE         : Boolean := False;
      --  ADC group injected contexts queue overflow interrupt
      JQOVFIE        : Boolean := False;
      --  unspecified
      Reserved_11_31 : STM32H753.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IER_Register use record
      ADRDYIE        at 0 range 0 .. 0;
      EOSMPIE        at 0 range 1 .. 1;
      EOCIE          at 0 range 2 .. 2;
      EOSIE          at 0 range 3 .. 3;
      OVRIE          at 0 range 4 .. 4;
      JEOCIE         at 0 range 5 .. 5;
      JEOSIE         at 0 range 6 .. 6;
      AWD1IE         at 0 range 7 .. 7;
      AWD2IE         at 0 range 8 .. 8;
      AWD3IE         at 0 range 9 .. 9;
      JQOVFIE        at 0 range 10 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   --  CR_LINCALRDYW array
   type CR_LINCALRDYW_Field_Array is array (1 .. 6) of Boolean
     with Component_Size => 1, Size => 6;

   --  Type definition for CR_LINCALRDYW
   type CR_LINCALRDYW_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  LINCALRDYW as a value
            Val : STM32H753.UInt6;
         when True =>
            --  LINCALRDYW as an array
            Arr : CR_LINCALRDYW_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 6;

   for CR_LINCALRDYW_Field use record
      Val at 0 range 0 .. 5;
      Arr at 0 range 0 .. 5;
   end record;

   --  ADC control register
   type CR_Register is record
      --  ADC enable
      ADEN           : Boolean := False;
      --  ADC disable
      ADDIS          : Boolean := False;
      --  ADC group regular conversion start
      ADSTART        : Boolean := False;
      --  ADC group injected conversion start
      JADSTART       : Boolean := False;
      --  ADC group regular conversion stop
      ADSTP          : Boolean := False;
      --  ADC group injected conversion stop
      JADSTP         : Boolean := False;
      --  unspecified
      Reserved_6_7   : STM32H753.UInt2 := 16#0#;
      --  Boost mode control
      BOOST          : Boolean := False;
      --  unspecified
      Reserved_9_15  : STM32H753.UInt7 := 16#0#;
      --  Linearity calibration
      ADCALLIN       : Boolean := False;
      --  unspecified
      Reserved_17_21 : STM32H753.UInt5 := 16#0#;
      --  Linearity calibration ready Word 1
      LINCALRDYW     : CR_LINCALRDYW_Field :=
                        (As_Array => False, Val => 16#0#);
      --  ADC voltage regulator enable
      ADVREGEN       : Boolean := False;
      --  ADC deep power down enable
      DEEPPWD        : Boolean := False;
      --  ADC differential mode for calibration
      ADCALDIF       : Boolean := False;
      --  ADC calibration
      ADCAL          : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      ADEN           at 0 range 0 .. 0;
      ADDIS          at 0 range 1 .. 1;
      ADSTART        at 0 range 2 .. 2;
      JADSTART       at 0 range 3 .. 3;
      ADSTP          at 0 range 4 .. 4;
      JADSTP         at 0 range 5 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      BOOST          at 0 range 8 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      ADCALLIN       at 0 range 16 .. 16;
      Reserved_17_21 at 0 range 17 .. 21;
      LINCALRDYW     at 0 range 22 .. 27;
      ADVREGEN       at 0 range 28 .. 28;
      DEEPPWD        at 0 range 29 .. 29;
      ADCALDIF       at 0 range 30 .. 30;
      ADCAL          at 0 range 31 .. 31;
   end record;

   subtype CFGR_DMNGT_Field is STM32H753.UInt2;
   subtype CFGR_RES_Field is STM32H753.UInt3;
   subtype CFGR_EXTSEL_Field is STM32H753.UInt5;
   subtype CFGR_EXTEN_Field is STM32H753.UInt2;
   subtype CFGR_DISCNUM_Field is STM32H753.UInt3;
   subtype CFGR_AWDCH1CH_Field is STM32H753.UInt5;

   --  ADC configuration register 1
   type CFGR_Register is record
      --  ADC DMA transfer enable
      DMNGT          : CFGR_DMNGT_Field := 16#0#;
      --  ADC data resolution
      RES            : CFGR_RES_Field := 16#0#;
      --  ADC group regular external trigger source
      EXTSEL         : CFGR_EXTSEL_Field := 16#0#;
      --  ADC group regular external trigger polarity
      EXTEN          : CFGR_EXTEN_Field := 16#0#;
      --  ADC group regular overrun configuration
      OVRMOD         : Boolean := False;
      --  ADC group regular continuous conversion mode
      CONT           : Boolean := False;
      --  ADC low power auto wait
      AUTDLY         : Boolean := False;
      --  unspecified
      Reserved_15_15 : STM32H753.Bit := 16#0#;
      --  ADC group regular sequencer discontinuous mode
      DISCEN         : Boolean := False;
      --  ADC group regular sequencer discontinuous number of ranks
      DISCNUM        : CFGR_DISCNUM_Field := 16#0#;
      --  ADC group injected sequencer discontinuous mode
      JDISCEN        : Boolean := False;
      --  ADC group injected contexts queue mode
      JQM            : Boolean := False;
      --  ADC analog watchdog 1 monitoring a single channel or all channels
      AWD1SGL        : Boolean := False;
      --  ADC analog watchdog 1 enable on scope ADC group regular
      AWD1EN         : Boolean := False;
      --  ADC analog watchdog 1 enable on scope ADC group injected
      JAWD1EN        : Boolean := False;
      --  ADC group injected automatic trigger mode
      JAUTO          : Boolean := False;
      --  ADC analog watchdog 1 monitored channel selection
      AWDCH1CH       : CFGR_AWDCH1CH_Field := 16#0#;
      --  ADC group injected contexts queue disable
      JQDIS          : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CFGR_Register use record
      DMNGT          at 0 range 0 .. 1;
      RES            at 0 range 2 .. 4;
      EXTSEL         at 0 range 5 .. 9;
      EXTEN          at 0 range 10 .. 11;
      OVRMOD         at 0 range 12 .. 12;
      CONT           at 0 range 13 .. 13;
      AUTDLY         at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      DISCEN         at 0 range 16 .. 16;
      DISCNUM        at 0 range 17 .. 19;
      JDISCEN        at 0 range 20 .. 20;
      JQM            at 0 range 21 .. 21;
      AWD1SGL        at 0 range 22 .. 22;
      AWD1EN         at 0 range 23 .. 23;
      JAWD1EN        at 0 range 24 .. 24;
      JAUTO          at 0 range 25 .. 25;
      AWDCH1CH       at 0 range 26 .. 30;
      JQDIS          at 0 range 31 .. 31;
   end record;

   subtype CFGR2_OVSS_Field is STM32H753.UInt4;

   --  CFGR2_RSHIFT array
   type CFGR2_RSHIFT_Field_Array is array (1 .. 4) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for CFGR2_RSHIFT
   type CFGR2_RSHIFT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  RSHIFT as a value
            Val : STM32H753.UInt4;
         when True =>
            --  RSHIFT as an array
            Arr : CFGR2_RSHIFT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for CFGR2_RSHIFT_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   subtype CFGR2_OSR_Field is STM32H753.UInt10;
   subtype CFGR2_LSHIFT_Field is STM32H753.UInt4;

   --  ADC configuration register 2
   type CFGR2_Register is record
      --  ADC oversampler enable on scope ADC group regular
      ROVSE          : Boolean := False;
      --  ADC oversampler enable on scope ADC group injected
      JOVSE          : Boolean := False;
      --  unspecified
      Reserved_2_4   : STM32H753.UInt3 := 16#0#;
      --  ADC oversampling shift
      OVSS           : CFGR2_OVSS_Field := 16#0#;
      --  ADC oversampling discontinuous mode (triggered mode) for ADC group
      --  regular
      TROVS          : Boolean := False;
      --  Regular Oversampling mode
      ROVSM          : Boolean := False;
      --  Right-shift data after Offset 1 correction
      RSHIFT         : CFGR2_RSHIFT_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_15_15 : STM32H753.Bit := 16#0#;
      --  Oversampling ratio
      OSR            : CFGR2_OSR_Field := 16#0#;
      --  unspecified
      Reserved_26_27 : STM32H753.UInt2 := 16#0#;
      --  Left shift factor
      LSHIFT         : CFGR2_LSHIFT_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CFGR2_Register use record
      ROVSE          at 0 range 0 .. 0;
      JOVSE          at 0 range 1 .. 1;
      Reserved_2_4   at 0 range 2 .. 4;
      OVSS           at 0 range 5 .. 8;
      TROVS          at 0 range 9 .. 9;
      ROVSM          at 0 range 10 .. 10;
      RSHIFT         at 0 range 11 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      OSR            at 0 range 16 .. 25;
      Reserved_26_27 at 0 range 26 .. 27;
      LSHIFT         at 0 range 28 .. 31;
   end record;

   --  SMPR1_SMP array element
   subtype SMPR1_SMP_Element is STM32H753.UInt3;

   --  SMPR1_SMP array
   type SMPR1_SMP_Field_Array is array (1 .. 9) of SMPR1_SMP_Element
     with Component_Size => 3, Size => 27;

   --  Type definition for SMPR1_SMP
   type SMPR1_SMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SMP as a value
            Val : STM32H753.UInt27;
         when True =>
            --  SMP as an array
            Arr : SMPR1_SMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 27;

   for SMPR1_SMP_Field use record
      Val at 0 range 0 .. 26;
      Arr at 0 range 0 .. 26;
   end record;

   --  ADC sampling time register 1
   type SMPR1_Register is record
      --  unspecified
      Reserved_0_2   : STM32H753.UInt3 := 16#0#;
      --  ADC channel 1 sampling time selection
      SMP            : SMPR1_SMP_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_30_31 : STM32H753.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SMPR1_Register use record
      Reserved_0_2   at 0 range 0 .. 2;
      SMP            at 0 range 3 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   --  SMPR2_SMP array element
   subtype SMPR2_SMP_Element is STM32H753.UInt3;

   --  SMPR2_SMP array
   type SMPR2_SMP_Field_Array is array (10 .. 19) of SMPR2_SMP_Element
     with Component_Size => 3, Size => 30;

   --  Type definition for SMPR2_SMP
   type SMPR2_SMP_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SMP as a value
            Val : STM32H753.UInt30;
         when True =>
            --  SMP as an array
            Arr : SMPR2_SMP_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 30;

   for SMPR2_SMP_Field use record
      Val at 0 range 0 .. 29;
      Arr at 0 range 0 .. 29;
   end record;

   --  ADC sampling time register 2
   type SMPR2_Register is record
      --  ADC channel 10 sampling time selection
      SMP            : SMPR2_SMP_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_30_31 : STM32H753.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SMPR2_Register use record
      SMP            at 0 range 0 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype PCSEL_PCSEL_Field is STM32H753.UInt20;

   --  ADC pre channel selection register
   type PCSEL_Register is record
      --  Channel x (VINP[i]) pre selection
      PCSEL          : PCSEL_PCSEL_Field := 16#0#;
      --  unspecified
      Reserved_20_31 : STM32H753.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PCSEL_Register use record
      PCSEL          at 0 range 0 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   subtype LTR1_LTR1_Field is STM32H753.UInt26;

   --  ADC analog watchdog 1 threshold register
   type LTR1_Register is record
      --  ADC analog watchdog 1 threshold low
      LTR1           : LTR1_LTR1_Field := 16#3FF0000#;
      --  unspecified
      Reserved_26_31 : STM32H753.UInt6 := 16#3#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LTR1_Register use record
      LTR1           at 0 range 0 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype LHTR1_LHTR1_Field is STM32H753.UInt26;

   --  ADC analog watchdog 2 threshold register
   type LHTR1_Register is record
      --  ADC analog watchdog 2 threshold low
      LHTR1          : LHTR1_LHTR1_Field := 16#3FF0000#;
      --  unspecified
      Reserved_26_31 : STM32H753.UInt6 := 16#3#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LHTR1_Register use record
      LHTR1          at 0 range 0 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype SQR1_L3_Field is STM32H753.UInt4;
   subtype SQR1_SQ1_Field is STM32H753.UInt5;
   subtype SQR1_SQ2_Field is STM32H753.UInt5;
   subtype SQR1_SQ3_Field is STM32H753.UInt5;
   subtype SQR1_SQ4_Field is STM32H753.UInt5;

   --  ADC group regular sequencer ranks register 1
   type SQR1_Register is record
      --  L3
      L3             : SQR1_L3_Field := 16#0#;
      --  unspecified
      Reserved_4_5   : STM32H753.UInt2 := 16#0#;
      --  ADC group regular sequencer rank 1
      SQ1            : SQR1_SQ1_Field := 16#0#;
      --  unspecified
      Reserved_11_11 : STM32H753.Bit := 16#0#;
      --  ADC group regular sequencer rank 2
      SQ2            : SQR1_SQ2_Field := 16#0#;
      --  unspecified
      Reserved_17_17 : STM32H753.Bit := 16#0#;
      --  ADC group regular sequencer rank 3
      SQ3            : SQR1_SQ3_Field := 16#0#;
      --  unspecified
      Reserved_23_23 : STM32H753.Bit := 16#0#;
      --  ADC group regular sequencer rank 4
      SQ4            : SQR1_SQ4_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : STM32H753.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SQR1_Register use record
      L3             at 0 range 0 .. 3;
      Reserved_4_5   at 0 range 4 .. 5;
      SQ1            at 0 range 6 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      SQ2            at 0 range 12 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      SQ3            at 0 range 18 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      SQ4            at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype SQR2_SQ5_Field is STM32H753.UInt5;
   subtype SQR2_SQ6_Field is STM32H753.UInt5;
   subtype SQR2_SQ7_Field is STM32H753.UInt5;
   subtype SQR2_SQ8_Field is STM32H753.UInt5;
   subtype SQR2_SQ9_Field is STM32H753.UInt5;

   --  ADC group regular sequencer ranks register 2
   type SQR2_Register is record
      --  ADC group regular sequencer rank 5
      SQ5            : SQR2_SQ5_Field := 16#0#;
      --  unspecified
      Reserved_5_5   : STM32H753.Bit := 16#0#;
      --  ADC group regular sequencer rank 6
      SQ6            : SQR2_SQ6_Field := 16#0#;
      --  unspecified
      Reserved_11_11 : STM32H753.Bit := 16#0#;
      --  ADC group regular sequencer rank 7
      SQ7            : SQR2_SQ7_Field := 16#0#;
      --  unspecified
      Reserved_17_17 : STM32H753.Bit := 16#0#;
      --  ADC group regular sequencer rank 8
      SQ8            : SQR2_SQ8_Field := 16#0#;
      --  unspecified
      Reserved_23_23 : STM32H753.Bit := 16#0#;
      --  ADC group regular sequencer rank 9
      SQ9            : SQR2_SQ9_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : STM32H753.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SQR2_Register use record
      SQ5            at 0 range 0 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      SQ6            at 0 range 6 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      SQ7            at 0 range 12 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      SQ8            at 0 range 18 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      SQ9            at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype SQR3_SQ10_Field is STM32H753.UInt5;
   subtype SQR3_SQ11_Field is STM32H753.UInt5;
   subtype SQR3_SQ12_Field is STM32H753.UInt5;
   subtype SQR3_SQ13_Field is STM32H753.UInt5;
   subtype SQR3_SQ14_Field is STM32H753.UInt5;

   --  ADC group regular sequencer ranks register 3
   type SQR3_Register is record
      --  ADC group regular sequencer rank 10
      SQ10           : SQR3_SQ10_Field := 16#0#;
      --  unspecified
      Reserved_5_5   : STM32H753.Bit := 16#0#;
      --  ADC group regular sequencer rank 11
      SQ11           : SQR3_SQ11_Field := 16#0#;
      --  unspecified
      Reserved_11_11 : STM32H753.Bit := 16#0#;
      --  ADC group regular sequencer rank 12
      SQ12           : SQR3_SQ12_Field := 16#0#;
      --  unspecified
      Reserved_17_17 : STM32H753.Bit := 16#0#;
      --  ADC group regular sequencer rank 13
      SQ13           : SQR3_SQ13_Field := 16#0#;
      --  unspecified
      Reserved_23_23 : STM32H753.Bit := 16#0#;
      --  ADC group regular sequencer rank 14
      SQ14           : SQR3_SQ14_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : STM32H753.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SQR3_Register use record
      SQ10           at 0 range 0 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      SQ11           at 0 range 6 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      SQ12           at 0 range 12 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      SQ13           at 0 range 18 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      SQ14           at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype SQR4_SQ15_Field is STM32H753.UInt5;
   subtype SQR4_SQ16_Field is STM32H753.UInt5;

   --  ADC group regular sequencer ranks register 4
   type SQR4_Register is record
      --  ADC group regular sequencer rank 15
      SQ15           : SQR4_SQ15_Field := 16#0#;
      --  unspecified
      Reserved_5_5   : STM32H753.Bit := 16#0#;
      --  ADC group regular sequencer rank 16
      SQ16           : SQR4_SQ16_Field := 16#0#;
      --  unspecified
      Reserved_11_31 : STM32H753.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SQR4_Register use record
      SQ15           at 0 range 0 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      SQ16           at 0 range 6 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   subtype DR_RDATA_Field is STM32H753.UInt16;

   --  ADC group regular conversion data register
   type DR_Register is record
      --  Read-only. ADC group regular conversion data
      RDATA          : DR_RDATA_Field;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DR_Register use record
      RDATA          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype JSQR_JL_Field is STM32H753.UInt2;
   subtype JSQR_JEXTSEL_Field is STM32H753.UInt5;
   subtype JSQR_JEXTEN_Field is STM32H753.UInt2;
   subtype JSQR_JSQ1_Field is STM32H753.UInt5;
   subtype JSQR_JSQ2_Field is STM32H753.UInt5;
   subtype JSQR_JSQ3_Field is STM32H753.UInt5;
   subtype JSQR_JSQ4_Field is STM32H753.UInt5;

   --  ADC group injected sequencer register
   type JSQR_Register is record
      --  ADC group injected sequencer scan length
      JL             : JSQR_JL_Field := 16#0#;
      --  ADC group injected external trigger source
      JEXTSEL        : JSQR_JEXTSEL_Field := 16#0#;
      --  ADC group injected external trigger polarity
      JEXTEN         : JSQR_JEXTEN_Field := 16#0#;
      --  ADC group injected sequencer rank 1
      JSQ1           : JSQR_JSQ1_Field := 16#0#;
      --  unspecified
      Reserved_14_14 : STM32H753.Bit := 16#0#;
      --  ADC group injected sequencer rank 2
      JSQ2           : JSQR_JSQ2_Field := 16#0#;
      --  unspecified
      Reserved_20_20 : STM32H753.Bit := 16#0#;
      --  ADC group injected sequencer rank 3
      JSQ3           : JSQR_JSQ3_Field := 16#0#;
      --  unspecified
      Reserved_26_26 : STM32H753.Bit := 16#0#;
      --  ADC group injected sequencer rank 4
      JSQ4           : JSQR_JSQ4_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for JSQR_Register use record
      JL             at 0 range 0 .. 1;
      JEXTSEL        at 0 range 2 .. 6;
      JEXTEN         at 0 range 7 .. 8;
      JSQ1           at 0 range 9 .. 13;
      Reserved_14_14 at 0 range 14 .. 14;
      JSQ2           at 0 range 15 .. 19;
      Reserved_20_20 at 0 range 20 .. 20;
      JSQ3           at 0 range 21 .. 25;
      Reserved_26_26 at 0 range 26 .. 26;
      JSQ4           at 0 range 27 .. 31;
   end record;

   subtype OFR_OFFSET1_Field is STM32H753.UInt26;
   subtype OFR_OFFSET1_CH_Field is STM32H753.UInt5;

   --  ADC offset number 1 register
   type OFR_Register is record
      --  ADC offset number 1 offset level
      OFFSET1    : OFR_OFFSET1_Field := 16#0#;
      --  ADC offset number 1 channel selection
      OFFSET1_CH : OFR_OFFSET1_CH_Field := 16#0#;
      --  ADC offset number 1 enable
      SSATE      : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OFR_Register use record
      OFFSET1    at 0 range 0 .. 25;
      OFFSET1_CH at 0 range 26 .. 30;
      SSATE      at 0 range 31 .. 31;
   end record;

   subtype AWD2CR_AWD2CH_Field is STM32H753.UInt20;

   --  ADC analog watchdog 2 configuration register
   type AWD2CR_Register is record
      --  ADC analog watchdog 2 monitored channel selection
      AWD2CH         : AWD2CR_AWD2CH_Field := 16#0#;
      --  unspecified
      Reserved_20_31 : STM32H753.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AWD2CR_Register use record
      AWD2CH         at 0 range 0 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   subtype AWD3CR_AWD3CH_Field is STM32H753.UInt20;

   --  ADC analog watchdog 3 configuration register
   type AWD3CR_Register is record
      --  unspecified
      Reserved_0_0   : STM32H753.Bit := 16#0#;
      --  ADC analog watchdog 3 monitored channel selection
      AWD3CH         : AWD3CR_AWD3CH_Field := 16#0#;
      --  unspecified
      Reserved_21_31 : STM32H753.UInt11 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AWD3CR_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      AWD3CH         at 0 range 1 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   subtype LTR2_LTR2_Field is STM32H753.UInt26;

   --  ADC watchdog lower threshold register 2
   type LTR2_Register is record
      --  Analog watchdog 2 lower threshold
      LTR2           : LTR2_LTR2_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : STM32H753.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LTR2_Register use record
      LTR2           at 0 range 0 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype HTR2_HTR2_Field is STM32H753.UInt26;

   --  ADC watchdog higher threshold register 2
   type HTR2_Register is record
      --  Analog watchdog 2 higher threshold
      HTR2           : HTR2_HTR2_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : STM32H753.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for HTR2_Register use record
      HTR2           at 0 range 0 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype LTR3_LTR3_Field is STM32H753.UInt26;

   --  ADC watchdog lower threshold register 3
   type LTR3_Register is record
      --  Analog watchdog 3 lower threshold
      LTR3           : LTR3_LTR3_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : STM32H753.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for LTR3_Register use record
      LTR3           at 0 range 0 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype HTR3_HTR3_Field is STM32H753.UInt26;

   --  ADC watchdog higher threshold register 3
   type HTR3_Register is record
      --  Analog watchdog 3 higher threshold
      HTR3           : HTR3_HTR3_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : STM32H753.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for HTR3_Register use record
      HTR3           at 0 range 0 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype DIFSEL_DIFSEL_Field is STM32H753.UInt20;

   --  ADC channel differential or single-ended mode selection register
   type DIFSEL_Register is record
      --  ADC channel differential or single-ended mode for channel
      DIFSEL         : DIFSEL_DIFSEL_Field := 16#0#;
      --  unspecified
      Reserved_20_31 : STM32H753.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DIFSEL_Register use record
      DIFSEL         at 0 range 0 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   subtype CALFACT_CALFACT_S_Field is STM32H753.UInt11;
   subtype CALFACT_CALFACT_D_Field is STM32H753.UInt11;

   --  ADC calibration factors register
   type CALFACT_Register is record
      --  ADC calibration factor in single-ended mode
      CALFACT_S      : CALFACT_CALFACT_S_Field := 16#0#;
      --  unspecified
      Reserved_11_15 : STM32H753.UInt5 := 16#0#;
      --  ADC calibration factor in differential mode
      CALFACT_D      : CALFACT_CALFACT_D_Field := 16#0#;
      --  unspecified
      Reserved_27_31 : STM32H753.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CALFACT_Register use record
      CALFACT_S      at 0 range 0 .. 10;
      Reserved_11_15 at 0 range 11 .. 15;
      CALFACT_D      at 0 range 16 .. 26;
      Reserved_27_31 at 0 range 27 .. 31;
   end record;

   subtype CALFACT2_LINCALFACT_Field is STM32H753.UInt30;

   --  ADC Calibration Factor register 2
   type CALFACT2_Register is record
      --  Linearity Calibration Factor
      LINCALFACT     : CALFACT2_LINCALFACT_Field := 16#0#;
      --  unspecified
      Reserved_30_31 : STM32H753.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CALFACT2_Register use record
      LINCALFACT     at 0 range 0 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   --  ADC Common status register
   type CSR_Register is record
      --  Read-only. Master ADC ready
      ADRDY_MST      : Boolean;
      --  Read-only. End of Sampling phase flag of the master ADC
      EOSMP_MST      : Boolean;
      --  Read-only. End of regular conversion of the master ADC
      EOC_MST        : Boolean;
      --  Read-only. End of regular sequence flag of the master ADC
      EOS_MST        : Boolean;
      --  Read-only. Overrun flag of the master ADC
      OVR_MST        : Boolean;
      --  Read-only. End of injected conversion flag of the master ADC
      JEOC_MST       : Boolean;
      --  Read-only. End of injected sequence flag of the master ADC
      JEOS_MST       : Boolean;
      --  Read-only. Analog watchdog 1 flag of the master ADC
      AWD1_MST       : Boolean;
      --  Read-only. Analog watchdog 2 flag of the master ADC
      AWD2_MST       : Boolean;
      --  Read-only. Analog watchdog 3 flag of the master ADC
      AWD3_MST       : Boolean;
      --  Read-only. Injected Context Queue Overflow flag of the master ADC
      JQOVF_MST      : Boolean;
      --  unspecified
      Reserved_11_15 : STM32H753.UInt5;
      --  Read-only. Slave ADC ready
      ADRDY_SLV      : Boolean;
      --  Read-only. End of Sampling phase flag of the slave ADC
      EOSMP_SLV      : Boolean;
      --  Read-only. End of regular conversion of the slave ADC
      EOC_SLV        : Boolean;
      --  Read-only. End of regular sequence flag of the slave ADC
      EOS_SLV        : Boolean;
      --  Read-only. Overrun flag of the slave ADC
      OVR_SLV        : Boolean;
      --  Read-only. End of injected conversion flag of the slave ADC
      JEOC_SLV       : Boolean;
      --  Read-only. End of injected sequence flag of the slave ADC
      JEOS_SLV       : Boolean;
      --  Read-only. Analog watchdog 1 flag of the slave ADC
      AWD1_SLV       : Boolean;
      --  Read-only. Analog watchdog 2 flag of the slave ADC
      AWD2_SLV       : Boolean;
      --  Read-only. Analog watchdog 3 flag of the slave ADC
      AWD3_SLV       : Boolean;
      --  Read-only. Injected Context Queue Overflow flag of the slave ADC
      JQOVF_SLV      : Boolean;
      --  unspecified
      Reserved_27_31 : STM32H753.UInt5;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CSR_Register use record
      ADRDY_MST      at 0 range 0 .. 0;
      EOSMP_MST      at 0 range 1 .. 1;
      EOC_MST        at 0 range 2 .. 2;
      EOS_MST        at 0 range 3 .. 3;
      OVR_MST        at 0 range 4 .. 4;
      JEOC_MST       at 0 range 5 .. 5;
      JEOS_MST       at 0 range 6 .. 6;
      AWD1_MST       at 0 range 7 .. 7;
      AWD2_MST       at 0 range 8 .. 8;
      AWD3_MST       at 0 range 9 .. 9;
      JQOVF_MST      at 0 range 10 .. 10;
      Reserved_11_15 at 0 range 11 .. 15;
      ADRDY_SLV      at 0 range 16 .. 16;
      EOSMP_SLV      at 0 range 17 .. 17;
      EOC_SLV        at 0 range 18 .. 18;
      EOS_SLV        at 0 range 19 .. 19;
      OVR_SLV        at 0 range 20 .. 20;
      JEOC_SLV       at 0 range 21 .. 21;
      JEOS_SLV       at 0 range 22 .. 22;
      AWD1_SLV       at 0 range 23 .. 23;
      AWD2_SLV       at 0 range 24 .. 24;
      AWD3_SLV       at 0 range 25 .. 25;
      JQOVF_SLV      at 0 range 26 .. 26;
      Reserved_27_31 at 0 range 27 .. 31;
   end record;

   subtype CCR_DUAL_Field is STM32H753.UInt5;
   subtype CCR_DELAY_Field is STM32H753.UInt4;
   subtype CCR_DAMDF_Field is STM32H753.UInt2;
   subtype CCR_CKMODE_Field is STM32H753.UInt2;
   subtype CCR_PRESC_Field is STM32H753.UInt4;

   --  ADC common control register
   type CCR_Register is record
      --  Dual ADC mode selection
      DUAL           : CCR_DUAL_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : STM32H753.UInt3 := 16#0#;
      --  Delay between 2 sampling phases
      DELAY_k        : CCR_DELAY_Field := 16#0#;
      --  unspecified
      Reserved_12_13 : STM32H753.UInt2 := 16#0#;
      --  Dual ADC Mode Data Format
      DAMDF          : CCR_DAMDF_Field := 16#0#;
      --  ADC clock mode
      CKMODE         : CCR_CKMODE_Field := 16#0#;
      --  ADC prescaler
      PRESC          : CCR_PRESC_Field := 16#0#;
      --  VREFINT enable
      VREFEN         : Boolean := False;
      --  Temperature sensor enable
      VSENSEEN       : Boolean := False;
      --  VBAT enable
      VBATEN         : Boolean := False;
      --  unspecified
      Reserved_25_31 : STM32H753.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCR_Register use record
      DUAL           at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      DELAY_k        at 0 range 8 .. 11;
      Reserved_12_13 at 0 range 12 .. 13;
      DAMDF          at 0 range 14 .. 15;
      CKMODE         at 0 range 16 .. 17;
      PRESC          at 0 range 18 .. 21;
      VREFEN         at 0 range 22 .. 22;
      VSENSEEN       at 0 range 23 .. 23;
      VBATEN         at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   subtype CDR_RDATA_MST_Field is STM32H753.UInt16;
   subtype CDR_RDATA_SLV_Field is STM32H753.UInt16;

   --  ADC common regular data register for dual and triple modes
   type CDR_Register is record
      --  Read-only. Regular data of the master ADC
      RDATA_MST : CDR_RDATA_MST_Field;
      --  Read-only. Regular data of the slave ADC
      RDATA_SLV : CDR_RDATA_SLV_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CDR_Register use record
      RDATA_MST at 0 range 0 .. 15;
      RDATA_SLV at 0 range 16 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Analog to Digital Converter
   type ADC1_Peripheral is record
      --  ADC interrupt and status register
      ISR      : aliased ISR_Register;
      --  ADC interrupt enable register
      IER      : aliased IER_Register;
      --  ADC control register
      CR       : aliased CR_Register;
      --  ADC configuration register 1
      CFGR     : aliased CFGR_Register;
      --  ADC configuration register 2
      CFGR2    : aliased CFGR2_Register;
      --  ADC sampling time register 1
      SMPR1    : aliased SMPR1_Register;
      --  ADC sampling time register 2
      SMPR2    : aliased SMPR2_Register;
      --  ADC pre channel selection register
      PCSEL    : aliased PCSEL_Register;
      --  ADC analog watchdog 1 threshold register
      LTR1     : aliased LTR1_Register;
      --  ADC analog watchdog 2 threshold register
      LHTR1    : aliased LHTR1_Register;
      --  ADC group regular sequencer ranks register 1
      SQR1     : aliased SQR1_Register;
      --  ADC group regular sequencer ranks register 2
      SQR2     : aliased SQR2_Register;
      --  ADC group regular sequencer ranks register 3
      SQR3     : aliased SQR3_Register;
      --  ADC group regular sequencer ranks register 4
      SQR4     : aliased SQR4_Register;
      --  ADC group regular conversion data register
      DR       : aliased DR_Register;
      --  ADC group injected sequencer register
      JSQR     : aliased JSQR_Register;
      --  ADC offset number 1 register
      OFR1     : aliased OFR_Register;
      --  ADC offset number 2 register
      OFR2     : aliased OFR_Register;
      --  ADC offset number 3 register
      OFR3     : aliased OFR_Register;
      --  ADC offset number 4 register
      OFR4     : aliased OFR_Register;
      --  ADC group injected sequencer rank 1 register
      JDR1     : aliased STM32H753.UInt32;
      --  ADC group injected sequencer rank 2 register
      JDR2     : aliased STM32H753.UInt32;
      --  ADC group injected sequencer rank 3 register
      JDR3     : aliased STM32H753.UInt32;
      --  ADC group injected sequencer rank 4 register
      JDR4     : aliased STM32H753.UInt32;
      --  ADC analog watchdog 2 configuration register
      AWD2CR   : aliased AWD2CR_Register;
      --  ADC analog watchdog 3 configuration register
      AWD3CR   : aliased AWD3CR_Register;
      --  ADC watchdog lower threshold register 2
      LTR2     : aliased LTR2_Register;
      --  ADC watchdog higher threshold register 2
      HTR2     : aliased HTR2_Register;
      --  ADC watchdog lower threshold register 3
      LTR3     : aliased LTR3_Register;
      --  ADC watchdog higher threshold register 3
      HTR3     : aliased HTR3_Register;
      --  ADC channel differential or single-ended mode selection register
      DIFSEL   : aliased DIFSEL_Register;
      --  ADC calibration factors register
      CALFACT  : aliased CALFACT_Register;
      --  ADC Calibration Factor register 2
      CALFACT2 : aliased CALFACT2_Register;
   end record
     with Volatile;

   for ADC1_Peripheral use record
      ISR      at 16#0# range 0 .. 31;
      IER      at 16#4# range 0 .. 31;
      CR       at 16#8# range 0 .. 31;
      CFGR     at 16#C# range 0 .. 31;
      CFGR2    at 16#10# range 0 .. 31;
      SMPR1    at 16#14# range 0 .. 31;
      SMPR2    at 16#18# range 0 .. 31;
      PCSEL    at 16#1C# range 0 .. 31;
      LTR1     at 16#20# range 0 .. 31;
      LHTR1    at 16#24# range 0 .. 31;
      SQR1     at 16#30# range 0 .. 31;
      SQR2     at 16#34# range 0 .. 31;
      SQR3     at 16#38# range 0 .. 31;
      SQR4     at 16#3C# range 0 .. 31;
      DR       at 16#40# range 0 .. 31;
      JSQR     at 16#4C# range 0 .. 31;
      OFR1     at 16#60# range 0 .. 31;
      OFR2     at 16#64# range 0 .. 31;
      OFR3     at 16#68# range 0 .. 31;
      OFR4     at 16#6C# range 0 .. 31;
      JDR1     at 16#80# range 0 .. 31;
      JDR2     at 16#84# range 0 .. 31;
      JDR3     at 16#88# range 0 .. 31;
      JDR4     at 16#8C# range 0 .. 31;
      AWD2CR   at 16#A0# range 0 .. 31;
      AWD3CR   at 16#A4# range 0 .. 31;
      LTR2     at 16#B0# range 0 .. 31;
      HTR2     at 16#B4# range 0 .. 31;
      LTR3     at 16#B8# range 0 .. 31;
      HTR3     at 16#BC# range 0 .. 31;
      DIFSEL   at 16#C0# range 0 .. 31;
      CALFACT  at 16#C4# range 0 .. 31;
      CALFACT2 at 16#C8# range 0 .. 31;
   end record;

   --  Analog to Digital Converter
   ADC1_Periph : aliased ADC1_Peripheral
     with Import, Address => ADC1_Base;

   --  Analog to Digital Converter
   ADC2_Periph : aliased ADC1_Peripheral
     with Import, Address => ADC2_Base;

   --  Analog to Digital Converter
   ADC3_Periph : aliased ADC1_Peripheral
     with Import, Address => ADC3_Base;

   --  Analog-to-Digital Converter
   type ADC12_Common_Peripheral is record
      --  ADC Common status register
      CSR  : aliased CSR_Register;
      --  ADC common control register
      CCR  : aliased CCR_Register;
      --  ADC common regular data register for dual and triple modes
      CDR  : aliased CDR_Register;
      --  ADC x common regular data register for 32-bit dual mode
      CDR2 : aliased STM32H753.UInt32;
   end record
     with Volatile;

   for ADC12_Common_Peripheral use record
      CSR  at 16#0# range 0 .. 31;
      CCR  at 16#8# range 0 .. 31;
      CDR  at 16#C# range 0 .. 31;
      CDR2 at 16#10# range 0 .. 31;
   end record;

   --  Analog-to-Digital Converter
   ADC12_Common_Periph : aliased ADC12_Common_Peripheral
     with Import, Address => ADC12_Common_Base;

   --  Analog-to-Digital Converter
   ADC3_Common_Periph : aliased ADC12_Common_Peripheral
     with Import, Address => ADC3_Common_Base;

end STM32H753.ADC;
