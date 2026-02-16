pragma Style_Checks (Off);

--  This spec has been automatically generated from STM32H753.svd


with System;

package STM32H753.COMP1 is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   --  Comparator status register
   type SR_Register is record
      --  Read-only. COMP channel 1 output status bit
      C1VAL          : Boolean;
      --  Read-only. COMP channel 2 output status bit
      C2VAL          : Boolean;
      --  unspecified
      Reserved_2_15  : STM32H753.UInt14;
      --  Read-only. COMP channel 1 Interrupt Flag
      C1IF           : Boolean;
      --  Read-only. COMP channel 2 Interrupt Flag
      C2IF           : Boolean;
      --  unspecified
      Reserved_18_31 : STM32H753.UInt14;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register use record
      C1VAL          at 0 range 0 .. 0;
      C2VAL          at 0 range 1 .. 1;
      Reserved_2_15  at 0 range 2 .. 15;
      C1IF           at 0 range 16 .. 16;
      C2IF           at 0 range 17 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   --  Comparator interrupt clear flag register
   type ICFR_Register is record
      --  unspecified
      Reserved_0_15  : STM32H753.UInt16 := 16#0#;
      --  Write-only. Clear COMP channel 1 Interrupt Flag
      CC1IF          : Boolean := False;
      --  Write-only. Clear COMP channel 2 Interrupt Flag
      CC2IF          : Boolean := False;
      --  unspecified
      Reserved_18_31 : STM32H753.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ICFR_Register use record
      Reserved_0_15  at 0 range 0 .. 15;
      CC1IF          at 0 range 16 .. 16;
      CC2IF          at 0 range 17 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   subtype OR_AFOP_Field is STM32H753.UInt11;
   subtype OR_OR_Field is STM32H753.UInt21;

   --  Comparator option register
   type OR_Register is record
      --  Selection of source for alternate function of output ports
      AFOP : OR_AFOP_Field := 16#0#;
      --  Option Register
      OR_k : OR_OR_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OR_Register use record
      AFOP at 0 range 0 .. 10;
      OR_k at 0 range 11 .. 31;
   end record;

   subtype CFGR1_HYST_Field is STM32H753.UInt2;
   subtype CFGR1_PWRMODE_Field is STM32H753.UInt2;
   subtype CFGR1_INMSEL_Field is STM32H753.UInt3;
   subtype CFGR1_BLANKING_Field is STM32H753.UInt4;

   --  Comparator configuration register 1
   type CFGR1_Register is record
      --  COMP channel 1 enable bit
      EN             : Boolean := False;
      --  Scaler bridge enable
      BRGEN          : Boolean := False;
      --  Voltage scaler enable bit
      SCALEN         : Boolean := False;
      --  COMP channel 1 polarity selection bit
      POLARITY       : Boolean := False;
      --  unspecified
      Reserved_4_5   : STM32H753.UInt2 := 16#0#;
      --  COMP channel 1 interrupt enable
      ITEN           : Boolean := False;
      --  unspecified
      Reserved_7_7   : STM32H753.Bit := 16#0#;
      --  COMP channel 1 hysteresis selection bits
      HYST           : CFGR1_HYST_Field := 16#0#;
      --  unspecified
      Reserved_10_11 : STM32H753.UInt2 := 16#0#;
      --  Power Mode of the COMP channel 1
      PWRMODE        : CFGR1_PWRMODE_Field := 16#0#;
      --  unspecified
      Reserved_14_15 : STM32H753.UInt2 := 16#0#;
      --  COMP channel 1 inverting input selection field
      INMSEL         : CFGR1_INMSEL_Field := 16#0#;
      --  unspecified
      Reserved_19_19 : STM32H753.Bit := 16#0#;
      --  COMP channel 1 non-inverting input selection bit
      INPSEL         : Boolean := False;
      --  unspecified
      Reserved_21_23 : STM32H753.UInt3 := 16#0#;
      --  COMP channel 1 blanking source selection bits
      BLANKING       : CFGR1_BLANKING_Field := 16#0#;
      --  unspecified
      Reserved_28_30 : STM32H753.UInt3 := 16#0#;
      --  Lock bit
      LOCK           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CFGR1_Register use record
      EN             at 0 range 0 .. 0;
      BRGEN          at 0 range 1 .. 1;
      SCALEN         at 0 range 2 .. 2;
      POLARITY       at 0 range 3 .. 3;
      Reserved_4_5   at 0 range 4 .. 5;
      ITEN           at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      HYST           at 0 range 8 .. 9;
      Reserved_10_11 at 0 range 10 .. 11;
      PWRMODE        at 0 range 12 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      INMSEL         at 0 range 16 .. 18;
      Reserved_19_19 at 0 range 19 .. 19;
      INPSEL         at 0 range 20 .. 20;
      Reserved_21_23 at 0 range 21 .. 23;
      BLANKING       at 0 range 24 .. 27;
      Reserved_28_30 at 0 range 28 .. 30;
      LOCK           at 0 range 31 .. 31;
   end record;

   subtype CFGR2_HYST_Field is STM32H753.UInt2;
   subtype CFGR2_PWRMODE_Field is STM32H753.UInt2;
   subtype CFGR2_INMSEL_Field is STM32H753.UInt3;
   subtype CFGR2_BLANKING_Field is STM32H753.UInt4;

   --  Comparator configuration register 2
   type CFGR2_Register is record
      --  COMP channel 1 enable bit
      EN             : Boolean := False;
      --  Scaler bridge enable
      BRGEN          : Boolean := False;
      --  Voltage scaler enable bit
      SCALEN         : Boolean := False;
      --  COMP channel 1 polarity selection bit
      POLARITY       : Boolean := False;
      --  Window comparator mode selection bit
      WINMODE        : Boolean := False;
      --  unspecified
      Reserved_5_5   : STM32H753.Bit := 16#0#;
      --  COMP channel 1 interrupt enable
      ITEN           : Boolean := False;
      --  unspecified
      Reserved_7_7   : STM32H753.Bit := 16#0#;
      --  COMP channel 1 hysteresis selection bits
      HYST           : CFGR2_HYST_Field := 16#0#;
      --  unspecified
      Reserved_10_11 : STM32H753.UInt2 := 16#0#;
      --  Power Mode of the COMP channel 1
      PWRMODE        : CFGR2_PWRMODE_Field := 16#0#;
      --  unspecified
      Reserved_14_15 : STM32H753.UInt2 := 16#0#;
      --  COMP channel 1 inverting input selection field
      INMSEL         : CFGR2_INMSEL_Field := 16#0#;
      --  unspecified
      Reserved_19_19 : STM32H753.Bit := 16#0#;
      --  COMP channel 1 non-inverting input selection bit
      INPSEL         : Boolean := False;
      --  unspecified
      Reserved_21_23 : STM32H753.UInt3 := 16#0#;
      --  COMP channel 1 blanking source selection bits
      BLANKING       : CFGR2_BLANKING_Field := 16#0#;
      --  unspecified
      Reserved_28_30 : STM32H753.UInt3 := 16#0#;
      --  Lock bit
      LOCK           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CFGR2_Register use record
      EN             at 0 range 0 .. 0;
      BRGEN          at 0 range 1 .. 1;
      SCALEN         at 0 range 2 .. 2;
      POLARITY       at 0 range 3 .. 3;
      WINMODE        at 0 range 4 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      ITEN           at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      HYST           at 0 range 8 .. 9;
      Reserved_10_11 at 0 range 10 .. 11;
      PWRMODE        at 0 range 12 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      INMSEL         at 0 range 16 .. 18;
      Reserved_19_19 at 0 range 19 .. 19;
      INPSEL         at 0 range 20 .. 20;
      Reserved_21_23 at 0 range 21 .. 23;
      BLANKING       at 0 range 24 .. 27;
      Reserved_28_30 at 0 range 28 .. 30;
      LOCK           at 0 range 31 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  COMP1
   type COMP1_Peripheral is record
      --  Comparator status register
      SR    : aliased SR_Register;
      --  Comparator interrupt clear flag register
      ICFR  : aliased ICFR_Register;
      --  Comparator option register
      OR_k  : aliased OR_Register;
      --  Comparator configuration register 1
      CFGR1 : aliased CFGR1_Register;
      --  Comparator configuration register 2
      CFGR2 : aliased CFGR2_Register;
   end record
     with Volatile;

   for COMP1_Peripheral use record
      SR    at 16#0# range 0 .. 31;
      ICFR  at 16#4# range 0 .. 31;
      OR_k  at 16#8# range 0 .. 31;
      CFGR1 at 16#C# range 0 .. 31;
      CFGR2 at 16#10# range 0 .. 31;
   end record;

   --  COMP1
   COMP1_Periph : aliased COMP1_Peripheral
     with Import, Address => COMP1_Base;

end STM32H753.COMP1;
