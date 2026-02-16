pragma Style_Checks (Off);

--  This spec has been automatically generated from STM32H753.svd


with System;

package STM32H753.OPAMP is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   subtype OPAMP1_CSR_VP_SEL_Field is STM32H753.UInt2;
   subtype OPAMP1_CSR_VM_SEL_Field is STM32H753.UInt2;
   subtype OPAMP1_CSR_CALSEL_Field is STM32H753.UInt2;
   subtype OPAMP1_CSR_PGA_GAIN_Field is STM32H753.UInt4;

   --  OPAMP1 control/status register
   type OPAMP1_CSR_Register is record
      --  Operational amplifier Enable
      OPAEN          : Boolean := False;
      --  Force internal reference on VP (reserved for test
      FORCE_VP       : Boolean := False;
      --  Operational amplifier PGA mode
      VP_SEL         : OPAMP1_CSR_VP_SEL_Field := 16#0#;
      --  unspecified
      Reserved_4_4   : STM32H753.Bit := 16#0#;
      --  Inverting input selection
      VM_SEL         : OPAMP1_CSR_VM_SEL_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : STM32H753.Bit := 16#0#;
      --  Operational amplifier high-speed mode
      OPAHSM         : Boolean := False;
      --  unspecified
      Reserved_9_10  : STM32H753.UInt2 := 16#0#;
      --  Calibration mode enabled
      CALON          : Boolean := False;
      --  Calibration selection
      CALSEL         : OPAMP1_CSR_CALSEL_Field := 16#0#;
      --  allows to switch from AOP offset trimmed values to AOP offset
      PGA_GAIN       : OPAMP1_CSR_PGA_GAIN_Field := 16#0#;
      --  User trimming enable
      USERTRIM       : Boolean := False;
      --  unspecified
      Reserved_19_28 : STM32H753.UInt10 := 16#0#;
      --  OPAMP calibration reference voltage output control (reserved for
      --  test)
      TSTREF         : Boolean := False;
      --  Operational amplifier calibration output
      CALOUT         : Boolean := False;
      --  unspecified
      Reserved_31_31 : STM32H753.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OPAMP1_CSR_Register use record
      OPAEN          at 0 range 0 .. 0;
      FORCE_VP       at 0 range 1 .. 1;
      VP_SEL         at 0 range 2 .. 3;
      Reserved_4_4   at 0 range 4 .. 4;
      VM_SEL         at 0 range 5 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      OPAHSM         at 0 range 8 .. 8;
      Reserved_9_10  at 0 range 9 .. 10;
      CALON          at 0 range 11 .. 11;
      CALSEL         at 0 range 12 .. 13;
      PGA_GAIN       at 0 range 14 .. 17;
      USERTRIM       at 0 range 18 .. 18;
      Reserved_19_28 at 0 range 19 .. 28;
      TSTREF         at 0 range 29 .. 29;
      CALOUT         at 0 range 30 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype OPAMP1_OTR_TRIMOFFSETN_Field is STM32H753.UInt5;
   subtype OPAMP1_OTR_TRIMOFFSETP_Field is STM32H753.UInt5;

   --  OPAMP1 offset trimming register in normal mode
   type OPAMP1_OTR_Register is record
      --  Trim for NMOS differential pairs
      TRIMOFFSETN    : OPAMP1_OTR_TRIMOFFSETN_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : STM32H753.UInt3 := 16#0#;
      --  Trim for PMOS differential pairs
      TRIMOFFSETP    : OPAMP1_OTR_TRIMOFFSETP_Field := 16#0#;
      --  unspecified
      Reserved_13_31 : STM32H753.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OPAMP1_OTR_Register use record
      TRIMOFFSETN    at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      TRIMOFFSETP    at 0 range 8 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   subtype OPAMP1_HSOTR_TRIMLPOFFSETN_Field is STM32H753.UInt5;
   subtype OPAMP1_HSOTR_TRIMLPOFFSETP_Field is STM32H753.UInt5;

   --  OPAMP1 offset trimming register in low-power mode
   type OPAMP1_HSOTR_Register is record
      --  Trim for NMOS differential pairs
      TRIMLPOFFSETN  : OPAMP1_HSOTR_TRIMLPOFFSETN_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : STM32H753.UInt3 := 16#0#;
      --  Trim for PMOS differential pairs
      TRIMLPOFFSETP  : OPAMP1_HSOTR_TRIMLPOFFSETP_Field := 16#0#;
      --  unspecified
      Reserved_13_31 : STM32H753.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OPAMP1_HSOTR_Register use record
      TRIMLPOFFSETN  at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      TRIMLPOFFSETP  at 0 range 8 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   subtype OPAMP2_CSR_VM_SEL_Field is STM32H753.UInt2;
   subtype OPAMP2_CSR_CALSEL_Field is STM32H753.UInt2;
   subtype OPAMP2_CSR_PGA_GAIN_Field is STM32H753.UInt4;

   --  OPAMP2 control/status register
   type OPAMP2_CSR_Register is record
      --  Operational amplifier Enable
      OPAEN          : Boolean := False;
      --  Force internal reference on VP (reserved for test)
      FORCE_VP       : Boolean := False;
      --  unspecified
      Reserved_2_4   : STM32H753.UInt3 := 16#0#;
      --  Inverting input selection
      VM_SEL         : OPAMP2_CSR_VM_SEL_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : STM32H753.Bit := 16#0#;
      --  Operational amplifier high-speed mode
      OPAHSM         : Boolean := False;
      --  unspecified
      Reserved_9_10  : STM32H753.UInt2 := 16#0#;
      --  Calibration mode enabled
      CALON          : Boolean := False;
      --  Calibration selection
      CALSEL         : OPAMP2_CSR_CALSEL_Field := 16#0#;
      --  Operational amplifier Programmable amplifier gain value
      PGA_GAIN       : OPAMP2_CSR_PGA_GAIN_Field := 16#0#;
      --  User trimming enable
      USERTRIM       : Boolean := False;
      --  unspecified
      Reserved_19_28 : STM32H753.UInt10 := 16#0#;
      --  OPAMP calibration reference voltage output control (reserved for
      --  test)
      TSTREF         : Boolean := False;
      --  Operational amplifier calibration output
      CALOUT         : Boolean := False;
      --  unspecified
      Reserved_31_31 : STM32H753.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OPAMP2_CSR_Register use record
      OPAEN          at 0 range 0 .. 0;
      FORCE_VP       at 0 range 1 .. 1;
      Reserved_2_4   at 0 range 2 .. 4;
      VM_SEL         at 0 range 5 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      OPAHSM         at 0 range 8 .. 8;
      Reserved_9_10  at 0 range 9 .. 10;
      CALON          at 0 range 11 .. 11;
      CALSEL         at 0 range 12 .. 13;
      PGA_GAIN       at 0 range 14 .. 17;
      USERTRIM       at 0 range 18 .. 18;
      Reserved_19_28 at 0 range 19 .. 28;
      TSTREF         at 0 range 29 .. 29;
      CALOUT         at 0 range 30 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype OPAMP2_OTR_TRIMOFFSETN_Field is STM32H753.UInt5;
   subtype OPAMP2_OTR_TRIMOFFSETP_Field is STM32H753.UInt5;

   --  OPAMP2 offset trimming register in normal mode
   type OPAMP2_OTR_Register is record
      --  Trim for NMOS differential pairs
      TRIMOFFSETN    : OPAMP2_OTR_TRIMOFFSETN_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : STM32H753.UInt3 := 16#0#;
      --  Trim for PMOS differential pairs
      TRIMOFFSETP    : OPAMP2_OTR_TRIMOFFSETP_Field := 16#0#;
      --  unspecified
      Reserved_13_31 : STM32H753.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OPAMP2_OTR_Register use record
      TRIMOFFSETN    at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      TRIMOFFSETP    at 0 range 8 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   subtype OPAMP2_HSOTR_TRIMLPOFFSETN_Field is STM32H753.UInt5;
   subtype OPAMP2_HSOTR_TRIMLPOFFSETP_Field is STM32H753.UInt5;

   --  OPAMP2 offset trimming register in low-power mode
   type OPAMP2_HSOTR_Register is record
      --  Trim for NMOS differential pairs
      TRIMLPOFFSETN  : OPAMP2_HSOTR_TRIMLPOFFSETN_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : STM32H753.UInt3 := 16#0#;
      --  Trim for PMOS differential pairs
      TRIMLPOFFSETP  : OPAMP2_HSOTR_TRIMLPOFFSETP_Field := 16#0#;
      --  unspecified
      Reserved_13_31 : STM32H753.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OPAMP2_HSOTR_Register use record
      TRIMLPOFFSETN  at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      TRIMLPOFFSETP  at 0 range 8 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Operational amplifiers
   type OPAMP_Peripheral is record
      --  OPAMP1 control/status register
      OPAMP1_CSR   : aliased OPAMP1_CSR_Register;
      --  OPAMP1 offset trimming register in normal mode
      OPAMP1_OTR   : aliased OPAMP1_OTR_Register;
      --  OPAMP1 offset trimming register in low-power mode
      OPAMP1_HSOTR : aliased OPAMP1_HSOTR_Register;
      --  OPAMP2 control/status register
      OPAMP2_CSR   : aliased OPAMP2_CSR_Register;
      --  OPAMP2 offset trimming register in normal mode
      OPAMP2_OTR   : aliased OPAMP2_OTR_Register;
      --  OPAMP2 offset trimming register in low-power mode
      OPAMP2_HSOTR : aliased OPAMP2_HSOTR_Register;
   end record
     with Volatile;

   for OPAMP_Peripheral use record
      OPAMP1_CSR   at 16#0# range 0 .. 31;
      OPAMP1_OTR   at 16#4# range 0 .. 31;
      OPAMP1_HSOTR at 16#8# range 0 .. 31;
      OPAMP2_CSR   at 16#10# range 0 .. 31;
      OPAMP2_OTR   at 16#14# range 0 .. 31;
      OPAMP2_HSOTR at 16#18# range 0 .. 31;
   end record;

   --  Operational amplifiers
   OPAMP_Periph : aliased OPAMP_Peripheral
     with Import, Address => OPAMP_Base;

end STM32H753.OPAMP;
