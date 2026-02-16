pragma Style_Checks (Off);

--  This spec has been automatically generated from STM32H753.svd


with System;

package STM32H753.SYSCFG is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   subtype PMCR_EPIS_Field is STM32H753.UInt3;

   --  peripheral mode configuration register
   type PMCR_Register is record
      --  I2C1 Fm+
      I2C1FMP        : Boolean := False;
      --  I2C2 Fm+
      I2C2FMP        : Boolean := False;
      --  I2C3 Fm+
      I2C3FMP        : Boolean := False;
      --  I2C4 Fm+
      I2C4FMP        : Boolean := False;
      --  PB(6) Fm+
      PB6FMP         : Boolean := False;
      --  PB(7) Fast Mode Plus
      PB7FMP         : Boolean := False;
      --  PB(8) Fast Mode Plus
      PB8FMP         : Boolean := False;
      --  PB(9) Fm+
      PB9FMP         : Boolean := False;
      --  Booster Enable
      BOOSTE         : Boolean := False;
      --  Analog switch supply voltage selection
      BOOSTVDDSEL    : Boolean := False;
      --  unspecified
      Reserved_10_20 : STM32H753.UInt11 := 16#0#;
      --  Ethernet PHY Interface Selection
      EPIS           : PMCR_EPIS_Field := 16#0#;
      --  PA0 Switch Open
      PA0SO          : Boolean := False;
      --  PA1 Switch Open
      PA1SO          : Boolean := False;
      --  PC2 Switch Open
      PC2SO          : Boolean := False;
      --  PC3 Switch Open
      PC3SO          : Boolean := False;
      --  unspecified
      Reserved_28_31 : STM32H753.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PMCR_Register use record
      I2C1FMP        at 0 range 0 .. 0;
      I2C2FMP        at 0 range 1 .. 1;
      I2C3FMP        at 0 range 2 .. 2;
      I2C4FMP        at 0 range 3 .. 3;
      PB6FMP         at 0 range 4 .. 4;
      PB7FMP         at 0 range 5 .. 5;
      PB8FMP         at 0 range 6 .. 6;
      PB9FMP         at 0 range 7 .. 7;
      BOOSTE         at 0 range 8 .. 8;
      BOOSTVDDSEL    at 0 range 9 .. 9;
      Reserved_10_20 at 0 range 10 .. 20;
      EPIS           at 0 range 21 .. 23;
      PA0SO          at 0 range 24 .. 24;
      PA1SO          at 0 range 25 .. 25;
      PC2SO          at 0 range 26 .. 26;
      PC3SO          at 0 range 27 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  EXTICR1_EXTI array element
   subtype EXTICR1_EXTI_Element is STM32H753.UInt4;

   --  EXTICR1_EXTI array
   type EXTICR1_EXTI_Field_Array is array (0 .. 3) of EXTICR1_EXTI_Element
     with Component_Size => 4, Size => 16;

   --  Type definition for EXTICR1_EXTI
   type EXTICR1_EXTI_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTI as a value
            Val : STM32H753.UInt16;
         when True =>
            --  EXTI as an array
            Arr : EXTICR1_EXTI_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for EXTICR1_EXTI_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  external interrupt configuration register 1
   type EXTICR1_Register is record
      --  EXTI x configuration (x = 0 to 3)
      EXTI           : EXTICR1_EXTI_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EXTICR1_Register use record
      EXTI           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  EXTICR2_EXTI array element
   subtype EXTICR2_EXTI_Element is STM32H753.UInt4;

   --  EXTICR2_EXTI array
   type EXTICR2_EXTI_Field_Array is array (4 .. 7) of EXTICR2_EXTI_Element
     with Component_Size => 4, Size => 16;

   --  Type definition for EXTICR2_EXTI
   type EXTICR2_EXTI_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTI as a value
            Val : STM32H753.UInt16;
         when True =>
            --  EXTI as an array
            Arr : EXTICR2_EXTI_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for EXTICR2_EXTI_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  external interrupt configuration register 2
   type EXTICR2_Register is record
      --  EXTI x configuration (x = 4 to 7)
      EXTI           : EXTICR2_EXTI_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EXTICR2_Register use record
      EXTI           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  EXTICR3_EXTI array element
   subtype EXTICR3_EXTI_Element is STM32H753.UInt4;

   --  EXTICR3_EXTI array
   type EXTICR3_EXTI_Field_Array is array (8 .. 11) of EXTICR3_EXTI_Element
     with Component_Size => 4, Size => 16;

   --  Type definition for EXTICR3_EXTI
   type EXTICR3_EXTI_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTI as a value
            Val : STM32H753.UInt16;
         when True =>
            --  EXTI as an array
            Arr : EXTICR3_EXTI_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for EXTICR3_EXTI_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  external interrupt configuration register 3
   type EXTICR3_Register is record
      --  EXTI x configuration (x = 8 to 11)
      EXTI           : EXTICR3_EXTI_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EXTICR3_Register use record
      EXTI           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  EXTICR4_EXTI array element
   subtype EXTICR4_EXTI_Element is STM32H753.UInt4;

   --  EXTICR4_EXTI array
   type EXTICR4_EXTI_Field_Array is array (12 .. 15) of EXTICR4_EXTI_Element
     with Component_Size => 4, Size => 16;

   --  Type definition for EXTICR4_EXTI
   type EXTICR4_EXTI_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EXTI as a value
            Val : STM32H753.UInt16;
         when True =>
            --  EXTI as an array
            Arr : EXTICR4_EXTI_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for EXTICR4_EXTI_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  external interrupt configuration register 4
   type EXTICR4_Register is record
      --  EXTI x configuration (x = 12 to 15)
      EXTI           : EXTICR4_EXTI_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for EXTICR4_Register use record
      EXTI           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  compensation cell control/status register
   type CCCSR_Register is record
      --  enable
      EN             : Boolean := False;
      --  Code selection
      CS             : Boolean := False;
      --  unspecified
      Reserved_2_7   : STM32H753.UInt6 := 16#0#;
      --  Compensation cell ready flag
      READY          : Boolean := False;
      --  unspecified
      Reserved_9_15  : STM32H753.UInt7 := 16#0#;
      --  High-speed at low-voltage
      HSLV           : Boolean := False;
      --  unspecified
      Reserved_17_31 : STM32H753.UInt15 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCCSR_Register use record
      EN             at 0 range 0 .. 0;
      CS             at 0 range 1 .. 1;
      Reserved_2_7   at 0 range 2 .. 7;
      READY          at 0 range 8 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      HSLV           at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype CCVR_NCV_Field is STM32H753.UInt4;
   subtype CCVR_PCV_Field is STM32H753.UInt4;

   --  SYSCFG compensation cell value register
   type CCVR_Register is record
      --  Read-only. NMOS compensation value
      NCV           : CCVR_NCV_Field;
      --  Read-only. PMOS compensation value
      PCV           : CCVR_PCV_Field;
      --  unspecified
      Reserved_8_31 : STM32H753.UInt24;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCVR_Register use record
      NCV           at 0 range 0 .. 3;
      PCV           at 0 range 4 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype CCCR_NCC_Field is STM32H753.UInt4;
   subtype CCCR_PCC_Field is STM32H753.UInt4;

   --  SYSCFG compensation cell code register
   type CCCR_Register is record
      --  NMOS compensation code
      NCC           : CCCR_NCC_Field := 16#0#;
      --  PMOS compensation code
      PCC           : CCCR_PCC_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : STM32H753.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCCR_Register use record
      NCC           at 0 range 0 .. 3;
      PCC           at 0 range 4 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype PWRCR_ODEN_Field is STM32H753.UInt4;

   --  SYSCFG power control register
   type PWRCR_Register is record
      --  Overdrive enable
      ODEN          : PWRCR_ODEN_Field := 16#0#;
      --  unspecified
      Reserved_4_31 : STM32H753.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PWRCR_Register use record
      ODEN          at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   subtype PKGR_PKG_Field is STM32H753.UInt4;

   --  SYSCFG package register
   type PKGR_Register is record
      --  Read-only. Package
      PKG           : PKGR_PKG_Field;
      --  unspecified
      Reserved_4_31 : STM32H753.UInt28;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PKGR_Register use record
      PKG           at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   subtype UR0_RDP_Field is STM32H753.Byte;

   --  SYSCFG user register 0
   type UR0_Register is record
      --  Read-only. Bank Swap
      BKS            : Boolean;
      --  unspecified
      Reserved_1_15  : STM32H753.UInt15;
      --  Read-only. Readout protection
      RDP            : UR0_RDP_Field;
      --  unspecified
      Reserved_24_31 : STM32H753.Byte;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for UR0_Register use record
      BKS            at 0 range 0 .. 0;
      Reserved_1_15  at 0 range 1 .. 15;
      RDP            at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype UR2_BORH_Field is STM32H753.UInt2;
   subtype UR2_BOOT_ADD0_Field is STM32H753.UInt16;

   --  SYSCFG user register 2
   type UR2_Register is record
      --  BOR_LVL Brownout Reset Threshold Level
      BORH          : UR2_BORH_Field := 16#0#;
      --  unspecified
      Reserved_2_15 : STM32H753.UInt14 := 16#0#;
      --  Boot Address 0
      BOOT_ADD0     : UR2_BOOT_ADD0_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for UR2_Register use record
      BORH          at 0 range 0 .. 1;
      Reserved_2_15 at 0 range 2 .. 15;
      BOOT_ADD0     at 0 range 16 .. 31;
   end record;

   subtype UR3_BOOT_ADD1_Field is STM32H753.UInt16;

   --  SYSCFG user register 3
   type UR3_Register is record
      --  unspecified
      Reserved_0_15 : STM32H753.UInt16 := 16#0#;
      --  Boot Address 1
      BOOT_ADD1     : UR3_BOOT_ADD1_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for UR3_Register use record
      Reserved_0_15 at 0 range 0 .. 15;
      BOOT_ADD1     at 0 range 16 .. 31;
   end record;

   --  SYSCFG user register 4
   type UR4_Register is record
      --  unspecified
      Reserved_0_15  : STM32H753.UInt16;
      --  Read-only. Mass Erase Protected Area Disabled for bank 1
      MEPAD_1        : Boolean;
      --  unspecified
      Reserved_17_31 : STM32H753.UInt15;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for UR4_Register use record
      Reserved_0_15  at 0 range 0 .. 15;
      MEPAD_1        at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype UR5_WRPN_1_Field is STM32H753.Byte;

   --  SYSCFG user register 5
   type UR5_Register is record
      --  Read-only. Mass erase secured area disabled for bank 1
      MESAD_1        : Boolean;
      --  unspecified
      Reserved_1_15  : STM32H753.UInt15;
      --  Read-only. Write protection for flash bank 1
      WRPN_1         : UR5_WRPN_1_Field;
      --  unspecified
      Reserved_24_31 : STM32H753.Byte;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for UR5_Register use record
      MESAD_1        at 0 range 0 .. 0;
      Reserved_1_15  at 0 range 1 .. 15;
      WRPN_1         at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype UR6_PA_BEG_1_Field is STM32H753.UInt12;
   subtype UR6_PA_END_1_Field is STM32H753.UInt12;

   --  SYSCFG user register 6
   type UR6_Register is record
      --  Read-only. Protected area start address for bank 1
      PA_BEG_1       : UR6_PA_BEG_1_Field;
      --  unspecified
      Reserved_12_15 : STM32H753.UInt4;
      --  Read-only. Protected area end address for bank 1
      PA_END_1       : UR6_PA_END_1_Field;
      --  unspecified
      Reserved_28_31 : STM32H753.UInt4;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for UR6_Register use record
      PA_BEG_1       at 0 range 0 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      PA_END_1       at 0 range 16 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype UR7_SA_BEG_1_Field is STM32H753.UInt12;
   subtype UR7_SA_END_1_Field is STM32H753.UInt12;

   --  SYSCFG user register 7
   type UR7_Register is record
      --  Read-only. Secured area start address for bank 1
      SA_BEG_1       : UR7_SA_BEG_1_Field;
      --  unspecified
      Reserved_12_15 : STM32H753.UInt4;
      --  Read-only. Secured area end address for bank 1
      SA_END_1       : UR7_SA_END_1_Field;
      --  unspecified
      Reserved_28_31 : STM32H753.UInt4;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for UR7_Register use record
      SA_BEG_1       at 0 range 0 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      SA_END_1       at 0 range 16 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  SYSCFG user register 8
   type UR8_Register is record
      --  Read-only. Mass erase protected area disabled for bank 2
      MEPAD_2        : Boolean;
      --  unspecified
      Reserved_1_15  : STM32H753.UInt15;
      --  Read-only. Mass erase secured area disabled for bank 2
      MESAD_2        : Boolean;
      --  unspecified
      Reserved_17_31 : STM32H753.UInt15;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for UR8_Register use record
      MEPAD_2        at 0 range 0 .. 0;
      Reserved_1_15  at 0 range 1 .. 15;
      MESAD_2        at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype UR9_WRPN_2_Field is STM32H753.Byte;
   subtype UR9_PA_BEG_2_Field is STM32H753.UInt12;

   --  SYSCFG user register 9
   type UR9_Register is record
      --  Read-only. Write protection for flash bank 2
      WRPN_2         : UR9_WRPN_2_Field;
      --  unspecified
      Reserved_8_15  : STM32H753.Byte;
      --  Read-only. Protected area start address for bank 2
      PA_BEG_2       : UR9_PA_BEG_2_Field;
      --  unspecified
      Reserved_28_31 : STM32H753.UInt4;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for UR9_Register use record
      WRPN_2         at 0 range 0 .. 7;
      Reserved_8_15  at 0 range 8 .. 15;
      PA_BEG_2       at 0 range 16 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype UR10_PA_END_2_Field is STM32H753.UInt12;
   subtype UR10_SA_BEG_2_Field is STM32H753.UInt12;

   --  SYSCFG user register 10
   type UR10_Register is record
      --  Read-only. Protected area end address for bank 2
      PA_END_2       : UR10_PA_END_2_Field;
      --  unspecified
      Reserved_12_15 : STM32H753.UInt4;
      --  Read-only. Secured area start address for bank 2
      SA_BEG_2       : UR10_SA_BEG_2_Field;
      --  unspecified
      Reserved_28_31 : STM32H753.UInt4;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for UR10_Register use record
      PA_END_2       at 0 range 0 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      SA_BEG_2       at 0 range 16 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype UR11_SA_END_2_Field is STM32H753.UInt12;

   --  SYSCFG user register 11
   type UR11_Register is record
      --  Read-only. Secured area end address for bank 2
      SA_END_2       : UR11_SA_END_2_Field;
      --  unspecified
      Reserved_12_15 : STM32H753.UInt4;
      --  Read-only. Independent Watchdog 1 mode
      IWDG1M         : Boolean;
      --  unspecified
      Reserved_17_31 : STM32H753.UInt15;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for UR11_Register use record
      SA_END_2       at 0 range 0 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      IWDG1M         at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   --  SYSCFG user register 12
   type UR12_Register is record
      --  unspecified
      Reserved_0_15  : STM32H753.UInt16;
      --  Read-only. Secure mode
      SECURE         : Boolean;
      --  unspecified
      Reserved_17_31 : STM32H753.UInt15;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for UR12_Register use record
      Reserved_0_15  at 0 range 0 .. 15;
      SECURE         at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype UR13_SDRS_Field is STM32H753.UInt2;

   --  SYSCFG user register 13
   type UR13_Register is record
      --  Read-only. Secured DTCM RAM Size
      SDRS           : UR13_SDRS_Field;
      --  unspecified
      Reserved_2_15  : STM32H753.UInt14;
      --  Read-only. D1 Standby reset
      D1SBRST        : Boolean;
      --  unspecified
      Reserved_17_31 : STM32H753.UInt15;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for UR13_Register use record
      SDRS           at 0 range 0 .. 1;
      Reserved_2_15  at 0 range 2 .. 15;
      D1SBRST        at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   --  SYSCFG user register 14
   type UR14_Register is record
      --  D1 Stop Reset
      D1STPRST      : Boolean := False;
      --  unspecified
      Reserved_1_31 : STM32H753.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for UR14_Register use record
      D1STPRST      at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  SYSCFG user register 15
   type UR15_Register is record
      --  unspecified
      Reserved_0_15  : STM32H753.UInt16;
      --  Read-only. Freeze independent watchdog in Standby mode
      FZIWDGSTB      : Boolean;
      --  unspecified
      Reserved_17_31 : STM32H753.UInt15;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for UR15_Register use record
      Reserved_0_15  at 0 range 0 .. 15;
      FZIWDGSTB      at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   --  SYSCFG user register 16
   type UR16_Register is record
      --  Read-only. Freeze independent watchdog in Stop mode
      FZIWDGSTP      : Boolean;
      --  unspecified
      Reserved_1_15  : STM32H753.UInt15;
      --  Read-only. Private key programmed
      PKP            : Boolean;
      --  unspecified
      Reserved_17_31 : STM32H753.UInt15;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for UR16_Register use record
      FZIWDGSTP      at 0 range 0 .. 0;
      Reserved_1_15  at 0 range 1 .. 15;
      PKP            at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   --  SYSCFG user register 17
   type UR17_Register is record
      --  Read-only. I/O high speed / low voltage
      IO_HSLV       : Boolean;
      --  unspecified
      Reserved_1_31 : STM32H753.UInt31;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for UR17_Register use record
      IO_HSLV       at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  System configuration controller
   type SYSCFG_Peripheral is record
      --  peripheral mode configuration register
      PMCR    : aliased PMCR_Register;
      --  external interrupt configuration register 1
      EXTICR1 : aliased EXTICR1_Register;
      --  external interrupt configuration register 2
      EXTICR2 : aliased EXTICR2_Register;
      --  external interrupt configuration register 3
      EXTICR3 : aliased EXTICR3_Register;
      --  external interrupt configuration register 4
      EXTICR4 : aliased EXTICR4_Register;
      --  compensation cell control/status register
      CCCSR   : aliased CCCSR_Register;
      --  SYSCFG compensation cell value register
      CCVR    : aliased CCVR_Register;
      --  SYSCFG compensation cell code register
      CCCR    : aliased CCCR_Register;
      --  SYSCFG power control register
      PWRCR   : aliased PWRCR_Register;
      --  SYSCFG package register
      PKGR    : aliased PKGR_Register;
      --  SYSCFG user register 0
      UR0     : aliased UR0_Register;
      --  SYSCFG user register 2
      UR2     : aliased UR2_Register;
      --  SYSCFG user register 3
      UR3     : aliased UR3_Register;
      --  SYSCFG user register 4
      UR4     : aliased UR4_Register;
      --  SYSCFG user register 5
      UR5     : aliased UR5_Register;
      --  SYSCFG user register 6
      UR6     : aliased UR6_Register;
      --  SYSCFG user register 7
      UR7     : aliased UR7_Register;
      --  SYSCFG user register 8
      UR8     : aliased UR8_Register;
      --  SYSCFG user register 9
      UR9     : aliased UR9_Register;
      --  SYSCFG user register 10
      UR10    : aliased UR10_Register;
      --  SYSCFG user register 11
      UR11    : aliased UR11_Register;
      --  SYSCFG user register 12
      UR12    : aliased UR12_Register;
      --  SYSCFG user register 13
      UR13    : aliased UR13_Register;
      --  SYSCFG user register 14
      UR14    : aliased UR14_Register;
      --  SYSCFG user register 15
      UR15    : aliased UR15_Register;
      --  SYSCFG user register 16
      UR16    : aliased UR16_Register;
      --  SYSCFG user register 17
      UR17    : aliased UR17_Register;
   end record
     with Volatile;

   for SYSCFG_Peripheral use record
      PMCR    at 16#4# range 0 .. 31;
      EXTICR1 at 16#8# range 0 .. 31;
      EXTICR2 at 16#C# range 0 .. 31;
      EXTICR3 at 16#10# range 0 .. 31;
      EXTICR4 at 16#14# range 0 .. 31;
      CCCSR   at 16#20# range 0 .. 31;
      CCVR    at 16#24# range 0 .. 31;
      CCCR    at 16#28# range 0 .. 31;
      PWRCR   at 16#2C# range 0 .. 31;
      PKGR    at 16#124# range 0 .. 31;
      UR0     at 16#300# range 0 .. 31;
      UR2     at 16#308# range 0 .. 31;
      UR3     at 16#30C# range 0 .. 31;
      UR4     at 16#310# range 0 .. 31;
      UR5     at 16#314# range 0 .. 31;
      UR6     at 16#318# range 0 .. 31;
      UR7     at 16#31C# range 0 .. 31;
      UR8     at 16#320# range 0 .. 31;
      UR9     at 16#324# range 0 .. 31;
      UR10    at 16#328# range 0 .. 31;
      UR11    at 16#32C# range 0 .. 31;
      UR12    at 16#330# range 0 .. 31;
      UR13    at 16#334# range 0 .. 31;
      UR14    at 16#338# range 0 .. 31;
      UR15    at 16#33C# range 0 .. 31;
      UR16    at 16#340# range 0 .. 31;
      UR17    at 16#344# range 0 .. 31;
   end record;

   --  System configuration controller
   SYSCFG_Periph : aliased SYSCFG_Peripheral
     with Import, Address => SYSCFG_Base;

end STM32H753.SYSCFG;
