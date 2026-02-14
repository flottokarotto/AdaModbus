pragma Style_Checks (Off);

--  This spec has been automatically generated from STM32H753.svd


with System;

package STM32H753.DBGMCU is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   subtype IDC_DEV_ID_Field is STM32H753.UInt12;
   subtype IDC_REV_ID_Field is STM32H753.UInt16;

   --  DBGMCU Identity Code Register
   type IDC_Register is record
      --  Read-only. Device ID
      DEV_ID         : IDC_DEV_ID_Field;
      --  unspecified
      Reserved_12_15 : STM32H753.UInt4;
      --  Read-only. Revision
      REV_ID         : IDC_REV_ID_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IDC_Register use record
      DEV_ID         at 0 range 0 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      REV_ID         at 0 range 16 .. 31;
   end record;

   --  DBGMCU Configuration Register
   type CR_Register is record
      --  Allow D1 domain debug in Sleep mode
      DBGSLEEP_D1    : Boolean := False;
      --  Allow D1 domain debug in Stop mode
      DBGSTOP_D1     : Boolean := False;
      --  Allow D1 domain debug in Standby mode
      DBGSTBY_D1     : Boolean := False;
      --  Allow D2 domain debug in Sleep mode
      DBGSLEEP_D2    : Boolean := False;
      --  Allow D2 domain debug in Stop mode
      DBGSTOP_D2     : Boolean := False;
      --  Allow D2 domain debug in Standby mode
      DBGSTBY_D2     : Boolean := False;
      --  unspecified
      Reserved_6_6   : STM32H753.Bit := 16#0#;
      --  Allow debug in D3 Stop mode
      DBGSTOP_D3     : Boolean := False;
      --  Allow debug in D3 Standby mode
      DBGSTBY_D3     : Boolean := False;
      --  unspecified
      Reserved_9_19  : STM32H753.UInt11 := 16#0#;
      --  Trace port clock enable
      TRACECLKEN     : Boolean := False;
      --  D1 debug clock enable
      D1DBGCKEN      : Boolean := False;
      --  D3 debug clock enable
      D3DBGCKEN      : Boolean := False;
      --  unspecified
      Reserved_23_27 : STM32H753.UInt5 := 16#0#;
      --  External trigger output enable
      TRGOEN         : Boolean := False;
      --  unspecified
      Reserved_29_31 : STM32H753.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      DBGSLEEP_D1    at 0 range 0 .. 0;
      DBGSTOP_D1     at 0 range 1 .. 1;
      DBGSTBY_D1     at 0 range 2 .. 2;
      DBGSLEEP_D2    at 0 range 3 .. 3;
      DBGSTOP_D2     at 0 range 4 .. 4;
      DBGSTBY_D2     at 0 range 5 .. 5;
      Reserved_6_6   at 0 range 6 .. 6;
      DBGSTOP_D3     at 0 range 7 .. 7;
      DBGSTBY_D3     at 0 range 8 .. 8;
      Reserved_9_19  at 0 range 9 .. 19;
      TRACECLKEN     at 0 range 20 .. 20;
      D1DBGCKEN      at 0 range 21 .. 21;
      D3DBGCKEN      at 0 range 22 .. 22;
      Reserved_23_27 at 0 range 23 .. 27;
      TRGOEN         at 0 range 28 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   --  DBGMCU APB3 peripheral freeze register
   type APB3FZ1_Register is record
      --  unspecified
      Reserved_0_5  : STM32H753.UInt6 := 16#0#;
      --  WWDG1 stop in debug
      WWDG1         : Boolean := False;
      --  unspecified
      Reserved_7_31 : STM32H753.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for APB3FZ1_Register use record
      Reserved_0_5  at 0 range 0 .. 5;
      WWDG1         at 0 range 6 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   --  APB1LFZ1_DBG_TIM array
   type APB1LFZ1_DBG_TIM_Field_Array is array (2 .. 10) of Boolean
     with Component_Size => 1, Size => 9;

   --  Type definition for APB1LFZ1_DBG_TIM
   type APB1LFZ1_DBG_TIM_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DBG_TIM as a value
            Val : STM32H753.UInt9;
         when True =>
            --  DBG_TIM as an array
            Arr : APB1LFZ1_DBG_TIM_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 9;

   for APB1LFZ1_DBG_TIM_Field use record
      Val at 0 range 0 .. 8;
      Arr at 0 range 0 .. 8;
   end record;

   --  APB1LFZ1_DBG_I2C array
   type APB1LFZ1_DBG_I2C_Field_Array is array (1 .. 3) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for APB1LFZ1_DBG_I2C
   type APB1LFZ1_DBG_I2C_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DBG_I2C as a value
            Val : STM32H753.UInt3;
         when True =>
            --  DBG_I2C as an array
            Arr : APB1LFZ1_DBG_I2C_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for APB1LFZ1_DBG_I2C_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  DBGMCU APB1L peripheral freeze register
   type APB1LFZ1_Register is record
      --  TIM2 stop in debug
      DBG_TIM        : APB1LFZ1_DBG_TIM_Field :=
                        (As_Array => False, Val => 16#0#);
      --  LPTIM1 stop in debug
      DBG_LPTIM1     : Boolean := False;
      --  unspecified
      Reserved_10_20 : STM32H753.UInt11 := 16#0#;
      --  I2C1 SMBUS timeout stop in debug
      DBG_I2C        : APB1LFZ1_DBG_I2C_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_24_31 : STM32H753.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for APB1LFZ1_Register use record
      DBG_TIM        at 0 range 0 .. 8;
      DBG_LPTIM1     at 0 range 9 .. 9;
      Reserved_10_20 at 0 range 10 .. 20;
      DBG_I2C        at 0 range 21 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  APB2FZ1_DBG_TIM array
   type APB2FZ1_DBG_TIM_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for APB2FZ1_DBG_TIM
   type APB2FZ1_DBG_TIM_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DBG_TIM as a value
            Val : STM32H753.UInt2;
         when True =>
            --  DBG_TIM as an array
            Arr : APB2FZ1_DBG_TIM_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for APB2FZ1_DBG_TIM_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  APB2FZ1_DBG_TIM array
   type APB2FZ1_DBG_TIM_Field_Array_1 is array (15 .. 17) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for APB2FZ1_DBG_TIM
   type APB2FZ1_DBG_TIM_Field_1
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DBG_TIM as a value
            Val : STM32H753.UInt3;
         when True =>
            --  DBG_TIM as an array
            Arr : APB2FZ1_DBG_TIM_Field_Array_1;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for APB2FZ1_DBG_TIM_Field_1 use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  DBGMCU APB2 peripheral freeze register
   type APB2FZ1_Register is record
      --  TIM1 stop in debug
      DBG_TIM        : APB2FZ1_DBG_TIM_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_2_15  : STM32H753.UInt14 := 16#0#;
      --  TIM15 stop in debug
      DBG_TIM_1      : APB2FZ1_DBG_TIM_Field_1 :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_19_28 : STM32H753.UInt10 := 16#0#;
      --  HRTIM stop in debug
      DBG_HRTIM      : Boolean := False;
      --  unspecified
      Reserved_30_31 : STM32H753.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for APB2FZ1_Register use record
      DBG_TIM        at 0 range 0 .. 1;
      Reserved_2_15  at 0 range 2 .. 15;
      DBG_TIM_1      at 0 range 16 .. 18;
      Reserved_19_28 at 0 range 19 .. 28;
      DBG_HRTIM      at 0 range 29 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   --  APB4FZ1_DBG_LPTIM array
   type APB4FZ1_DBG_LPTIM_Field_Array is array (2 .. 5) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for APB4FZ1_DBG_LPTIM
   type APB4FZ1_DBG_LPTIM_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  DBG_LPTIM as a value
            Val : STM32H753.UInt4;
         when True =>
            --  DBG_LPTIM as an array
            Arr : APB4FZ1_DBG_LPTIM_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for APB4FZ1_DBG_LPTIM_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  DBGMCU APB4 peripheral freeze register
   type APB4FZ1_Register is record
      --  unspecified
      Reserved_0_6   : STM32H753.UInt7 := 16#0#;
      --  I2C4 SMBUS timeout stop in debug
      DBG_I2C4       : Boolean := False;
      --  unspecified
      Reserved_8_8   : STM32H753.Bit := 16#0#;
      --  LPTIM2 stop in debug
      DBG_LPTIM      : APB4FZ1_DBG_LPTIM_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_13_15 : STM32H753.UInt3 := 16#0#;
      --  RTC stop in debug
      DBG_RTC        : Boolean := False;
      --  unspecified
      Reserved_17_17 : STM32H753.Bit := 16#0#;
      --  Independent watchdog for D1 stop in debug
      DBG_WDGLSD1    : Boolean := False;
      --  unspecified
      Reserved_19_31 : STM32H753.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for APB4FZ1_Register use record
      Reserved_0_6   at 0 range 0 .. 6;
      DBG_I2C4       at 0 range 7 .. 7;
      Reserved_8_8   at 0 range 8 .. 8;
      DBG_LPTIM      at 0 range 9 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      DBG_RTC        at 0 range 16 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      DBG_WDGLSD1    at 0 range 18 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Microcontroller Debug Unit
   type DBGMCU_Peripheral is record
      --  DBGMCU Identity Code Register
      IDC      : aliased IDC_Register;
      --  DBGMCU Configuration Register
      CR       : aliased CR_Register;
      --  DBGMCU APB3 peripheral freeze register
      APB3FZ1  : aliased APB3FZ1_Register;
      --  DBGMCU APB1L peripheral freeze register
      APB1LFZ1 : aliased APB1LFZ1_Register;
      --  DBGMCU APB2 peripheral freeze register
      APB2FZ1  : aliased APB2FZ1_Register;
      --  DBGMCU APB4 peripheral freeze register
      APB4FZ1  : aliased APB4FZ1_Register;
   end record
     with Volatile;

   for DBGMCU_Peripheral use record
      IDC      at 16#0# range 0 .. 31;
      CR       at 16#4# range 0 .. 31;
      APB3FZ1  at 16#34# range 0 .. 31;
      APB1LFZ1 at 16#3C# range 0 .. 31;
      APB2FZ1  at 16#4C# range 0 .. 31;
      APB4FZ1  at 16#54# range 0 .. 31;
   end record;

   --  Microcontroller Debug Unit
   DBGMCU_Periph : aliased DBGMCU_Peripheral
     with Import, Address => DBGMCU_Base;

end STM32H753.DBGMCU;
