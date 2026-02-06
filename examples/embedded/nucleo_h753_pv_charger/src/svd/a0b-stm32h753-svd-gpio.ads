pragma Style_Checks (Off);

--  This spec has been automatically generated from STM32H753.svd


with A0B.Types.SVD;
with System;

package A0B.STM32H753.SVD.GPIO is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   --  MODER_MODE array element
   subtype MODER_MODE_Element is A0B.Types.SVD.UInt2;

   --  MODER_MODE array
   type MODER_MODE_Field_Array is array (0 .. 15) of MODER_MODE_Element
     with Component_Size => 2, Size => 32;

   --  GPIO port mode register
   type MODER_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MODE as a value
            Val : A0B.Types.SVD.UInt32;
         when True =>
            --  MODE as an array
            Arr : MODER_MODE_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for MODER_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  OTYPER_OT array
   type OTYPER_OT_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for OTYPER_OT
   type OTYPER_OT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  OT as a value
            Val : A0B.Types.SVD.UInt16;
         when True =>
            --  OT as an array
            Arr : OTYPER_OT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for OTYPER_OT_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  GPIO port output type register
   type OTYPER_Register is record
      --  Port x configuration bits (y = 0..15) These bits are written by
      --  software to configure the I/O output type.
      OT             : OTYPER_OT_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : A0B.Types.SVD.UInt16 := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for OTYPER_Register use record
      OT             at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  OSPEEDR_OSPEED array element
   subtype OSPEEDR_OSPEED_Element is A0B.Types.SVD.UInt2;

   --  OSPEEDR_OSPEED array
   type OSPEEDR_OSPEED_Field_Array is array (0 .. 15)
     of OSPEEDR_OSPEED_Element
     with Component_Size => 2, Size => 32;

   --  GPIO port output speed register
   type OSPEEDR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  OSPEED as a value
            Val : A0B.Types.SVD.UInt32;
         when True =>
            --  OSPEED as an array
            Arr : OSPEEDR_OSPEED_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OSPEEDR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  PUPDR_PUPD array element
   subtype PUPDR_PUPD_Element is A0B.Types.SVD.UInt2;

   --  PUPDR_PUPD array
   type PUPDR_PUPD_Field_Array is array (0 .. 15) of PUPDR_PUPD_Element
     with Component_Size => 2, Size => 32;

   --  GPIO port pull-up/pull-down register
   type PUPDR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PUPD as a value
            Val : A0B.Types.SVD.UInt32;
         when True =>
            --  PUPD as an array
            Arr : PUPDR_PUPD_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PUPDR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  IDR_ID array
   type IDR_ID_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for IDR_ID
   type IDR_ID_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  ID as a value
            Val : A0B.Types.SVD.UInt16;
         when True =>
            --  ID as an array
            Arr : IDR_ID_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for IDR_ID_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  GPIO port input data register
   type IDR_Register is record
      --  Read-only. Port input data bit (y = 0..15) These bits are read-only.
      --  They contain the input value of the corresponding I/O port.
      ID             : IDR_ID_Field;
      --  unspecified
      Reserved_16_31 : A0B.Types.SVD.UInt16;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for IDR_Register use record
      ID             at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  ODR_OD array
   type ODR_OD_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for ODR_OD
   type ODR_OD_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  OD as a value
            Val : A0B.Types.SVD.UInt16;
         when True =>
            --  OD as an array
            Arr : ODR_OD_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for ODR_OD_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  GPIO port output data register
   type ODR_Register is record
      --  Port output data bit These bits can be read and written by software.
      --  Note: For atomic bit set/reset, the OD bits can be individually set
      --  and/or reset by writing to the GPIOx_BSRR or GPIOx_BRR registers (x =
      --  A..F).
      OD             : ODR_OD_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : A0B.Types.SVD.UInt16 := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for ODR_Register use record
      OD             at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  BSRR_BS array
   type BSRR_BS_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for BSRR_BS
   type BSRR_BS_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  BS as a value
            Val : A0B.Types.SVD.UInt16;
         when True =>
            --  BS as an array
            Arr : BSRR_BS_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for BSRR_BS_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  BSRR_BR array
   type BSRR_BR_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for BSRR_BR
   type BSRR_BR_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  BR as a value
            Val : A0B.Types.SVD.UInt16;
         when True =>
            --  BR as an array
            Arr : BSRR_BR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for BSRR_BR_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  GPIO port bit set/reset register
   type BSRR_Register is record
      --  Write-only. Port x set bit y (y= 0..15) These bits are write-only. A
      --  read to these bits returns the value 0x0000.
      BS : BSRR_BS_Field := (As_Array => False, Val => 16#0#);
      --  Write-only. Port x reset bit y (y = 0..15) These bits are write-only.
      --  A read to these bits returns the value 0x0000. Note: If both BSx and
      --  BRx are set, BSx has priority.
      BR : BSRR_BR_Field := (As_Array => False, Val => 16#0#);
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for BSRR_Register use record
      BS at 0 range 0 .. 15;
      BR at 0 range 16 .. 31;
   end record;

   --  LCKR_LCK array
   type LCKR_LCK_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for LCKR_LCK
   type LCKR_LCK_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  LCK as a value
            Val : A0B.Types.SVD.UInt16;
         when True =>
            --  LCK as an array
            Arr : LCKR_LCK_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for LCKR_LCK_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  This register is used to lock the configuration of the port bits when a
   --  correct write sequence is applied to bit 16 (LCKK). The value of bits
   --  [15:0] is used to lock the configuration of the GPIO. During the write
   --  sequence, the value of LCKR[15:0] must not change. When the LOCK
   --  sequence has been applied on a port bit, the value of this port bit can
   --  no longer be modified until the next MCU reset or peripheral reset.A
   --  specific write sequence is used to write to the GPIOx_LCKR register.
   --  Only word access (32-bit long) is allowed during this locking
   --  sequence.Each lock bit freezes a specific configuration register
   --  (control and alternate function registers).
   type LCKR_Register is record
      --  Port x lock bit y (y= 0..15) These bits are read/write but can only
      --  be written when the LCKK bit is 0.
      LCK            : LCKR_LCK_Field := (As_Array => False, Val => 16#0#);
      --  Lock key This bit can be read any time. It can only be modified using
      --  the lock key write sequence. LOCK key write sequence: WR LCKR[16] = 1
      --  + LCKR[15:0] WR LCKR[16] = 0 + LCKR[15:0] WR LCKR[16] = 1 +
      --  LCKR[15:0] RD LCKR RD LCKR[16] = 1 (this read operation is optional
      --  but it confirms that the lock is active) Note: During the LOCK key
      --  write sequence, the value of LCK[15:0] must not change. Any error in
      --  the lock sequence aborts the lock. After the first lock sequence on
      --  any bit of the port, any read access on the LCKK bit will return 1
      --  until the next MCU reset or peripheral reset.
      LCKK           : Boolean := False;
      --  unspecified
      Reserved_17_31 : A0B.Types.SVD.UInt15 := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for LCKR_Register use record
      LCK            at 0 range 0 .. 15;
      LCKK           at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   --  AFRL_AFSEL array element
   subtype AFRL_AFSEL_Element is A0B.Types.SVD.UInt4;

   --  AFRL_AFSEL array
   type AFRL_AFSEL_Field_Array is array (0 .. 7) of AFRL_AFSEL_Element
     with Component_Size => 4, Size => 32;

   --  GPIO alternate function low register
   type AFRL_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  AFSEL as a value
            Val : A0B.Types.SVD.UInt32;
         when True =>
            --  AFSEL as an array
            Arr : AFRL_AFSEL_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AFRL_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  AFRH_AFSEL array element
   subtype AFRH_AFSEL_Element is A0B.Types.SVD.UInt4;

   --  AFRH_AFSEL array
   type AFRH_AFSEL_Field_Array is array (8 .. 15) of AFRH_AFSEL_Element
     with Component_Size => 4, Size => 32;

   --  GPIO alternate function high register
   type AFRH_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  AFSEL as a value
            Val : A0B.Types.SVD.UInt32;
         when True =>
            --  AFSEL as an array
            Arr : AFRH_AFSEL_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AFRH_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  GPIO
   type GPIO_Peripheral is record
      --  GPIO port mode register
      MODER   : aliased MODER_Register;
      pragma Volatile_Full_Access (MODER);
      --  GPIO port output type register
      OTYPER  : aliased OTYPER_Register;
      pragma Volatile_Full_Access (OTYPER);
      --  GPIO port output speed register
      OSPEEDR : aliased OSPEEDR_Register;
      pragma Volatile_Full_Access (OSPEEDR);
      --  GPIO port pull-up/pull-down register
      PUPDR   : aliased PUPDR_Register;
      pragma Volatile_Full_Access (PUPDR);
      --  GPIO port input data register
      IDR     : aliased IDR_Register;
      pragma Volatile_Full_Access (IDR);
      --  GPIO port output data register
      ODR     : aliased ODR_Register;
      pragma Volatile_Full_Access (ODR);
      --  GPIO port bit set/reset register
      BSRR    : aliased BSRR_Register;
      pragma Volatile_Full_Access (BSRR);
      --  This register is used to lock the configuration of the port bits when
      --  a correct write sequence is applied to bit 16 (LCKK). The value of
      --  bits [15:0] is used to lock the configuration of the GPIO. During the
      --  write sequence, the value of LCKR[15:0] must not change. When the
      --  LOCK sequence has been applied on a port bit, the value of this port
      --  bit can no longer be modified until the next MCU reset or peripheral
      --  reset.A specific write sequence is used to write to the GPIOx_LCKR
      --  register. Only word access (32-bit long) is allowed during this
      --  locking sequence.Each lock bit freezes a specific configuration
      --  register (control and alternate function registers).
      LCKR    : aliased LCKR_Register;
      pragma Volatile_Full_Access (LCKR);
      --  GPIO alternate function low register
      AFRL    : aliased AFRL_Register;
      pragma Volatile_Full_Access (AFRL);
      --  GPIO alternate function high register
      AFRH    : aliased AFRH_Register;
      pragma Volatile_Full_Access (AFRH);
   end record
     with Volatile;

   for GPIO_Peripheral use record
      MODER   at 16#0# range 0 .. 31;
      OTYPER  at 16#4# range 0 .. 31;
      OSPEEDR at 16#8# range 0 .. 31;
      PUPDR   at 16#C# range 0 .. 31;
      IDR     at 16#10# range 0 .. 31;
      ODR     at 16#14# range 0 .. 31;
      BSRR    at 16#18# range 0 .. 31;
      LCKR    at 16#1C# range 0 .. 31;
      AFRL    at 16#20# range 0 .. 31;
      AFRH    at 16#24# range 0 .. 31;
   end record;

   --  GPIO
   GPIOA_Periph : aliased GPIO_Peripheral
     with Import, Address => GPIOA_Base;

   --  GPIO
   GPIOB_Periph : aliased GPIO_Peripheral
     with Import, Address => GPIOB_Base;

   --  GPIO
   GPIOC_Periph : aliased GPIO_Peripheral
     with Import, Address => GPIOC_Base;

   --  GPIO
   GPIOD_Periph : aliased GPIO_Peripheral
     with Import, Address => GPIOD_Base;

   --  GPIO
   GPIOE_Periph : aliased GPIO_Peripheral
     with Import, Address => GPIOE_Base;

   --  GPIO
   GPIOF_Periph : aliased GPIO_Peripheral
     with Import, Address => GPIOF_Base;

   --  GPIO
   GPIOG_Periph : aliased GPIO_Peripheral
     with Import, Address => GPIOG_Base;

   --  GPIO
   GPIOH_Periph : aliased GPIO_Peripheral
     with Import, Address => GPIOH_Base;

   --  GPIO
   GPIOJ_Periph : aliased GPIO_Peripheral
     with Import, Address => GPIOJ_Base;

   --  GPIO
   GPIOK_Periph : aliased GPIO_Peripheral
     with Import, Address => GPIOK_Base;

end A0B.STM32H753.SVD.GPIO;
