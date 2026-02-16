pragma Style_Checks (Off);

--  This spec has been automatically generated from STM32H753.svd


with System;

package STM32H753.EXTI is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   --  RTSR1_TR array
   type RTSR1_TR_Field_Array is array (0 .. 21) of Boolean
     with Component_Size => 1, Size => 22;

   --  Type definition for RTSR1_TR
   type RTSR1_TR_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TR as a value
            Val : STM32H753.UInt22;
         when True =>
            --  TR as an array
            Arr : RTSR1_TR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 22;

   for RTSR1_TR_Field use record
      Val at 0 range 0 .. 21;
      Arr at 0 range 0 .. 21;
   end record;

   --  EXTI rising trigger selection register
   type RTSR1_Register is record
      --  Rising trigger event configuration bit of Configurable Event input
      TR             : RTSR1_TR_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_22_31 : STM32H753.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTSR1_Register use record
      TR             at 0 range 0 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  FTSR1_TR array
   type FTSR1_TR_Field_Array is array (0 .. 21) of Boolean
     with Component_Size => 1, Size => 22;

   --  Type definition for FTSR1_TR
   type FTSR1_TR_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TR as a value
            Val : STM32H753.UInt22;
         when True =>
            --  TR as an array
            Arr : FTSR1_TR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 22;

   for FTSR1_TR_Field use record
      Val at 0 range 0 .. 21;
      Arr at 0 range 0 .. 21;
   end record;

   --  EXTI falling trigger selection register
   type FTSR1_Register is record
      --  Rising trigger event configuration bit of Configurable Event input
      TR             : FTSR1_TR_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_22_31 : STM32H753.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FTSR1_Register use record
      TR             at 0 range 0 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  SWIER1_SWIER array
   type SWIER1_SWIER_Field_Array is array (0 .. 21) of Boolean
     with Component_Size => 1, Size => 22;

   --  Type definition for SWIER1_SWIER
   type SWIER1_SWIER_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SWIER as a value
            Val : STM32H753.UInt22;
         when True =>
            --  SWIER as an array
            Arr : SWIER1_SWIER_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 22;

   for SWIER1_SWIER_Field use record
      Val at 0 range 0 .. 21;
      Arr at 0 range 0 .. 21;
   end record;

   --  EXTI software interrupt event register
   type SWIER1_Register is record
      --  Rising trigger event configuration bit of Configurable Event input
      SWIER          : SWIER1_SWIER_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_22_31 : STM32H753.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SWIER1_Register use record
      SWIER          at 0 range 0 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  D3PMR1_MR array
   type D3PMR1_MR_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for D3PMR1_MR
   type D3PMR1_MR_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MR as a value
            Val : STM32H753.UInt16;
         when True =>
            --  MR as an array
            Arr : D3PMR1_MR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for D3PMR1_MR_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   --  D3PMR1_MR array
   type D3PMR1_MR_Field_Array_1 is array (19 .. 21) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for D3PMR1_MR
   type D3PMR1_MR_Field_1
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MR as a value
            Val : STM32H753.UInt3;
         when True =>
            --  MR as an array
            Arr : D3PMR1_MR_Field_Array_1;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for D3PMR1_MR_Field_1 use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  EXTI D3 pending mask register
   type D3PMR1_Register is record
      --  Rising trigger event configuration bit of Configurable Event input
      MR             : D3PMR1_MR_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_18 : STM32H753.UInt3 := 16#0#;
      --  Rising trigger event configuration bit of Configurable Event input
      MR_1           : D3PMR1_MR_Field_1 := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_22_24 : STM32H753.UInt3 := 16#0#;
      --  Rising trigger event configuration bit of Configurable Event input
      MR25           : Boolean := False;
      --  unspecified
      Reserved_26_31 : STM32H753.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for D3PMR1_Register use record
      MR             at 0 range 0 .. 15;
      Reserved_16_18 at 0 range 16 .. 18;
      MR_1           at 0 range 19 .. 21;
      Reserved_22_24 at 0 range 22 .. 24;
      MR25           at 0 range 25 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   --  D3PCR1L_PCS array element
   subtype D3PCR1L_PCS_Element is STM32H753.UInt2;

   --  D3PCR1L_PCS array
   type D3PCR1L_PCS_Field_Array is array (0 .. 15) of D3PCR1L_PCS_Element
     with Component_Size => 2, Size => 32;

   --  EXTI D3 pending clear selection register low
   type D3PCR1L_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PCS as a value
            Val : STM32H753.UInt32;
         when True =>
            --  PCS as an array
            Arr : D3PCR1L_PCS_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for D3PCR1L_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  D3PCR1H_PCS array element
   subtype D3PCR1H_PCS_Element is STM32H753.UInt2;

   --  D3PCR1H_PCS array
   type D3PCR1H_PCS_Field_Array is array (19 .. 21) of D3PCR1H_PCS_Element
     with Component_Size => 2, Size => 6;

   --  Type definition for D3PCR1H_PCS
   type D3PCR1H_PCS_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PCS as a value
            Val : STM32H753.UInt6;
         when True =>
            --  PCS as an array
            Arr : D3PCR1H_PCS_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 6;

   for D3PCR1H_PCS_Field use record
      Val at 0 range 0 .. 5;
      Arr at 0 range 0 .. 5;
   end record;

   subtype D3PCR1H_PCS25_Field is STM32H753.UInt2;

   --  EXTI D3 pending clear selection register high
   type D3PCR1H_Register is record
      --  unspecified
      Reserved_0_5   : STM32H753.UInt6 := 16#0#;
      --  D3 Pending request clear input signal selection on Event input x =
      --  truncate ((n+32)/2)
      PCS            : D3PCR1H_PCS_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_12_17 : STM32H753.UInt6 := 16#0#;
      --  D3 Pending request clear input signal selection on Event input x =
      --  truncate ((n+32)/2)
      PCS25          : D3PCR1H_PCS25_Field := 16#0#;
      --  unspecified
      Reserved_20_31 : STM32H753.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for D3PCR1H_Register use record
      Reserved_0_5   at 0 range 0 .. 5;
      PCS            at 0 range 6 .. 11;
      Reserved_12_17 at 0 range 12 .. 17;
      PCS25          at 0 range 18 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  EXTI rising trigger selection register
   type RTSR2_Register is record
      --  unspecified
      Reserved_0_16  : STM32H753.UInt17 := 16#0#;
      --  Rising trigger event configuration bit of Configurable Event input
      --  x+32
      TR49           : Boolean := False;
      --  unspecified
      Reserved_18_18 : STM32H753.Bit := 16#0#;
      --  Rising trigger event configuration bit of Configurable Event input
      --  x+32
      TR51           : Boolean := False;
      --  unspecified
      Reserved_20_31 : STM32H753.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTSR2_Register use record
      Reserved_0_16  at 0 range 0 .. 16;
      TR49           at 0 range 17 .. 17;
      Reserved_18_18 at 0 range 18 .. 18;
      TR51           at 0 range 19 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  EXTI falling trigger selection register
   type FTSR2_Register is record
      --  unspecified
      Reserved_0_16  : STM32H753.UInt17 := 16#0#;
      --  Falling trigger event configuration bit of Configurable Event input
      --  x+32
      TR49           : Boolean := False;
      --  unspecified
      Reserved_18_18 : STM32H753.Bit := 16#0#;
      --  Falling trigger event configuration bit of Configurable Event input
      --  x+32
      TR51           : Boolean := False;
      --  unspecified
      Reserved_20_31 : STM32H753.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FTSR2_Register use record
      Reserved_0_16  at 0 range 0 .. 16;
      TR49           at 0 range 17 .. 17;
      Reserved_18_18 at 0 range 18 .. 18;
      TR51           at 0 range 19 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  EXTI software interrupt event register
   type SWIER2_Register is record
      --  unspecified
      Reserved_0_16  : STM32H753.UInt17 := 16#0#;
      --  Software interrupt on line x+32
      SWIER49        : Boolean := False;
      --  unspecified
      Reserved_18_18 : STM32H753.Bit := 16#0#;
      --  Software interrupt on line x+32
      SWIER51        : Boolean := False;
      --  unspecified
      Reserved_20_31 : STM32H753.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SWIER2_Register use record
      Reserved_0_16  at 0 range 0 .. 16;
      SWIER49        at 0 range 17 .. 17;
      Reserved_18_18 at 0 range 18 .. 18;
      SWIER51        at 0 range 19 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  D3PMR2_MR array
   type D3PMR2_MR_Field_Array is array (34 .. 35) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for D3PMR2_MR
   type D3PMR2_MR_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MR as a value
            Val : STM32H753.UInt2;
         when True =>
            --  MR as an array
            Arr : D3PMR2_MR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for D3PMR2_MR_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  D3PMR2_MR array
   type D3PMR2_MR_Field_Array_1 is array (48 .. 53) of Boolean
     with Component_Size => 1, Size => 6;

   --  Type definition for D3PMR2_MR
   type D3PMR2_MR_Field_1
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MR as a value
            Val : STM32H753.UInt6;
         when True =>
            --  MR as an array
            Arr : D3PMR2_MR_Field_Array_1;
      end case;
   end record
     with Unchecked_Union, Size => 6;

   for D3PMR2_MR_Field_1 use record
      Val at 0 range 0 .. 5;
      Arr at 0 range 0 .. 5;
   end record;

   --  EXTI D3 pending mask register
   type D3PMR2_Register is record
      --  unspecified
      Reserved_0_1   : STM32H753.UInt2 := 16#0#;
      --  D3 Pending Mask on Event input x+32
      MR             : D3PMR2_MR_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_4_8   : STM32H753.UInt5 := 16#0#;
      --  D3 Pending Mask on Event input x+32
      MR41           : Boolean := False;
      --  unspecified
      Reserved_10_15 : STM32H753.UInt6 := 16#0#;
      --  D3 Pending Mask on Event input x+32
      MR_1           : D3PMR2_MR_Field_1 := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_22_31 : STM32H753.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for D3PMR2_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      MR             at 0 range 2 .. 3;
      Reserved_4_8   at 0 range 4 .. 8;
      MR41           at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      MR_1           at 0 range 16 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  D3PCR2L_PCS array element
   subtype D3PCR2L_PCS_Element is STM32H753.UInt2;

   --  D3PCR2L_PCS array
   type D3PCR2L_PCS_Field_Array is array (34 .. 35) of D3PCR2L_PCS_Element
     with Component_Size => 2, Size => 4;

   --  Type definition for D3PCR2L_PCS
   type D3PCR2L_PCS_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PCS as a value
            Val : STM32H753.UInt4;
         when True =>
            --  PCS as an array
            Arr : D3PCR2L_PCS_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for D3PCR2L_PCS_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   subtype D3PCR2L_PCS41_Field is STM32H753.UInt2;

   --  EXTI D3 pending clear selection register low
   type D3PCR2L_Register is record
      --  unspecified
      Reserved_0_3   : STM32H753.UInt4 := 16#0#;
      --  D3 Pending request clear input signal selection on Event input x =
      --  truncate ((n+64)/2)
      PCS            : D3PCR2L_PCS_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_8_17  : STM32H753.UInt10 := 16#0#;
      --  D3 Pending request clear input signal selection on Event input x =
      --  truncate ((n+64)/2)
      PCS41          : D3PCR2L_PCS41_Field := 16#0#;
      --  unspecified
      Reserved_20_31 : STM32H753.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for D3PCR2L_Register use record
      Reserved_0_3   at 0 range 0 .. 3;
      PCS            at 0 range 4 .. 7;
      Reserved_8_17  at 0 range 8 .. 17;
      PCS41          at 0 range 18 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  D3PCR2H_PCS array element
   subtype D3PCR2H_PCS_Element is STM32H753.UInt2;

   --  D3PCR2H_PCS array
   type D3PCR2H_PCS_Field_Array is array (48 .. 53) of D3PCR2H_PCS_Element
     with Component_Size => 2, Size => 12;

   --  Type definition for D3PCR2H_PCS
   type D3PCR2H_PCS_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PCS as a value
            Val : STM32H753.UInt12;
         when True =>
            --  PCS as an array
            Arr : D3PCR2H_PCS_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 12;

   for D3PCR2H_PCS_Field use record
      Val at 0 range 0 .. 11;
      Arr at 0 range 0 .. 11;
   end record;

   --  EXTI D3 pending clear selection register high
   type D3PCR2H_Register is record
      --  Pending request clear input signal selection on Event input x=
      --  truncate ((n+96)/2)
      PCS            : D3PCR2H_PCS_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_12_31 : STM32H753.UInt20 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for D3PCR2H_Register use record
      PCS            at 0 range 0 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   --  RTSR3_TR array
   type RTSR3_TR_Field_Array is array (84 .. 86) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for RTSR3_TR
   type RTSR3_TR_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TR as a value
            Val : STM32H753.UInt3;
         when True =>
            --  TR as an array
            Arr : RTSR3_TR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for RTSR3_TR_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  EXTI rising trigger selection register
   type RTSR3_Register is record
      --  unspecified
      Reserved_0_17  : STM32H753.UInt18 := 16#0#;
      --  Rising trigger event configuration bit of Configurable Event input
      --  x+64
      TR82           : Boolean := False;
      --  unspecified
      Reserved_19_19 : STM32H753.Bit := 16#0#;
      --  Rising trigger event configuration bit of Configurable Event input
      --  x+64
      TR             : RTSR3_TR_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_23_31 : STM32H753.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTSR3_Register use record
      Reserved_0_17  at 0 range 0 .. 17;
      TR82           at 0 range 18 .. 18;
      Reserved_19_19 at 0 range 19 .. 19;
      TR             at 0 range 20 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   --  FTSR3_TR array
   type FTSR3_TR_Field_Array is array (84 .. 86) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for FTSR3_TR
   type FTSR3_TR_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  TR as a value
            Val : STM32H753.UInt3;
         when True =>
            --  TR as an array
            Arr : FTSR3_TR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for FTSR3_TR_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  EXTI falling trigger selection register
   type FTSR3_Register is record
      --  unspecified
      Reserved_0_17  : STM32H753.UInt18 := 16#0#;
      --  Falling trigger event configuration bit of Configurable Event input
      --  x+64
      TR82           : Boolean := False;
      --  unspecified
      Reserved_19_19 : STM32H753.Bit := 16#0#;
      --  Falling trigger event configuration bit of Configurable Event input
      --  x+64
      TR             : FTSR3_TR_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_23_31 : STM32H753.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FTSR3_Register use record
      Reserved_0_17  at 0 range 0 .. 17;
      TR82           at 0 range 18 .. 18;
      Reserved_19_19 at 0 range 19 .. 19;
      TR             at 0 range 20 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   --  SWIER3_SWIER array
   type SWIER3_SWIER_Field_Array is array (84 .. 86) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for SWIER3_SWIER
   type SWIER3_SWIER_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SWIER as a value
            Val : STM32H753.UInt3;
         when True =>
            --  SWIER as an array
            Arr : SWIER3_SWIER_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for SWIER3_SWIER_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  EXTI software interrupt event register
   type SWIER3_Register is record
      --  unspecified
      Reserved_0_17  : STM32H753.UInt18 := 16#0#;
      --  Software interrupt on line x+64
      SWIER82        : Boolean := False;
      --  unspecified
      Reserved_19_19 : STM32H753.Bit := 16#0#;
      --  Software interrupt on line x+64
      SWIER          : SWIER3_SWIER_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_23_31 : STM32H753.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SWIER3_Register use record
      Reserved_0_17  at 0 range 0 .. 17;
      SWIER82        at 0 range 18 .. 18;
      Reserved_19_19 at 0 range 19 .. 19;
      SWIER          at 0 range 20 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   --  EXTI D3 pending mask register
   type D3PMR3_Register is record
      --  unspecified
      Reserved_0_23  : STM32H753.UInt24 := 16#0#;
      --  D3 Pending Mask on Event input x+64
      MR88           : Boolean := False;
      --  unspecified
      Reserved_25_31 : STM32H753.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for D3PMR3_Register use record
      Reserved_0_23  at 0 range 0 .. 23;
      MR88           at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   subtype D3PCR3H_PCS88_Field is STM32H753.UInt2;

   --  EXTI D3 pending clear selection register high
   type D3PCR3H_Register is record
      --  unspecified
      Reserved_0_17  : STM32H753.UInt18 := 16#0#;
      --  D3 Pending request clear input signal selection on Event input x=
      --  truncate N+160/2
      PCS88          : D3PCR3H_PCS88_Field := 16#0#;
      --  unspecified
      Reserved_20_31 : STM32H753.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for D3PCR3H_Register use record
      Reserved_0_17  at 0 range 0 .. 17;
      PCS88          at 0 range 18 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  CPUIMR1_MR array
   type CPUIMR1_MR_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  EXTI interrupt mask register
   type CPUIMR1_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MR as a value
            Val : STM32H753.UInt32;
         when True =>
            --  MR as an array
            Arr : CPUIMR1_MR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPUIMR1_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  CPUEMR1_MR array
   type CPUEMR1_MR_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  EXTI event mask register
   type CPUEMR1_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MR as a value
            Val : STM32H753.UInt32;
         when True =>
            --  MR as an array
            Arr : CPUEMR1_MR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPUEMR1_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  CPUPR1_PR array
   type CPUPR1_PR_Field_Array is array (0 .. 21) of Boolean
     with Component_Size => 1, Size => 22;

   --  Type definition for CPUPR1_PR
   type CPUPR1_PR_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PR as a value
            Val : STM32H753.UInt22;
         when True =>
            --  PR as an array
            Arr : CPUPR1_PR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 22;

   for CPUPR1_PR_Field use record
      Val at 0 range 0 .. 21;
      Arr at 0 range 0 .. 21;
   end record;

   --  EXTI pending register
   type CPUPR1_Register is record
      --  CPU Event mask on Event input x
      PR             : CPUPR1_PR_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_22_31 : STM32H753.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPUPR1_Register use record
      PR             at 0 range 0 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   --  CPUIMR2_MR array
   type CPUIMR2_MR_Field_Array is array (0 .. 12) of Boolean
     with Component_Size => 1, Size => 13;

   --  Type definition for CPUIMR2_MR
   type CPUIMR2_MR_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MR as a value
            Val : STM32H753.UInt13;
         when True =>
            --  MR as an array
            Arr : CPUIMR2_MR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 13;

   for CPUIMR2_MR_Field use record
      Val at 0 range 0 .. 12;
      Arr at 0 range 0 .. 12;
   end record;

   --  CPUIMR2_MR array
   type CPUIMR2_MR_Field_Array_1 is array (14 .. 31) of Boolean
     with Component_Size => 1, Size => 18;

   --  Type definition for CPUIMR2_MR
   type CPUIMR2_MR_Field_1
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MR as a value
            Val : STM32H753.UInt18;
         when True =>
            --  MR as an array
            Arr : CPUIMR2_MR_Field_Array_1;
      end case;
   end record
     with Unchecked_Union, Size => 18;

   for CPUIMR2_MR_Field_1 use record
      Val at 0 range 0 .. 17;
      Arr at 0 range 0 .. 17;
   end record;

   --  EXTI interrupt mask register
   type CPUIMR2_Register is record
      --  CPU Interrupt Mask on Direct Event input x+32
      MR             : CPUIMR2_MR_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_13_13 : STM32H753.Bit := 16#0#;
      --  CPU Interrupt Mask on Direct Event input x+32
      MR_1           : CPUIMR2_MR_Field_1 :=
                        (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPUIMR2_Register use record
      MR             at 0 range 0 .. 12;
      Reserved_13_13 at 0 range 13 .. 13;
      MR_1           at 0 range 14 .. 31;
   end record;

   --  CPUEMR2_MR array
   type CPUEMR2_MR_Field_Array is array (32 .. 44) of Boolean
     with Component_Size => 1, Size => 13;

   --  Type definition for CPUEMR2_MR
   type CPUEMR2_MR_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MR as a value
            Val : STM32H753.UInt13;
         when True =>
            --  MR as an array
            Arr : CPUEMR2_MR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 13;

   for CPUEMR2_MR_Field use record
      Val at 0 range 0 .. 12;
      Arr at 0 range 0 .. 12;
   end record;

   --  CPUEMR2_MR array
   type CPUEMR2_MR_Field_Array_1 is array (46 .. 63) of Boolean
     with Component_Size => 1, Size => 18;

   --  Type definition for CPUEMR2_MR
   type CPUEMR2_MR_Field_1
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MR as a value
            Val : STM32H753.UInt18;
         when True =>
            --  MR as an array
            Arr : CPUEMR2_MR_Field_Array_1;
      end case;
   end record
     with Unchecked_Union, Size => 18;

   for CPUEMR2_MR_Field_1 use record
      Val at 0 range 0 .. 17;
      Arr at 0 range 0 .. 17;
   end record;

   --  EXTI event mask register
   type CPUEMR2_Register is record
      --  CPU Interrupt Mask on Direct Event input x+32
      MR             : CPUEMR2_MR_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_13_13 : STM32H753.Bit := 16#0#;
      --  CPU Interrupt Mask on Direct Event input x+32
      MR_1           : CPUEMR2_MR_Field_1 :=
                        (As_Array => False, Val => 16#0#);
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPUEMR2_Register use record
      MR             at 0 range 0 .. 12;
      Reserved_13_13 at 0 range 13 .. 13;
      MR_1           at 0 range 14 .. 31;
   end record;

   --  EXTI pending register
   type CPUPR2_Register is record
      --  unspecified
      Reserved_0_16  : STM32H753.UInt17;
      --  Read-only. Configurable event inputs x+32 Pending bit
      PR49           : Boolean;
      --  unspecified
      Reserved_18_18 : STM32H753.Bit;
      --  Read-only. Configurable event inputs x+32 Pending bit
      PR51           : Boolean;
      --  unspecified
      Reserved_20_31 : STM32H753.UInt12;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPUPR2_Register use record
      Reserved_0_16  at 0 range 0 .. 16;
      PR49           at 0 range 17 .. 17;
      Reserved_18_18 at 0 range 18 .. 18;
      PR51           at 0 range 19 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   --  CPUIMR3_MR array
   type CPUIMR3_MR_Field_Array is array (64 .. 80) of Boolean
     with Component_Size => 1, Size => 17;

   --  Type definition for CPUIMR3_MR
   type CPUIMR3_MR_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MR as a value
            Val : STM32H753.UInt17;
         when True =>
            --  MR as an array
            Arr : CPUIMR3_MR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 17;

   for CPUIMR3_MR_Field use record
      Val at 0 range 0 .. 16;
      Arr at 0 range 0 .. 16;
   end record;

   --  CPUIMR3_MR array
   type CPUIMR3_MR_Field_Array_1 is array (84 .. 88) of Boolean
     with Component_Size => 1, Size => 5;

   --  Type definition for CPUIMR3_MR
   type CPUIMR3_MR_Field_1
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MR as a value
            Val : STM32H753.UInt5;
         when True =>
            --  MR as an array
            Arr : CPUIMR3_MR_Field_Array_1;
      end case;
   end record
     with Unchecked_Union, Size => 5;

   for CPUIMR3_MR_Field_1 use record
      Val at 0 range 0 .. 4;
      Arr at 0 range 0 .. 4;
   end record;

   --  EXTI interrupt mask register
   type CPUIMR3_Register is record
      --  Read-only. CPU Interrupt Mask on Direct Event input x+64
      MR             : CPUIMR3_MR_Field;
      --  unspecified
      Reserved_17_17 : STM32H753.Bit;
      --  Read-only. CPU Interrupt Mask on Direct Event input x+64
      MR82           : Boolean;
      --  unspecified
      Reserved_19_19 : STM32H753.Bit;
      --  Read-only. CPU Interrupt Mask on Direct Event input x+64
      MR_1           : CPUIMR3_MR_Field_1;
      --  unspecified
      Reserved_25_31 : STM32H753.UInt7;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPUIMR3_Register use record
      MR             at 0 range 0 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      MR82           at 0 range 18 .. 18;
      Reserved_19_19 at 0 range 19 .. 19;
      MR_1           at 0 range 20 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   --  CPUEMR3_MR array
   type CPUEMR3_MR_Field_Array is array (64 .. 80) of Boolean
     with Component_Size => 1, Size => 17;

   --  Type definition for CPUEMR3_MR
   type CPUEMR3_MR_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MR as a value
            Val : STM32H753.UInt17;
         when True =>
            --  MR as an array
            Arr : CPUEMR3_MR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 17;

   for CPUEMR3_MR_Field use record
      Val at 0 range 0 .. 16;
      Arr at 0 range 0 .. 16;
   end record;

   --  CPUEMR3_MR array
   type CPUEMR3_MR_Field_Array_1 is array (84 .. 88) of Boolean
     with Component_Size => 1, Size => 5;

   --  Type definition for CPUEMR3_MR
   type CPUEMR3_MR_Field_1
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  MR as a value
            Val : STM32H753.UInt5;
         when True =>
            --  MR as an array
            Arr : CPUEMR3_MR_Field_Array_1;
      end case;
   end record
     with Unchecked_Union, Size => 5;

   for CPUEMR3_MR_Field_1 use record
      Val at 0 range 0 .. 4;
      Arr at 0 range 0 .. 4;
   end record;

   --  EXTI event mask register
   type CPUEMR3_Register is record
      --  Read-only. CPU Event mask on Event input x+64
      MR             : CPUEMR3_MR_Field;
      --  unspecified
      Reserved_17_17 : STM32H753.Bit;
      --  Read-only. CPU Event mask on Event input x+64
      MR82           : Boolean;
      --  unspecified
      Reserved_19_19 : STM32H753.Bit;
      --  Read-only. CPU Event mask on Event input x+64
      MR_1           : CPUEMR3_MR_Field_1;
      --  unspecified
      Reserved_25_31 : STM32H753.UInt7;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPUEMR3_Register use record
      MR             at 0 range 0 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      MR82           at 0 range 18 .. 18;
      Reserved_19_19 at 0 range 19 .. 19;
      MR_1           at 0 range 20 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   --  CPUPR3_PR array
   type CPUPR3_PR_Field_Array is array (84 .. 86) of Boolean
     with Component_Size => 1, Size => 3;

   --  Type definition for CPUPR3_PR
   type CPUPR3_PR_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  PR as a value
            Val : STM32H753.UInt3;
         when True =>
            --  PR as an array
            Arr : CPUPR3_PR_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 3;

   for CPUPR3_PR_Field use record
      Val at 0 range 0 .. 2;
      Arr at 0 range 0 .. 2;
   end record;

   --  EXTI pending register
   type CPUPR3_Register is record
      --  unspecified
      Reserved_0_17  : STM32H753.UInt18;
      --  Read-only. Configurable event inputs x+64 Pending bit
      PR82           : Boolean;
      --  unspecified
      Reserved_19_19 : STM32H753.Bit;
      --  Read-only. Configurable event inputs x+64 Pending bit
      PR             : CPUPR3_PR_Field;
      --  unspecified
      Reserved_23_31 : STM32H753.UInt9;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CPUPR3_Register use record
      Reserved_0_17  at 0 range 0 .. 17;
      PR82           at 0 range 18 .. 18;
      Reserved_19_19 at 0 range 19 .. 19;
      PR             at 0 range 20 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  External interrupt/event controller
   type EXTI_Peripheral is record
      --  EXTI rising trigger selection register
      RTSR1   : aliased RTSR1_Register;
      --  EXTI falling trigger selection register
      FTSR1   : aliased FTSR1_Register;
      --  EXTI software interrupt event register
      SWIER1  : aliased SWIER1_Register;
      --  EXTI D3 pending mask register
      D3PMR1  : aliased D3PMR1_Register;
      --  EXTI D3 pending clear selection register low
      D3PCR1L : aliased D3PCR1L_Register;
      --  EXTI D3 pending clear selection register high
      D3PCR1H : aliased D3PCR1H_Register;
      --  EXTI rising trigger selection register
      RTSR2   : aliased RTSR2_Register;
      --  EXTI falling trigger selection register
      FTSR2   : aliased FTSR2_Register;
      --  EXTI software interrupt event register
      SWIER2  : aliased SWIER2_Register;
      --  EXTI D3 pending mask register
      D3PMR2  : aliased D3PMR2_Register;
      --  EXTI D3 pending clear selection register low
      D3PCR2L : aliased D3PCR2L_Register;
      --  EXTI D3 pending clear selection register high
      D3PCR2H : aliased D3PCR2H_Register;
      --  EXTI rising trigger selection register
      RTSR3   : aliased RTSR3_Register;
      --  EXTI falling trigger selection register
      FTSR3   : aliased FTSR3_Register;
      --  EXTI software interrupt event register
      SWIER3  : aliased SWIER3_Register;
      --  EXTI D3 pending mask register
      D3PMR3  : aliased D3PMR3_Register;
      --  EXTI D3 pending clear selection register high
      D3PCR3H : aliased D3PCR3H_Register;
      --  EXTI interrupt mask register
      CPUIMR1 : aliased CPUIMR1_Register;
      --  EXTI event mask register
      CPUEMR1 : aliased CPUEMR1_Register;
      --  EXTI pending register
      CPUPR1  : aliased CPUPR1_Register;
      --  EXTI interrupt mask register
      CPUIMR2 : aliased CPUIMR2_Register;
      --  EXTI event mask register
      CPUEMR2 : aliased CPUEMR2_Register;
      --  EXTI pending register
      CPUPR2  : aliased CPUPR2_Register;
      --  EXTI interrupt mask register
      CPUIMR3 : aliased CPUIMR3_Register;
      --  EXTI event mask register
      CPUEMR3 : aliased CPUEMR3_Register;
      --  EXTI pending register
      CPUPR3  : aliased CPUPR3_Register;
   end record
     with Volatile;

   for EXTI_Peripheral use record
      RTSR1   at 16#0# range 0 .. 31;
      FTSR1   at 16#4# range 0 .. 31;
      SWIER1  at 16#8# range 0 .. 31;
      D3PMR1  at 16#C# range 0 .. 31;
      D3PCR1L at 16#10# range 0 .. 31;
      D3PCR1H at 16#14# range 0 .. 31;
      RTSR2   at 16#20# range 0 .. 31;
      FTSR2   at 16#24# range 0 .. 31;
      SWIER2  at 16#28# range 0 .. 31;
      D3PMR2  at 16#2C# range 0 .. 31;
      D3PCR2L at 16#30# range 0 .. 31;
      D3PCR2H at 16#34# range 0 .. 31;
      RTSR3   at 16#40# range 0 .. 31;
      FTSR3   at 16#44# range 0 .. 31;
      SWIER3  at 16#48# range 0 .. 31;
      D3PMR3  at 16#4C# range 0 .. 31;
      D3PCR3H at 16#54# range 0 .. 31;
      CPUIMR1 at 16#80# range 0 .. 31;
      CPUEMR1 at 16#84# range 0 .. 31;
      CPUPR1  at 16#88# range 0 .. 31;
      CPUIMR2 at 16#90# range 0 .. 31;
      CPUEMR2 at 16#94# range 0 .. 31;
      CPUPR2  at 16#98# range 0 .. 31;
      CPUIMR3 at 16#A0# range 0 .. 31;
      CPUEMR3 at 16#A4# range 0 .. 31;
      CPUPR3  at 16#A8# range 0 .. 31;
   end record;

   --  External interrupt/event controller
   EXTI_Periph : aliased EXTI_Peripheral
     with Import, Address => EXTI_Base;

end STM32H753.EXTI;
