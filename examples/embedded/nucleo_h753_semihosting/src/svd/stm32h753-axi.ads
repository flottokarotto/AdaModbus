pragma Style_Checks (Off);

--  This spec has been automatically generated from STM32H753.svd


with System;

package STM32H753.AXI is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   subtype AXI_PERIPH_ID_4_JEP106CON_Field is STM32H753.UInt4;
   subtype AXI_PERIPH_ID_4_KCOUNT4_Field is STM32H753.UInt4;

   --  AXI interconnect - peripheral ID4 register
   type AXI_PERIPH_ID_4_Register is record
      --  Read-only. JEP106 continuation code
      JEP106CON     : AXI_PERIPH_ID_4_JEP106CON_Field;
      --  Read-only. Register file size
      KCOUNT4       : AXI_PERIPH_ID_4_KCOUNT4_Field;
      --  unspecified
      Reserved_8_31 : STM32H753.UInt24;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_PERIPH_ID_4_Register use record
      JEP106CON     at 0 range 0 .. 3;
      KCOUNT4       at 0 range 4 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype AXI_PERIPH_ID_0_PARTNUM_Field is STM32H753.Byte;

   --  AXI interconnect - peripheral ID0 register
   type AXI_PERIPH_ID_0_Register is record
      --  Read-only. Peripheral part number bits 0 to 7
      PARTNUM       : AXI_PERIPH_ID_0_PARTNUM_Field;
      --  unspecified
      Reserved_8_31 : STM32H753.UInt24;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_PERIPH_ID_0_Register use record
      PARTNUM       at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype AXI_PERIPH_ID_1_PARTNUM_Field is STM32H753.UInt4;
   subtype AXI_PERIPH_ID_1_JEP106I_Field is STM32H753.UInt4;

   --  AXI interconnect - peripheral ID1 register
   type AXI_PERIPH_ID_1_Register is record
      --  Read-only. Peripheral part number bits 8 to 11
      PARTNUM       : AXI_PERIPH_ID_1_PARTNUM_Field;
      --  Read-only. JEP106 identity bits 0 to 3
      JEP106I       : AXI_PERIPH_ID_1_JEP106I_Field;
      --  unspecified
      Reserved_8_31 : STM32H753.UInt24;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_PERIPH_ID_1_Register use record
      PARTNUM       at 0 range 0 .. 3;
      JEP106I       at 0 range 4 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype AXI_PERIPH_ID_2_JEP106ID_Field is STM32H753.UInt3;
   subtype AXI_PERIPH_ID_2_REVISION_Field is STM32H753.UInt4;

   --  AXI interconnect - peripheral ID2 register
   type AXI_PERIPH_ID_2_Register is record
      --  Read-only. JEP106 Identity bits 4 to 6
      JEP106ID      : AXI_PERIPH_ID_2_JEP106ID_Field;
      --  Read-only. JEP106 code flag
      JEDEC         : Boolean;
      --  Read-only. Peripheral revision number
      REVISION      : AXI_PERIPH_ID_2_REVISION_Field;
      --  unspecified
      Reserved_8_31 : STM32H753.UInt24;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_PERIPH_ID_2_Register use record
      JEP106ID      at 0 range 0 .. 2;
      JEDEC         at 0 range 3 .. 3;
      REVISION      at 0 range 4 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype AXI_PERIPH_ID_3_CUST_MOD_NUM_Field is STM32H753.UInt4;
   subtype AXI_PERIPH_ID_3_REV_AND_Field is STM32H753.UInt4;

   --  AXI interconnect - peripheral ID3 register
   type AXI_PERIPH_ID_3_Register is record
      --  Read-only. Customer modification
      CUST_MOD_NUM  : AXI_PERIPH_ID_3_CUST_MOD_NUM_Field;
      --  Read-only. Customer version
      REV_AND       : AXI_PERIPH_ID_3_REV_AND_Field;
      --  unspecified
      Reserved_8_31 : STM32H753.UInt24;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_PERIPH_ID_3_Register use record
      CUST_MOD_NUM  at 0 range 0 .. 3;
      REV_AND       at 0 range 4 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype AXI_COMP_ID_0_PREAMBLE_Field is STM32H753.Byte;

   --  AXI interconnect - component ID0 register
   type AXI_COMP_ID_0_Register is record
      --  Read-only. Preamble bits 0 to 7
      PREAMBLE      : AXI_COMP_ID_0_PREAMBLE_Field;
      --  unspecified
      Reserved_8_31 : STM32H753.UInt24;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_COMP_ID_0_Register use record
      PREAMBLE      at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype AXI_COMP_ID_1_PREAMBLE_Field is STM32H753.UInt4;
   subtype AXI_COMP_ID_1_CLASS_Field is STM32H753.UInt4;

   --  AXI interconnect - component ID1 register
   type AXI_COMP_ID_1_Register is record
      --  Read-only. Preamble bits 8 to 11
      PREAMBLE      : AXI_COMP_ID_1_PREAMBLE_Field;
      --  Read-only. Component class
      CLASS         : AXI_COMP_ID_1_CLASS_Field;
      --  unspecified
      Reserved_8_31 : STM32H753.UInt24;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_COMP_ID_1_Register use record
      PREAMBLE      at 0 range 0 .. 3;
      CLASS         at 0 range 4 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype AXI_COMP_ID_2_PREAMBLE_Field is STM32H753.Byte;

   --  AXI interconnect - component ID2 register
   type AXI_COMP_ID_2_Register is record
      --  Read-only. Preamble bits 12 to 19
      PREAMBLE      : AXI_COMP_ID_2_PREAMBLE_Field;
      --  unspecified
      Reserved_8_31 : STM32H753.UInt24;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_COMP_ID_2_Register use record
      PREAMBLE      at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype AXI_COMP_ID_3_PREAMBLE_Field is STM32H753.Byte;

   --  AXI interconnect - component ID3 register
   type AXI_COMP_ID_3_Register is record
      --  Read-only. Preamble bits 20 to 27
      PREAMBLE      : AXI_COMP_ID_3_PREAMBLE_Field;
      --  unspecified
      Reserved_8_31 : STM32H753.UInt24;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_COMP_ID_3_Register use record
      PREAMBLE      at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  AXI interconnect - TARG x bus matrix issuing functionality register
   type AXI_TARG1_FN_MOD_ISS_BM_Register is record
      --  READ_ISS_OVERRIDE
      READ_ISS_OVERRIDE  : Boolean := False;
      --  Switch matrix write issuing override for target
      WRITE_ISS_OVERRIDE : Boolean := False;
      --  unspecified
      Reserved_2_31      : STM32H753.UInt30 := 16#1#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_TARG1_FN_MOD_ISS_BM_Register use record
      READ_ISS_OVERRIDE  at 0 range 0 .. 0;
      WRITE_ISS_OVERRIDE at 0 range 1 .. 1;
      Reserved_2_31      at 0 range 2 .. 31;
   end record;

   --  AXI interconnect - TARG x bus matrix functionality 2 register
   type AXI_TARG1_FN_MOD2_Register is record
      --  Disable packing of beats to match the output data width
      BYPASS_MERGE  : Boolean := False;
      --  unspecified
      Reserved_1_31 : STM32H753.UInt31 := 16#2#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_TARG1_FN_MOD2_Register use record
      BYPASS_MERGE  at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  AXI interconnect - TARG x long burst functionality modification
   type AXI_TARG1_FN_MOD_LB_Register is record
      --  Controls burst breaking of long bursts
      FN_MOD_LB     : Boolean := False;
      --  unspecified
      Reserved_1_31 : STM32H753.UInt31 := 16#2#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_TARG1_FN_MOD_LB_Register use record
      FN_MOD_LB     at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  AXI interconnect - TARG x long burst functionality modification
   type AXI_TARG1_FN_MOD_Register is record
      --  Override AMIB read issuing capability
      READ_ISS_OVERRIDE  : Boolean := False;
      --  Override AMIB write issuing capability
      WRITE_ISS_OVERRIDE : Boolean := False;
      --  unspecified
      Reserved_2_31      : STM32H753.UInt30 := 16#1#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_TARG1_FN_MOD_Register use record
      READ_ISS_OVERRIDE  at 0 range 0 .. 0;
      WRITE_ISS_OVERRIDE at 0 range 1 .. 1;
      Reserved_2_31      at 0 range 2 .. 31;
   end record;

   --  AXI interconnect - TARG x bus matrix issuing functionality register
   type AXI_TARG2_FN_MOD_ISS_BM_Register is record
      --  READ_ISS_OVERRIDE
      READ_ISS_OVERRIDE  : Boolean := False;
      --  Switch matrix write issuing override for target
      WRITE_ISS_OVERRIDE : Boolean := False;
      --  unspecified
      Reserved_2_31      : STM32H753.UInt30 := 16#1#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_TARG2_FN_MOD_ISS_BM_Register use record
      READ_ISS_OVERRIDE  at 0 range 0 .. 0;
      WRITE_ISS_OVERRIDE at 0 range 1 .. 1;
      Reserved_2_31      at 0 range 2 .. 31;
   end record;

   --  AXI interconnect - TARG x bus matrix functionality 2 register
   type AXI_TARG2_FN_MOD2_Register is record
      --  Disable packing of beats to match the output data width
      BYPASS_MERGE  : Boolean := False;
      --  unspecified
      Reserved_1_31 : STM32H753.UInt31 := 16#2#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_TARG2_FN_MOD2_Register use record
      BYPASS_MERGE  at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  AXI interconnect - TARG x long burst functionality modification
   type AXI_TARG2_FN_MOD_LB_Register is record
      --  Controls burst breaking of long bursts
      FN_MOD_LB     : Boolean := False;
      --  unspecified
      Reserved_1_31 : STM32H753.UInt31 := 16#2#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_TARG2_FN_MOD_LB_Register use record
      FN_MOD_LB     at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  AXI interconnect - TARG x long burst functionality modification
   type AXI_TARG2_FN_MOD_Register is record
      --  Override AMIB read issuing capability
      READ_ISS_OVERRIDE  : Boolean := False;
      --  Override AMIB write issuing capability
      WRITE_ISS_OVERRIDE : Boolean := False;
      --  unspecified
      Reserved_2_31      : STM32H753.UInt30 := 16#1#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_TARG2_FN_MOD_Register use record
      READ_ISS_OVERRIDE  at 0 range 0 .. 0;
      WRITE_ISS_OVERRIDE at 0 range 1 .. 1;
      Reserved_2_31      at 0 range 2 .. 31;
   end record;

   --  AXI interconnect - TARG x bus matrix issuing functionality register
   type AXI_TARG3_FN_MOD_ISS_BM_Register is record
      --  READ_ISS_OVERRIDE
      READ_ISS_OVERRIDE  : Boolean := False;
      --  Switch matrix write issuing override for target
      WRITE_ISS_OVERRIDE : Boolean := False;
      --  unspecified
      Reserved_2_31      : STM32H753.UInt30 := 16#1#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_TARG3_FN_MOD_ISS_BM_Register use record
      READ_ISS_OVERRIDE  at 0 range 0 .. 0;
      WRITE_ISS_OVERRIDE at 0 range 1 .. 1;
      Reserved_2_31      at 0 range 2 .. 31;
   end record;

   --  AXI interconnect - TARG x bus matrix issuing functionality register
   type AXI_TARG4_FN_MOD_ISS_BM_Register is record
      --  READ_ISS_OVERRIDE
      READ_ISS_OVERRIDE  : Boolean := False;
      --  Switch matrix write issuing override for target
      WRITE_ISS_OVERRIDE : Boolean := False;
      --  unspecified
      Reserved_2_31      : STM32H753.UInt30 := 16#1#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_TARG4_FN_MOD_ISS_BM_Register use record
      READ_ISS_OVERRIDE  at 0 range 0 .. 0;
      WRITE_ISS_OVERRIDE at 0 range 1 .. 1;
      Reserved_2_31      at 0 range 2 .. 31;
   end record;

   --  AXI interconnect - TARG x bus matrix issuing functionality register
   type AXI_TARG5_FN_MOD_ISS_BM_Register is record
      --  READ_ISS_OVERRIDE
      READ_ISS_OVERRIDE  : Boolean := False;
      --  Switch matrix write issuing override for target
      WRITE_ISS_OVERRIDE : Boolean := False;
      --  unspecified
      Reserved_2_31      : STM32H753.UInt30 := 16#1#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_TARG5_FN_MOD_ISS_BM_Register use record
      READ_ISS_OVERRIDE  at 0 range 0 .. 0;
      WRITE_ISS_OVERRIDE at 0 range 1 .. 1;
      Reserved_2_31      at 0 range 2 .. 31;
   end record;

   --  AXI interconnect - TARG x bus matrix issuing functionality register
   type AXI_TARG6_FN_MOD_ISS_BM_Register is record
      --  READ_ISS_OVERRIDE
      READ_ISS_OVERRIDE  : Boolean := False;
      --  Switch matrix write issuing override for target
      WRITE_ISS_OVERRIDE : Boolean := False;
      --  unspecified
      Reserved_2_31      : STM32H753.UInt30 := 16#1#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_TARG6_FN_MOD_ISS_BM_Register use record
      READ_ISS_OVERRIDE  at 0 range 0 .. 0;
      WRITE_ISS_OVERRIDE at 0 range 1 .. 1;
      Reserved_2_31      at 0 range 2 .. 31;
   end record;

   --  AXI interconnect - TARG x bus matrix issuing functionality register
   type AXI_TARG7_FN_MOD_ISS_BM_Register is record
      --  READ_ISS_OVERRIDE
      READ_ISS_OVERRIDE  : Boolean := False;
      --  Switch matrix write issuing override for target
      WRITE_ISS_OVERRIDE : Boolean := False;
      --  unspecified
      Reserved_2_31      : STM32H753.UInt30 := 16#1#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_TARG7_FN_MOD_ISS_BM_Register use record
      READ_ISS_OVERRIDE  at 0 range 0 .. 0;
      WRITE_ISS_OVERRIDE at 0 range 1 .. 1;
      Reserved_2_31      at 0 range 2 .. 31;
   end record;

   --  AXI interconnect - TARG x bus matrix functionality 2 register
   type AXI_TARG7_FN_MOD2_Register is record
      --  Disable packing of beats to match the output data width
      BYPASS_MERGE  : Boolean := False;
      --  unspecified
      Reserved_1_31 : STM32H753.UInt31 := 16#2#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_TARG7_FN_MOD2_Register use record
      BYPASS_MERGE  at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  AXI interconnect - TARG x long burst functionality modification
   type AXI_TARG7_FN_MOD_Register is record
      --  Override AMIB read issuing capability
      READ_ISS_OVERRIDE  : Boolean := False;
      --  Override AMIB write issuing capability
      WRITE_ISS_OVERRIDE : Boolean := False;
      --  unspecified
      Reserved_2_31      : STM32H753.UInt30 := 16#1#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_TARG7_FN_MOD_Register use record
      READ_ISS_OVERRIDE  at 0 range 0 .. 0;
      WRITE_ISS_OVERRIDE at 0 range 1 .. 1;
      Reserved_2_31      at 0 range 2 .. 31;
   end record;

   --  AXI interconnect - INI x functionality modification 2 register
   type AXI_INI1_FN_MOD2_Register is record
      --  Disables alteration of transactions by the up-sizer unless required
      --  by the protocol
      BYPASS_MERGE  : Boolean := False;
      --  unspecified
      Reserved_1_31 : STM32H753.UInt31 := 16#2#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_INI1_FN_MOD2_Register use record
      BYPASS_MERGE  at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  AXI interconnect - INI x AHB functionality modification register
   type AXI_INI1_FN_MOD_AHB_Register is record
      --  Converts all AHB-Lite write transactions to a series of single beat
      --  AXI
      RD_INC_OVERRIDE : Boolean := False;
      --  Converts all AHB-Lite read transactions to a series of single beat
      --  AXI
      WR_INC_OVERRIDE : Boolean := False;
      --  unspecified
      Reserved_2_31   : STM32H753.UInt30 := 16#1#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_INI1_FN_MOD_AHB_Register use record
      RD_INC_OVERRIDE at 0 range 0 .. 0;
      WR_INC_OVERRIDE at 0 range 1 .. 1;
      Reserved_2_31   at 0 range 2 .. 31;
   end record;

   subtype AXI_INI1_READ_QOS_AR_QOS_Field is STM32H753.UInt4;

   --  AXI interconnect - INI x read QoS register
   type AXI_INI1_READ_QOS_Register is record
      --  Read channel QoS setting
      AR_QOS        : AXI_INI1_READ_QOS_AR_QOS_Field := 16#4#;
      --  unspecified
      Reserved_4_31 : STM32H753.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_INI1_READ_QOS_Register use record
      AR_QOS        at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   subtype AXI_INI1_WRITE_QOS_AW_QOS_Field is STM32H753.UInt4;

   --  AXI interconnect - INI x write QoS register
   type AXI_INI1_WRITE_QOS_Register is record
      --  Write channel QoS setting
      AW_QOS        : AXI_INI1_WRITE_QOS_AW_QOS_Field := 16#4#;
      --  unspecified
      Reserved_4_31 : STM32H753.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_INI1_WRITE_QOS_Register use record
      AW_QOS        at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  AXI interconnect - INI x issuing functionality modification register
   type AXI_INI1_FN_MOD_Register is record
      --  Override ASIB read issuing capability
      READ_ISS_OVERRIDE  : Boolean := False;
      --  Override ASIB write issuing capability
      WRITE_ISS_OVERRIDE : Boolean := False;
      --  unspecified
      Reserved_2_31      : STM32H753.UInt30 := 16#1#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_INI1_FN_MOD_Register use record
      READ_ISS_OVERRIDE  at 0 range 0 .. 0;
      WRITE_ISS_OVERRIDE at 0 range 1 .. 1;
      Reserved_2_31      at 0 range 2 .. 31;
   end record;

   subtype AXI_INI2_READ_QOS_AR_QOS_Field is STM32H753.UInt4;

   --  AXI interconnect - INI x read QoS register
   type AXI_INI2_READ_QOS_Register is record
      --  Read channel QoS setting
      AR_QOS        : AXI_INI2_READ_QOS_AR_QOS_Field := 16#4#;
      --  unspecified
      Reserved_4_31 : STM32H753.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_INI2_READ_QOS_Register use record
      AR_QOS        at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   subtype AXI_INI2_WRITE_QOS_AW_QOS_Field is STM32H753.UInt4;

   --  AXI interconnect - INI x write QoS register
   type AXI_INI2_WRITE_QOS_Register is record
      --  Write channel QoS setting
      AW_QOS        : AXI_INI2_WRITE_QOS_AW_QOS_Field := 16#4#;
      --  unspecified
      Reserved_4_31 : STM32H753.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_INI2_WRITE_QOS_Register use record
      AW_QOS        at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  AXI interconnect - INI x issuing functionality modification register
   type AXI_INI2_FN_MOD_Register is record
      --  Override ASIB read issuing capability
      READ_ISS_OVERRIDE  : Boolean := False;
      --  Override ASIB write issuing capability
      WRITE_ISS_OVERRIDE : Boolean := False;
      --  unspecified
      Reserved_2_31      : STM32H753.UInt30 := 16#1#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_INI2_FN_MOD_Register use record
      READ_ISS_OVERRIDE  at 0 range 0 .. 0;
      WRITE_ISS_OVERRIDE at 0 range 1 .. 1;
      Reserved_2_31      at 0 range 2 .. 31;
   end record;

   --  AXI interconnect - INI x functionality modification 2 register
   type AXI_INI3_FN_MOD2_Register is record
      --  Disables alteration of transactions by the up-sizer unless required
      --  by the protocol
      BYPASS_MERGE  : Boolean := False;
      --  unspecified
      Reserved_1_31 : STM32H753.UInt31 := 16#2#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_INI3_FN_MOD2_Register use record
      BYPASS_MERGE  at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   --  AXI interconnect - INI x AHB functionality modification register
   type AXI_INI3_FN_MOD_AHB_Register is record
      --  Converts all AHB-Lite write transactions to a series of single beat
      --  AXI
      RD_INC_OVERRIDE : Boolean := False;
      --  Converts all AHB-Lite read transactions to a series of single beat
      --  AXI
      WR_INC_OVERRIDE : Boolean := False;
      --  unspecified
      Reserved_2_31   : STM32H753.UInt30 := 16#1#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_INI3_FN_MOD_AHB_Register use record
      RD_INC_OVERRIDE at 0 range 0 .. 0;
      WR_INC_OVERRIDE at 0 range 1 .. 1;
      Reserved_2_31   at 0 range 2 .. 31;
   end record;

   subtype AXI_INI3_READ_QOS_AR_QOS_Field is STM32H753.UInt4;

   --  AXI interconnect - INI x read QoS register
   type AXI_INI3_READ_QOS_Register is record
      --  Read channel QoS setting
      AR_QOS        : AXI_INI3_READ_QOS_AR_QOS_Field := 16#4#;
      --  unspecified
      Reserved_4_31 : STM32H753.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_INI3_READ_QOS_Register use record
      AR_QOS        at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   subtype AXI_INI3_WRITE_QOS_AW_QOS_Field is STM32H753.UInt4;

   --  AXI interconnect - INI x write QoS register
   type AXI_INI3_WRITE_QOS_Register is record
      --  Write channel QoS setting
      AW_QOS        : AXI_INI3_WRITE_QOS_AW_QOS_Field := 16#4#;
      --  unspecified
      Reserved_4_31 : STM32H753.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_INI3_WRITE_QOS_Register use record
      AW_QOS        at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  AXI interconnect - INI x issuing functionality modification register
   type AXI_INI3_FN_MOD_Register is record
      --  Override ASIB read issuing capability
      READ_ISS_OVERRIDE  : Boolean := False;
      --  Override ASIB write issuing capability
      WRITE_ISS_OVERRIDE : Boolean := False;
      --  unspecified
      Reserved_2_31      : STM32H753.UInt30 := 16#1#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_INI3_FN_MOD_Register use record
      READ_ISS_OVERRIDE  at 0 range 0 .. 0;
      WRITE_ISS_OVERRIDE at 0 range 1 .. 1;
      Reserved_2_31      at 0 range 2 .. 31;
   end record;

   subtype AXI_INI4_READ_QOS_AR_QOS_Field is STM32H753.UInt4;

   --  AXI interconnect - INI x read QoS register
   type AXI_INI4_READ_QOS_Register is record
      --  Read channel QoS setting
      AR_QOS        : AXI_INI4_READ_QOS_AR_QOS_Field := 16#4#;
      --  unspecified
      Reserved_4_31 : STM32H753.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_INI4_READ_QOS_Register use record
      AR_QOS        at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   subtype AXI_INI4_WRITE_QOS_AW_QOS_Field is STM32H753.UInt4;

   --  AXI interconnect - INI x write QoS register
   type AXI_INI4_WRITE_QOS_Register is record
      --  Write channel QoS setting
      AW_QOS        : AXI_INI4_WRITE_QOS_AW_QOS_Field := 16#4#;
      --  unspecified
      Reserved_4_31 : STM32H753.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_INI4_WRITE_QOS_Register use record
      AW_QOS        at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  AXI interconnect - INI x issuing functionality modification register
   type AXI_INI4_FN_MOD_Register is record
      --  Override ASIB read issuing capability
      READ_ISS_OVERRIDE  : Boolean := False;
      --  Override ASIB write issuing capability
      WRITE_ISS_OVERRIDE : Boolean := False;
      --  unspecified
      Reserved_2_31      : STM32H753.UInt30 := 16#1#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_INI4_FN_MOD_Register use record
      READ_ISS_OVERRIDE  at 0 range 0 .. 0;
      WRITE_ISS_OVERRIDE at 0 range 1 .. 1;
      Reserved_2_31      at 0 range 2 .. 31;
   end record;

   subtype AXI_INI5_READ_QOS_AR_QOS_Field is STM32H753.UInt4;

   --  AXI interconnect - INI x read QoS register
   type AXI_INI5_READ_QOS_Register is record
      --  Read channel QoS setting
      AR_QOS        : AXI_INI5_READ_QOS_AR_QOS_Field := 16#4#;
      --  unspecified
      Reserved_4_31 : STM32H753.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_INI5_READ_QOS_Register use record
      AR_QOS        at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   subtype AXI_INI5_WRITE_QOS_AW_QOS_Field is STM32H753.UInt4;

   --  AXI interconnect - INI x write QoS register
   type AXI_INI5_WRITE_QOS_Register is record
      --  Write channel QoS setting
      AW_QOS        : AXI_INI5_WRITE_QOS_AW_QOS_Field := 16#4#;
      --  unspecified
      Reserved_4_31 : STM32H753.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_INI5_WRITE_QOS_Register use record
      AW_QOS        at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  AXI interconnect - INI x issuing functionality modification register
   type AXI_INI5_FN_MOD_Register is record
      --  Override ASIB read issuing capability
      READ_ISS_OVERRIDE  : Boolean := False;
      --  Override ASIB write issuing capability
      WRITE_ISS_OVERRIDE : Boolean := False;
      --  unspecified
      Reserved_2_31      : STM32H753.UInt30 := 16#1#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_INI5_FN_MOD_Register use record
      READ_ISS_OVERRIDE  at 0 range 0 .. 0;
      WRITE_ISS_OVERRIDE at 0 range 1 .. 1;
      Reserved_2_31      at 0 range 2 .. 31;
   end record;

   subtype AXI_INI6_READ_QOS_AR_QOS_Field is STM32H753.UInt4;

   --  AXI interconnect - INI x read QoS register
   type AXI_INI6_READ_QOS_Register is record
      --  Read channel QoS setting
      AR_QOS        : AXI_INI6_READ_QOS_AR_QOS_Field := 16#4#;
      --  unspecified
      Reserved_4_31 : STM32H753.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_INI6_READ_QOS_Register use record
      AR_QOS        at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   subtype AXI_INI6_WRITE_QOS_AW_QOS_Field is STM32H753.UInt4;

   --  AXI interconnect - INI x write QoS register
   type AXI_INI6_WRITE_QOS_Register is record
      --  Write channel QoS setting
      AW_QOS        : AXI_INI6_WRITE_QOS_AW_QOS_Field := 16#4#;
      --  unspecified
      Reserved_4_31 : STM32H753.UInt28 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_INI6_WRITE_QOS_Register use record
      AW_QOS        at 0 range 0 .. 3;
      Reserved_4_31 at 0 range 4 .. 31;
   end record;

   --  AXI interconnect - INI x issuing functionality modification register
   type AXI_INI6_FN_MOD_Register is record
      --  Override ASIB read issuing capability
      READ_ISS_OVERRIDE  : Boolean := False;
      --  Override ASIB write issuing capability
      WRITE_ISS_OVERRIDE : Boolean := False;
      --  unspecified
      Reserved_2_31      : STM32H753.UInt30 := 16#1#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for AXI_INI6_FN_MOD_Register use record
      READ_ISS_OVERRIDE  at 0 range 0 .. 0;
      WRITE_ISS_OVERRIDE at 0 range 1 .. 1;
      Reserved_2_31      at 0 range 2 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  AXI interconnect registers
   type AXI_Peripheral is record
      --  AXI interconnect - peripheral ID4 register
      AXI_PERIPH_ID_4         : aliased AXI_PERIPH_ID_4_Register;
      --  AXI interconnect - peripheral ID0 register
      AXI_PERIPH_ID_0         : aliased AXI_PERIPH_ID_0_Register;
      --  AXI interconnect - peripheral ID1 register
      AXI_PERIPH_ID_1         : aliased AXI_PERIPH_ID_1_Register;
      --  AXI interconnect - peripheral ID2 register
      AXI_PERIPH_ID_2         : aliased AXI_PERIPH_ID_2_Register;
      --  AXI interconnect - peripheral ID3 register
      AXI_PERIPH_ID_3         : aliased AXI_PERIPH_ID_3_Register;
      --  AXI interconnect - component ID0 register
      AXI_COMP_ID_0           : aliased AXI_COMP_ID_0_Register;
      --  AXI interconnect - component ID1 register
      AXI_COMP_ID_1           : aliased AXI_COMP_ID_1_Register;
      --  AXI interconnect - component ID2 register
      AXI_COMP_ID_2           : aliased AXI_COMP_ID_2_Register;
      --  AXI interconnect - component ID3 register
      AXI_COMP_ID_3           : aliased AXI_COMP_ID_3_Register;
      --  AXI interconnect - TARG x bus matrix issuing functionality register
      AXI_TARG1_FN_MOD_ISS_BM : aliased AXI_TARG1_FN_MOD_ISS_BM_Register;
      --  AXI interconnect - TARG x bus matrix functionality 2 register
      AXI_TARG1_FN_MOD2       : aliased AXI_TARG1_FN_MOD2_Register;
      --  AXI interconnect - TARG x long burst functionality modification
      AXI_TARG1_FN_MOD_LB     : aliased AXI_TARG1_FN_MOD_LB_Register;
      --  AXI interconnect - TARG x long burst functionality modification
      AXI_TARG1_FN_MOD        : aliased AXI_TARG1_FN_MOD_Register;
      --  AXI interconnect - TARG x bus matrix issuing functionality register
      AXI_TARG2_FN_MOD_ISS_BM : aliased AXI_TARG2_FN_MOD_ISS_BM_Register;
      --  AXI interconnect - TARG x bus matrix functionality 2 register
      AXI_TARG2_FN_MOD2       : aliased AXI_TARG2_FN_MOD2_Register;
      --  AXI interconnect - TARG x long burst functionality modification
      AXI_TARG2_FN_MOD_LB     : aliased AXI_TARG2_FN_MOD_LB_Register;
      --  AXI interconnect - TARG x long burst functionality modification
      AXI_TARG2_FN_MOD        : aliased AXI_TARG2_FN_MOD_Register;
      --  AXI interconnect - TARG x bus matrix issuing functionality register
      AXI_TARG3_FN_MOD_ISS_BM : aliased AXI_TARG3_FN_MOD_ISS_BM_Register;
      --  AXI interconnect - TARG x bus matrix issuing functionality register
      AXI_TARG4_FN_MOD_ISS_BM : aliased AXI_TARG4_FN_MOD_ISS_BM_Register;
      --  AXI interconnect - TARG x bus matrix issuing functionality register
      AXI_TARG5_FN_MOD_ISS_BM : aliased AXI_TARG5_FN_MOD_ISS_BM_Register;
      --  AXI interconnect - TARG x bus matrix issuing functionality register
      AXI_TARG6_FN_MOD_ISS_BM : aliased AXI_TARG6_FN_MOD_ISS_BM_Register;
      --  AXI interconnect - TARG x bus matrix issuing functionality register
      AXI_TARG7_FN_MOD_ISS_BM : aliased AXI_TARG7_FN_MOD_ISS_BM_Register;
      --  AXI interconnect - TARG x bus matrix functionality 2 register
      AXI_TARG7_FN_MOD2       : aliased AXI_TARG7_FN_MOD2_Register;
      --  AXI interconnect - TARG x long burst functionality modification
      AXI_TARG7_FN_MOD        : aliased AXI_TARG7_FN_MOD_Register;
      --  AXI interconnect - INI x functionality modification 2 register
      AXI_INI1_FN_MOD2        : aliased AXI_INI1_FN_MOD2_Register;
      --  AXI interconnect - INI x AHB functionality modification register
      AXI_INI1_FN_MOD_AHB     : aliased AXI_INI1_FN_MOD_AHB_Register;
      --  AXI interconnect - INI x read QoS register
      AXI_INI1_READ_QOS       : aliased AXI_INI1_READ_QOS_Register;
      --  AXI interconnect - INI x write QoS register
      AXI_INI1_WRITE_QOS      : aliased AXI_INI1_WRITE_QOS_Register;
      --  AXI interconnect - INI x issuing functionality modification register
      AXI_INI1_FN_MOD         : aliased AXI_INI1_FN_MOD_Register;
      --  AXI interconnect - INI x read QoS register
      AXI_INI2_READ_QOS       : aliased AXI_INI2_READ_QOS_Register;
      --  AXI interconnect - INI x write QoS register
      AXI_INI2_WRITE_QOS      : aliased AXI_INI2_WRITE_QOS_Register;
      --  AXI interconnect - INI x issuing functionality modification register
      AXI_INI2_FN_MOD         : aliased AXI_INI2_FN_MOD_Register;
      --  AXI interconnect - INI x functionality modification 2 register
      AXI_INI3_FN_MOD2        : aliased AXI_INI3_FN_MOD2_Register;
      --  AXI interconnect - INI x AHB functionality modification register
      AXI_INI3_FN_MOD_AHB     : aliased AXI_INI3_FN_MOD_AHB_Register;
      --  AXI interconnect - INI x read QoS register
      AXI_INI3_READ_QOS       : aliased AXI_INI3_READ_QOS_Register;
      --  AXI interconnect - INI x write QoS register
      AXI_INI3_WRITE_QOS      : aliased AXI_INI3_WRITE_QOS_Register;
      --  AXI interconnect - INI x issuing functionality modification register
      AXI_INI3_FN_MOD         : aliased AXI_INI3_FN_MOD_Register;
      --  AXI interconnect - INI x read QoS register
      AXI_INI4_READ_QOS       : aliased AXI_INI4_READ_QOS_Register;
      --  AXI interconnect - INI x write QoS register
      AXI_INI4_WRITE_QOS      : aliased AXI_INI4_WRITE_QOS_Register;
      --  AXI interconnect - INI x issuing functionality modification register
      AXI_INI4_FN_MOD         : aliased AXI_INI4_FN_MOD_Register;
      --  AXI interconnect - INI x read QoS register
      AXI_INI5_READ_QOS       : aliased AXI_INI5_READ_QOS_Register;
      --  AXI interconnect - INI x write QoS register
      AXI_INI5_WRITE_QOS      : aliased AXI_INI5_WRITE_QOS_Register;
      --  AXI interconnect - INI x issuing functionality modification register
      AXI_INI5_FN_MOD         : aliased AXI_INI5_FN_MOD_Register;
      --  AXI interconnect - INI x read QoS register
      AXI_INI6_READ_QOS       : aliased AXI_INI6_READ_QOS_Register;
      --  AXI interconnect - INI x write QoS register
      AXI_INI6_WRITE_QOS      : aliased AXI_INI6_WRITE_QOS_Register;
      --  AXI interconnect - INI x issuing functionality modification register
      AXI_INI6_FN_MOD         : aliased AXI_INI6_FN_MOD_Register;
   end record
     with Volatile;

   for AXI_Peripheral use record
      AXI_PERIPH_ID_4         at 16#1FD0# range 0 .. 31;
      AXI_PERIPH_ID_0         at 16#1FE0# range 0 .. 31;
      AXI_PERIPH_ID_1         at 16#1FE4# range 0 .. 31;
      AXI_PERIPH_ID_2         at 16#1FE8# range 0 .. 31;
      AXI_PERIPH_ID_3         at 16#1FEC# range 0 .. 31;
      AXI_COMP_ID_0           at 16#1FF0# range 0 .. 31;
      AXI_COMP_ID_1           at 16#1FF4# range 0 .. 31;
      AXI_COMP_ID_2           at 16#1FF8# range 0 .. 31;
      AXI_COMP_ID_3           at 16#1FFC# range 0 .. 31;
      AXI_TARG1_FN_MOD_ISS_BM at 16#2008# range 0 .. 31;
      AXI_TARG1_FN_MOD2       at 16#2024# range 0 .. 31;
      AXI_TARG1_FN_MOD_LB     at 16#202C# range 0 .. 31;
      AXI_TARG1_FN_MOD        at 16#2108# range 0 .. 31;
      AXI_TARG2_FN_MOD_ISS_BM at 16#3008# range 0 .. 31;
      AXI_TARG2_FN_MOD2       at 16#3024# range 0 .. 31;
      AXI_TARG2_FN_MOD_LB     at 16#302C# range 0 .. 31;
      AXI_TARG2_FN_MOD        at 16#3108# range 0 .. 31;
      AXI_TARG3_FN_MOD_ISS_BM at 16#4008# range 0 .. 31;
      AXI_TARG4_FN_MOD_ISS_BM at 16#5008# range 0 .. 31;
      AXI_TARG5_FN_MOD_ISS_BM at 16#6008# range 0 .. 31;
      AXI_TARG6_FN_MOD_ISS_BM at 16#7008# range 0 .. 31;
      AXI_TARG7_FN_MOD_ISS_BM at 16#800C# range 0 .. 31;
      AXI_TARG7_FN_MOD2       at 16#8024# range 0 .. 31;
      AXI_TARG7_FN_MOD        at 16#8108# range 0 .. 31;
      AXI_INI1_FN_MOD2        at 16#42024# range 0 .. 31;
      AXI_INI1_FN_MOD_AHB     at 16#42028# range 0 .. 31;
      AXI_INI1_READ_QOS       at 16#42100# range 0 .. 31;
      AXI_INI1_WRITE_QOS      at 16#42104# range 0 .. 31;
      AXI_INI1_FN_MOD         at 16#42108# range 0 .. 31;
      AXI_INI2_READ_QOS       at 16#43100# range 0 .. 31;
      AXI_INI2_WRITE_QOS      at 16#43104# range 0 .. 31;
      AXI_INI2_FN_MOD         at 16#43108# range 0 .. 31;
      AXI_INI3_FN_MOD2        at 16#44024# range 0 .. 31;
      AXI_INI3_FN_MOD_AHB     at 16#44028# range 0 .. 31;
      AXI_INI3_READ_QOS       at 16#44100# range 0 .. 31;
      AXI_INI3_WRITE_QOS      at 16#44104# range 0 .. 31;
      AXI_INI3_FN_MOD         at 16#44108# range 0 .. 31;
      AXI_INI4_READ_QOS       at 16#45100# range 0 .. 31;
      AXI_INI4_WRITE_QOS      at 16#45104# range 0 .. 31;
      AXI_INI4_FN_MOD         at 16#45108# range 0 .. 31;
      AXI_INI5_READ_QOS       at 16#46100# range 0 .. 31;
      AXI_INI5_WRITE_QOS      at 16#46104# range 0 .. 31;
      AXI_INI5_FN_MOD         at 16#46108# range 0 .. 31;
      AXI_INI6_READ_QOS       at 16#47100# range 0 .. 31;
      AXI_INI6_WRITE_QOS      at 16#47104# range 0 .. 31;
      AXI_INI6_FN_MOD         at 16#47108# range 0 .. 31;
   end record;

   --  AXI interconnect registers
   AXI_Periph : aliased AXI_Peripheral
     with Import, Address => AXI_Base;

end STM32H753.AXI;
