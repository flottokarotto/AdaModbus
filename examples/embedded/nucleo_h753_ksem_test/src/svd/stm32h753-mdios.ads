pragma Style_Checks (Off);

--  This spec has been automatically generated from STM32H753.svd


with System;

package STM32H753.MDIOS is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   subtype CR_PORT_ADDRESS_Field is STM32H753.UInt5;

   --  MDIOS configuration register
   type CR_Register is record
      --  Peripheral enable
      EN             : Boolean := False;
      --  Register write interrupt enable
      WRIE           : Boolean := False;
      --  Register Read Interrupt Enable
      RDIE           : Boolean := False;
      --  Error interrupt enable
      EIE            : Boolean := False;
      --  unspecified
      Reserved_4_6   : STM32H753.UInt3 := 16#0#;
      --  Disable Preamble Check
      DPC            : Boolean := False;
      --  Slaves's address
      PORT_ADDRESS   : CR_PORT_ADDRESS_Field := 16#0#;
      --  unspecified
      Reserved_13_31 : STM32H753.UInt19 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      EN             at 0 range 0 .. 0;
      WRIE           at 0 range 1 .. 1;
      RDIE           at 0 range 2 .. 2;
      EIE            at 0 range 3 .. 3;
      Reserved_4_6   at 0 range 4 .. 6;
      DPC            at 0 range 7 .. 7;
      PORT_ADDRESS   at 0 range 8 .. 12;
      Reserved_13_31 at 0 range 13 .. 31;
   end record;

   --  MDIOS status register
   type SR_Register is record
      --  Read-only. Preamble error flag
      PERF          : Boolean;
      --  Read-only. Start error flag
      SERF          : Boolean;
      --  Read-only. Turnaround error flag
      TERF          : Boolean;
      --  unspecified
      Reserved_3_31 : STM32H753.UInt29;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register use record
      PERF          at 0 range 0 .. 0;
      SERF          at 0 range 1 .. 1;
      TERF          at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   --  MDIOS clear flag register
   type CLRFR_Register is record
      --  Clear the preamble error flag
      CPERF         : Boolean := False;
      --  Clear the start error flag
      CSERF         : Boolean := False;
      --  Clear the turnaround error flag
      CTERF         : Boolean := False;
      --  unspecified
      Reserved_3_31 : STM32H753.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CLRFR_Register use record
      CPERF         at 0 range 0 .. 0;
      CSERF         at 0 range 1 .. 1;
      CTERF         at 0 range 2 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   subtype DINR0_DIN0_Field is STM32H753.UInt16;

   --  MDIOS input data register 0
   type DINR0_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN0           : DINR0_DIN0_Field;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DINR0_Register use record
      DIN0           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DINR1_DIN1_Field is STM32H753.UInt16;

   --  MDIOS input data register 1
   type DINR1_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN1           : DINR1_DIN1_Field;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DINR1_Register use record
      DIN1           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DINR2_DIN2_Field is STM32H753.UInt16;

   --  MDIOS input data register 2
   type DINR2_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN2           : DINR2_DIN2_Field;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DINR2_Register use record
      DIN2           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DINR3_DIN3_Field is STM32H753.UInt16;

   --  MDIOS input data register 3
   type DINR3_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN3           : DINR3_DIN3_Field;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DINR3_Register use record
      DIN3           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DINR4_DIN4_Field is STM32H753.UInt16;

   --  MDIOS input data register 4
   type DINR4_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN4           : DINR4_DIN4_Field;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DINR4_Register use record
      DIN4           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DINR5_DIN5_Field is STM32H753.UInt16;

   --  MDIOS input data register 5
   type DINR5_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN5           : DINR5_DIN5_Field;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DINR5_Register use record
      DIN5           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DINR6_DIN6_Field is STM32H753.UInt16;

   --  MDIOS input data register 6
   type DINR6_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN6           : DINR6_DIN6_Field;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DINR6_Register use record
      DIN6           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DINR7_DIN7_Field is STM32H753.UInt16;

   --  MDIOS input data register 7
   type DINR7_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN7           : DINR7_DIN7_Field;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DINR7_Register use record
      DIN7           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DINR8_DIN8_Field is STM32H753.UInt16;

   --  MDIOS input data register 8
   type DINR8_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN8           : DINR8_DIN8_Field;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DINR8_Register use record
      DIN8           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DINR9_DIN9_Field is STM32H753.UInt16;

   --  MDIOS input data register 9
   type DINR9_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN9           : DINR9_DIN9_Field;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DINR9_Register use record
      DIN9           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DINR10_DIN10_Field is STM32H753.UInt16;

   --  MDIOS input data register 10
   type DINR10_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN10          : DINR10_DIN10_Field;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DINR10_Register use record
      DIN10          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DINR11_DIN11_Field is STM32H753.UInt16;

   --  MDIOS input data register 11
   type DINR11_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN11          : DINR11_DIN11_Field;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DINR11_Register use record
      DIN11          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DINR12_DIN12_Field is STM32H753.UInt16;

   --  MDIOS input data register 12
   type DINR12_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN12          : DINR12_DIN12_Field;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DINR12_Register use record
      DIN12          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DINR13_DIN13_Field is STM32H753.UInt16;

   --  MDIOS input data register 13
   type DINR13_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN13          : DINR13_DIN13_Field;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DINR13_Register use record
      DIN13          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DINR14_DIN14_Field is STM32H753.UInt16;

   --  MDIOS input data register 14
   type DINR14_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN14          : DINR14_DIN14_Field;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DINR14_Register use record
      DIN14          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DINR15_DIN15_Field is STM32H753.UInt16;

   --  MDIOS input data register 15
   type DINR15_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN15          : DINR15_DIN15_Field;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DINR15_Register use record
      DIN15          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DINR16_DIN16_Field is STM32H753.UInt16;

   --  MDIOS input data register 16
   type DINR16_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN16          : DINR16_DIN16_Field;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DINR16_Register use record
      DIN16          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DINR17_DIN17_Field is STM32H753.UInt16;

   --  MDIOS input data register 17
   type DINR17_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN17          : DINR17_DIN17_Field;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DINR17_Register use record
      DIN17          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DINR18_DIN18_Field is STM32H753.UInt16;

   --  MDIOS input data register 18
   type DINR18_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN18          : DINR18_DIN18_Field;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DINR18_Register use record
      DIN18          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DINR19_DIN19_Field is STM32H753.UInt16;

   --  MDIOS input data register 19
   type DINR19_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN19          : DINR19_DIN19_Field;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DINR19_Register use record
      DIN19          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DINR20_DIN20_Field is STM32H753.UInt16;

   --  MDIOS input data register 20
   type DINR20_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN20          : DINR20_DIN20_Field;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DINR20_Register use record
      DIN20          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DINR21_DIN21_Field is STM32H753.UInt16;

   --  MDIOS input data register 21
   type DINR21_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN21          : DINR21_DIN21_Field;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DINR21_Register use record
      DIN21          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DINR22_DIN22_Field is STM32H753.UInt16;

   --  MDIOS input data register 22
   type DINR22_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN22          : DINR22_DIN22_Field;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DINR22_Register use record
      DIN22          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DINR23_DIN23_Field is STM32H753.UInt16;

   --  MDIOS input data register 23
   type DINR23_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN23          : DINR23_DIN23_Field;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DINR23_Register use record
      DIN23          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DINR24_DIN24_Field is STM32H753.UInt16;

   --  MDIOS input data register 24
   type DINR24_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN24          : DINR24_DIN24_Field;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DINR24_Register use record
      DIN24          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DINR25_DIN25_Field is STM32H753.UInt16;

   --  MDIOS input data register 25
   type DINR25_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN25          : DINR25_DIN25_Field;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DINR25_Register use record
      DIN25          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DINR26_DIN26_Field is STM32H753.UInt16;

   --  MDIOS input data register 26
   type DINR26_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN26          : DINR26_DIN26_Field;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DINR26_Register use record
      DIN26          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DINR27_DIN27_Field is STM32H753.UInt16;

   --  MDIOS input data register 27
   type DINR27_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN27          : DINR27_DIN27_Field;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DINR27_Register use record
      DIN27          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DINR28_DIN28_Field is STM32H753.UInt16;

   --  MDIOS input data register 28
   type DINR28_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN28          : DINR28_DIN28_Field;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DINR28_Register use record
      DIN28          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DINR29_DIN29_Field is STM32H753.UInt16;

   --  MDIOS input data register 29
   type DINR29_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN29          : DINR29_DIN29_Field;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DINR29_Register use record
      DIN29          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DINR30_DIN30_Field is STM32H753.UInt16;

   --  MDIOS input data register 30
   type DINR30_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN30          : DINR30_DIN30_Field;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DINR30_Register use record
      DIN30          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DINR31_DIN31_Field is STM32H753.UInt16;

   --  MDIOS input data register 31
   type DINR31_Register is record
      --  Read-only. Input data received from MDIO Master during write frames
      DIN31          : DINR31_DIN31_Field;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DINR31_Register use record
      DIN31          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DOUTR0_DOUT0_Field is STM32H753.UInt16;

   --  MDIOS output data register 0
   type DOUTR0_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT0          : DOUTR0_DOUT0_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOUTR0_Register use record
      DOUT0          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DOUTR1_DOUT1_Field is STM32H753.UInt16;

   --  MDIOS output data register 1
   type DOUTR1_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT1          : DOUTR1_DOUT1_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOUTR1_Register use record
      DOUT1          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DOUTR2_DOUT2_Field is STM32H753.UInt16;

   --  MDIOS output data register 2
   type DOUTR2_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT2          : DOUTR2_DOUT2_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOUTR2_Register use record
      DOUT2          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DOUTR3_DOUT3_Field is STM32H753.UInt16;

   --  MDIOS output data register 3
   type DOUTR3_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT3          : DOUTR3_DOUT3_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOUTR3_Register use record
      DOUT3          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DOUTR4_DOUT4_Field is STM32H753.UInt16;

   --  MDIOS output data register 4
   type DOUTR4_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT4          : DOUTR4_DOUT4_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOUTR4_Register use record
      DOUT4          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DOUTR5_DOUT5_Field is STM32H753.UInt16;

   --  MDIOS output data register 5
   type DOUTR5_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT5          : DOUTR5_DOUT5_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOUTR5_Register use record
      DOUT5          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DOUTR6_DOUT6_Field is STM32H753.UInt16;

   --  MDIOS output data register 6
   type DOUTR6_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT6          : DOUTR6_DOUT6_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOUTR6_Register use record
      DOUT6          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DOUTR7_DOUT7_Field is STM32H753.UInt16;

   --  MDIOS output data register 7
   type DOUTR7_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT7          : DOUTR7_DOUT7_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOUTR7_Register use record
      DOUT7          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DOUTR8_DOUT8_Field is STM32H753.UInt16;

   --  MDIOS output data register 8
   type DOUTR8_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT8          : DOUTR8_DOUT8_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOUTR8_Register use record
      DOUT8          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DOUTR9_DOUT9_Field is STM32H753.UInt16;

   --  MDIOS output data register 9
   type DOUTR9_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT9          : DOUTR9_DOUT9_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOUTR9_Register use record
      DOUT9          at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DOUTR10_DOUT10_Field is STM32H753.UInt16;

   --  MDIOS output data register 10
   type DOUTR10_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT10         : DOUTR10_DOUT10_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOUTR10_Register use record
      DOUT10         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DOUTR11_DOUT11_Field is STM32H753.UInt16;

   --  MDIOS output data register 11
   type DOUTR11_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT11         : DOUTR11_DOUT11_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOUTR11_Register use record
      DOUT11         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DOUTR12_DOUT12_Field is STM32H753.UInt16;

   --  MDIOS output data register 12
   type DOUTR12_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT12         : DOUTR12_DOUT12_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOUTR12_Register use record
      DOUT12         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DOUTR13_DOUT13_Field is STM32H753.UInt16;

   --  MDIOS output data register 13
   type DOUTR13_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT13         : DOUTR13_DOUT13_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOUTR13_Register use record
      DOUT13         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DOUTR14_DOUT14_Field is STM32H753.UInt16;

   --  MDIOS output data register 14
   type DOUTR14_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT14         : DOUTR14_DOUT14_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOUTR14_Register use record
      DOUT14         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DOUTR15_DOUT15_Field is STM32H753.UInt16;

   --  MDIOS output data register 15
   type DOUTR15_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT15         : DOUTR15_DOUT15_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOUTR15_Register use record
      DOUT15         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DOUTR16_DOUT16_Field is STM32H753.UInt16;

   --  MDIOS output data register 16
   type DOUTR16_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT16         : DOUTR16_DOUT16_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOUTR16_Register use record
      DOUT16         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DOUTR17_DOUT17_Field is STM32H753.UInt16;

   --  MDIOS output data register 17
   type DOUTR17_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT17         : DOUTR17_DOUT17_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOUTR17_Register use record
      DOUT17         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DOUTR18_DOUT18_Field is STM32H753.UInt16;

   --  MDIOS output data register 18
   type DOUTR18_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT18         : DOUTR18_DOUT18_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOUTR18_Register use record
      DOUT18         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DOUTR19_DOUT19_Field is STM32H753.UInt16;

   --  MDIOS output data register 19
   type DOUTR19_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT19         : DOUTR19_DOUT19_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOUTR19_Register use record
      DOUT19         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DOUTR20_DOUT20_Field is STM32H753.UInt16;

   --  MDIOS output data register 20
   type DOUTR20_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT20         : DOUTR20_DOUT20_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOUTR20_Register use record
      DOUT20         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DOUTR21_DOUT21_Field is STM32H753.UInt16;

   --  MDIOS output data register 21
   type DOUTR21_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT21         : DOUTR21_DOUT21_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOUTR21_Register use record
      DOUT21         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DOUTR22_DOUT22_Field is STM32H753.UInt16;

   --  MDIOS output data register 22
   type DOUTR22_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT22         : DOUTR22_DOUT22_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOUTR22_Register use record
      DOUT22         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DOUTR23_DOUT23_Field is STM32H753.UInt16;

   --  MDIOS output data register 23
   type DOUTR23_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT23         : DOUTR23_DOUT23_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOUTR23_Register use record
      DOUT23         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DOUTR24_DOUT24_Field is STM32H753.UInt16;

   --  MDIOS output data register 24
   type DOUTR24_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT24         : DOUTR24_DOUT24_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOUTR24_Register use record
      DOUT24         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DOUTR25_DOUT25_Field is STM32H753.UInt16;

   --  MDIOS output data register 25
   type DOUTR25_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT25         : DOUTR25_DOUT25_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOUTR25_Register use record
      DOUT25         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DOUTR26_DOUT26_Field is STM32H753.UInt16;

   --  MDIOS output data register 26
   type DOUTR26_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT26         : DOUTR26_DOUT26_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOUTR26_Register use record
      DOUT26         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DOUTR27_DOUT27_Field is STM32H753.UInt16;

   --  MDIOS output data register 27
   type DOUTR27_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT27         : DOUTR27_DOUT27_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOUTR27_Register use record
      DOUT27         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DOUTR28_DOUT28_Field is STM32H753.UInt16;

   --  MDIOS output data register 28
   type DOUTR28_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT28         : DOUTR28_DOUT28_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOUTR28_Register use record
      DOUT28         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DOUTR29_DOUT29_Field is STM32H753.UInt16;

   --  MDIOS output data register 29
   type DOUTR29_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT29         : DOUTR29_DOUT29_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOUTR29_Register use record
      DOUT29         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DOUTR30_DOUT30_Field is STM32H753.UInt16;

   --  MDIOS output data register 30
   type DOUTR30_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT30         : DOUTR30_DOUT30_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOUTR30_Register use record
      DOUT30         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DOUTR31_DOUT31_Field is STM32H753.UInt16;

   --  MDIOS output data register 31
   type DOUTR31_Register is record
      --  Output data sent to MDIO Master during read frames
      DOUT31         : DOUTR31_DOUT31_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DOUTR31_Register use record
      DOUT31         at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Management data input/output slave
   type MDIOS_Peripheral is record
      --  MDIOS configuration register
      CR      : aliased CR_Register;
      --  MDIOS write flag register
      WRFR    : aliased STM32H753.UInt32;
      --  MDIOS clear write flag register
      CWRFR   : aliased STM32H753.UInt32;
      --  MDIOS read flag register
      RDFR    : aliased STM32H753.UInt32;
      --  MDIOS clear read flag register
      CRDFR   : aliased STM32H753.UInt32;
      --  MDIOS status register
      SR      : aliased SR_Register;
      --  MDIOS clear flag register
      CLRFR   : aliased CLRFR_Register;
      --  MDIOS input data register 0
      DINR0   : aliased DINR0_Register;
      --  MDIOS input data register 1
      DINR1   : aliased DINR1_Register;
      --  MDIOS input data register 2
      DINR2   : aliased DINR2_Register;
      --  MDIOS input data register 3
      DINR3   : aliased DINR3_Register;
      --  MDIOS input data register 4
      DINR4   : aliased DINR4_Register;
      --  MDIOS input data register 5
      DINR5   : aliased DINR5_Register;
      --  MDIOS input data register 6
      DINR6   : aliased DINR6_Register;
      --  MDIOS input data register 7
      DINR7   : aliased DINR7_Register;
      --  MDIOS input data register 8
      DINR8   : aliased DINR8_Register;
      --  MDIOS input data register 9
      DINR9   : aliased DINR9_Register;
      --  MDIOS input data register 10
      DINR10  : aliased DINR10_Register;
      --  MDIOS input data register 11
      DINR11  : aliased DINR11_Register;
      --  MDIOS input data register 12
      DINR12  : aliased DINR12_Register;
      --  MDIOS input data register 13
      DINR13  : aliased DINR13_Register;
      --  MDIOS input data register 14
      DINR14  : aliased DINR14_Register;
      --  MDIOS input data register 15
      DINR15  : aliased DINR15_Register;
      --  MDIOS input data register 16
      DINR16  : aliased DINR16_Register;
      --  MDIOS input data register 17
      DINR17  : aliased DINR17_Register;
      --  MDIOS input data register 18
      DINR18  : aliased DINR18_Register;
      --  MDIOS input data register 19
      DINR19  : aliased DINR19_Register;
      --  MDIOS input data register 20
      DINR20  : aliased DINR20_Register;
      --  MDIOS input data register 21
      DINR21  : aliased DINR21_Register;
      --  MDIOS input data register 22
      DINR22  : aliased DINR22_Register;
      --  MDIOS input data register 23
      DINR23  : aliased DINR23_Register;
      --  MDIOS input data register 24
      DINR24  : aliased DINR24_Register;
      --  MDIOS input data register 25
      DINR25  : aliased DINR25_Register;
      --  MDIOS input data register 26
      DINR26  : aliased DINR26_Register;
      --  MDIOS input data register 27
      DINR27  : aliased DINR27_Register;
      --  MDIOS input data register 28
      DINR28  : aliased DINR28_Register;
      --  MDIOS input data register 29
      DINR29  : aliased DINR29_Register;
      --  MDIOS input data register 30
      DINR30  : aliased DINR30_Register;
      --  MDIOS input data register 31
      DINR31  : aliased DINR31_Register;
      --  MDIOS output data register 0
      DOUTR0  : aliased DOUTR0_Register;
      --  MDIOS output data register 1
      DOUTR1  : aliased DOUTR1_Register;
      --  MDIOS output data register 2
      DOUTR2  : aliased DOUTR2_Register;
      --  MDIOS output data register 3
      DOUTR3  : aliased DOUTR3_Register;
      --  MDIOS output data register 4
      DOUTR4  : aliased DOUTR4_Register;
      --  MDIOS output data register 5
      DOUTR5  : aliased DOUTR5_Register;
      --  MDIOS output data register 6
      DOUTR6  : aliased DOUTR6_Register;
      --  MDIOS output data register 7
      DOUTR7  : aliased DOUTR7_Register;
      --  MDIOS output data register 8
      DOUTR8  : aliased DOUTR8_Register;
      --  MDIOS output data register 9
      DOUTR9  : aliased DOUTR9_Register;
      --  MDIOS output data register 10
      DOUTR10 : aliased DOUTR10_Register;
      --  MDIOS output data register 11
      DOUTR11 : aliased DOUTR11_Register;
      --  MDIOS output data register 12
      DOUTR12 : aliased DOUTR12_Register;
      --  MDIOS output data register 13
      DOUTR13 : aliased DOUTR13_Register;
      --  MDIOS output data register 14
      DOUTR14 : aliased DOUTR14_Register;
      --  MDIOS output data register 15
      DOUTR15 : aliased DOUTR15_Register;
      --  MDIOS output data register 16
      DOUTR16 : aliased DOUTR16_Register;
      --  MDIOS output data register 17
      DOUTR17 : aliased DOUTR17_Register;
      --  MDIOS output data register 18
      DOUTR18 : aliased DOUTR18_Register;
      --  MDIOS output data register 19
      DOUTR19 : aliased DOUTR19_Register;
      --  MDIOS output data register 20
      DOUTR20 : aliased DOUTR20_Register;
      --  MDIOS output data register 21
      DOUTR21 : aliased DOUTR21_Register;
      --  MDIOS output data register 22
      DOUTR22 : aliased DOUTR22_Register;
      --  MDIOS output data register 23
      DOUTR23 : aliased DOUTR23_Register;
      --  MDIOS output data register 24
      DOUTR24 : aliased DOUTR24_Register;
      --  MDIOS output data register 25
      DOUTR25 : aliased DOUTR25_Register;
      --  MDIOS output data register 26
      DOUTR26 : aliased DOUTR26_Register;
      --  MDIOS output data register 27
      DOUTR27 : aliased DOUTR27_Register;
      --  MDIOS output data register 28
      DOUTR28 : aliased DOUTR28_Register;
      --  MDIOS output data register 29
      DOUTR29 : aliased DOUTR29_Register;
      --  MDIOS output data register 30
      DOUTR30 : aliased DOUTR30_Register;
      --  MDIOS output data register 31
      DOUTR31 : aliased DOUTR31_Register;
   end record
     with Volatile;

   for MDIOS_Peripheral use record
      CR      at 16#0# range 0 .. 31;
      WRFR    at 16#4# range 0 .. 31;
      CWRFR   at 16#8# range 0 .. 31;
      RDFR    at 16#C# range 0 .. 31;
      CRDFR   at 16#10# range 0 .. 31;
      SR      at 16#14# range 0 .. 31;
      CLRFR   at 16#18# range 0 .. 31;
      DINR0   at 16#1C# range 0 .. 31;
      DINR1   at 16#20# range 0 .. 31;
      DINR2   at 16#24# range 0 .. 31;
      DINR3   at 16#28# range 0 .. 31;
      DINR4   at 16#2C# range 0 .. 31;
      DINR5   at 16#30# range 0 .. 31;
      DINR6   at 16#34# range 0 .. 31;
      DINR7   at 16#38# range 0 .. 31;
      DINR8   at 16#3C# range 0 .. 31;
      DINR9   at 16#40# range 0 .. 31;
      DINR10  at 16#44# range 0 .. 31;
      DINR11  at 16#48# range 0 .. 31;
      DINR12  at 16#4C# range 0 .. 31;
      DINR13  at 16#50# range 0 .. 31;
      DINR14  at 16#54# range 0 .. 31;
      DINR15  at 16#58# range 0 .. 31;
      DINR16  at 16#5C# range 0 .. 31;
      DINR17  at 16#60# range 0 .. 31;
      DINR18  at 16#64# range 0 .. 31;
      DINR19  at 16#68# range 0 .. 31;
      DINR20  at 16#6C# range 0 .. 31;
      DINR21  at 16#70# range 0 .. 31;
      DINR22  at 16#74# range 0 .. 31;
      DINR23  at 16#78# range 0 .. 31;
      DINR24  at 16#7C# range 0 .. 31;
      DINR25  at 16#80# range 0 .. 31;
      DINR26  at 16#84# range 0 .. 31;
      DINR27  at 16#88# range 0 .. 31;
      DINR28  at 16#8C# range 0 .. 31;
      DINR29  at 16#90# range 0 .. 31;
      DINR30  at 16#94# range 0 .. 31;
      DINR31  at 16#98# range 0 .. 31;
      DOUTR0  at 16#9C# range 0 .. 31;
      DOUTR1  at 16#A0# range 0 .. 31;
      DOUTR2  at 16#A4# range 0 .. 31;
      DOUTR3  at 16#A8# range 0 .. 31;
      DOUTR4  at 16#AC# range 0 .. 31;
      DOUTR5  at 16#B0# range 0 .. 31;
      DOUTR6  at 16#B4# range 0 .. 31;
      DOUTR7  at 16#B8# range 0 .. 31;
      DOUTR8  at 16#BC# range 0 .. 31;
      DOUTR9  at 16#C0# range 0 .. 31;
      DOUTR10 at 16#C4# range 0 .. 31;
      DOUTR11 at 16#C8# range 0 .. 31;
      DOUTR12 at 16#CC# range 0 .. 31;
      DOUTR13 at 16#D0# range 0 .. 31;
      DOUTR14 at 16#D4# range 0 .. 31;
      DOUTR15 at 16#D8# range 0 .. 31;
      DOUTR16 at 16#DC# range 0 .. 31;
      DOUTR17 at 16#E0# range 0 .. 31;
      DOUTR18 at 16#E4# range 0 .. 31;
      DOUTR19 at 16#E8# range 0 .. 31;
      DOUTR20 at 16#EC# range 0 .. 31;
      DOUTR21 at 16#F0# range 0 .. 31;
      DOUTR22 at 16#F4# range 0 .. 31;
      DOUTR23 at 16#F8# range 0 .. 31;
      DOUTR24 at 16#FC# range 0 .. 31;
      DOUTR25 at 16#100# range 0 .. 31;
      DOUTR26 at 16#104# range 0 .. 31;
      DOUTR27 at 16#108# range 0 .. 31;
      DOUTR28 at 16#10C# range 0 .. 31;
      DOUTR29 at 16#110# range 0 .. 31;
      DOUTR30 at 16#114# range 0 .. 31;
      DOUTR31 at 16#118# range 0 .. 31;
   end record;

   --  Management data input/output slave
   MDIOS_Periph : aliased MDIOS_Peripheral
     with Import, Address => MDIOS_Base;

end STM32H753.MDIOS;
