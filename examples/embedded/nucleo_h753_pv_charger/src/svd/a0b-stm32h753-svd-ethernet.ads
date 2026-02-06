pragma Style_Checks (Off);

--  This spec has been automatically generated from STM32H753.svd


with A0B.Types.SVD;
with System;

package A0B.STM32H753.SVD.Ethernet is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   subtype MACCR_PRELEN_Field is A0B.Types.SVD.UInt2;
   subtype MACCR_BL_Field is A0B.Types.SVD.UInt2;
   subtype MACCR_IPG_Field is A0B.Types.SVD.UInt3;
   subtype MACCR_SARC_Field is A0B.Types.SVD.UInt3;

   --  Operating mode configuration register
   type MACCR_Register is record
      --  Receiver Enable
      RE             : Boolean := False;
      --  TE
      TE             : Boolean := False;
      --  PRELEN
      PRELEN         : MACCR_PRELEN_Field := 16#0#;
      --  DC
      DC             : Boolean := False;
      --  BL
      BL             : MACCR_BL_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : A0B.Types.SVD.Bit := 16#0#;
      --  DR
      DR             : Boolean := False;
      --  DCRS
      DCRS           : Boolean := False;
      --  DO
      DO_k           : Boolean := False;
      --  ECRSFD
      ECRSFD         : Boolean := False;
      --  LM
      LM             : Boolean := False;
      --  DM
      DM             : Boolean := False;
      --  FES
      FES            : Boolean := False;
      --  unspecified
      Reserved_15_15 : A0B.Types.SVD.Bit := 16#0#;
      --  JE
      JE             : Boolean := False;
      --  JD
      JD             : Boolean := False;
      --  unspecified
      Reserved_18_18 : A0B.Types.SVD.Bit := 16#0#;
      --  WD
      WD             : Boolean := False;
      --  ACS
      ACS            : Boolean := False;
      --  CST
      CST            : Boolean := False;
      --  S2KP
      S2KP           : Boolean := False;
      --  GPSLCE
      GPSLCE         : Boolean := False;
      --  IPG
      IPG            : MACCR_IPG_Field := 16#0#;
      --  IPC
      IPC            : Boolean := False;
      --  SARC
      SARC           : MACCR_SARC_Field := 16#0#;
      --  ARPEN
      ARPEN          : Boolean := False;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MACCR_Register use record
      RE             at 0 range 0 .. 0;
      TE             at 0 range 1 .. 1;
      PRELEN         at 0 range 2 .. 3;
      DC             at 0 range 4 .. 4;
      BL             at 0 range 5 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      DR             at 0 range 8 .. 8;
      DCRS           at 0 range 9 .. 9;
      DO_k           at 0 range 10 .. 10;
      ECRSFD         at 0 range 11 .. 11;
      LM             at 0 range 12 .. 12;
      DM             at 0 range 13 .. 13;
      FES            at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      JE             at 0 range 16 .. 16;
      JD             at 0 range 17 .. 17;
      Reserved_18_18 at 0 range 18 .. 18;
      WD             at 0 range 19 .. 19;
      ACS            at 0 range 20 .. 20;
      CST            at 0 range 21 .. 21;
      S2KP           at 0 range 22 .. 22;
      GPSLCE         at 0 range 23 .. 23;
      IPG            at 0 range 24 .. 26;
      IPC            at 0 range 27 .. 27;
      SARC           at 0 range 28 .. 30;
      ARPEN          at 0 range 31 .. 31;
   end record;

   subtype MACECR_GPSL_Field is A0B.Types.SVD.UInt14;
   subtype MACECR_EIPG_Field is A0B.Types.SVD.UInt5;

   --  Extended operating mode configuration register
   type MACECR_Register is record
      --  GPSL
      GPSL           : MACECR_GPSL_Field := 16#0#;
      --  unspecified
      Reserved_14_15 : A0B.Types.SVD.UInt2 := 16#0#;
      --  DCRCC
      DCRCC          : Boolean := False;
      --  SPEN
      SPEN           : Boolean := False;
      --  USP
      USP            : Boolean := False;
      --  unspecified
      Reserved_19_23 : A0B.Types.SVD.UInt5 := 16#0#;
      --  EIPGEN
      EIPGEN         : Boolean := False;
      --  EIPG
      EIPG           : MACECR_EIPG_Field := 16#0#;
      --  unspecified
      Reserved_30_31 : A0B.Types.SVD.UInt2 := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MACECR_Register use record
      GPSL           at 0 range 0 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      DCRCC          at 0 range 16 .. 16;
      SPEN           at 0 range 17 .. 17;
      USP            at 0 range 18 .. 18;
      Reserved_19_23 at 0 range 19 .. 23;
      EIPGEN         at 0 range 24 .. 24;
      EIPG           at 0 range 25 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype MACPFR_PCF_Field is A0B.Types.SVD.UInt2;

   --  Packet filtering control register
   type MACPFR_Register is record
      --  PR
      PR             : Boolean := False;
      --  HUC
      HUC            : Boolean := False;
      --  HMC
      HMC            : Boolean := False;
      --  DAIF
      DAIF           : Boolean := False;
      --  PM
      PM             : Boolean := False;
      --  DBF
      DBF            : Boolean := False;
      --  PCF
      PCF            : MACPFR_PCF_Field := 16#0#;
      --  SAIF
      SAIF           : Boolean := False;
      --  SAF
      SAF            : Boolean := False;
      --  HPF
      HPF            : Boolean := False;
      --  unspecified
      Reserved_11_15 : A0B.Types.SVD.UInt5 := 16#0#;
      --  VTFE
      VTFE           : Boolean := False;
      --  unspecified
      Reserved_17_19 : A0B.Types.SVD.UInt3 := 16#0#;
      --  IPFE
      IPFE           : Boolean := False;
      --  DNTU
      DNTU           : Boolean := False;
      --  unspecified
      Reserved_22_30 : A0B.Types.SVD.UInt9 := 16#0#;
      --  RA
      RA             : Boolean := False;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MACPFR_Register use record
      PR             at 0 range 0 .. 0;
      HUC            at 0 range 1 .. 1;
      HMC            at 0 range 2 .. 2;
      DAIF           at 0 range 3 .. 3;
      PM             at 0 range 4 .. 4;
      DBF            at 0 range 5 .. 5;
      PCF            at 0 range 6 .. 7;
      SAIF           at 0 range 8 .. 8;
      SAF            at 0 range 9 .. 9;
      HPF            at 0 range 10 .. 10;
      Reserved_11_15 at 0 range 11 .. 15;
      VTFE           at 0 range 16 .. 16;
      Reserved_17_19 at 0 range 17 .. 19;
      IPFE           at 0 range 20 .. 20;
      DNTU           at 0 range 21 .. 21;
      Reserved_22_30 at 0 range 22 .. 30;
      RA             at 0 range 31 .. 31;
   end record;

   subtype MACWTR_WTO_Field is A0B.Types.SVD.UInt4;

   --  Watchdog timeout register
   type MACWTR_Register is record
      --  WTO
      WTO           : MACWTR_WTO_Field := 16#0#;
      --  unspecified
      Reserved_4_7  : A0B.Types.SVD.UInt4 := 16#0#;
      --  PWE
      PWE           : Boolean := False;
      --  unspecified
      Reserved_9_31 : A0B.Types.SVD.UInt23 := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MACWTR_Register use record
      WTO           at 0 range 0 .. 3;
      Reserved_4_7  at 0 range 4 .. 7;
      PWE           at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   subtype MACVTR_VL_Field is A0B.Types.SVD.UInt16;
   subtype MACVTR_EVLS_Field is A0B.Types.SVD.UInt2;
   subtype MACVTR_EIVLS_Field is A0B.Types.SVD.UInt2;

   --  VLAN tag register
   type MACVTR_Register is record
      --  VL
      VL             : MACVTR_VL_Field := 16#0#;
      --  ETV
      ETV            : Boolean := False;
      --  VTIM
      VTIM           : Boolean := False;
      --  ESVL
      ESVL           : Boolean := False;
      --  ERSVLM
      ERSVLM         : Boolean := False;
      --  DOVLTC
      DOVLTC         : Boolean := False;
      --  EVLS
      EVLS           : MACVTR_EVLS_Field := 16#0#;
      --  unspecified
      Reserved_23_23 : A0B.Types.SVD.Bit := 16#0#;
      --  EVLRXS
      EVLRXS         : Boolean := False;
      --  VTHM
      VTHM           : Boolean := False;
      --  EDVLP
      EDVLP          : Boolean := False;
      --  ERIVLT
      ERIVLT         : Boolean := False;
      --  EIVLS
      EIVLS          : MACVTR_EIVLS_Field := 16#0#;
      --  unspecified
      Reserved_30_30 : A0B.Types.SVD.Bit := 16#0#;
      --  EIVLRXS
      EIVLRXS        : Boolean := False;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MACVTR_Register use record
      VL             at 0 range 0 .. 15;
      ETV            at 0 range 16 .. 16;
      VTIM           at 0 range 17 .. 17;
      ESVL           at 0 range 18 .. 18;
      ERSVLM         at 0 range 19 .. 19;
      DOVLTC         at 0 range 20 .. 20;
      EVLS           at 0 range 21 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      EVLRXS         at 0 range 24 .. 24;
      VTHM           at 0 range 25 .. 25;
      EDVLP          at 0 range 26 .. 26;
      ERIVLT         at 0 range 27 .. 27;
      EIVLS          at 0 range 28 .. 29;
      Reserved_30_30 at 0 range 30 .. 30;
      EIVLRXS        at 0 range 31 .. 31;
   end record;

   subtype MACVHTR_VLHT_Field is A0B.Types.SVD.UInt16;

   --  VLAN Hash table register
   type MACVHTR_Register is record
      --  VLHT
      VLHT           : MACVHTR_VLHT_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : A0B.Types.SVD.UInt16 := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MACVHTR_Register use record
      VLHT           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MACVIR_VLT_Field is A0B.Types.SVD.UInt16;
   subtype MACVIR_VLC_Field is A0B.Types.SVD.UInt2;

   --  VLAN inclusion register
   type MACVIR_Register is record
      --  VLT
      VLT            : MACVIR_VLT_Field := 16#0#;
      --  VLC
      VLC            : MACVIR_VLC_Field := 16#0#;
      --  VLP
      VLP            : Boolean := False;
      --  CSVL
      CSVL           : Boolean := False;
      --  VLTI
      VLTI           : Boolean := False;
      --  unspecified
      Reserved_21_31 : A0B.Types.SVD.UInt11 := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MACVIR_Register use record
      VLT            at 0 range 0 .. 15;
      VLC            at 0 range 16 .. 17;
      VLP            at 0 range 18 .. 18;
      CSVL           at 0 range 19 .. 19;
      VLTI           at 0 range 20 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   subtype MACIVIR_VLT_Field is A0B.Types.SVD.UInt16;
   subtype MACIVIR_VLC_Field is A0B.Types.SVD.UInt2;

   --  Inner VLAN inclusion register
   type MACIVIR_Register is record
      --  VLT
      VLT            : MACIVIR_VLT_Field := 16#0#;
      --  VLC
      VLC            : MACIVIR_VLC_Field := 16#0#;
      --  VLP
      VLP            : Boolean := False;
      --  CSVL
      CSVL           : Boolean := False;
      --  VLTI
      VLTI           : Boolean := False;
      --  unspecified
      Reserved_21_31 : A0B.Types.SVD.UInt11 := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MACIVIR_Register use record
      VLT            at 0 range 0 .. 15;
      VLC            at 0 range 16 .. 17;
      VLP            at 0 range 18 .. 18;
      CSVL           at 0 range 19 .. 19;
      VLTI           at 0 range 20 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   subtype MACQTxFCR_PLT_Field is A0B.Types.SVD.UInt3;
   subtype MACQTxFCR_PT_Field is A0B.Types.SVD.UInt16;

   --  Tx Queue flow control register
   type MACQTxFCR_Register is record
      --  FCB_BPA
      FCB_BPA       : Boolean := False;
      --  TFE
      TFE           : Boolean := False;
      --  unspecified
      Reserved_2_3  : A0B.Types.SVD.UInt2 := 16#0#;
      --  PLT
      PLT           : MACQTxFCR_PLT_Field := 16#0#;
      --  DZPQ
      DZPQ          : Boolean := False;
      --  unspecified
      Reserved_8_15 : A0B.Types.SVD.Byte := 16#0#;
      --  PT
      PT            : MACQTxFCR_PT_Field := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MACQTxFCR_Register use record
      FCB_BPA       at 0 range 0 .. 0;
      TFE           at 0 range 1 .. 1;
      Reserved_2_3  at 0 range 2 .. 3;
      PLT           at 0 range 4 .. 6;
      DZPQ          at 0 range 7 .. 7;
      Reserved_8_15 at 0 range 8 .. 15;
      PT            at 0 range 16 .. 31;
   end record;

   --  Rx flow control register
   type MACRxFCR_Register is record
      --  RFE
      RFE           : Boolean := False;
      --  UP
      UP            : Boolean := False;
      --  unspecified
      Reserved_2_31 : A0B.Types.SVD.UInt30 := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MACRxFCR_Register use record
      RFE           at 0 range 0 .. 0;
      UP            at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  Interrupt status register
   type MACISR_Register is record
      --  unspecified
      Reserved_0_2   : A0B.Types.SVD.UInt3;
      --  Read-only. PHYIS
      PHYIS          : Boolean;
      --  Read-only. PMTIS
      PMTIS          : Boolean;
      --  Read-only. LPIIS
      LPIIS          : Boolean;
      --  unspecified
      Reserved_6_7   : A0B.Types.SVD.UInt2;
      --  Read-only. MMCIS
      MMCIS          : Boolean;
      --  Read-only. MMCRXIS
      MMCRXIS        : Boolean;
      --  Read-only. MMCTXIS
      MMCTXIS        : Boolean;
      --  unspecified
      Reserved_11_11 : A0B.Types.SVD.Bit;
      --  Read-only. TSIS
      TSIS           : Boolean;
      --  Read-only. TXSTSIS
      TXSTSIS        : Boolean;
      --  Read-only. RXSTSIS
      RXSTSIS        : Boolean;
      --  unspecified
      Reserved_15_31 : A0B.Types.SVD.UInt17;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MACISR_Register use record
      Reserved_0_2   at 0 range 0 .. 2;
      PHYIS          at 0 range 3 .. 3;
      PMTIS          at 0 range 4 .. 4;
      LPIIS          at 0 range 5 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      MMCIS          at 0 range 8 .. 8;
      MMCRXIS        at 0 range 9 .. 9;
      MMCTXIS        at 0 range 10 .. 10;
      Reserved_11_11 at 0 range 11 .. 11;
      TSIS           at 0 range 12 .. 12;
      TXSTSIS        at 0 range 13 .. 13;
      RXSTSIS        at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   --  Interrupt enable register
   type MACIER_Register is record
      --  unspecified
      Reserved_0_2   : A0B.Types.SVD.UInt3 := 16#0#;
      --  PHYIE
      PHYIE          : Boolean := False;
      --  PMTIE
      PMTIE          : Boolean := False;
      --  LPIIE
      LPIIE          : Boolean := False;
      --  unspecified
      Reserved_6_11  : A0B.Types.SVD.UInt6 := 16#0#;
      --  TSIE
      TSIE           : Boolean := False;
      --  TXSTSIE
      TXSTSIE        : Boolean := False;
      --  RXSTSIE
      RXSTSIE        : Boolean := False;
      --  unspecified
      Reserved_15_31 : A0B.Types.SVD.UInt17 := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MACIER_Register use record
      Reserved_0_2   at 0 range 0 .. 2;
      PHYIE          at 0 range 3 .. 3;
      PMTIE          at 0 range 4 .. 4;
      LPIIE          at 0 range 5 .. 5;
      Reserved_6_11  at 0 range 6 .. 11;
      TSIE           at 0 range 12 .. 12;
      TXSTSIE        at 0 range 13 .. 13;
      RXSTSIE        at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   --  Rx Tx status register
   type MACRxTxSR_Register is record
      --  Read-only. TJT
      TJT           : Boolean;
      --  Read-only. NCARR
      NCARR         : Boolean;
      --  Read-only. LCARR
      LCARR         : Boolean;
      --  Read-only. EXDEF
      EXDEF         : Boolean;
      --  Read-only. LCOL
      LCOL          : Boolean;
      --  Read-only. LCOL
      EXCOL         : Boolean;
      --  unspecified
      Reserved_6_7  : A0B.Types.SVD.UInt2;
      --  Read-only. RWT
      RWT           : Boolean;
      --  unspecified
      Reserved_9_31 : A0B.Types.SVD.UInt23;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MACRxTxSR_Register use record
      TJT           at 0 range 0 .. 0;
      NCARR         at 0 range 1 .. 1;
      LCARR         at 0 range 2 .. 2;
      EXDEF         at 0 range 3 .. 3;
      LCOL          at 0 range 4 .. 4;
      EXCOL         at 0 range 5 .. 5;
      Reserved_6_7  at 0 range 6 .. 7;
      RWT           at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   subtype MACPCSR_RWKPTR_Field is A0B.Types.SVD.UInt5;

   --  PMT control status register
   type MACPCSR_Register is record
      --  PWRDWN
      PWRDWN         : Boolean := False;
      --  MGKPKTEN
      MGKPKTEN       : Boolean := False;
      --  RWKPKTEN
      RWKPKTEN       : Boolean := False;
      --  unspecified
      Reserved_3_4   : A0B.Types.SVD.UInt2 := 16#0#;
      --  Read-only. MGKPRCVD
      MGKPRCVD       : Boolean := False;
      --  Read-only. RWKPRCVD
      RWKPRCVD       : Boolean := False;
      --  unspecified
      Reserved_7_8   : A0B.Types.SVD.UInt2 := 16#0#;
      --  GLBLUCAST
      GLBLUCAST      : Boolean := False;
      --  RWKPFE
      RWKPFE         : Boolean := False;
      --  unspecified
      Reserved_11_23 : A0B.Types.SVD.UInt13 := 16#0#;
      --  RWKPTR
      RWKPTR         : MACPCSR_RWKPTR_Field := 16#0#;
      --  unspecified
      Reserved_29_30 : A0B.Types.SVD.UInt2 := 16#0#;
      --  RWKFILTRST
      RWKFILTRST     : Boolean := False;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MACPCSR_Register use record
      PWRDWN         at 0 range 0 .. 0;
      MGKPKTEN       at 0 range 1 .. 1;
      RWKPKTEN       at 0 range 2 .. 2;
      Reserved_3_4   at 0 range 3 .. 4;
      MGKPRCVD       at 0 range 5 .. 5;
      RWKPRCVD       at 0 range 6 .. 6;
      Reserved_7_8   at 0 range 7 .. 8;
      GLBLUCAST      at 0 range 9 .. 9;
      RWKPFE         at 0 range 10 .. 10;
      Reserved_11_23 at 0 range 11 .. 23;
      RWKPTR         at 0 range 24 .. 28;
      Reserved_29_30 at 0 range 29 .. 30;
      RWKFILTRST     at 0 range 31 .. 31;
   end record;

   --  LPI control status register
   type MACLCSR_Register is record
      --  Read-only. TLPIEN
      TLPIEN         : Boolean := False;
      --  Read-only. TLPIEX
      TLPIEX         : Boolean := False;
      --  Read-only. RLPIEN
      RLPIEN         : Boolean := False;
      --  Read-only. RLPIEX
      RLPIEX         : Boolean := False;
      --  unspecified
      Reserved_4_7   : A0B.Types.SVD.UInt4 := 16#0#;
      --  Read-only. TLPIST
      TLPIST         : Boolean := False;
      --  Read-only. RLPIST
      RLPIST         : Boolean := False;
      --  unspecified
      Reserved_10_15 : A0B.Types.SVD.UInt6 := 16#0#;
      --  LPIEN
      LPIEN          : Boolean := False;
      --  PLS
      PLS            : Boolean := False;
      --  PLSEN
      PLSEN          : Boolean := False;
      --  LPITXA
      LPITXA         : Boolean := False;
      --  LPITE
      LPITE          : Boolean := False;
      --  LPITCSE
      LPITCSE        : Boolean := False;
      --  unspecified
      Reserved_22_31 : A0B.Types.SVD.UInt10 := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MACLCSR_Register use record
      TLPIEN         at 0 range 0 .. 0;
      TLPIEX         at 0 range 1 .. 1;
      RLPIEN         at 0 range 2 .. 2;
      RLPIEX         at 0 range 3 .. 3;
      Reserved_4_7   at 0 range 4 .. 7;
      TLPIST         at 0 range 8 .. 8;
      RLPIST         at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      LPIEN          at 0 range 16 .. 16;
      PLS            at 0 range 17 .. 17;
      PLSEN          at 0 range 18 .. 18;
      LPITXA         at 0 range 19 .. 19;
      LPITE          at 0 range 20 .. 20;
      LPITCSE        at 0 range 21 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   subtype MACLTCR_TWT_Field is A0B.Types.SVD.UInt16;
   subtype MACLTCR_LST_Field is A0B.Types.SVD.UInt10;

   --  LPI timers control register
   type MACLTCR_Register is record
      --  TWT
      TWT            : MACLTCR_TWT_Field := 16#0#;
      --  LST
      LST            : MACLTCR_LST_Field := 16#3E8#;
      --  unspecified
      Reserved_26_31 : A0B.Types.SVD.UInt6 := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MACLTCR_Register use record
      TWT            at 0 range 0 .. 15;
      LST            at 0 range 16 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype MACLETR_LPIET_Field is A0B.Types.SVD.UInt17;

   --  LPI entry timer register
   type MACLETR_Register is record
      --  LPIET
      LPIET          : MACLETR_LPIET_Field := 16#0#;
      --  unspecified
      Reserved_17_31 : A0B.Types.SVD.UInt15 := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MACLETR_Register use record
      LPIET          at 0 range 0 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   subtype MAC1USTCR_TIC_1US_CNTR_Field is A0B.Types.SVD.UInt12;

   --  1-microsecond-tick counter register
   type MAC1USTCR_Register is record
      --  TIC_1US_CNTR
      TIC_1US_CNTR   : MAC1USTCR_TIC_1US_CNTR_Field := 16#0#;
      --  unspecified
      Reserved_12_31 : A0B.Types.SVD.UInt20 := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MAC1USTCR_Register use record
      TIC_1US_CNTR   at 0 range 0 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype MACVR_SNPSVER_Field is A0B.Types.SVD.Byte;
   subtype MACVR_USERVER_Field is A0B.Types.SVD.Byte;

   --  Version register
   type MACVR_Register is record
      --  Read-only. SNPSVER
      SNPSVER        : MACVR_SNPSVER_Field;
      --  Read-only. USERVER
      USERVER        : MACVR_USERVER_Field;
      --  unspecified
      Reserved_16_31 : A0B.Types.SVD.UInt16;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MACVR_Register use record
      SNPSVER        at 0 range 0 .. 7;
      USERVER        at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MACDR_RFCFCSTS_Field is A0B.Types.SVD.UInt2;
   subtype MACDR_TFCSTS_Field is A0B.Types.SVD.UInt2;

   --  Debug register
   type MACDR_Register is record
      --  Read-only. RPESTS
      RPESTS         : Boolean;
      --  Read-only. RFCFCSTS
      RFCFCSTS       : MACDR_RFCFCSTS_Field;
      --  unspecified
      Reserved_3_15  : A0B.Types.SVD.UInt13;
      --  Read-only. TPESTS
      TPESTS         : Boolean;
      --  Read-only. TFCSTS
      TFCSTS         : MACDR_TFCSTS_Field;
      --  unspecified
      Reserved_19_31 : A0B.Types.SVD.UInt13;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MACDR_Register use record
      RPESTS         at 0 range 0 .. 0;
      RFCFCSTS       at 0 range 1 .. 2;
      Reserved_3_15  at 0 range 3 .. 15;
      TPESTS         at 0 range 16 .. 16;
      TFCSTS         at 0 range 17 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   subtype MACHWF1R_RXFIFOSIZE_Field is A0B.Types.SVD.UInt5;
   subtype MACHWF1R_TXFIFOSIZE_Field is A0B.Types.SVD.UInt5;
   subtype MACHWF1R_HASHTBLSZ_Field is A0B.Types.SVD.UInt2;
   subtype MACHWF1R_L3L4FNUM_Field is A0B.Types.SVD.UInt4;

   --  HW feature 1 register
   type MACHWF1R_Register is record
      --  Read-only. RXFIFOSIZE
      RXFIFOSIZE     : MACHWF1R_RXFIFOSIZE_Field;
      --  unspecified
      Reserved_5_5   : A0B.Types.SVD.Bit;
      --  Read-only. TXFIFOSIZE
      TXFIFOSIZE     : MACHWF1R_TXFIFOSIZE_Field;
      --  Read-only. OSTEN
      OSTEN          : Boolean;
      --  Read-only. PTOEN
      PTOEN          : Boolean;
      --  Read-only. ADVTHWORD
      ADVTHWORD      : Boolean;
      --  unspecified
      Reserved_14_15 : A0B.Types.SVD.UInt2;
      --  Read-only. DCBEN
      DCBEN          : Boolean;
      --  Read-only. SPHEN
      SPHEN          : Boolean;
      --  Read-only. TSOEN
      TSOEN          : Boolean;
      --  Read-only. DBGMEMA
      DBGMEMA        : Boolean;
      --  Read-only. AVSEL
      AVSEL          : Boolean;
      --  unspecified
      Reserved_21_23 : A0B.Types.SVD.UInt3;
      --  Read-only. HASHTBLSZ
      HASHTBLSZ      : MACHWF1R_HASHTBLSZ_Field;
      --  unspecified
      Reserved_26_26 : A0B.Types.SVD.Bit;
      --  Read-only. L3L4FNUM
      L3L4FNUM       : MACHWF1R_L3L4FNUM_Field;
      --  unspecified
      Reserved_31_31 : A0B.Types.SVD.Bit;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MACHWF1R_Register use record
      RXFIFOSIZE     at 0 range 0 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      TXFIFOSIZE     at 0 range 6 .. 10;
      OSTEN          at 0 range 11 .. 11;
      PTOEN          at 0 range 12 .. 12;
      ADVTHWORD      at 0 range 13 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      DCBEN          at 0 range 16 .. 16;
      SPHEN          at 0 range 17 .. 17;
      TSOEN          at 0 range 18 .. 18;
      DBGMEMA        at 0 range 19 .. 19;
      AVSEL          at 0 range 20 .. 20;
      Reserved_21_23 at 0 range 21 .. 23;
      HASHTBLSZ      at 0 range 24 .. 25;
      Reserved_26_26 at 0 range 26 .. 26;
      L3L4FNUM       at 0 range 27 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype MACHWF2R_RXQCNT_Field is A0B.Types.SVD.UInt4;
   subtype MACHWF2R_TXQCNT_Field is A0B.Types.SVD.UInt4;
   subtype MACHWF2R_RXCHCNT_Field is A0B.Types.SVD.UInt4;
   subtype MACHWF2R_TXCHCNT_Field is A0B.Types.SVD.UInt4;
   subtype MACHWF2R_PPSOUTNUM_Field is A0B.Types.SVD.UInt3;
   subtype MACHWF2R_AUXSNAPNUM_Field is A0B.Types.SVD.UInt3;

   --  HW feature 2 register
   type MACHWF2R_Register is record
      --  Read-only. RXQCNT
      RXQCNT         : MACHWF2R_RXQCNT_Field;
      --  unspecified
      Reserved_4_5   : A0B.Types.SVD.UInt2;
      --  Read-only. TXQCNT
      TXQCNT         : MACHWF2R_TXQCNT_Field;
      --  unspecified
      Reserved_10_11 : A0B.Types.SVD.UInt2;
      --  Read-only. RXCHCNT
      RXCHCNT        : MACHWF2R_RXCHCNT_Field;
      --  unspecified
      Reserved_16_17 : A0B.Types.SVD.UInt2;
      --  Read-only. TXCHCNT
      TXCHCNT        : MACHWF2R_TXCHCNT_Field;
      --  unspecified
      Reserved_22_23 : A0B.Types.SVD.UInt2;
      --  Read-only. PPSOUTNUM
      PPSOUTNUM      : MACHWF2R_PPSOUTNUM_Field;
      --  unspecified
      Reserved_27_27 : A0B.Types.SVD.Bit;
      --  Read-only. AUXSNAPNUM
      AUXSNAPNUM     : MACHWF2R_AUXSNAPNUM_Field;
      --  unspecified
      Reserved_31_31 : A0B.Types.SVD.Bit;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MACHWF2R_Register use record
      RXQCNT         at 0 range 0 .. 3;
      Reserved_4_5   at 0 range 4 .. 5;
      TXQCNT         at 0 range 6 .. 9;
      Reserved_10_11 at 0 range 10 .. 11;
      RXCHCNT        at 0 range 12 .. 15;
      Reserved_16_17 at 0 range 16 .. 17;
      TXCHCNT        at 0 range 18 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      PPSOUTNUM      at 0 range 24 .. 26;
      Reserved_27_27 at 0 range 27 .. 27;
      AUXSNAPNUM     at 0 range 28 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype MACMDIOAR_GOC_Field is A0B.Types.SVD.UInt2;
   subtype MACMDIOAR_CR_Field is A0B.Types.SVD.UInt4;
   subtype MACMDIOAR_NTC_Field is A0B.Types.SVD.UInt3;
   subtype MACMDIOAR_RDA_Field is A0B.Types.SVD.UInt5;
   subtype MACMDIOAR_PA_Field is A0B.Types.SVD.UInt5;

   --  MDIO address register
   type MACMDIOAR_Register is record
      --  MB
      MB             : Boolean := False;
      --  C45E
      C45E           : Boolean := False;
      --  GOC
      GOC            : MACMDIOAR_GOC_Field := 16#0#;
      --  SKAP
      SKAP           : Boolean := False;
      --  unspecified
      Reserved_5_7   : A0B.Types.SVD.UInt3 := 16#0#;
      --  CR
      CR             : MACMDIOAR_CR_Field := 16#0#;
      --  NTC
      NTC            : MACMDIOAR_NTC_Field := 16#0#;
      --  unspecified
      Reserved_15_15 : A0B.Types.SVD.Bit := 16#0#;
      --  RDA
      RDA            : MACMDIOAR_RDA_Field := 16#0#;
      --  PA
      PA             : MACMDIOAR_PA_Field := 16#0#;
      --  BTB
      BTB            : Boolean := False;
      --  PSE
      PSE            : Boolean := False;
      --  unspecified
      Reserved_28_31 : A0B.Types.SVD.UInt4 := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MACMDIOAR_Register use record
      MB             at 0 range 0 .. 0;
      C45E           at 0 range 1 .. 1;
      GOC            at 0 range 2 .. 3;
      SKAP           at 0 range 4 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      CR             at 0 range 8 .. 11;
      NTC            at 0 range 12 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      RDA            at 0 range 16 .. 20;
      PA             at 0 range 21 .. 25;
      BTB            at 0 range 26 .. 26;
      PSE            at 0 range 27 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype MACMDIODR_MD_Field is A0B.Types.SVD.UInt16;
   subtype MACMDIODR_RA_Field is A0B.Types.SVD.UInt16;

   --  MDIO data register
   type MACMDIODR_Register is record
      --  MD
      MD : MACMDIODR_MD_Field := 16#0#;
      --  RA
      RA : MACMDIODR_RA_Field := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MACMDIODR_Register use record
      MD at 0 range 0 .. 15;
      RA at 0 range 16 .. 31;
   end record;

   subtype MACA0HR_ADDRHI_Field is A0B.Types.SVD.UInt16;

   --  Address 0 high register
   type MACA0HR_Register is record
      --  ADDRHI
      ADDRHI         : MACA0HR_ADDRHI_Field := 16#FFFF#;
      --  unspecified
      Reserved_16_30 : A0B.Types.SVD.UInt15 := 16#0#;
      --  Read-only. AE
      AE             : Boolean := True;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MACA0HR_Register use record
      ADDRHI         at 0 range 0 .. 15;
      Reserved_16_30 at 0 range 16 .. 30;
      AE             at 0 range 31 .. 31;
   end record;

   subtype MACA1HR_ADDRHI_Field is A0B.Types.SVD.UInt16;
   subtype MACA1HR_MBC_Field is A0B.Types.SVD.UInt6;

   --  Address 1 high register
   type MACA1HR_Register is record
      --  ADDRHI
      ADDRHI         : MACA1HR_ADDRHI_Field := 16#FFFF#;
      --  unspecified
      Reserved_16_23 : A0B.Types.SVD.Byte := 16#0#;
      --  MBC
      MBC            : MACA1HR_MBC_Field := 16#0#;
      --  SA
      SA             : Boolean := False;
      --  AE
      AE             : Boolean := False;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MACA1HR_Register use record
      ADDRHI         at 0 range 0 .. 15;
      Reserved_16_23 at 0 range 16 .. 23;
      MBC            at 0 range 24 .. 29;
      SA             at 0 range 30 .. 30;
      AE             at 0 range 31 .. 31;
   end record;

   subtype MACA2HR_ADDRHI_Field is A0B.Types.SVD.UInt16;
   subtype MACA2HR_MBC_Field is A0B.Types.SVD.UInt6;

   --  Address 2 high register
   type MACA2HR_Register is record
      --  ADDRHI
      ADDRHI         : MACA2HR_ADDRHI_Field := 16#FFFF#;
      --  unspecified
      Reserved_16_23 : A0B.Types.SVD.Byte := 16#0#;
      --  MBC
      MBC            : MACA2HR_MBC_Field := 16#0#;
      --  SA
      SA             : Boolean := False;
      --  AE
      AE             : Boolean := False;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MACA2HR_Register use record
      ADDRHI         at 0 range 0 .. 15;
      Reserved_16_23 at 0 range 16 .. 23;
      MBC            at 0 range 24 .. 29;
      SA             at 0 range 30 .. 30;
      AE             at 0 range 31 .. 31;
   end record;

   subtype MACA3HR_ADDRHI_Field is A0B.Types.SVD.UInt16;
   subtype MACA3HR_MBC_Field is A0B.Types.SVD.UInt6;

   --  Address 3 high register
   type MACA3HR_Register is record
      --  ADDRHI
      ADDRHI         : MACA3HR_ADDRHI_Field := 16#FFFF#;
      --  unspecified
      Reserved_16_23 : A0B.Types.SVD.Byte := 16#0#;
      --  MBC
      MBC            : MACA3HR_MBC_Field := 16#0#;
      --  SA
      SA             : Boolean := False;
      --  AE
      AE             : Boolean := False;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MACA3HR_Register use record
      ADDRHI         at 0 range 0 .. 15;
      Reserved_16_23 at 0 range 16 .. 23;
      MBC            at 0 range 24 .. 29;
      SA             at 0 range 30 .. 30;
      AE             at 0 range 31 .. 31;
   end record;

   --  MMC control register
   type MMC_CONTROL_Register is record
      --  CNTRST
      CNTRST        : Boolean := False;
      --  CNTSTOPRO
      CNTSTOPRO     : Boolean := False;
      --  RSTONRD
      RSTONRD       : Boolean := False;
      --  CNTFREEZ
      CNTFREEZ      : Boolean := False;
      --  CNTPRST
      CNTPRST       : Boolean := False;
      --  CNTPRSTLVL
      CNTPRSTLVL    : Boolean := False;
      --  unspecified
      Reserved_6_7  : A0B.Types.SVD.UInt2 := 16#0#;
      --  UCDBC
      UCDBC         : Boolean := False;
      --  unspecified
      Reserved_9_31 : A0B.Types.SVD.UInt23 := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MMC_CONTROL_Register use record
      CNTRST        at 0 range 0 .. 0;
      CNTSTOPRO     at 0 range 1 .. 1;
      RSTONRD       at 0 range 2 .. 2;
      CNTFREEZ      at 0 range 3 .. 3;
      CNTPRST       at 0 range 4 .. 4;
      CNTPRSTLVL    at 0 range 5 .. 5;
      Reserved_6_7  at 0 range 6 .. 7;
      UCDBC         at 0 range 8 .. 8;
      Reserved_9_31 at 0 range 9 .. 31;
   end record;

   --  MMC Rx interrupt register
   type MMC_RX_INTERRUPT_Register is record
      --  unspecified
      Reserved_0_4   : A0B.Types.SVD.UInt5;
      --  Read-only. RXCRCERPIS
      RXCRCERPIS     : Boolean;
      --  Read-only. RXALGNERPIS
      RXALGNERPIS    : Boolean;
      --  unspecified
      Reserved_7_16  : A0B.Types.SVD.UInt10;
      --  Read-only. RXUCGPIS
      RXUCGPIS       : Boolean;
      --  unspecified
      Reserved_18_25 : A0B.Types.SVD.Byte;
      --  Read-only. RXLPIUSCIS
      RXLPIUSCIS     : Boolean;
      --  Read-only. RXLPITRCIS
      RXLPITRCIS     : Boolean;
      --  unspecified
      Reserved_28_31 : A0B.Types.SVD.UInt4;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MMC_RX_INTERRUPT_Register use record
      Reserved_0_4   at 0 range 0 .. 4;
      RXCRCERPIS     at 0 range 5 .. 5;
      RXALGNERPIS    at 0 range 6 .. 6;
      Reserved_7_16  at 0 range 7 .. 16;
      RXUCGPIS       at 0 range 17 .. 17;
      Reserved_18_25 at 0 range 18 .. 25;
      RXLPIUSCIS     at 0 range 26 .. 26;
      RXLPITRCIS     at 0 range 27 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  MMC Tx interrupt register
   type MMC_TX_INTERRUPT_Register is record
      --  unspecified
      Reserved_0_13  : A0B.Types.SVD.UInt14;
      --  Read-only. TXSCOLGPIS
      TXSCOLGPIS     : Boolean;
      --  Read-only. TXMCOLGPIS
      TXMCOLGPIS     : Boolean;
      --  unspecified
      Reserved_16_20 : A0B.Types.SVD.UInt5;
      --  Read-only. TXGPKTIS
      TXGPKTIS       : Boolean;
      --  unspecified
      Reserved_22_25 : A0B.Types.SVD.UInt4;
      --  Read-only. TXLPIUSCIS
      TXLPIUSCIS     : Boolean;
      --  Read-only. TXLPITRCIS
      TXLPITRCIS     : Boolean;
      --  unspecified
      Reserved_28_31 : A0B.Types.SVD.UInt4;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MMC_TX_INTERRUPT_Register use record
      Reserved_0_13  at 0 range 0 .. 13;
      TXSCOLGPIS     at 0 range 14 .. 14;
      TXMCOLGPIS     at 0 range 15 .. 15;
      Reserved_16_20 at 0 range 16 .. 20;
      TXGPKTIS       at 0 range 21 .. 21;
      Reserved_22_25 at 0 range 22 .. 25;
      TXLPIUSCIS     at 0 range 26 .. 26;
      TXLPITRCIS     at 0 range 27 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  MMC Rx interrupt mask register
   type MMC_RX_INTERRUPT_MASK_Register is record
      --  unspecified
      Reserved_0_4   : A0B.Types.SVD.UInt5 := 16#0#;
      --  RXCRCERPIM
      RXCRCERPIM     : Boolean := False;
      --  RXALGNERPIM
      RXALGNERPIM    : Boolean := False;
      --  unspecified
      Reserved_7_16  : A0B.Types.SVD.UInt10 := 16#0#;
      --  RXUCGPIM
      RXUCGPIM       : Boolean := False;
      --  unspecified
      Reserved_18_25 : A0B.Types.SVD.Byte := 16#0#;
      --  RXLPIUSCIM
      RXLPIUSCIM     : Boolean := False;
      --  Read-only. RXLPITRCIM
      RXLPITRCIM     : Boolean := False;
      --  unspecified
      Reserved_28_31 : A0B.Types.SVD.UInt4 := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MMC_RX_INTERRUPT_MASK_Register use record
      Reserved_0_4   at 0 range 0 .. 4;
      RXCRCERPIM     at 0 range 5 .. 5;
      RXALGNERPIM    at 0 range 6 .. 6;
      Reserved_7_16  at 0 range 7 .. 16;
      RXUCGPIM       at 0 range 17 .. 17;
      Reserved_18_25 at 0 range 18 .. 25;
      RXLPIUSCIM     at 0 range 26 .. 26;
      RXLPITRCIM     at 0 range 27 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  MMC Tx interrupt mask register
   type MMC_TX_INTERRUPT_MASK_Register is record
      --  unspecified
      Reserved_0_13  : A0B.Types.SVD.UInt14 := 16#0#;
      --  TXSCOLGPIM
      TXSCOLGPIM     : Boolean := False;
      --  TXMCOLGPIM
      TXMCOLGPIM     : Boolean := False;
      --  unspecified
      Reserved_16_20 : A0B.Types.SVD.UInt5 := 16#0#;
      --  TXGPKTIM
      TXGPKTIM       : Boolean := False;
      --  unspecified
      Reserved_22_25 : A0B.Types.SVD.UInt4 := 16#0#;
      --  TXLPIUSCIM
      TXLPIUSCIM     : Boolean := False;
      --  Read-only. TXLPITRCIM
      TXLPITRCIM     : Boolean := False;
      --  unspecified
      Reserved_28_31 : A0B.Types.SVD.UInt4 := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MMC_TX_INTERRUPT_MASK_Register use record
      Reserved_0_13  at 0 range 0 .. 13;
      TXSCOLGPIM     at 0 range 14 .. 14;
      TXMCOLGPIM     at 0 range 15 .. 15;
      Reserved_16_20 at 0 range 16 .. 20;
      TXGPKTIM       at 0 range 21 .. 21;
      Reserved_22_25 at 0 range 22 .. 25;
      TXLPIUSCIM     at 0 range 26 .. 26;
      TXLPITRCIM     at 0 range 27 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype MACL3L4C0R_L3HSBM0_Field is A0B.Types.SVD.UInt5;
   subtype MACL3L4C0R_L3HDBM0_Field is A0B.Types.SVD.UInt5;

   --  L3 and L4 control 0 register
   type MACL3L4C0R_Register is record
      --  L3PEN0
      L3PEN0         : Boolean := False;
      --  unspecified
      Reserved_1_1   : A0B.Types.SVD.Bit := 16#0#;
      --  L3SAM0
      L3SAM0         : Boolean := False;
      --  L3SAIM0
      L3SAIM0        : Boolean := False;
      --  L3DAM0
      L3DAM0         : Boolean := False;
      --  L3DAIM0
      L3DAIM0        : Boolean := False;
      --  L3HSBM0
      L3HSBM0        : MACL3L4C0R_L3HSBM0_Field := 16#0#;
      --  L3HDBM0
      L3HDBM0        : MACL3L4C0R_L3HDBM0_Field := 16#0#;
      --  L4PEN0
      L4PEN0         : Boolean := False;
      --  unspecified
      Reserved_17_17 : A0B.Types.SVD.Bit := 16#0#;
      --  L4SPM0
      L4SPM0         : Boolean := False;
      --  L4SPIM0
      L4SPIM0        : Boolean := False;
      --  L4DPM0
      L4DPM0         : Boolean := False;
      --  L4DPIM0
      L4DPIM0        : Boolean := False;
      --  unspecified
      Reserved_22_31 : A0B.Types.SVD.UInt10 := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MACL3L4C0R_Register use record
      L3PEN0         at 0 range 0 .. 0;
      Reserved_1_1   at 0 range 1 .. 1;
      L3SAM0         at 0 range 2 .. 2;
      L3SAIM0        at 0 range 3 .. 3;
      L3DAM0         at 0 range 4 .. 4;
      L3DAIM0        at 0 range 5 .. 5;
      L3HSBM0        at 0 range 6 .. 10;
      L3HDBM0        at 0 range 11 .. 15;
      L4PEN0         at 0 range 16 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      L4SPM0         at 0 range 18 .. 18;
      L4SPIM0        at 0 range 19 .. 19;
      L4DPM0         at 0 range 20 .. 20;
      L4DPIM0        at 0 range 21 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   subtype MACL4A0R_L4SP0_Field is A0B.Types.SVD.UInt16;
   subtype MACL4A0R_L4DP0_Field is A0B.Types.SVD.UInt16;

   --  Layer4 address filter 0 register
   type MACL4A0R_Register is record
      --  L4SP0
      L4SP0 : MACL4A0R_L4SP0_Field := 16#0#;
      --  L4DP0
      L4DP0 : MACL4A0R_L4DP0_Field := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MACL4A0R_Register use record
      L4SP0 at 0 range 0 .. 15;
      L4DP0 at 0 range 16 .. 31;
   end record;

   subtype MACL3L4C1R_L3HSBM1_Field is A0B.Types.SVD.UInt5;
   subtype MACL3L4C1R_L3HDBM1_Field is A0B.Types.SVD.UInt5;

   --  L3 and L4 control 1 register
   type MACL3L4C1R_Register is record
      --  L3PEN1
      L3PEN1         : Boolean := False;
      --  unspecified
      Reserved_1_1   : A0B.Types.SVD.Bit := 16#0#;
      --  L3SAM1
      L3SAM1         : Boolean := False;
      --  L3SAIM1
      L3SAIM1        : Boolean := False;
      --  L3DAM1
      L3DAM1         : Boolean := False;
      --  L3DAIM1
      L3DAIM1        : Boolean := False;
      --  L3HSBM1
      L3HSBM1        : MACL3L4C1R_L3HSBM1_Field := 16#0#;
      --  L3HDBM1
      L3HDBM1        : MACL3L4C1R_L3HDBM1_Field := 16#0#;
      --  L4PEN1
      L4PEN1         : Boolean := False;
      --  unspecified
      Reserved_17_17 : A0B.Types.SVD.Bit := 16#0#;
      --  L4SPM1
      L4SPM1         : Boolean := False;
      --  L4SPIM1
      L4SPIM1        : Boolean := False;
      --  L4DPM1
      L4DPM1         : Boolean := False;
      --  L4DPIM1
      L4DPIM1        : Boolean := False;
      --  unspecified
      Reserved_22_31 : A0B.Types.SVD.UInt10 := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MACL3L4C1R_Register use record
      L3PEN1         at 0 range 0 .. 0;
      Reserved_1_1   at 0 range 1 .. 1;
      L3SAM1         at 0 range 2 .. 2;
      L3SAIM1        at 0 range 3 .. 3;
      L3DAM1         at 0 range 4 .. 4;
      L3DAIM1        at 0 range 5 .. 5;
      L3HSBM1        at 0 range 6 .. 10;
      L3HDBM1        at 0 range 11 .. 15;
      L4PEN1         at 0 range 16 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      L4SPM1         at 0 range 18 .. 18;
      L4SPIM1        at 0 range 19 .. 19;
      L4DPM1         at 0 range 20 .. 20;
      L4DPIM1        at 0 range 21 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   subtype MACL4A1R_L4SP1_Field is A0B.Types.SVD.UInt16;
   subtype MACL4A1R_L4DP1_Field is A0B.Types.SVD.UInt16;

   --  Layer 4 address filter 1 register
   type MACL4A1R_Register is record
      --  L4SP1
      L4SP1 : MACL4A1R_L4SP1_Field := 16#0#;
      --  L4DP1
      L4DP1 : MACL4A1R_L4DP1_Field := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MACL4A1R_Register use record
      L4SP1 at 0 range 0 .. 15;
      L4DP1 at 0 range 16 .. 31;
   end record;

   subtype MACTSCR_SNAPTYPSEL_Field is A0B.Types.SVD.UInt2;

   --  Timestamp control Register
   type MACTSCR_Register is record
      --  TSENA
      TSENA          : Boolean := False;
      --  TSCFUPDT
      TSCFUPDT       : Boolean := False;
      --  TSINIT
      TSINIT         : Boolean := False;
      --  TSUPDT
      TSUPDT         : Boolean := False;
      --  unspecified
      Reserved_4_4   : A0B.Types.SVD.Bit := 16#0#;
      --  TSADDREG
      TSADDREG       : Boolean := False;
      --  unspecified
      Reserved_6_7   : A0B.Types.SVD.UInt2 := 16#0#;
      --  TSENALL
      TSENALL        : Boolean := False;
      --  TSCTRLSSR
      TSCTRLSSR      : Boolean := True;
      --  TSVER2ENA
      TSVER2ENA      : Boolean := False;
      --  TSIPENA
      TSIPENA        : Boolean := False;
      --  TSIPV6ENA
      TSIPV6ENA      : Boolean := False;
      --  TSIPV4ENA
      TSIPV4ENA      : Boolean := False;
      --  TSEVNTENA
      TSEVNTENA      : Boolean := False;
      --  TSMSTRENA
      TSMSTRENA      : Boolean := False;
      --  SNAPTYPSEL
      SNAPTYPSEL     : MACTSCR_SNAPTYPSEL_Field := 16#0#;
      --  TSENMACADDR
      TSENMACADDR    : Boolean := False;
      --  Read-only. CSC
      CSC            : Boolean := False;
      --  unspecified
      Reserved_20_23 : A0B.Types.SVD.UInt4 := 16#0#;
      --  TXTSSTSM
      TXTSSTSM       : Boolean := False;
      --  unspecified
      Reserved_25_31 : A0B.Types.SVD.UInt7 := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MACTSCR_Register use record
      TSENA          at 0 range 0 .. 0;
      TSCFUPDT       at 0 range 1 .. 1;
      TSINIT         at 0 range 2 .. 2;
      TSUPDT         at 0 range 3 .. 3;
      Reserved_4_4   at 0 range 4 .. 4;
      TSADDREG       at 0 range 5 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      TSENALL        at 0 range 8 .. 8;
      TSCTRLSSR      at 0 range 9 .. 9;
      TSVER2ENA      at 0 range 10 .. 10;
      TSIPENA        at 0 range 11 .. 11;
      TSIPV6ENA      at 0 range 12 .. 12;
      TSIPV4ENA      at 0 range 13 .. 13;
      TSEVNTENA      at 0 range 14 .. 14;
      TSMSTRENA      at 0 range 15 .. 15;
      SNAPTYPSEL     at 0 range 16 .. 17;
      TSENMACADDR    at 0 range 18 .. 18;
      CSC            at 0 range 19 .. 19;
      Reserved_20_23 at 0 range 20 .. 23;
      TXTSSTSM       at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   subtype MACSSIR_SNSINC_Field is A0B.Types.SVD.Byte;
   subtype MACSSIR_SSINC_Field is A0B.Types.SVD.Byte;

   --  Sub-second increment register
   type MACSSIR_Register is record
      --  unspecified
      Reserved_0_7   : A0B.Types.SVD.Byte := 16#0#;
      --  SNSINC
      SNSINC         : MACSSIR_SNSINC_Field := 16#0#;
      --  SSINC
      SSINC          : MACSSIR_SSINC_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : A0B.Types.SVD.Byte := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MACSSIR_Register use record
      Reserved_0_7   at 0 range 0 .. 7;
      SNSINC         at 0 range 8 .. 15;
      SSINC          at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype MACSTNR_TSSS_Field is A0B.Types.SVD.UInt31;

   --  System time nanoseconds register
   type MACSTNR_Register is record
      --  Read-only. TSSS
      TSSS           : MACSTNR_TSSS_Field;
      --  unspecified
      Reserved_31_31 : A0B.Types.SVD.Bit;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MACSTNR_Register use record
      TSSS           at 0 range 0 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype MACSTNUR_TSSS_Field is A0B.Types.SVD.UInt31;

   --  System time nanoseconds update register
   type MACSTNUR_Register is record
      --  TSSS
      TSSS   : MACSTNUR_TSSS_Field := 16#0#;
      --  ADDSUB
      ADDSUB : Boolean := False;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MACSTNUR_Register use record
      TSSS   at 0 range 0 .. 30;
      ADDSUB at 0 range 31 .. 31;
   end record;

   subtype MACTSSR_ATSSTN_Field is A0B.Types.SVD.UInt4;
   subtype MACTSSR_ATSNS_Field is A0B.Types.SVD.UInt5;

   --  Timestamp status register
   type MACTSSR_Register is record
      --  Read-only. TSSOVF
      TSSOVF         : Boolean;
      --  Read-only. TSTARGT0
      TSTARGT0       : Boolean;
      --  Read-only. AUXTSTRIG
      AUXTSTRIG      : Boolean;
      --  Read-only. TSTRGTERR0
      TSTRGTERR0     : Boolean;
      --  unspecified
      Reserved_4_14  : A0B.Types.SVD.UInt11;
      --  Read-only. TXTSSIS
      TXTSSIS        : Boolean;
      --  Read-only. ATSSTN
      ATSSTN         : MACTSSR_ATSSTN_Field;
      --  unspecified
      Reserved_20_23 : A0B.Types.SVD.UInt4;
      --  Read-only. ATSSTM
      ATSSTM         : Boolean;
      --  Read-only. ATSNS
      ATSNS          : MACTSSR_ATSNS_Field;
      --  unspecified
      Reserved_30_31 : A0B.Types.SVD.UInt2;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MACTSSR_Register use record
      TSSOVF         at 0 range 0 .. 0;
      TSTARGT0       at 0 range 1 .. 1;
      AUXTSTRIG      at 0 range 2 .. 2;
      TSTRGTERR0     at 0 range 3 .. 3;
      Reserved_4_14  at 0 range 4 .. 14;
      TXTSSIS        at 0 range 15 .. 15;
      ATSSTN         at 0 range 16 .. 19;
      Reserved_20_23 at 0 range 20 .. 23;
      ATSSTM         at 0 range 24 .. 24;
      ATSNS          at 0 range 25 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype MACTxTSSNR_TXTSSLO_Field is A0B.Types.SVD.UInt31;

   --  Tx timestamp status nanoseconds register
   type MACTxTSSNR_Register is record
      --  Read-only. TXTSSLO
      TXTSSLO  : MACTxTSSNR_TXTSSLO_Field;
      --  Read-only. TXTSSMIS
      TXTSSMIS : Boolean;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MACTxTSSNR_Register use record
      TXTSSLO  at 0 range 0 .. 30;
      TXTSSMIS at 0 range 31 .. 31;
   end record;

   --  MACACR_ATSEN array
   type MACACR_ATSEN_Field_Array is array (0 .. 3) of Boolean
     with Component_Size => 1, Size => 4;

   --  Type definition for MACACR_ATSEN
   type MACACR_ATSEN_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  ATSEN as a value
            Val : A0B.Types.SVD.UInt4;
         when True =>
            --  ATSEN as an array
            Arr : MACACR_ATSEN_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 4;

   for MACACR_ATSEN_Field use record
      Val at 0 range 0 .. 3;
      Arr at 0 range 0 .. 3;
   end record;

   --  Auxiliary control register
   type MACACR_Register is record
      --  ATSFC
      ATSFC         : Boolean := False;
      --  unspecified
      Reserved_1_3  : A0B.Types.SVD.UInt3 := 16#0#;
      --  ATSEN0
      ATSEN         : MACACR_ATSEN_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_8_31 : A0B.Types.SVD.UInt24 := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MACACR_Register use record
      ATSFC         at 0 range 0 .. 0;
      Reserved_1_3  at 0 range 1 .. 3;
      ATSEN         at 0 range 4 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype MACATSNR_AUXTSLO_Field is A0B.Types.SVD.UInt31;

   --  Auxiliary timestamp nanoseconds register
   type MACATSNR_Register is record
      --  Read-only. AUXTSLO
      AUXTSLO        : MACATSNR_AUXTSLO_Field;
      --  unspecified
      Reserved_31_31 : A0B.Types.SVD.Bit;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MACATSNR_Register use record
      AUXTSLO        at 0 range 0 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype MACPPSCR_PPSCTRL_Field is A0B.Types.SVD.UInt4;
   subtype MACPPSCR_TRGTMODSEL0_Field is A0B.Types.SVD.UInt2;

   --  PPS control register
   type MACPPSCR_Register is record
      --  PPSCTRL
      PPSCTRL       : MACPPSCR_PPSCTRL_Field := 16#0#;
      --  PPSEN0
      PPSEN0        : Boolean := False;
      --  TRGTMODSEL0
      TRGTMODSEL0   : MACPPSCR_TRGTMODSEL0_Field := 16#0#;
      --  unspecified
      Reserved_7_31 : A0B.Types.SVD.UInt25 := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MACPPSCR_Register use record
      PPSCTRL       at 0 range 0 .. 3;
      PPSEN0        at 0 range 4 .. 4;
      TRGTMODSEL0   at 0 range 5 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   subtype MACPPSTTSR_TSTRH0_Field is A0B.Types.SVD.UInt31;

   --  PPS target time seconds register
   type MACPPSTTSR_Register is record
      --  TSTRH0
      TSTRH0         : MACPPSTTSR_TSTRH0_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : A0B.Types.SVD.Bit := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MACPPSTTSR_Register use record
      TSTRH0         at 0 range 0 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype MACPPSTTNR_TTSL0_Field is A0B.Types.SVD.UInt31;

   --  PPS target time nanoseconds register
   type MACPPSTTNR_Register is record
      --  TTSL0
      TTSL0     : MACPPSTTNR_TTSL0_Field := 16#0#;
      --  TRGTBUSY0
      TRGTBUSY0 : Boolean := False;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MACPPSTTNR_Register use record
      TTSL0     at 0 range 0 .. 30;
      TRGTBUSY0 at 0 range 31 .. 31;
   end record;

   subtype MACPOCR_DN_Field is A0B.Types.SVD.Byte;

   --  PTP Offload control register
   type MACPOCR_Register is record
      --  PTOEN
      PTOEN          : Boolean := False;
      --  ASYNCEN
      ASYNCEN        : Boolean := False;
      --  APDREQEN
      APDREQEN       : Boolean := False;
      --  unspecified
      Reserved_3_3   : A0B.Types.SVD.Bit := 16#0#;
      --  ASYNCTRIG
      ASYNCTRIG      : Boolean := False;
      --  APDREQTRIG
      APDREQTRIG     : Boolean := False;
      --  DRRDIS
      DRRDIS         : Boolean := False;
      --  unspecified
      Reserved_7_7   : A0B.Types.SVD.Bit := 16#0#;
      --  DN
      DN             : MACPOCR_DN_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : A0B.Types.SVD.UInt16 := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MACPOCR_Register use record
      PTOEN          at 0 range 0 .. 0;
      ASYNCEN        at 0 range 1 .. 1;
      APDREQEN       at 0 range 2 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      ASYNCTRIG      at 0 range 4 .. 4;
      APDREQTRIG     at 0 range 5 .. 5;
      DRRDIS         at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      DN             at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MACSPI2R_SPI2_Field is A0B.Types.SVD.UInt16;

   --  PTP Source port identity 2 register
   type MACSPI2R_Register is record
      --  SPI2
      SPI2           : MACSPI2R_SPI2_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : A0B.Types.SVD.UInt16 := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MACSPI2R_Register use record
      SPI2           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype MACLMIR_LSI_Field is A0B.Types.SVD.Byte;
   subtype MACLMIR_DRSYNCR_Field is A0B.Types.SVD.UInt3;
   subtype MACLMIR_LMPDRI_Field is A0B.Types.SVD.Byte;

   --  Log message interval register
   type MACLMIR_Register is record
      --  LSI
      LSI            : MACLMIR_LSI_Field := 16#0#;
      --  DRSYNCR
      DRSYNCR        : MACLMIR_DRSYNCR_Field := 16#0#;
      --  unspecified
      Reserved_11_23 : A0B.Types.SVD.UInt13 := 16#0#;
      --  LMPDRI
      LMPDRI         : MACLMIR_LMPDRI_Field := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MACLMIR_Register use record
      LSI            at 0 range 0 .. 7;
      DRSYNCR        at 0 range 8 .. 10;
      Reserved_11_23 at 0 range 11 .. 23;
      LMPDRI         at 0 range 24 .. 31;
   end record;

   --  Operating mode Register
   type MTLOMR_Register is record
      --  unspecified
      Reserved_0_0   : A0B.Types.SVD.Bit := 16#0#;
      --  DTXSTS
      DTXSTS         : Boolean := False;
      --  unspecified
      Reserved_2_7   : A0B.Types.SVD.UInt6 := 16#0#;
      --  CNTPRST
      CNTPRST        : Boolean := False;
      --  CNTCLR
      CNTCLR         : Boolean := False;
      --  unspecified
      Reserved_10_31 : A0B.Types.SVD.UInt22 := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MTLOMR_Register use record
      Reserved_0_0   at 0 range 0 .. 0;
      DTXSTS         at 0 range 1 .. 1;
      Reserved_2_7   at 0 range 2 .. 7;
      CNTPRST        at 0 range 8 .. 8;
      CNTCLR         at 0 range 9 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   --  Interrupt status Register
   type MTLISR_Register is record
      --  Read-only. Queue interrupt status
      Q0IS          : Boolean;
      --  unspecified
      Reserved_1_31 : A0B.Types.SVD.UInt31;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MTLISR_Register use record
      Q0IS          at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   subtype MTLTxQOMR_TXQEN_Field is A0B.Types.SVD.UInt2;
   subtype MTLTxQOMR_TTC_Field is A0B.Types.SVD.UInt3;
   subtype MTLTxQOMR_TQS_Field is A0B.Types.SVD.UInt9;

   --  Tx queue operating mode Register
   type MTLTxQOMR_Register is record
      --  Flush Transmit Queue
      FTQ            : Boolean := False;
      --  Transmit Store and Forward
      TSF            : Boolean := False;
      --  Read-only. Transmit Queue Enable
      TXQEN          : MTLTxQOMR_TXQEN_Field := 16#2#;
      --  Transmit Threshold Control
      TTC            : MTLTxQOMR_TTC_Field := 16#0#;
      --  unspecified
      Reserved_7_15  : A0B.Types.SVD.UInt9 := 16#0#;
      --  Transmit Queue Size
      TQS            : MTLTxQOMR_TQS_Field := 16#7#;
      --  unspecified
      Reserved_25_31 : A0B.Types.SVD.UInt7 := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MTLTxQOMR_Register use record
      FTQ            at 0 range 0 .. 0;
      TSF            at 0 range 1 .. 1;
      TXQEN          at 0 range 2 .. 3;
      TTC            at 0 range 4 .. 6;
      Reserved_7_15  at 0 range 7 .. 15;
      TQS            at 0 range 16 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   subtype MTLTxQUR_UFFRMCNT_Field is A0B.Types.SVD.UInt11;

   --  Tx queue underflow register
   type MTLTxQUR_Register is record
      --  Read-only. Underflow Packet Counter
      UFFRMCNT       : MTLTxQUR_UFFRMCNT_Field;
      --  Read-only. UFCNTOVF
      UFCNTOVF       : Boolean;
      --  unspecified
      Reserved_12_31 : A0B.Types.SVD.UInt20;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MTLTxQUR_Register use record
      UFFRMCNT       at 0 range 0 .. 10;
      UFCNTOVF       at 0 range 11 .. 11;
      Reserved_12_31 at 0 range 12 .. 31;
   end record;

   subtype MTLTxQDR_TRCSTS_Field is A0B.Types.SVD.UInt2;
   subtype MTLTxQDR_PTXQ_Field is A0B.Types.SVD.UInt3;
   subtype MTLTxQDR_STXSTSF_Field is A0B.Types.SVD.UInt3;

   --  Tx queue debug Register
   type MTLTxQDR_Register is record
      --  Read-only. TXQPAUSED
      TXQPAUSED      : Boolean;
      --  Read-only. TRCSTS
      TRCSTS         : MTLTxQDR_TRCSTS_Field;
      --  Read-only. TWCSTS
      TWCSTS         : Boolean;
      --  Read-only. TXQSTS
      TXQSTS         : Boolean;
      --  Read-only. TXSTSFSTS
      TXSTSFSTS      : Boolean;
      --  unspecified
      Reserved_6_15  : A0B.Types.SVD.UInt10;
      --  Read-only. PTXQ
      PTXQ           : MTLTxQDR_PTXQ_Field;
      --  unspecified
      Reserved_19_19 : A0B.Types.SVD.Bit;
      --  Read-only. STXSTSF
      STXSTSF        : MTLTxQDR_STXSTSF_Field;
      --  unspecified
      Reserved_23_31 : A0B.Types.SVD.UInt9;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MTLTxQDR_Register use record
      TXQPAUSED      at 0 range 0 .. 0;
      TRCSTS         at 0 range 1 .. 2;
      TWCSTS         at 0 range 3 .. 3;
      TXQSTS         at 0 range 4 .. 4;
      TXSTSFSTS      at 0 range 5 .. 5;
      Reserved_6_15  at 0 range 6 .. 15;
      PTXQ           at 0 range 16 .. 18;
      Reserved_19_19 at 0 range 19 .. 19;
      STXSTSF        at 0 range 20 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   --  Queue interrupt control status Register
   type MTLQICSR_Register is record
      --  TXUNFIS
      TXUNFIS        : Boolean := False;
      --  unspecified
      Reserved_1_7   : A0B.Types.SVD.UInt7 := 16#0#;
      --  TXUIE
      TXUIE          : Boolean := False;
      --  unspecified
      Reserved_9_15  : A0B.Types.SVD.UInt7 := 16#0#;
      --  RXOVFIS
      RXOVFIS        : Boolean := False;
      --  unspecified
      Reserved_17_23 : A0B.Types.SVD.UInt7 := 16#0#;
      --  RXOIE
      RXOIE          : Boolean := False;
      --  unspecified
      Reserved_25_31 : A0B.Types.SVD.UInt7 := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MTLQICSR_Register use record
      TXUNFIS        at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      TXUIE          at 0 range 8 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      RXOVFIS        at 0 range 16 .. 16;
      Reserved_17_23 at 0 range 17 .. 23;
      RXOIE          at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   subtype MTLRxQOMR_RTC_Field is A0B.Types.SVD.UInt2;
   subtype MTLRxQOMR_RFA_Field is A0B.Types.SVD.UInt3;
   subtype MTLRxQOMR_RFD_Field is A0B.Types.SVD.UInt3;
   subtype MTLRxQOMR_RQS_Field is A0B.Types.SVD.UInt3;

   --  Rx queue operating mode register
   type MTLRxQOMR_Register is record
      --  RTC
      RTC            : MTLRxQOMR_RTC_Field := 16#0#;
      --  unspecified
      Reserved_2_2   : A0B.Types.SVD.Bit := 16#0#;
      --  FUP
      FUP            : Boolean := False;
      --  FEP
      FEP            : Boolean := False;
      --  RSF
      RSF            : Boolean := False;
      --  DIS_TCP_EF
      DIS_TCP_EF     : Boolean := False;
      --  EHFC
      EHFC           : Boolean := False;
      --  RFA
      RFA            : MTLRxQOMR_RFA_Field := 16#0#;
      --  unspecified
      Reserved_11_13 : A0B.Types.SVD.UInt3 := 16#0#;
      --  RFD
      RFD            : MTLRxQOMR_RFD_Field := 16#0#;
      --  unspecified
      Reserved_17_19 : A0B.Types.SVD.UInt3 := 16#0#;
      --  Read-only. RQS
      RQS            : MTLRxQOMR_RQS_Field := 16#7#;
      --  unspecified
      Reserved_23_31 : A0B.Types.SVD.UInt9 := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MTLRxQOMR_Register use record
      RTC            at 0 range 0 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      FUP            at 0 range 3 .. 3;
      FEP            at 0 range 4 .. 4;
      RSF            at 0 range 5 .. 5;
      DIS_TCP_EF     at 0 range 6 .. 6;
      EHFC           at 0 range 7 .. 7;
      RFA            at 0 range 8 .. 10;
      Reserved_11_13 at 0 range 11 .. 13;
      RFD            at 0 range 14 .. 16;
      Reserved_17_19 at 0 range 17 .. 19;
      RQS            at 0 range 20 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   subtype MTLRxQMPOCR_OVFPKTCNT_Field is A0B.Types.SVD.UInt11;
   subtype MTLRxQMPOCR_MISPKTCNT_Field is A0B.Types.SVD.UInt11;

   --  Rx queue missed packet and overflow counter register
   type MTLRxQMPOCR_Register is record
      --  Read-only. OVFPKTCNT
      OVFPKTCNT      : MTLRxQMPOCR_OVFPKTCNT_Field;
      --  Read-only. OVFCNTOVF
      OVFCNTOVF      : Boolean;
      --  unspecified
      Reserved_12_15 : A0B.Types.SVD.UInt4;
      --  Read-only. MISPKTCNT
      MISPKTCNT      : MTLRxQMPOCR_MISPKTCNT_Field;
      --  Read-only. MISCNTOVF
      MISCNTOVF      : Boolean;
      --  unspecified
      Reserved_28_31 : A0B.Types.SVD.UInt4;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MTLRxQMPOCR_Register use record
      OVFPKTCNT      at 0 range 0 .. 10;
      OVFCNTOVF      at 0 range 11 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      MISPKTCNT      at 0 range 16 .. 26;
      MISCNTOVF      at 0 range 27 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype MTLRxQDR_RRCSTS_Field is A0B.Types.SVD.UInt2;
   subtype MTLRxQDR_RXQSTS_Field is A0B.Types.SVD.UInt2;
   subtype MTLRxQDR_PRXQ_Field is A0B.Types.SVD.UInt14;

   --  Rx queue debug register
   type MTLRxQDR_Register is record
      --  Read-only. RWCSTS
      RWCSTS         : Boolean;
      --  Read-only. RRCSTS
      RRCSTS         : MTLRxQDR_RRCSTS_Field;
      --  unspecified
      Reserved_3_3   : A0B.Types.SVD.Bit;
      --  Read-only. RXQSTS
      RXQSTS         : MTLRxQDR_RXQSTS_Field;
      --  unspecified
      Reserved_6_15  : A0B.Types.SVD.UInt10;
      --  Read-only. PRXQ
      PRXQ           : MTLRxQDR_PRXQ_Field;
      --  unspecified
      Reserved_30_31 : A0B.Types.SVD.UInt2;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for MTLRxQDR_Register use record
      RWCSTS         at 0 range 0 .. 0;
      RRCSTS         at 0 range 1 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      RXQSTS         at 0 range 4 .. 5;
      Reserved_6_15  at 0 range 6 .. 15;
      PRXQ           at 0 range 16 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype DMAMR_PR_Field is A0B.Types.SVD.UInt3;

   --  DMA mode register
   type DMAMR_Register is record
      --  Software Reset
      SWR            : Boolean := False;
      --  Read-only. DMA Tx or Rx Arbitration Scheme
      DA             : Boolean := False;
      --  unspecified
      Reserved_2_10  : A0B.Types.SVD.UInt9 := 16#0#;
      --  Read-only. Transmit priority
      TXPR           : Boolean := False;
      --  Read-only. Priority ratio
      PR             : DMAMR_PR_Field := 16#0#;
      --  unspecified
      Reserved_15_15 : A0B.Types.SVD.Bit := 16#0#;
      --  Interrupt Mode
      INTM           : Boolean := False;
      --  unspecified
      Reserved_17_31 : A0B.Types.SVD.UInt15 := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for DMAMR_Register use record
      SWR            at 0 range 0 .. 0;
      DA             at 0 range 1 .. 1;
      Reserved_2_10  at 0 range 2 .. 10;
      TXPR           at 0 range 11 .. 11;
      PR             at 0 range 12 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      INTM           at 0 range 16 .. 16;
      Reserved_17_31 at 0 range 17 .. 31;
   end record;

   --  System bus mode register
   type DMASBMR_Register is record
      --  Fixed Burst Length
      FB             : Boolean := False;
      --  unspecified
      Reserved_1_11  : A0B.Types.SVD.UInt11 := 16#0#;
      --  Address-Aligned Beats
      AAL            : Boolean := False;
      --  unspecified
      Reserved_13_13 : A0B.Types.SVD.Bit := 16#0#;
      --  Read-only. Mixed Burst
      MB             : Boolean := False;
      --  Read-only. Rebuild INCRx Burst
      RB             : Boolean := False;
      --  unspecified
      Reserved_16_31 : A0B.Types.SVD.UInt16 := 16#101#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for DMASBMR_Register use record
      FB             at 0 range 0 .. 0;
      Reserved_1_11  at 0 range 1 .. 11;
      AAL            at 0 range 12 .. 12;
      Reserved_13_13 at 0 range 13 .. 13;
      MB             at 0 range 14 .. 14;
      RB             at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  Interrupt status register
   type DMAISR_Register is record
      --  Read-only. DMA Channel Interrupt Status
      DC0IS          : Boolean;
      --  unspecified
      Reserved_1_15  : A0B.Types.SVD.UInt15;
      --  Read-only. MTL Interrupt Status
      MTLIS          : Boolean;
      --  Read-only. MAC Interrupt Status
      MACIS          : Boolean;
      --  unspecified
      Reserved_18_31 : A0B.Types.SVD.UInt14;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for DMAISR_Register use record
      DC0IS          at 0 range 0 .. 0;
      Reserved_1_15  at 0 range 1 .. 15;
      MTLIS          at 0 range 16 .. 16;
      MACIS          at 0 range 17 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   subtype DMADSR_RPS0_Field is A0B.Types.SVD.UInt4;
   subtype DMADSR_TPS0_Field is A0B.Types.SVD.UInt4;

   --  Debug status register
   type DMADSR_Register is record
      --  Read-only. AHB Master Write Channel
      AXWHSTS        : Boolean;
      --  unspecified
      Reserved_1_7   : A0B.Types.SVD.UInt7;
      --  Read-only. DMA Channel Receive Process State
      RPS0           : DMADSR_RPS0_Field;
      --  Read-only. DMA Channel Transmit Process State
      TPS0           : DMADSR_TPS0_Field;
      --  unspecified
      Reserved_16_31 : A0B.Types.SVD.UInt16;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for DMADSR_Register use record
      AXWHSTS        at 0 range 0 .. 0;
      Reserved_1_7   at 0 range 1 .. 7;
      RPS0           at 0 range 8 .. 11;
      TPS0           at 0 range 12 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DMACCR_MSS_Field is A0B.Types.SVD.UInt14;
   subtype DMACCR_DSL_Field is A0B.Types.SVD.UInt3;

   --  Channel control register
   type DMACCR_Register is record
      --  Maximum Segment Size
      MSS            : DMACCR_MSS_Field := 16#0#;
      --  unspecified
      Reserved_14_15 : A0B.Types.SVD.UInt2 := 16#0#;
      --  8xPBL mode
      PBLX8          : Boolean := False;
      --  unspecified
      Reserved_17_17 : A0B.Types.SVD.Bit := 16#0#;
      --  Descriptor Skip Length
      DSL            : DMACCR_DSL_Field := 16#0#;
      --  unspecified
      Reserved_21_31 : A0B.Types.SVD.UInt11 := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for DMACCR_Register use record
      MSS            at 0 range 0 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      PBLX8          at 0 range 16 .. 16;
      Reserved_17_17 at 0 range 17 .. 17;
      DSL            at 0 range 18 .. 20;
      Reserved_21_31 at 0 range 21 .. 31;
   end record;

   subtype DMACTxCR_TXPBL_Field is A0B.Types.SVD.UInt6;

   --  Channel transmit control register
   type DMACTxCR_Register is record
      --  Start or Stop Transmission Command
      ST             : Boolean := False;
      --  unspecified
      Reserved_1_3   : A0B.Types.SVD.UInt3 := 16#0#;
      --  Operate on Second Packet
      OSF            : Boolean := False;
      --  unspecified
      Reserved_5_11  : A0B.Types.SVD.UInt7 := 16#0#;
      --  TCP Segmentation Enabled
      TSE            : Boolean := False;
      --  unspecified
      Reserved_13_15 : A0B.Types.SVD.UInt3 := 16#0#;
      --  Transmit Programmable Burst Length
      TXPBL          : DMACTxCR_TXPBL_Field := 16#0#;
      --  unspecified
      Reserved_22_31 : A0B.Types.SVD.UInt10 := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for DMACTxCR_Register use record
      ST             at 0 range 0 .. 0;
      Reserved_1_3   at 0 range 1 .. 3;
      OSF            at 0 range 4 .. 4;
      Reserved_5_11  at 0 range 5 .. 11;
      TSE            at 0 range 12 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      TXPBL          at 0 range 16 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   subtype DMACRxCR_RBSZ_Field is A0B.Types.SVD.UInt14;
   subtype DMACRxCR_RXPBL_Field is A0B.Types.SVD.UInt6;

   --  Channel receive control register
   type DMACRxCR_Register is record
      --  Start or Stop Receive Command
      SR             : Boolean := False;
      --  Receive Buffer size
      RBSZ           : DMACRxCR_RBSZ_Field := 16#0#;
      --  unspecified
      Reserved_15_15 : A0B.Types.SVD.Bit := 16#0#;
      --  RXPBL
      RXPBL          : DMACRxCR_RXPBL_Field := 16#0#;
      --  unspecified
      Reserved_22_30 : A0B.Types.SVD.UInt9 := 16#0#;
      --  DMA Rx Channel Packet Flush
      RPF            : Boolean := False;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for DMACRxCR_Register use record
      SR             at 0 range 0 .. 0;
      RBSZ           at 0 range 1 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      RXPBL          at 0 range 16 .. 21;
      Reserved_22_30 at 0 range 22 .. 30;
      RPF            at 0 range 31 .. 31;
   end record;

   subtype DMACTxDLAR_TDESLA_Field is A0B.Types.SVD.UInt30;

   --  Channel Tx descriptor list address register
   type DMACTxDLAR_Register is record
      --  unspecified
      Reserved_0_1 : A0B.Types.SVD.UInt2 := 16#0#;
      --  Start of Transmit List
      TDESLA       : DMACTxDLAR_TDESLA_Field := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for DMACTxDLAR_Register use record
      Reserved_0_1 at 0 range 0 .. 1;
      TDESLA       at 0 range 2 .. 31;
   end record;

   subtype DMACRxDLAR_RDESLA_Field is A0B.Types.SVD.UInt30;

   --  Channel Rx descriptor list address register
   type DMACRxDLAR_Register is record
      --  unspecified
      Reserved_0_1 : A0B.Types.SVD.UInt2 := 16#0#;
      --  Start of Receive List
      RDESLA       : DMACRxDLAR_RDESLA_Field := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for DMACRxDLAR_Register use record
      Reserved_0_1 at 0 range 0 .. 1;
      RDESLA       at 0 range 2 .. 31;
   end record;

   subtype DMACTxDTPR_TDT_Field is A0B.Types.SVD.UInt30;

   --  Channel Tx descriptor tail pointer register
   type DMACTxDTPR_Register is record
      --  unspecified
      Reserved_0_1 : A0B.Types.SVD.UInt2 := 16#0#;
      --  Transmit Descriptor Tail Pointer
      TDT          : DMACTxDTPR_TDT_Field := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for DMACTxDTPR_Register use record
      Reserved_0_1 at 0 range 0 .. 1;
      TDT          at 0 range 2 .. 31;
   end record;

   subtype DMACRxDTPR_RDT_Field is A0B.Types.SVD.UInt30;

   --  Channel Rx descriptor tail pointer register
   type DMACRxDTPR_Register is record
      --  unspecified
      Reserved_0_1 : A0B.Types.SVD.UInt2 := 16#0#;
      --  Receive Descriptor Tail Pointer
      RDT          : DMACRxDTPR_RDT_Field := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for DMACRxDTPR_Register use record
      Reserved_0_1 at 0 range 0 .. 1;
      RDT          at 0 range 2 .. 31;
   end record;

   subtype DMACTxRLR_TDRL_Field is A0B.Types.SVD.UInt10;

   --  Channel Tx descriptor ring length register
   type DMACTxRLR_Register is record
      --  Transmit Descriptor Ring Length
      TDRL           : DMACTxRLR_TDRL_Field := 16#0#;
      --  unspecified
      Reserved_10_31 : A0B.Types.SVD.UInt22 := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for DMACTxRLR_Register use record
      TDRL           at 0 range 0 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   subtype DMACRxRLR_RDRL_Field is A0B.Types.SVD.UInt10;

   --  Channel Rx descriptor ring length register
   type DMACRxRLR_Register is record
      --  Receive Descriptor Ring Length
      RDRL           : DMACRxRLR_RDRL_Field := 16#0#;
      --  unspecified
      Reserved_10_31 : A0B.Types.SVD.UInt22 := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for DMACRxRLR_Register use record
      RDRL           at 0 range 0 .. 9;
      Reserved_10_31 at 0 range 10 .. 31;
   end record;

   --  Channel interrupt enable register
   type DMACIER_Register is record
      --  Transmit Interrupt Enable
      TIE            : Boolean := False;
      --  Transmit Stopped Enable
      TXSE           : Boolean := False;
      --  Transmit Buffer Unavailable Enable
      TBUE           : Boolean := False;
      --  unspecified
      Reserved_3_5   : A0B.Types.SVD.UInt3 := 16#0#;
      --  Receive Interrupt Enable
      RIE            : Boolean := False;
      --  Receive Buffer Unavailable Enable
      RBUE           : Boolean := False;
      --  Receive Stopped Enable
      RSE            : Boolean := False;
      --  Receive Watchdog Timeout Enable
      RWTE           : Boolean := False;
      --  Early Transmit Interrupt Enable
      ETIE           : Boolean := False;
      --  Early Receive Interrupt Enable
      ERIE           : Boolean := False;
      --  Fatal Bus Error Enable
      FBEE           : Boolean := False;
      --  Context Descriptor Error Enable
      CDEE           : Boolean := False;
      --  Abnormal Interrupt Summary Enable
      AIE            : Boolean := False;
      --  Normal Interrupt Summary Enable
      NIE            : Boolean := False;
      --  unspecified
      Reserved_16_31 : A0B.Types.SVD.UInt16 := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for DMACIER_Register use record
      TIE            at 0 range 0 .. 0;
      TXSE           at 0 range 1 .. 1;
      TBUE           at 0 range 2 .. 2;
      Reserved_3_5   at 0 range 3 .. 5;
      RIE            at 0 range 6 .. 6;
      RBUE           at 0 range 7 .. 7;
      RSE            at 0 range 8 .. 8;
      RWTE           at 0 range 9 .. 9;
      ETIE           at 0 range 10 .. 10;
      ERIE           at 0 range 11 .. 11;
      FBEE           at 0 range 12 .. 12;
      CDEE           at 0 range 13 .. 13;
      AIE            at 0 range 14 .. 14;
      NIE            at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DMACRxIWTR_RWT_Field is A0B.Types.SVD.Byte;

   --  Channel Rx interrupt watchdog timer register
   type DMACRxIWTR_Register is record
      --  Receive Interrupt Watchdog Timer Count
      RWT           : DMACRxIWTR_RWT_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : A0B.Types.SVD.UInt24 := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for DMACRxIWTR_Register use record
      RWT           at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype DMACSR_TEB_Field is A0B.Types.SVD.UInt3;
   subtype DMACSR_REB_Field is A0B.Types.SVD.UInt3;

   --  Channel status register
   type DMACSR_Register is record
      --  Transmit Interrupt
      TI             : Boolean := False;
      --  Transmit Process Stopped
      TPS            : Boolean := False;
      --  Transmit Buffer Unavailable
      TBU            : Boolean := False;
      --  unspecified
      Reserved_3_5   : A0B.Types.SVD.UInt3 := 16#0#;
      --  Receive Interrupt
      RI             : Boolean := False;
      --  Receive Buffer Unavailable
      RBU            : Boolean := False;
      --  Receive Process Stopped
      RPS            : Boolean := False;
      --  Receive Watchdog Timeout
      RWT            : Boolean := False;
      --  Early Transmit Interrupt
      ET             : Boolean := False;
      --  Early Receive Interrupt
      ER             : Boolean := False;
      --  Fatal Bus Error
      FBE            : Boolean := False;
      --  Context Descriptor Error
      CDE            : Boolean := False;
      --  Abnormal Interrupt Summary
      AIS            : Boolean := False;
      --  Normal Interrupt Summary
      NIS            : Boolean := False;
      --  Read-only. Tx DMA Error Bits
      TEB            : DMACSR_TEB_Field := 16#0#;
      --  Read-only. Rx DMA Error Bits
      REB            : DMACSR_REB_Field := 16#0#;
      --  unspecified
      Reserved_22_31 : A0B.Types.SVD.UInt10 := 16#0#;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for DMACSR_Register use record
      TI             at 0 range 0 .. 0;
      TPS            at 0 range 1 .. 1;
      TBU            at 0 range 2 .. 2;
      Reserved_3_5   at 0 range 3 .. 5;
      RI             at 0 range 6 .. 6;
      RBU            at 0 range 7 .. 7;
      RPS            at 0 range 8 .. 8;
      RWT            at 0 range 9 .. 9;
      ET             at 0 range 10 .. 10;
      ER             at 0 range 11 .. 11;
      FBE            at 0 range 12 .. 12;
      CDE            at 0 range 13 .. 13;
      AIS            at 0 range 14 .. 14;
      NIS            at 0 range 15 .. 15;
      TEB            at 0 range 16 .. 18;
      REB            at 0 range 19 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   subtype DMACMFCR_MFC_Field is A0B.Types.SVD.UInt11;

   --  Channel missed frame count register
   type DMACMFCR_Register is record
      --  Read-only. Dropped Packet Counters
      MFC            : DMACMFCR_MFC_Field;
      --  unspecified
      Reserved_11_14 : A0B.Types.SVD.UInt4;
      --  Read-only. Overflow status of the MFC Counter
      MFCO           : Boolean;
      --  unspecified
      Reserved_16_31 : A0B.Types.SVD.UInt16;
   end record
     with Object_Size => 32, Bit_Order => System.Low_Order_First;

   for DMACMFCR_Register use record
      MFC            at 0 range 0 .. 10;
      Reserved_11_14 at 0 range 11 .. 14;
      MFCO           at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Ethernet: media access control (MAC)
   type Ethernet_MAC_Peripheral is record
      --  Operating mode configuration register
      MACCR                              : aliased MACCR_Register;
      pragma Volatile_Full_Access (MACCR);
      --  Extended operating mode configuration register
      MACECR                             : aliased MACECR_Register;
      pragma Volatile_Full_Access (MACECR);
      --  Packet filtering control register
      MACPFR                             : aliased MACPFR_Register;
      pragma Volatile_Full_Access (MACPFR);
      --  Watchdog timeout register
      MACWTR                             : aliased MACWTR_Register;
      pragma Volatile_Full_Access (MACWTR);
      --  Hash Table 0 register
      MACHT0R                            : aliased A0B.Types.SVD.UInt32;
      --  Hash Table 1 register
      MACHT1R                            : aliased A0B.Types.SVD.UInt32;
      --  VLAN tag register
      MACVTR                             : aliased MACVTR_Register;
      pragma Volatile_Full_Access (MACVTR);
      --  VLAN Hash table register
      MACVHTR                            : aliased MACVHTR_Register;
      pragma Volatile_Full_Access (MACVHTR);
      --  VLAN inclusion register
      MACVIR                             : aliased MACVIR_Register;
      pragma Volatile_Full_Access (MACVIR);
      --  Inner VLAN inclusion register
      MACIVIR                            : aliased MACIVIR_Register;
      pragma Volatile_Full_Access (MACIVIR);
      --  Tx Queue flow control register
      MACQTxFCR                          : aliased MACQTxFCR_Register;
      pragma Volatile_Full_Access (MACQTxFCR);
      --  Rx flow control register
      MACRxFCR                           : aliased MACRxFCR_Register;
      pragma Volatile_Full_Access (MACRxFCR);
      --  Interrupt status register
      MACISR                             : aliased MACISR_Register;
      pragma Volatile_Full_Access (MACISR);
      --  Interrupt enable register
      MACIER                             : aliased MACIER_Register;
      pragma Volatile_Full_Access (MACIER);
      --  Rx Tx status register
      MACRxTxSR                          : aliased MACRxTxSR_Register;
      pragma Volatile_Full_Access (MACRxTxSR);
      --  PMT control status register
      MACPCSR                            : aliased MACPCSR_Register;
      pragma Volatile_Full_Access (MACPCSR);
      --  Remove wakeup packet filter register
      MACRWKPFR                          : aliased A0B.Types.SVD.UInt32;
      --  LPI control status register
      MACLCSR                            : aliased MACLCSR_Register;
      pragma Volatile_Full_Access (MACLCSR);
      --  LPI timers control register
      MACLTCR                            : aliased MACLTCR_Register;
      pragma Volatile_Full_Access (MACLTCR);
      --  LPI entry timer register
      MACLETR                            : aliased MACLETR_Register;
      pragma Volatile_Full_Access (MACLETR);
      --  1-microsecond-tick counter register
      MAC1USTCR                          : aliased MAC1USTCR_Register;
      pragma Volatile_Full_Access (MAC1USTCR);
      --  Version register
      MACVR                              : aliased MACVR_Register;
      pragma Volatile_Full_Access (MACVR);
      --  Debug register
      MACDR                              : aliased MACDR_Register;
      pragma Volatile_Full_Access (MACDR);
      --  HW feature 1 register
      MACHWF1R                           : aliased MACHWF1R_Register;
      pragma Volatile_Full_Access (MACHWF1R);
      --  HW feature 2 register
      MACHWF2R                           : aliased MACHWF2R_Register;
      pragma Volatile_Full_Access (MACHWF2R);
      --  MDIO address register
      MACMDIOAR                          : aliased MACMDIOAR_Register;
      pragma Volatile_Full_Access (MACMDIOAR);
      --  MDIO data register
      MACMDIODR                          : aliased MACMDIODR_Register;
      pragma Volatile_Full_Access (MACMDIODR);
      --  Address 0 high register
      MACA0HR                            : aliased MACA0HR_Register;
      pragma Volatile_Full_Access (MACA0HR);
      --  Address 0 low register
      MACA0LR                            : aliased A0B.Types.SVD.UInt32;
      --  Address 1 high register
      MACA1HR                            : aliased MACA1HR_Register;
      pragma Volatile_Full_Access (MACA1HR);
      --  Address 1 low register
      MACA1LR                            : aliased A0B.Types.SVD.UInt32;
      --  Address 2 high register
      MACA2HR                            : aliased MACA2HR_Register;
      pragma Volatile_Full_Access (MACA2HR);
      --  Address 2 low register
      MACA2LR                            : aliased A0B.Types.SVD.UInt32;
      --  Address 3 high register
      MACA3HR                            : aliased MACA3HR_Register;
      pragma Volatile_Full_Access (MACA3HR);
      --  Address 3 low register
      MACA3LR                            : aliased A0B.Types.SVD.UInt32;
      --  MMC control register
      MMC_CONTROL                        : aliased MMC_CONTROL_Register;
      pragma Volatile_Full_Access (MMC_CONTROL);
      --  MMC Rx interrupt register
      MMC_RX_INTERRUPT                   : aliased MMC_RX_INTERRUPT_Register;
      pragma Volatile_Full_Access (MMC_RX_INTERRUPT);
      --  MMC Tx interrupt register
      MMC_TX_INTERRUPT                   : aliased MMC_TX_INTERRUPT_Register;
      pragma Volatile_Full_Access (MMC_TX_INTERRUPT);
      --  MMC Rx interrupt mask register
      MMC_RX_INTERRUPT_MASK              : aliased MMC_RX_INTERRUPT_MASK_Register;
      pragma Volatile_Full_Access (MMC_RX_INTERRUPT_MASK);
      --  MMC Tx interrupt mask register
      MMC_TX_INTERRUPT_MASK              : aliased MMC_TX_INTERRUPT_MASK_Register;
      pragma Volatile_Full_Access (MMC_TX_INTERRUPT_MASK);
      --  Tx single collision good packets register
      TX_SINGLE_COLLISION_GOOD_PACKETS   : aliased A0B.Types.SVD.UInt32;
      --  Tx multiple collision good packets register
      TX_MULTIPLE_COLLISION_GOOD_PACKETS : aliased A0B.Types.SVD.UInt32;
      --  Tx packet count good register
      TX_PACKET_COUNT_GOOD               : aliased A0B.Types.SVD.UInt32;
      --  Rx CRC error packets register
      RX_CRC_ERROR_PACKETS               : aliased A0B.Types.SVD.UInt32;
      --  Rx alignment error packets register
      RX_ALIGNMENT_ERROR_PACKETS         : aliased A0B.Types.SVD.UInt32;
      --  Rx unicast packets good register
      RX_UNICAST_PACKETS_GOOD            : aliased A0B.Types.SVD.UInt32;
      --  Tx LPI microsecond timer register
      TX_LPI_USEC_CNTR                   : aliased A0B.Types.SVD.UInt32;
      --  Tx LPI transition counter register
      TX_LPI_TRAN_CNTR                   : aliased A0B.Types.SVD.UInt32;
      --  Rx LPI microsecond counter register
      RX_LPI_USEC_CNTR                   : aliased A0B.Types.SVD.UInt32;
      --  Rx LPI transition counter register
      RX_LPI_TRAN_CNTR                   : aliased A0B.Types.SVD.UInt32;
      --  L3 and L4 control 0 register
      MACL3L4C0R                         : aliased MACL3L4C0R_Register;
      pragma Volatile_Full_Access (MACL3L4C0R);
      --  Layer4 address filter 0 register
      MACL4A0R                           : aliased MACL4A0R_Register;
      pragma Volatile_Full_Access (MACL4A0R);
      --  MACL3A00R
      MACL3A00R                          : aliased A0B.Types.SVD.UInt32;
      --  Layer3 address 1 filter 0 register
      MACL3A10R                          : aliased A0B.Types.SVD.UInt32;
      --  Layer3 Address 2 filter 0 register
      MACL3A20                           : aliased A0B.Types.SVD.UInt32;
      --  Layer3 Address 3 filter 0 register
      MACL3A30                           : aliased A0B.Types.SVD.UInt32;
      --  L3 and L4 control 1 register
      MACL3L4C1R                         : aliased MACL3L4C1R_Register;
      pragma Volatile_Full_Access (MACL3L4C1R);
      --  Layer 4 address filter 1 register
      MACL4A1R                           : aliased MACL4A1R_Register;
      pragma Volatile_Full_Access (MACL4A1R);
      --  Layer3 address 0 filter 1 Register
      MACL3A01R                          : aliased A0B.Types.SVD.UInt32;
      --  Layer3 address 1 filter 1 register
      MACL3A11R                          : aliased A0B.Types.SVD.UInt32;
      --  Layer3 address 2 filter 1 Register
      MACL3A21R                          : aliased A0B.Types.SVD.UInt32;
      --  Layer3 address 3 filter 1 register
      MACL3A31R                          : aliased A0B.Types.SVD.UInt32;
      --  ARP address register
      MACARPAR                           : aliased A0B.Types.SVD.UInt32;
      --  Timestamp control Register
      MACTSCR                            : aliased MACTSCR_Register;
      pragma Volatile_Full_Access (MACTSCR);
      --  Sub-second increment register
      MACSSIR                            : aliased MACSSIR_Register;
      pragma Volatile_Full_Access (MACSSIR);
      --  System time seconds register
      MACSTSR                            : aliased A0B.Types.SVD.UInt32;
      --  System time nanoseconds register
      MACSTNR                            : aliased MACSTNR_Register;
      pragma Volatile_Full_Access (MACSTNR);
      --  System time seconds update register
      MACSTSUR                           : aliased A0B.Types.SVD.UInt32;
      --  System time nanoseconds update register
      MACSTNUR                           : aliased MACSTNUR_Register;
      pragma Volatile_Full_Access (MACSTNUR);
      --  Timestamp addend register
      MACTSAR                            : aliased A0B.Types.SVD.UInt32;
      --  Timestamp status register
      MACTSSR                            : aliased MACTSSR_Register;
      pragma Volatile_Full_Access (MACTSSR);
      --  Tx timestamp status nanoseconds register
      MACTxTSSNR                         : aliased MACTxTSSNR_Register;
      pragma Volatile_Full_Access (MACTxTSSNR);
      --  Tx timestamp status seconds register
      MACTxTSSSR                         : aliased A0B.Types.SVD.UInt32;
      --  Auxiliary control register
      MACACR                             : aliased MACACR_Register;
      pragma Volatile_Full_Access (MACACR);
      --  Auxiliary timestamp nanoseconds register
      MACATSNR                           : aliased MACATSNR_Register;
      pragma Volatile_Full_Access (MACATSNR);
      --  Auxiliary timestamp seconds register
      MACATSSR                           : aliased A0B.Types.SVD.UInt32;
      --  Timestamp Ingress asymmetric correction register
      MACTSIACR                          : aliased A0B.Types.SVD.UInt32;
      --  Timestamp Egress asymmetric correction register
      MACTSEACR                          : aliased A0B.Types.SVD.UInt32;
      --  Timestamp Ingress correction nanosecond register
      MACTSICNR                          : aliased A0B.Types.SVD.UInt32;
      --  Timestamp Egress correction nanosecond register
      MACTSECNR                          : aliased A0B.Types.SVD.UInt32;
      --  PPS control register
      MACPPSCR                           : aliased MACPPSCR_Register;
      pragma Volatile_Full_Access (MACPPSCR);
      --  PPS target time seconds register
      MACPPSTTSR                         : aliased MACPPSTTSR_Register;
      pragma Volatile_Full_Access (MACPPSTTSR);
      --  PPS target time nanoseconds register
      MACPPSTTNR                         : aliased MACPPSTTNR_Register;
      pragma Volatile_Full_Access (MACPPSTTNR);
      --  PPS interval register
      MACPPSIR                           : aliased A0B.Types.SVD.UInt32;
      --  PPS width register
      MACPPSWR                           : aliased A0B.Types.SVD.UInt32;
      --  PTP Offload control register
      MACPOCR                            : aliased MACPOCR_Register;
      pragma Volatile_Full_Access (MACPOCR);
      --  PTP Source Port Identity 0 Register
      MACSPI0R                           : aliased A0B.Types.SVD.UInt32;
      --  PTP Source port identity 1 register
      MACSPI1R                           : aliased A0B.Types.SVD.UInt32;
      --  PTP Source port identity 2 register
      MACSPI2R                           : aliased MACSPI2R_Register;
      pragma Volatile_Full_Access (MACSPI2R);
      --  Log message interval register
      MACLMIR                            : aliased MACLMIR_Register;
      pragma Volatile_Full_Access (MACLMIR);
      --  Operating mode Register
      MTLOMR                             : aliased MTLOMR_Register;
      pragma Volatile_Full_Access (MTLOMR);
      --  Interrupt status Register
      MTLISR                             : aliased MTLISR_Register;
      pragma Volatile_Full_Access (MTLISR);
      --  Tx queue operating mode Register
      MTLTxQOMR                          : aliased MTLTxQOMR_Register;
      pragma Volatile_Full_Access (MTLTxQOMR);
      --  Tx queue underflow register
      MTLTxQUR                           : aliased MTLTxQUR_Register;
      pragma Volatile_Full_Access (MTLTxQUR);
      --  Tx queue debug Register
      MTLTxQDR                           : aliased MTLTxQDR_Register;
      pragma Volatile_Full_Access (MTLTxQDR);
      --  Queue interrupt control status Register
      MTLQICSR                           : aliased MTLQICSR_Register;
      pragma Volatile_Full_Access (MTLQICSR);
      --  Rx queue operating mode register
      MTLRxQOMR                          : aliased MTLRxQOMR_Register;
      pragma Volatile_Full_Access (MTLRxQOMR);
      --  Rx queue missed packet and overflow counter register
      MTLRxQMPOCR                        : aliased MTLRxQMPOCR_Register;
      pragma Volatile_Full_Access (MTLRxQMPOCR);
      --  Rx queue debug register
      MTLRxQDR                           : aliased MTLRxQDR_Register;
      pragma Volatile_Full_Access (MTLRxQDR);
      --  DMA mode register
      DMAMR                              : aliased DMAMR_Register;
      pragma Volatile_Full_Access (DMAMR);
      --  System bus mode register
      DMASBMR                            : aliased DMASBMR_Register;
      pragma Volatile_Full_Access (DMASBMR);
      --  Interrupt status register
      DMAISR                             : aliased DMAISR_Register;
      pragma Volatile_Full_Access (DMAISR);
      --  Debug status register
      DMADSR                             : aliased DMADSR_Register;
      pragma Volatile_Full_Access (DMADSR);
      --  Channel control register
      DMACCR                             : aliased DMACCR_Register;
      pragma Volatile_Full_Access (DMACCR);
      --  Channel transmit control register
      DMACTxCR                           : aliased DMACTxCR_Register;
      pragma Volatile_Full_Access (DMACTxCR);
      --  Channel receive control register
      DMACRxCR                           : aliased DMACRxCR_Register;
      pragma Volatile_Full_Access (DMACRxCR);
      --  Channel Tx descriptor list address register
      DMACTxDLAR                         : aliased DMACTxDLAR_Register;
      pragma Volatile_Full_Access (DMACTxDLAR);
      --  Channel Rx descriptor list address register
      DMACRxDLAR                         : aliased DMACRxDLAR_Register;
      pragma Volatile_Full_Access (DMACRxDLAR);
      --  Channel Tx descriptor tail pointer register
      DMACTxDTPR                         : aliased DMACTxDTPR_Register;
      pragma Volatile_Full_Access (DMACTxDTPR);
      --  Channel Rx descriptor tail pointer register
      DMACRxDTPR                         : aliased DMACRxDTPR_Register;
      pragma Volatile_Full_Access (DMACRxDTPR);
      --  Channel Tx descriptor ring length register
      DMACTxRLR                          : aliased DMACTxRLR_Register;
      pragma Volatile_Full_Access (DMACTxRLR);
      --  Channel Rx descriptor ring length register
      DMACRxRLR                          : aliased DMACRxRLR_Register;
      pragma Volatile_Full_Access (DMACRxRLR);
      --  Channel interrupt enable register
      DMACIER                            : aliased DMACIER_Register;
      pragma Volatile_Full_Access (DMACIER);
      --  Channel Rx interrupt watchdog timer register
      DMACRxIWTR                         : aliased DMACRxIWTR_Register;
      pragma Volatile_Full_Access (DMACRxIWTR);
      --  Channel current application transmit descriptor register
      DMACCATxDR                         : aliased A0B.Types.SVD.UInt32;
      --  Channel current application receive descriptor register
      DMACCARxDR                         : aliased A0B.Types.SVD.UInt32;
      --  Channel current application transmit buffer register
      DMACCATxBR                         : aliased A0B.Types.SVD.UInt32;
      --  Channel current application receive buffer register
      DMACCARxBR                         : aliased A0B.Types.SVD.UInt32;
      --  Channel status register
      DMACSR                             : aliased DMACSR_Register;
      pragma Volatile_Full_Access (DMACSR);
      --  Channel missed frame count register
      DMACMFCR                           : aliased DMACMFCR_Register;
      pragma Volatile_Full_Access (DMACMFCR);
   end record
     with Volatile;

   for Ethernet_MAC_Peripheral use record
      MACCR                              at 16#0# range 0 .. 31;
      MACECR                             at 16#4# range 0 .. 31;
      MACPFR                             at 16#8# range 0 .. 31;
      MACWTR                             at 16#C# range 0 .. 31;
      MACHT0R                            at 16#10# range 0 .. 31;
      MACHT1R                            at 16#14# range 0 .. 31;
      MACVTR                             at 16#50# range 0 .. 31;
      MACVHTR                            at 16#58# range 0 .. 31;
      MACVIR                             at 16#60# range 0 .. 31;
      MACIVIR                            at 16#64# range 0 .. 31;
      MACQTxFCR                          at 16#70# range 0 .. 31;
      MACRxFCR                           at 16#90# range 0 .. 31;
      MACISR                             at 16#B0# range 0 .. 31;
      MACIER                             at 16#B4# range 0 .. 31;
      MACRxTxSR                          at 16#B8# range 0 .. 31;
      MACPCSR                            at 16#C0# range 0 .. 31;
      MACRWKPFR                          at 16#C4# range 0 .. 31;
      MACLCSR                            at 16#D0# range 0 .. 31;
      MACLTCR                            at 16#D4# range 0 .. 31;
      MACLETR                            at 16#D8# range 0 .. 31;
      MAC1USTCR                          at 16#DC# range 0 .. 31;
      MACVR                              at 16#110# range 0 .. 31;
      MACDR                              at 16#114# range 0 .. 31;
      MACHWF1R                           at 16#120# range 0 .. 31;
      MACHWF2R                           at 16#124# range 0 .. 31;
      MACMDIOAR                          at 16#200# range 0 .. 31;
      MACMDIODR                          at 16#204# range 0 .. 31;
      MACA0HR                            at 16#300# range 0 .. 31;
      MACA0LR                            at 16#304# range 0 .. 31;
      MACA1HR                            at 16#308# range 0 .. 31;
      MACA1LR                            at 16#30C# range 0 .. 31;
      MACA2HR                            at 16#310# range 0 .. 31;
      MACA2LR                            at 16#314# range 0 .. 31;
      MACA3HR                            at 16#318# range 0 .. 31;
      MACA3LR                            at 16#31C# range 0 .. 31;
      MMC_CONTROL                        at 16#700# range 0 .. 31;
      MMC_RX_INTERRUPT                   at 16#704# range 0 .. 31;
      MMC_TX_INTERRUPT                   at 16#708# range 0 .. 31;
      MMC_RX_INTERRUPT_MASK              at 16#70C# range 0 .. 31;
      MMC_TX_INTERRUPT_MASK              at 16#710# range 0 .. 31;
      TX_SINGLE_COLLISION_GOOD_PACKETS   at 16#74C# range 0 .. 31;
      TX_MULTIPLE_COLLISION_GOOD_PACKETS at 16#750# range 0 .. 31;
      TX_PACKET_COUNT_GOOD               at 16#768# range 0 .. 31;
      RX_CRC_ERROR_PACKETS               at 16#794# range 0 .. 31;
      RX_ALIGNMENT_ERROR_PACKETS         at 16#798# range 0 .. 31;
      RX_UNICAST_PACKETS_GOOD            at 16#7C4# range 0 .. 31;
      TX_LPI_USEC_CNTR                   at 16#7EC# range 0 .. 31;
      TX_LPI_TRAN_CNTR                   at 16#7F0# range 0 .. 31;
      RX_LPI_USEC_CNTR                   at 16#7F4# range 0 .. 31;
      RX_LPI_TRAN_CNTR                   at 16#7F8# range 0 .. 31;
      MACL3L4C0R                         at 16#900# range 0 .. 31;
      MACL4A0R                           at 16#904# range 0 .. 31;
      MACL3A00R                          at 16#910# range 0 .. 31;
      MACL3A10R                          at 16#914# range 0 .. 31;
      MACL3A20                           at 16#918# range 0 .. 31;
      MACL3A30                           at 16#91C# range 0 .. 31;
      MACL3L4C1R                         at 16#930# range 0 .. 31;
      MACL4A1R                           at 16#934# range 0 .. 31;
      MACL3A01R                          at 16#940# range 0 .. 31;
      MACL3A11R                          at 16#944# range 0 .. 31;
      MACL3A21R                          at 16#948# range 0 .. 31;
      MACL3A31R                          at 16#94C# range 0 .. 31;
      MACARPAR                           at 16#AE0# range 0 .. 31;
      MACTSCR                            at 16#B00# range 0 .. 31;
      MACSSIR                            at 16#B04# range 0 .. 31;
      MACSTSR                            at 16#B08# range 0 .. 31;
      MACSTNR                            at 16#B0C# range 0 .. 31;
      MACSTSUR                           at 16#B10# range 0 .. 31;
      MACSTNUR                           at 16#B14# range 0 .. 31;
      MACTSAR                            at 16#B18# range 0 .. 31;
      MACTSSR                            at 16#B20# range 0 .. 31;
      MACTxTSSNR                         at 16#B30# range 0 .. 31;
      MACTxTSSSR                         at 16#B34# range 0 .. 31;
      MACACR                             at 16#B40# range 0 .. 31;
      MACATSNR                           at 16#B48# range 0 .. 31;
      MACATSSR                           at 16#B4C# range 0 .. 31;
      MACTSIACR                          at 16#B50# range 0 .. 31;
      MACTSEACR                          at 16#B54# range 0 .. 31;
      MACTSICNR                          at 16#B58# range 0 .. 31;
      MACTSECNR                          at 16#B5C# range 0 .. 31;
      MACPPSCR                           at 16#B70# range 0 .. 31;
      MACPPSTTSR                         at 16#B80# range 0 .. 31;
      MACPPSTTNR                         at 16#B84# range 0 .. 31;
      MACPPSIR                           at 16#B88# range 0 .. 31;
      MACPPSWR                           at 16#B8C# range 0 .. 31;
      MACPOCR                            at 16#BC0# range 0 .. 31;
      MACSPI0R                           at 16#BC4# range 0 .. 31;
      MACSPI1R                           at 16#BC8# range 0 .. 31;
      MACSPI2R                           at 16#BCC# range 0 .. 31;
      MACLMIR                            at 16#BD0# range 0 .. 31;
      MTLOMR                             at 16#C00# range 0 .. 31;
      MTLISR                             at 16#C20# range 0 .. 31;
      MTLTxQOMR                          at 16#D00# range 0 .. 31;
      MTLTxQUR                           at 16#D04# range 0 .. 31;
      MTLTxQDR                           at 16#D08# range 0 .. 31;
      MTLQICSR                           at 16#D2C# range 0 .. 31;
      MTLRxQOMR                          at 16#D30# range 0 .. 31;
      MTLRxQMPOCR                        at 16#D34# range 0 .. 31;
      MTLRxQDR                           at 16#D38# range 0 .. 31;
      DMAMR                              at 16#1000# range 0 .. 31;
      DMASBMR                            at 16#1004# range 0 .. 31;
      DMAISR                             at 16#1008# range 0 .. 31;
      DMADSR                             at 16#100C# range 0 .. 31;
      DMACCR                             at 16#1100# range 0 .. 31;
      DMACTxCR                           at 16#1104# range 0 .. 31;
      DMACRxCR                           at 16#1108# range 0 .. 31;
      DMACTxDLAR                         at 16#1114# range 0 .. 31;
      DMACRxDLAR                         at 16#111C# range 0 .. 31;
      DMACTxDTPR                         at 16#1120# range 0 .. 31;
      DMACRxDTPR                         at 16#1128# range 0 .. 31;
      DMACTxRLR                          at 16#112C# range 0 .. 31;
      DMACRxRLR                          at 16#1130# range 0 .. 31;
      DMACIER                            at 16#1134# range 0 .. 31;
      DMACRxIWTR                         at 16#1138# range 0 .. 31;
      DMACCATxDR                         at 16#1144# range 0 .. 31;
      DMACCARxDR                         at 16#114C# range 0 .. 31;
      DMACCATxBR                         at 16#1154# range 0 .. 31;
      DMACCARxBR                         at 16#115C# range 0 .. 31;
      DMACSR                             at 16#1160# range 0 .. 31;
      DMACMFCR                           at 16#116C# range 0 .. 31;
   end record;

   --  Ethernet: media access control (MAC)
   Ethernet_MAC_Periph : aliased Ethernet_MAC_Peripheral
     with Import, Address => Ethernet_MAC_Base;

end A0B.STM32H753.SVD.Ethernet;
