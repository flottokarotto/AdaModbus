pragma Style_Checks (Off);

--  This spec has been automatically generated from STM32H753.svd


with System;

package STM32H753.FDCAN is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   subtype CREL_DAY_Field is STM32H753.Byte;
   subtype CREL_MON_Field is STM32H753.Byte;
   subtype CREL_YEAR_Field is STM32H753.UInt4;
   subtype CREL_SUBSTEP_Field is STM32H753.UInt4;
   subtype CREL_STEP_Field is STM32H753.UInt4;
   subtype CREL_REL_Field is STM32H753.UInt4;

   --  Clock Calibration Unit Core Release Register
   type CREL_Register is record
      --  Time Stamp Day
      DAY     : CREL_DAY_Field := 16#0#;
      --  Time Stamp Month
      MON     : CREL_MON_Field := 16#0#;
      --  Time Stamp Year
      YEAR    : CREL_YEAR_Field := 16#0#;
      --  Sub-step of Core Release
      SUBSTEP : CREL_SUBSTEP_Field := 16#0#;
      --  Step of Core Release
      STEP    : CREL_STEP_Field := 16#0#;
      --  Core Release
      REL     : CREL_REL_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CREL_Register use record
      DAY     at 0 range 0 .. 7;
      MON     at 0 range 8 .. 15;
      YEAR    at 0 range 16 .. 19;
      SUBSTEP at 0 range 20 .. 23;
      STEP    at 0 range 24 .. 27;
      REL     at 0 range 28 .. 31;
   end record;

   subtype CCFG_TQBT_Field is STM32H753.UInt5;
   subtype CCFG_OCPM_Field is STM32H753.Byte;
   subtype CCFG_CDIV_Field is STM32H753.UInt4;

   --  Calibration Configuration Register
   type CCFG_Register is record
      --  Time Quanta per Bit Time
      TQBT           : CCFG_TQBT_Field := 16#0#;
      --  unspecified
      Reserved_5_5   : STM32H753.Bit := 16#0#;
      --  Bypass Clock Calibration
      BCC            : Boolean := False;
      --  Calibration Field Length
      CFL            : Boolean := False;
      --  Oscillator Clock Periods Minimum
      OCPM           : CCFG_OCPM_Field := 16#0#;
      --  Clock Divider
      CDIV           : CCFG_CDIV_Field := 16#0#;
      --  unspecified
      Reserved_20_30 : STM32H753.UInt11 := 16#0#;
      --  Software Reset
      SWR            : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCFG_Register use record
      TQBT           at 0 range 0 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      BCC            at 0 range 6 .. 6;
      CFL            at 0 range 7 .. 7;
      OCPM           at 0 range 8 .. 15;
      CDIV           at 0 range 16 .. 19;
      Reserved_20_30 at 0 range 20 .. 30;
      SWR            at 0 range 31 .. 31;
   end record;

   subtype CSTAT_OCPC_Field is STM32H753.UInt18;
   subtype CSTAT_TQC_Field is STM32H753.UInt11;
   subtype CSTAT_CALS_Field is STM32H753.UInt2;

   --  Calibration Status Register
   type CSTAT_Register is record
      --  Oscillator Clock Period Counter
      OCPC           : CSTAT_OCPC_Field := 16#0#;
      --  Time Quanta Counter
      TQC            : CSTAT_TQC_Field := 16#0#;
      --  unspecified
      Reserved_29_29 : STM32H753.Bit := 16#0#;
      --  Calibration State
      CALS           : CSTAT_CALS_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CSTAT_Register use record
      OCPC           at 0 range 0 .. 17;
      TQC            at 0 range 18 .. 28;
      Reserved_29_29 at 0 range 29 .. 29;
      CALS           at 0 range 30 .. 31;
   end record;

   subtype CWD_WDC_Field is STM32H753.UInt16;
   subtype CWD_WDV_Field is STM32H753.UInt16;

   --  Calibration Watchdog Register
   type CWD_Register is record
      --  WDC
      WDC : CWD_WDC_Field := 16#0#;
      --  WDV
      WDV : CWD_WDV_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CWD_Register use record
      WDC at 0 range 0 .. 15;
      WDV at 0 range 16 .. 31;
   end record;

   --  Clock Calibration Unit Interrupt Register
   type IR_Register is record
      --  Calibration Watchdog Event
      CWE           : Boolean := False;
      --  Calibration State Changed
      CSC           : Boolean := False;
      --  unspecified
      Reserved_2_31 : STM32H753.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IR_Register use record
      CWE           at 0 range 0 .. 0;
      CSC           at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   --  Clock Calibration Unit Interrupt Enable Register
   type IE_Register is record
      --  Calibration Watchdog Event Enable
      CWEE          : Boolean := False;
      --  Calibration State Changed Enable
      CSCE          : Boolean := False;
      --  unspecified
      Reserved_2_31 : STM32H753.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for IE_Register use record
      CWEE          at 0 range 0 .. 0;
      CSCE          at 0 range 1 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   subtype FDCAN_CREL_DAY_Field is STM32H753.Byte;
   subtype FDCAN_CREL_MON_Field is STM32H753.Byte;
   subtype FDCAN_CREL_YEAR_Field is STM32H753.UInt4;
   subtype FDCAN_CREL_SUBSTEP_Field is STM32H753.UInt4;
   subtype FDCAN_CREL_STEP_Field is STM32H753.UInt4;
   subtype FDCAN_CREL_REL_Field is STM32H753.UInt4;

   --  FDCAN Core Release Register
   type FDCAN_CREL_Register is record
      --  Read-only. Timestamp Day
      DAY     : FDCAN_CREL_DAY_Field;
      --  Read-only. Timestamp Month
      MON     : FDCAN_CREL_MON_Field;
      --  Read-only. Timestamp Year
      YEAR    : FDCAN_CREL_YEAR_Field;
      --  Read-only. Sub-step of Core release
      SUBSTEP : FDCAN_CREL_SUBSTEP_Field;
      --  Read-only. Step of Core release
      STEP    : FDCAN_CREL_STEP_Field;
      --  Read-only. Core release
      REL     : FDCAN_CREL_REL_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_CREL_Register use record
      DAY     at 0 range 0 .. 7;
      MON     at 0 range 8 .. 15;
      YEAR    at 0 range 16 .. 19;
      SUBSTEP at 0 range 20 .. 23;
      STEP    at 0 range 24 .. 27;
      REL     at 0 range 28 .. 31;
   end record;

   subtype FDCAN_DBTP_DSJW_Field is STM32H753.UInt4;
   subtype FDCAN_DBTP_DTSEG2_Field is STM32H753.UInt4;
   subtype FDCAN_DBTP_DTSEG1_Field is STM32H753.UInt5;
   subtype FDCAN_DBTP_DBRP_Field is STM32H753.UInt5;

   --  FDCAN Data Bit Timing and Prescaler Register
   type FDCAN_DBTP_Register is record
      --  Read-only. Synchronization Jump Width
      DSJW           : FDCAN_DBTP_DSJW_Field;
      --  Read-only. Data time segment after sample point
      DTSEG2         : FDCAN_DBTP_DTSEG2_Field;
      --  Read-only. Data time segment after sample point
      DTSEG1         : FDCAN_DBTP_DTSEG1_Field;
      --  unspecified
      Reserved_13_15 : STM32H753.UInt3;
      --  Read-only. Data BIt Rate Prescaler
      DBRP           : FDCAN_DBTP_DBRP_Field;
      --  unspecified
      Reserved_21_22 : STM32H753.UInt2;
      --  Read-only. Transceiver Delay Compensation
      TDC            : Boolean;
      --  unspecified
      Reserved_24_31 : STM32H753.Byte;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_DBTP_Register use record
      DSJW           at 0 range 0 .. 3;
      DTSEG2         at 0 range 4 .. 7;
      DTSEG1         at 0 range 8 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      DBRP           at 0 range 16 .. 20;
      Reserved_21_22 at 0 range 21 .. 22;
      TDC            at 0 range 23 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype FDCAN_TEST_TX_Field is STM32H753.UInt2;

   --  FDCAN Test Register
   type FDCAN_TEST_Register is record
      --  unspecified
      Reserved_0_3  : STM32H753.UInt4;
      --  Read-only. Loop Back mode
      LBCK          : Boolean;
      --  Read-only. Loop Back mode
      TX            : FDCAN_TEST_TX_Field;
      --  Read-only. Control of Transmit Pin
      RX            : Boolean;
      --  unspecified
      Reserved_8_31 : STM32H753.UInt24;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TEST_Register use record
      Reserved_0_3  at 0 range 0 .. 3;
      LBCK          at 0 range 4 .. 4;
      TX            at 0 range 5 .. 6;
      RX            at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype FDCAN_RWD_WDC_Field is STM32H753.Byte;
   subtype FDCAN_RWD_WDV_Field is STM32H753.Byte;

   --  FDCAN RAM Watchdog Register
   type FDCAN_RWD_Register is record
      --  Read-only. Watchdog configuration
      WDC            : FDCAN_RWD_WDC_Field;
      --  Read-only. Watchdog value
      WDV            : FDCAN_RWD_WDV_Field;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_RWD_Register use record
      WDC            at 0 range 0 .. 7;
      WDV            at 0 range 8 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  FDCAN CC Control Register
   type FDCAN_CCCR_Register is record
      --  Initialization
      INIT           : Boolean := False;
      --  Configuration Change Enable
      CCE            : Boolean := False;
      --  ASM Restricted Operation Mode
      ASM            : Boolean := False;
      --  Clock Stop Acknowledge
      CSA            : Boolean := False;
      --  Clock Stop Request
      CSR            : Boolean := False;
      --  Bus Monitoring Mode
      MON            : Boolean := False;
      --  Disable Automatic Retransmission
      DAR            : Boolean := False;
      --  Test Mode Enable
      TEST           : Boolean := False;
      --  FD Operation Enable
      FDOE           : Boolean := False;
      --  FDCAN Bit Rate Switching
      BSE            : Boolean := False;
      --  unspecified
      Reserved_10_11 : STM32H753.UInt2 := 16#0#;
      --  Protocol Exception Handling Disable
      PXHD           : Boolean := False;
      --  Edge Filtering during Bus Integration
      EFBI           : Boolean := False;
      --  TXP
      TXP            : Boolean := False;
      --  Non ISO Operation
      NISO           : Boolean := False;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_CCCR_Register use record
      INIT           at 0 range 0 .. 0;
      CCE            at 0 range 1 .. 1;
      ASM            at 0 range 2 .. 2;
      CSA            at 0 range 3 .. 3;
      CSR            at 0 range 4 .. 4;
      MON            at 0 range 5 .. 5;
      DAR            at 0 range 6 .. 6;
      TEST           at 0 range 7 .. 7;
      FDOE           at 0 range 8 .. 8;
      BSE            at 0 range 9 .. 9;
      Reserved_10_11 at 0 range 10 .. 11;
      PXHD           at 0 range 12 .. 12;
      EFBI           at 0 range 13 .. 13;
      TXP            at 0 range 14 .. 14;
      NISO           at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype FDCAN_NBTP_TSEG2_Field is STM32H753.UInt7;
   subtype FDCAN_NBTP_NTSEG1_Field is STM32H753.Byte;
   subtype FDCAN_NBTP_NBRP_Field is STM32H753.UInt9;
   subtype FDCAN_NBTP_NSJW_Field is STM32H753.UInt7;

   --  FDCAN Nominal Bit Timing and Prescaler Register
   type FDCAN_NBTP_Register is record
      --  Nominal Time segment after sample point
      TSEG2        : FDCAN_NBTP_TSEG2_Field := 16#0#;
      --  unspecified
      Reserved_7_7 : STM32H753.Bit := 16#0#;
      --  Nominal Time segment before sample point
      NTSEG1       : FDCAN_NBTP_NTSEG1_Field := 16#0#;
      --  Bit Rate Prescaler
      NBRP         : FDCAN_NBTP_NBRP_Field := 16#0#;
      --  NSJW: Nominal (Re)Synchronization Jump Width
      NSJW         : FDCAN_NBTP_NSJW_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_NBTP_Register use record
      TSEG2        at 0 range 0 .. 6;
      Reserved_7_7 at 0 range 7 .. 7;
      NTSEG1       at 0 range 8 .. 15;
      NBRP         at 0 range 16 .. 24;
      NSJW         at 0 range 25 .. 31;
   end record;

   subtype FDCAN_TSCC_TSS_Field is STM32H753.UInt2;
   subtype FDCAN_TSCC_TCP_Field is STM32H753.UInt4;

   --  FDCAN Timestamp Counter Configuration Register
   type FDCAN_TSCC_Register is record
      --  Timestamp Select
      TSS            : FDCAN_TSCC_TSS_Field := 16#0#;
      --  unspecified
      Reserved_2_15  : STM32H753.UInt14 := 16#0#;
      --  Timestamp Counter Prescaler
      TCP            : FDCAN_TSCC_TCP_Field := 16#0#;
      --  unspecified
      Reserved_20_31 : STM32H753.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TSCC_Register use record
      TSS            at 0 range 0 .. 1;
      Reserved_2_15  at 0 range 2 .. 15;
      TCP            at 0 range 16 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   subtype FDCAN_TSCV_TSC_Field is STM32H753.UInt16;

   --  FDCAN Timestamp Counter Value Register
   type FDCAN_TSCV_Register is record
      --  Timestamp Counter
      TSC            : FDCAN_TSCV_TSC_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TSCV_Register use record
      TSC            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype FDCAN_TOCC_TOS_Field is STM32H753.UInt2;
   subtype FDCAN_TOCC_TOP_Field is STM32H753.UInt16;

   --  FDCAN Timeout Counter Configuration Register
   type FDCAN_TOCC_Register is record
      --  Enable Timeout Counter
      ETOC          : Boolean := False;
      --  Timeout Select
      TOS           : FDCAN_TOCC_TOS_Field := 16#0#;
      --  unspecified
      Reserved_3_15 : STM32H753.UInt13 := 16#0#;
      --  Timeout Period
      TOP           : FDCAN_TOCC_TOP_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TOCC_Register use record
      ETOC          at 0 range 0 .. 0;
      TOS           at 0 range 1 .. 2;
      Reserved_3_15 at 0 range 3 .. 15;
      TOP           at 0 range 16 .. 31;
   end record;

   subtype FDCAN_TOCV_TOC_Field is STM32H753.UInt16;

   --  FDCAN Timeout Counter Value Register
   type FDCAN_TOCV_Register is record
      --  Timeout Counter
      TOC            : FDCAN_TOCV_TOC_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TOCV_Register use record
      TOC            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype FDCAN_ECR_TEC_Field is STM32H753.Byte;
   subtype FDCAN_ECR_TREC_Field is STM32H753.UInt7;
   subtype FDCAN_ECR_CEL_Field is STM32H753.Byte;

   --  FDCAN Error Counter Register
   type FDCAN_ECR_Register is record
      --  Transmit Error Counter
      TEC            : FDCAN_ECR_TEC_Field := 16#0#;
      --  Receive Error Counter
      TREC           : FDCAN_ECR_TREC_Field := 16#0#;
      --  Receive Error Passive
      RP             : Boolean := False;
      --  AN Error Logging
      CEL            : FDCAN_ECR_CEL_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : STM32H753.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_ECR_Register use record
      TEC            at 0 range 0 .. 7;
      TREC           at 0 range 8 .. 14;
      RP             at 0 range 15 .. 15;
      CEL            at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype FDCAN_PSR_LEC_Field is STM32H753.UInt3;
   subtype FDCAN_PSR_ACT_Field is STM32H753.UInt2;
   subtype FDCAN_PSR_DLEC_Field is STM32H753.UInt3;
   subtype FDCAN_PSR_TDCV_Field is STM32H753.UInt7;

   --  FDCAN Protocol Status Register
   type FDCAN_PSR_Register is record
      --  Last Error Code
      LEC            : FDCAN_PSR_LEC_Field := 16#0#;
      --  Activity
      ACT            : FDCAN_PSR_ACT_Field := 16#0#;
      --  Error Passive
      EP             : Boolean := False;
      --  Warning Status
      EW             : Boolean := False;
      --  Bus_Off Status
      BO             : Boolean := False;
      --  Data Last Error Code
      DLEC           : FDCAN_PSR_DLEC_Field := 16#0#;
      --  ESI flag of last received FDCAN Message
      RESI           : Boolean := False;
      --  BRS flag of last received FDCAN Message
      RBRS           : Boolean := False;
      --  Received FDCAN Message
      REDL           : Boolean := False;
      --  Protocol Exception Event
      PXE            : Boolean := False;
      --  unspecified
      Reserved_15_15 : STM32H753.Bit := 16#0#;
      --  Transmitter Delay Compensation Value
      TDCV           : FDCAN_PSR_TDCV_Field := 16#0#;
      --  unspecified
      Reserved_23_31 : STM32H753.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_PSR_Register use record
      LEC            at 0 range 0 .. 2;
      ACT            at 0 range 3 .. 4;
      EP             at 0 range 5 .. 5;
      EW             at 0 range 6 .. 6;
      BO             at 0 range 7 .. 7;
      DLEC           at 0 range 8 .. 10;
      RESI           at 0 range 11 .. 11;
      RBRS           at 0 range 12 .. 12;
      REDL           at 0 range 13 .. 13;
      PXE            at 0 range 14 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      TDCV           at 0 range 16 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   subtype FDCAN_TDCR_TDCF_Field is STM32H753.UInt7;
   subtype FDCAN_TDCR_TDCO_Field is STM32H753.UInt7;

   --  FDCAN Transmitter Delay Compensation Register
   type FDCAN_TDCR_Register is record
      --  Read-only. Transmitter Delay Compensation Filter Window Length
      TDCF           : FDCAN_TDCR_TDCF_Field;
      --  unspecified
      Reserved_7_7   : STM32H753.Bit;
      --  Read-only. Transmitter Delay Compensation Offset
      TDCO           : FDCAN_TDCR_TDCO_Field;
      --  unspecified
      Reserved_15_31 : STM32H753.UInt17;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TDCR_Register use record
      TDCF           at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      TDCO           at 0 range 8 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   --  FDCAN Interrupt Register
   type FDCAN_IR_Register is record
      --  Read-only. Rx FIFO 0 New Message
      RF0N           : Boolean;
      --  Read-only. Rx FIFO 0 Full
      RF0W           : Boolean;
      --  Read-only. Rx FIFO 0 Full
      RF0F           : Boolean;
      --  Read-only. Rx FIFO 0 Message Lost
      RF0L           : Boolean;
      --  Read-only. Rx FIFO 1 New Message
      RF1N           : Boolean;
      --  Read-only. Rx FIFO 1 Watermark Reached
      RF1W           : Boolean;
      --  Read-only. Rx FIFO 1 Watermark Reached
      RF1F           : Boolean;
      --  Read-only. Rx FIFO 1 Message Lost
      RF1L           : Boolean;
      --  Read-only. High Priority Message
      HPM            : Boolean;
      --  Read-only. Transmission Completed
      TC             : Boolean;
      --  Read-only. Transmission Cancellation Finished
      TCF            : Boolean;
      --  Read-only. Tx FIFO Empty
      TEF            : Boolean;
      --  Read-only. Tx Event FIFO New Entry
      TEFN           : Boolean;
      --  Read-only. Tx Event FIFO Watermark Reached
      TEFW           : Boolean;
      --  Read-only. Tx Event FIFO Full
      TEFF           : Boolean;
      --  Read-only. Tx Event FIFO Element Lost
      TEFL           : Boolean;
      --  Read-only. Timestamp Wraparound
      TSW            : Boolean;
      --  Read-only. Message RAM Access Failure
      MRAF           : Boolean;
      --  Read-only. Timeout Occurred
      TOO            : Boolean;
      --  Read-only. Message stored to Dedicated Rx Buffer
      DRX            : Boolean;
      --  unspecified
      Reserved_20_21 : STM32H753.UInt2;
      --  Read-only. Error Logging Overflow
      ELO            : Boolean;
      --  Read-only. Error Passive
      EP             : Boolean;
      --  Read-only. Warning Status
      EW             : Boolean;
      --  Read-only. Bus_Off Status
      BO             : Boolean;
      --  Read-only. Watchdog Interrupt
      WDI            : Boolean;
      --  Read-only. Protocol Error in Arbitration Phase (Nominal Bit Time is
      --  used)
      PEA            : Boolean;
      --  Read-only. Protocol Error in Data Phase (Data Bit Time is used)
      PED            : Boolean;
      --  Read-only. Access to Reserved Address
      ARA            : Boolean;
      --  unspecified
      Reserved_30_31 : STM32H753.UInt2;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_IR_Register use record
      RF0N           at 0 range 0 .. 0;
      RF0W           at 0 range 1 .. 1;
      RF0F           at 0 range 2 .. 2;
      RF0L           at 0 range 3 .. 3;
      RF1N           at 0 range 4 .. 4;
      RF1W           at 0 range 5 .. 5;
      RF1F           at 0 range 6 .. 6;
      RF1L           at 0 range 7 .. 7;
      HPM            at 0 range 8 .. 8;
      TC             at 0 range 9 .. 9;
      TCF            at 0 range 10 .. 10;
      TEF            at 0 range 11 .. 11;
      TEFN           at 0 range 12 .. 12;
      TEFW           at 0 range 13 .. 13;
      TEFF           at 0 range 14 .. 14;
      TEFL           at 0 range 15 .. 15;
      TSW            at 0 range 16 .. 16;
      MRAF           at 0 range 17 .. 17;
      TOO            at 0 range 18 .. 18;
      DRX            at 0 range 19 .. 19;
      Reserved_20_21 at 0 range 20 .. 21;
      ELO            at 0 range 22 .. 22;
      EP             at 0 range 23 .. 23;
      EW             at 0 range 24 .. 24;
      BO             at 0 range 25 .. 25;
      WDI            at 0 range 26 .. 26;
      PEA            at 0 range 27 .. 27;
      PED            at 0 range 28 .. 28;
      ARA            at 0 range 29 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   --  FDCAN Interrupt Enable Register
   type FDCAN_IE_Register is record
      --  Read-only. Rx FIFO 0 New Message Enable
      RF0NE          : Boolean;
      --  Read-only. Rx FIFO 0 Full Enable
      RF0WE          : Boolean;
      --  Read-only. Rx FIFO 0 Full Enable
      RF0FE          : Boolean;
      --  Read-only. Rx FIFO 0 Message Lost Enable
      RF0LE          : Boolean;
      --  Read-only. Rx FIFO 1 New Message Enable
      RF1NE          : Boolean;
      --  Read-only. Rx FIFO 1 Watermark Reached Enable
      RF1WE          : Boolean;
      --  Read-only. Rx FIFO 1 Watermark Reached Enable
      RF1FE          : Boolean;
      --  Read-only. Rx FIFO 1 Message Lost Enable
      RF1LE          : Boolean;
      --  Read-only. High Priority Message Enable
      HPME           : Boolean;
      --  Read-only. Transmission Completed Enable
      TCE            : Boolean;
      --  Read-only. Transmission Cancellation Finished Enable
      TCFE           : Boolean;
      --  Read-only. Tx FIFO Empty Enable
      TEFE           : Boolean;
      --  Read-only. Tx Event FIFO New Entry Enable
      TEFNE          : Boolean;
      --  Read-only. Tx Event FIFO Watermark Reached Enable
      TEFWE          : Boolean;
      --  Read-only. Tx Event FIFO Full Enable
      TEFFE          : Boolean;
      --  Read-only. Tx Event FIFO Element Lost Enable
      TEFLE          : Boolean;
      --  Read-only. Timestamp Wraparound Enable
      TSWE           : Boolean;
      --  Read-only. Message RAM Access Failure Enable
      MRAFE          : Boolean;
      --  Read-only. Timeout Occurred Enable
      TOOE           : Boolean;
      --  Read-only. Message stored to Dedicated Rx Buffer Enable
      DRXE           : Boolean;
      --  Read-only. Bit Error Corrected Interrupt Enable
      BECE           : Boolean;
      --  Read-only. Bit Error Uncorrected Interrupt Enable
      BEUE           : Boolean;
      --  Read-only. Error Logging Overflow Enable
      ELOE           : Boolean;
      --  Read-only. Error Passive Enable
      EPE            : Boolean;
      --  Read-only. Warning Status Enable
      EWE            : Boolean;
      --  Read-only. Bus_Off Status Enable
      BOE            : Boolean;
      --  Read-only. Watchdog Interrupt Enable
      WDIE           : Boolean;
      --  Read-only. Protocol Error in Arbitration Phase Enable
      PEAE           : Boolean;
      --  Read-only. Protocol Error in Data Phase Enable
      PEDE           : Boolean;
      --  Read-only. Access to Reserved Address Enable
      ARAE           : Boolean;
      --  unspecified
      Reserved_30_31 : STM32H753.UInt2;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_IE_Register use record
      RF0NE          at 0 range 0 .. 0;
      RF0WE          at 0 range 1 .. 1;
      RF0FE          at 0 range 2 .. 2;
      RF0LE          at 0 range 3 .. 3;
      RF1NE          at 0 range 4 .. 4;
      RF1WE          at 0 range 5 .. 5;
      RF1FE          at 0 range 6 .. 6;
      RF1LE          at 0 range 7 .. 7;
      HPME           at 0 range 8 .. 8;
      TCE            at 0 range 9 .. 9;
      TCFE           at 0 range 10 .. 10;
      TEFE           at 0 range 11 .. 11;
      TEFNE          at 0 range 12 .. 12;
      TEFWE          at 0 range 13 .. 13;
      TEFFE          at 0 range 14 .. 14;
      TEFLE          at 0 range 15 .. 15;
      TSWE           at 0 range 16 .. 16;
      MRAFE          at 0 range 17 .. 17;
      TOOE           at 0 range 18 .. 18;
      DRXE           at 0 range 19 .. 19;
      BECE           at 0 range 20 .. 20;
      BEUE           at 0 range 21 .. 21;
      ELOE           at 0 range 22 .. 22;
      EPE            at 0 range 23 .. 23;
      EWE            at 0 range 24 .. 24;
      BOE            at 0 range 25 .. 25;
      WDIE           at 0 range 26 .. 26;
      PEAE           at 0 range 27 .. 27;
      PEDE           at 0 range 28 .. 28;
      ARAE           at 0 range 29 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   --  FDCAN Interrupt Line Select Register
   type FDCAN_ILS_Register is record
      --  Read-only. Rx FIFO 0 New Message Interrupt Line
      RF0NL          : Boolean;
      --  Read-only. Rx FIFO 0 Watermark Reached Interrupt Line
      RF0WL          : Boolean;
      --  Read-only. Rx FIFO 0 Full Interrupt Line
      RF0FL          : Boolean;
      --  Read-only. Rx FIFO 0 Message Lost Interrupt Line
      RF0LL          : Boolean;
      --  Read-only. Rx FIFO 1 New Message Interrupt Line
      RF1NL          : Boolean;
      --  Read-only. Rx FIFO 1 Watermark Reached Interrupt Line
      RF1WL          : Boolean;
      --  Read-only. Rx FIFO 1 Full Interrupt Line
      RF1FL          : Boolean;
      --  Read-only. Rx FIFO 1 Message Lost Interrupt Line
      RF1LL          : Boolean;
      --  Read-only. High Priority Message Interrupt Line
      HPML           : Boolean;
      --  Read-only. Transmission Completed Interrupt Line
      TCL            : Boolean;
      --  Read-only. Transmission Cancellation Finished Interrupt Line
      TCFL           : Boolean;
      --  Read-only. Tx FIFO Empty Interrupt Line
      TEFL           : Boolean;
      --  Read-only. Tx Event FIFO New Entry Interrupt Line
      TEFNL          : Boolean;
      --  Read-only. Tx Event FIFO Watermark Reached Interrupt Line
      TEFWL          : Boolean;
      --  Read-only. Tx Event FIFO Full Interrupt Line
      TEFFL          : Boolean;
      --  Read-only. Tx Event FIFO Element Lost Interrupt Line
      TEFLL          : Boolean;
      --  Read-only. Timestamp Wraparound Interrupt Line
      TSWL           : Boolean;
      --  Read-only. Message RAM Access Failure Interrupt Line
      MRAFL          : Boolean;
      --  Read-only. Timeout Occurred Interrupt Line
      TOOL           : Boolean;
      --  Read-only. Message stored to Dedicated Rx Buffer Interrupt Line
      DRXL           : Boolean;
      --  Read-only. Bit Error Corrected Interrupt Line
      BECL           : Boolean;
      --  Read-only. Bit Error Uncorrected Interrupt Line
      BEUL           : Boolean;
      --  Read-only. Error Logging Overflow Interrupt Line
      ELOL           : Boolean;
      --  Read-only. Error Passive Interrupt Line
      EPL            : Boolean;
      --  Read-only. Warning Status Interrupt Line
      EWL            : Boolean;
      --  Read-only. Bus_Off Status
      BOL            : Boolean;
      --  Read-only. Watchdog Interrupt Line
      WDIL           : Boolean;
      --  Read-only. Protocol Error in Arbitration Phase Line
      PEAL           : Boolean;
      --  Read-only. Protocol Error in Data Phase Line
      PEDL           : Boolean;
      --  Read-only. Access to Reserved Address Line
      ARAL           : Boolean;
      --  unspecified
      Reserved_30_31 : STM32H753.UInt2;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_ILS_Register use record
      RF0NL          at 0 range 0 .. 0;
      RF0WL          at 0 range 1 .. 1;
      RF0FL          at 0 range 2 .. 2;
      RF0LL          at 0 range 3 .. 3;
      RF1NL          at 0 range 4 .. 4;
      RF1WL          at 0 range 5 .. 5;
      RF1FL          at 0 range 6 .. 6;
      RF1LL          at 0 range 7 .. 7;
      HPML           at 0 range 8 .. 8;
      TCL            at 0 range 9 .. 9;
      TCFL           at 0 range 10 .. 10;
      TEFL           at 0 range 11 .. 11;
      TEFNL          at 0 range 12 .. 12;
      TEFWL          at 0 range 13 .. 13;
      TEFFL          at 0 range 14 .. 14;
      TEFLL          at 0 range 15 .. 15;
      TSWL           at 0 range 16 .. 16;
      MRAFL          at 0 range 17 .. 17;
      TOOL           at 0 range 18 .. 18;
      DRXL           at 0 range 19 .. 19;
      BECL           at 0 range 20 .. 20;
      BEUL           at 0 range 21 .. 21;
      ELOL           at 0 range 22 .. 22;
      EPL            at 0 range 23 .. 23;
      EWL            at 0 range 24 .. 24;
      BOL            at 0 range 25 .. 25;
      WDIL           at 0 range 26 .. 26;
      PEAL           at 0 range 27 .. 27;
      PEDL           at 0 range 28 .. 28;
      ARAL           at 0 range 29 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   --  FDCAN_ILE_EINT array
   type FDCAN_ILE_EINT_Field_Array is array (0 .. 1) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for FDCAN_ILE_EINT
   type FDCAN_ILE_EINT_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  EINT as a value
            Val : STM32H753.UInt2;
         when True =>
            --  EINT as an array
            Arr : FDCAN_ILE_EINT_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for FDCAN_ILE_EINT_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  FDCAN Interrupt Line Enable Register
   type FDCAN_ILE_Register is record
      --  Enable Interrupt Line 0
      EINT          : FDCAN_ILE_EINT_Field :=
                       (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_2_31 : STM32H753.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_ILE_Register use record
      EINT          at 0 range 0 .. 1;
      Reserved_2_31 at 0 range 2 .. 31;
   end record;

   subtype FDCAN_GFC_ANFE_Field is STM32H753.UInt2;
   subtype FDCAN_GFC_ANFS_Field is STM32H753.UInt2;

   --  FDCAN Global Filter Configuration Register
   type FDCAN_GFC_Register is record
      --  Reject Remote Frames Extended
      RRFE          : Boolean := False;
      --  Reject Remote Frames Standard
      RRFS          : Boolean := False;
      --  Accept Non-matching Frames Extended
      ANFE          : FDCAN_GFC_ANFE_Field := 16#0#;
      --  Accept Non-matching Frames Standard
      ANFS          : FDCAN_GFC_ANFS_Field := 16#0#;
      --  unspecified
      Reserved_6_31 : STM32H753.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_GFC_Register use record
      RRFE          at 0 range 0 .. 0;
      RRFS          at 0 range 1 .. 1;
      ANFE          at 0 range 2 .. 3;
      ANFS          at 0 range 4 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   subtype FDCAN_SIDFC_FLSSA_Field is STM32H753.UInt14;
   subtype FDCAN_SIDFC_LSS_Field is STM32H753.Byte;

   --  FDCAN Standard ID Filter Configuration Register
   type FDCAN_SIDFC_Register is record
      --  unspecified
      Reserved_0_1   : STM32H753.UInt2 := 16#0#;
      --  Filter List Standard Start Address
      FLSSA          : FDCAN_SIDFC_FLSSA_Field := 16#0#;
      --  List Size Standard
      LSS            : FDCAN_SIDFC_LSS_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : STM32H753.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_SIDFC_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      FLSSA          at 0 range 2 .. 15;
      LSS            at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype FDCAN_XIDFC_FLESA_Field is STM32H753.UInt14;
   subtype FDCAN_XIDFC_LSE_Field is STM32H753.Byte;

   --  FDCAN Extended ID Filter Configuration Register
   type FDCAN_XIDFC_Register is record
      --  unspecified
      Reserved_0_1   : STM32H753.UInt2 := 16#0#;
      --  Filter List Standard Start Address
      FLESA          : FDCAN_XIDFC_FLESA_Field := 16#0#;
      --  List Size Extended
      LSE            : FDCAN_XIDFC_LSE_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : STM32H753.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_XIDFC_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      FLESA          at 0 range 2 .. 15;
      LSE            at 0 range 16 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype FDCAN_XIDAM_EIDM_Field is STM32H753.UInt29;

   --  FDCAN Extended ID and Mask Register
   type FDCAN_XIDAM_Register is record
      --  Extended ID Mask
      EIDM           : FDCAN_XIDAM_EIDM_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : STM32H753.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_XIDAM_Register use record
      EIDM           at 0 range 0 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype FDCAN_HPMS_BIDX_Field is STM32H753.UInt6;
   subtype FDCAN_HPMS_MSI_Field is STM32H753.UInt2;
   subtype FDCAN_HPMS_FIDX_Field is STM32H753.UInt7;

   --  FDCAN High Priority Message Status Register
   type FDCAN_HPMS_Register is record
      --  Read-only. Buffer Index
      BIDX           : FDCAN_HPMS_BIDX_Field;
      --  Read-only. Message Storage Indicator
      MSI            : FDCAN_HPMS_MSI_Field;
      --  Read-only. Filter Index
      FIDX           : FDCAN_HPMS_FIDX_Field;
      --  Read-only. Filter List
      FLST           : Boolean;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_HPMS_Register use record
      BIDX           at 0 range 0 .. 5;
      MSI            at 0 range 6 .. 7;
      FIDX           at 0 range 8 .. 14;
      FLST           at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  FDCAN_NDAT1_ND array
   type FDCAN_NDAT1_ND_Field_Array is array (0 .. 31) of Boolean
     with Component_Size => 1, Size => 32;

   --  FDCAN New Data 1 Register
   type FDCAN_NDAT1_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  ND as a value
            Val : STM32H753.UInt32;
         when True =>
            --  ND as an array
            Arr : FDCAN_NDAT1_ND_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_NDAT1_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  FDCAN_NDAT2_ND array
   type FDCAN_NDAT2_ND_Field_Array is array (32 .. 63) of Boolean
     with Component_Size => 1, Size => 32;

   --  FDCAN New Data 2 Register
   type FDCAN_NDAT2_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  ND as a value
            Val : STM32H753.UInt32;
         when True =>
            --  ND as an array
            Arr : FDCAN_NDAT2_ND_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_NDAT2_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   subtype FDCAN_RXF0C_F0SA_Field is STM32H753.UInt14;
   subtype FDCAN_RXF0C_F0S_Field is STM32H753.Byte;
   subtype FDCAN_RXF0C_F0WM_Field is STM32H753.Byte;

   --  FDCAN Rx FIFO 0 Configuration Register
   type FDCAN_RXF0C_Register is record
      --  unspecified
      Reserved_0_1 : STM32H753.UInt2 := 16#0#;
      --  Rx FIFO 0 Start Address
      F0SA         : FDCAN_RXF0C_F0SA_Field := 16#0#;
      --  Rx FIFO 0 Size
      F0S          : FDCAN_RXF0C_F0S_Field := 16#0#;
      --  FIFO 0 Watermark
      F0WM         : FDCAN_RXF0C_F0WM_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_RXF0C_Register use record
      Reserved_0_1 at 0 range 0 .. 1;
      F0SA         at 0 range 2 .. 15;
      F0S          at 0 range 16 .. 23;
      F0WM         at 0 range 24 .. 31;
   end record;

   subtype FDCAN_RXF0S_F0FL_Field is STM32H753.UInt7;
   subtype FDCAN_RXF0S_F0G_Field is STM32H753.UInt6;
   subtype FDCAN_RXF0S_F0P_Field is STM32H753.UInt6;

   --  FDCAN Rx FIFO 0 Status Register
   type FDCAN_RXF0S_Register is record
      --  Rx FIFO 0 Fill Level
      F0FL           : FDCAN_RXF0S_F0FL_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : STM32H753.Bit := 16#0#;
      --  Rx FIFO 0 Get Index
      F0G            : FDCAN_RXF0S_F0G_Field := 16#0#;
      --  unspecified
      Reserved_14_15 : STM32H753.UInt2 := 16#0#;
      --  Rx FIFO 0 Put Index
      F0P            : FDCAN_RXF0S_F0P_Field := 16#0#;
      --  unspecified
      Reserved_22_23 : STM32H753.UInt2 := 16#0#;
      --  Rx FIFO 0 Full
      F0F            : Boolean := False;
      --  Rx FIFO 0 Message Lost
      RF0L           : Boolean := False;
      --  unspecified
      Reserved_26_31 : STM32H753.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_RXF0S_Register use record
      F0FL           at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      F0G            at 0 range 8 .. 13;
      Reserved_14_15 at 0 range 14 .. 15;
      F0P            at 0 range 16 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      F0F            at 0 range 24 .. 24;
      RF0L           at 0 range 25 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype FDCAN_RXF0A_FA01_Field is STM32H753.UInt6;

   --  CAN Rx FIFO 0 Acknowledge Register
   type FDCAN_RXF0A_Register is record
      --  Rx FIFO 0 Acknowledge Index
      FA01          : FDCAN_RXF0A_FA01_Field := 16#0#;
      --  unspecified
      Reserved_6_31 : STM32H753.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_RXF0A_Register use record
      FA01          at 0 range 0 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   subtype FDCAN_RXBC_RBSA_Field is STM32H753.UInt14;

   --  FDCAN Rx Buffer Configuration Register
   type FDCAN_RXBC_Register is record
      --  unspecified
      Reserved_0_1   : STM32H753.UInt2 := 16#0#;
      --  Rx Buffer Start Address
      RBSA           : FDCAN_RXBC_RBSA_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_RXBC_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      RBSA           at 0 range 2 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype FDCAN_RXF1C_F1SA_Field is STM32H753.UInt14;
   subtype FDCAN_RXF1C_F1S_Field is STM32H753.UInt7;
   subtype FDCAN_RXF1C_F1WM_Field is STM32H753.UInt7;

   --  FDCAN Rx FIFO 1 Configuration Register
   type FDCAN_RXF1C_Register is record
      --  unspecified
      Reserved_0_1   : STM32H753.UInt2 := 16#0#;
      --  Rx FIFO 1 Start Address
      F1SA           : FDCAN_RXF1C_F1SA_Field := 16#0#;
      --  Rx FIFO 1 Size
      F1S            : FDCAN_RXF1C_F1S_Field := 16#0#;
      --  unspecified
      Reserved_23_23 : STM32H753.Bit := 16#0#;
      --  Rx FIFO 1 Watermark
      F1WM           : FDCAN_RXF1C_F1WM_Field := 16#0#;
      --  unspecified
      Reserved_31_31 : STM32H753.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_RXF1C_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      F1SA           at 0 range 2 .. 15;
      F1S            at 0 range 16 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      F1WM           at 0 range 24 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype FDCAN_RXF1S_F1FL_Field is STM32H753.UInt7;
   subtype FDCAN_RXF1S_F1GI_Field is STM32H753.UInt7;
   subtype FDCAN_RXF1S_F1PI_Field is STM32H753.UInt7;
   subtype FDCAN_RXF1S_DMS_Field is STM32H753.UInt2;

   --  FDCAN Rx FIFO 1 Status Register
   type FDCAN_RXF1S_Register is record
      --  Rx FIFO 1 Fill Level
      F1FL           : FDCAN_RXF1S_F1FL_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : STM32H753.Bit := 16#0#;
      --  Rx FIFO 1 Get Index
      F1GI           : FDCAN_RXF1S_F1GI_Field := 16#0#;
      --  unspecified
      Reserved_15_15 : STM32H753.Bit := 16#0#;
      --  Rx FIFO 1 Put Index
      F1PI           : FDCAN_RXF1S_F1PI_Field := 16#0#;
      --  unspecified
      Reserved_23_23 : STM32H753.Bit := 16#0#;
      --  Rx FIFO 1 Full
      F1F            : Boolean := False;
      --  Rx FIFO 1 Message Lost
      RF1L           : Boolean := False;
      --  unspecified
      Reserved_26_29 : STM32H753.UInt4 := 16#0#;
      --  Debug Message Status
      DMS            : FDCAN_RXF1S_DMS_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_RXF1S_Register use record
      F1FL           at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      F1GI           at 0 range 8 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      F1PI           at 0 range 16 .. 22;
      Reserved_23_23 at 0 range 23 .. 23;
      F1F            at 0 range 24 .. 24;
      RF1L           at 0 range 25 .. 25;
      Reserved_26_29 at 0 range 26 .. 29;
      DMS            at 0 range 30 .. 31;
   end record;

   subtype FDCAN_RXF1A_F1AI_Field is STM32H753.UInt6;

   --  FDCAN Rx FIFO 1 Acknowledge Register
   type FDCAN_RXF1A_Register is record
      --  Rx FIFO 1 Acknowledge Index
      F1AI          : FDCAN_RXF1A_F1AI_Field := 16#0#;
      --  unspecified
      Reserved_6_31 : STM32H753.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_RXF1A_Register use record
      F1AI          at 0 range 0 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   subtype FDCAN_RXESC_F0DS_Field is STM32H753.UInt3;
   subtype FDCAN_RXESC_F1DS_Field is STM32H753.UInt3;
   subtype FDCAN_RXESC_RBDS_Field is STM32H753.UInt3;

   --  FDCAN Rx Buffer Element Size Configuration Register
   type FDCAN_RXESC_Register is record
      --  Rx FIFO 1 Data Field Size:
      F0DS           : FDCAN_RXESC_F0DS_Field := 16#0#;
      --  unspecified
      Reserved_3_3   : STM32H753.Bit := 16#0#;
      --  Rx FIFO 0 Data Field Size:
      F1DS           : FDCAN_RXESC_F1DS_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : STM32H753.Bit := 16#0#;
      --  Rx Buffer Data Field Size:
      RBDS           : FDCAN_RXESC_RBDS_Field := 16#0#;
      --  unspecified
      Reserved_11_31 : STM32H753.UInt21 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_RXESC_Register use record
      F0DS           at 0 range 0 .. 2;
      Reserved_3_3   at 0 range 3 .. 3;
      F1DS           at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      RBDS           at 0 range 8 .. 10;
      Reserved_11_31 at 0 range 11 .. 31;
   end record;

   subtype FDCAN_TXBC_TBSA_Field is STM32H753.UInt14;
   subtype FDCAN_TXBC_NDTB_Field is STM32H753.UInt6;
   subtype FDCAN_TXBC_TFQS_Field is STM32H753.UInt6;

   --  FDCAN Tx Buffer Configuration Register
   type FDCAN_TXBC_Register is record
      --  unspecified
      Reserved_0_1   : STM32H753.UInt2 := 16#0#;
      --  Tx Buffers Start Address
      TBSA           : FDCAN_TXBC_TBSA_Field := 16#0#;
      --  Number of Dedicated Transmit Buffers
      NDTB           : FDCAN_TXBC_NDTB_Field := 16#0#;
      --  unspecified
      Reserved_22_23 : STM32H753.UInt2 := 16#0#;
      --  Transmit FIFO/Queue Size
      TFQS           : FDCAN_TXBC_TFQS_Field := 16#0#;
      --  Tx FIFO/Queue Mode
      TFQM           : Boolean := False;
      --  unspecified
      Reserved_31_31 : STM32H753.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TXBC_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      TBSA           at 0 range 2 .. 15;
      NDTB           at 0 range 16 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      TFQS           at 0 range 24 .. 29;
      TFQM           at 0 range 30 .. 30;
      Reserved_31_31 at 0 range 31 .. 31;
   end record;

   subtype FDCAN_TXFQS_TFFL_Field is STM32H753.UInt6;
   subtype FDCAN_TXFQS_TFGI_Field is STM32H753.UInt5;
   subtype FDCAN_TXFQS_TFQPI_Field is STM32H753.UInt5;

   --  FDCAN Tx FIFO/Queue Status Register
   type FDCAN_TXFQS_Register is record
      --  Read-only. Tx FIFO Free Level
      TFFL           : FDCAN_TXFQS_TFFL_Field;
      --  unspecified
      Reserved_6_7   : STM32H753.UInt2;
      --  Read-only. TFGI
      TFGI           : FDCAN_TXFQS_TFGI_Field;
      --  unspecified
      Reserved_13_15 : STM32H753.UInt3;
      --  Read-only. Tx FIFO/Queue Put Index
      TFQPI          : FDCAN_TXFQS_TFQPI_Field;
      --  Read-only. Tx FIFO/Queue Full
      TFQF           : Boolean;
      --  unspecified
      Reserved_22_31 : STM32H753.UInt10;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TXFQS_Register use record
      TFFL           at 0 range 0 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      TFGI           at 0 range 8 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      TFQPI          at 0 range 16 .. 20;
      TFQF           at 0 range 21 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   subtype FDCAN_TXESC_TBDS_Field is STM32H753.UInt3;

   --  FDCAN Tx Buffer Element Size Configuration Register
   type FDCAN_TXESC_Register is record
      --  Tx Buffer Data Field Size:
      TBDS          : FDCAN_TXESC_TBDS_Field := 16#0#;
      --  unspecified
      Reserved_3_31 : STM32H753.UInt29 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TXESC_Register use record
      TBDS          at 0 range 0 .. 2;
      Reserved_3_31 at 0 range 3 .. 31;
   end record;

   subtype FDCAN_TXEFC_EFSA_Field is STM32H753.UInt14;
   subtype FDCAN_TXEFC_EFS_Field is STM32H753.UInt6;
   subtype FDCAN_TXEFC_EFWM_Field is STM32H753.UInt6;

   --  FDCAN Tx Event FIFO Configuration Register
   type FDCAN_TXEFC_Register is record
      --  unspecified
      Reserved_0_1   : STM32H753.UInt2 := 16#0#;
      --  Event FIFO Start Address
      EFSA           : FDCAN_TXEFC_EFSA_Field := 16#0#;
      --  Event FIFO Size
      EFS            : FDCAN_TXEFC_EFS_Field := 16#0#;
      --  unspecified
      Reserved_22_23 : STM32H753.UInt2 := 16#0#;
      --  Event FIFO Watermark
      EFWM           : FDCAN_TXEFC_EFWM_Field := 16#0#;
      --  unspecified
      Reserved_30_31 : STM32H753.UInt2 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TXEFC_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      EFSA           at 0 range 2 .. 15;
      EFS            at 0 range 16 .. 21;
      Reserved_22_23 at 0 range 22 .. 23;
      EFWM           at 0 range 24 .. 29;
      Reserved_30_31 at 0 range 30 .. 31;
   end record;

   subtype FDCAN_TXEFS_EFFL_Field is STM32H753.UInt6;
   subtype FDCAN_TXEFS_EFGI_Field is STM32H753.UInt5;
   subtype FDCAN_TXEFS_EFPI_Field is STM32H753.UInt5;

   --  FDCAN Tx Event FIFO Status Register
   type FDCAN_TXEFS_Register is record
      --  Event FIFO Fill Level
      EFFL           : FDCAN_TXEFS_EFFL_Field := 16#0#;
      --  unspecified
      Reserved_6_7   : STM32H753.UInt2 := 16#0#;
      --  Event FIFO Get Index.
      EFGI           : FDCAN_TXEFS_EFGI_Field := 16#0#;
      --  unspecified
      Reserved_13_15 : STM32H753.UInt3 := 16#0#;
      --  Event FIFO put index.
      EFPI           : FDCAN_TXEFS_EFPI_Field := 16#0#;
      --  unspecified
      Reserved_21_23 : STM32H753.UInt3 := 16#0#;
      --  Event FIFO Full.
      EFF            : Boolean := False;
      --  Tx Event FIFO Element Lost.
      TEFL           : Boolean := False;
      --  unspecified
      Reserved_26_31 : STM32H753.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TXEFS_Register use record
      EFFL           at 0 range 0 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      EFGI           at 0 range 8 .. 12;
      Reserved_13_15 at 0 range 13 .. 15;
      EFPI           at 0 range 16 .. 20;
      Reserved_21_23 at 0 range 21 .. 23;
      EFF            at 0 range 24 .. 24;
      TEFL           at 0 range 25 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype FDCAN_TXEFA_EFAI_Field is STM32H753.UInt5;

   --  FDCAN Tx Event FIFO Acknowledge Register
   type FDCAN_TXEFA_Register is record
      --  Event FIFO Acknowledge Index
      EFAI          : FDCAN_TXEFA_EFAI_Field := 16#0#;
      --  unspecified
      Reserved_5_31 : STM32H753.UInt27 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TXEFA_Register use record
      EFAI          at 0 range 0 .. 4;
      Reserved_5_31 at 0 range 5 .. 31;
   end record;

   subtype FDCAN_TTTMC_TMSA_Field is STM32H753.UInt14;
   subtype FDCAN_TTTMC_TME_Field is STM32H753.UInt7;

   --  FDCAN TT Trigger Memory Configuration Register
   type FDCAN_TTTMC_Register is record
      --  unspecified
      Reserved_0_1   : STM32H753.UInt2 := 16#0#;
      --  Trigger Memory Start Address
      TMSA           : FDCAN_TTTMC_TMSA_Field := 16#0#;
      --  Trigger Memory Elements
      TME            : FDCAN_TTTMC_TME_Field := 16#0#;
      --  unspecified
      Reserved_23_31 : STM32H753.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TTTMC_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      TMSA           at 0 range 2 .. 15;
      TME            at 0 range 16 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   subtype FDCAN_TTRMC_RID_Field is STM32H753.UInt29;

   --  FDCAN TT Reference Message Configuration Register
   type FDCAN_TTRMC_Register is record
      --  Reference Identifier.
      RID            : FDCAN_TTRMC_RID_Field := 16#0#;
      --  unspecified
      Reserved_29_29 : STM32H753.Bit := 16#0#;
      --  Extended Identifier
      XTD            : Boolean := False;
      --  Reference Message Payload Select
      RMPS           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TTRMC_Register use record
      RID            at 0 range 0 .. 28;
      Reserved_29_29 at 0 range 29 .. 29;
      XTD            at 0 range 30 .. 30;
      RMPS           at 0 range 31 .. 31;
   end record;

   subtype FDCAN_TTOCF_OM_Field is STM32H753.UInt2;
   subtype FDCAN_TTOCF_LDSDL_Field is STM32H753.UInt3;
   subtype FDCAN_TTOCF_IRTO_Field is STM32H753.UInt7;
   subtype FDCAN_TTOCF_AWL_Field is STM32H753.Byte;

   --  FDCAN TT Operation Configuration Register
   type FDCAN_TTOCF_Register is record
      --  Operation Mode
      OM             : FDCAN_TTOCF_OM_Field := 16#0#;
      --  unspecified
      Reserved_2_2   : STM32H753.Bit := 16#0#;
      --  Gap Enable
      GEN            : Boolean := False;
      --  Time Master
      TM             : Boolean := False;
      --  LD of Synchronization Deviation Limit
      LDSDL          : FDCAN_TTOCF_LDSDL_Field := 16#0#;
      --  Initial Reference Trigger Offset
      IRTO           : FDCAN_TTOCF_IRTO_Field := 16#0#;
      --  Enable External Clock Synchronization
      EECS           : Boolean := False;
      --  Application Watchdog Limit
      AWL            : FDCAN_TTOCF_AWL_Field := 16#0#;
      --  Enable Global Time Filtering
      EGTF           : Boolean := False;
      --  Enable Clock Calibration
      ECC            : Boolean := False;
      --  Event Trigger Polarity
      EVTP           : Boolean := False;
      --  unspecified
      Reserved_27_31 : STM32H753.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TTOCF_Register use record
      OM             at 0 range 0 .. 1;
      Reserved_2_2   at 0 range 2 .. 2;
      GEN            at 0 range 3 .. 3;
      TM             at 0 range 4 .. 4;
      LDSDL          at 0 range 5 .. 7;
      IRTO           at 0 range 8 .. 14;
      EECS           at 0 range 15 .. 15;
      AWL            at 0 range 16 .. 23;
      EGTF           at 0 range 24 .. 24;
      ECC            at 0 range 25 .. 25;
      EVTP           at 0 range 26 .. 26;
      Reserved_27_31 at 0 range 27 .. 31;
   end record;

   subtype FDCAN_TTMLM_CCM_Field is STM32H753.UInt6;
   subtype FDCAN_TTMLM_CSS_Field is STM32H753.UInt2;
   subtype FDCAN_TTMLM_TXEW_Field is STM32H753.UInt4;
   subtype FDCAN_TTMLM_ENTT_Field is STM32H753.UInt12;

   --  FDCAN TT Matrix Limits Register
   type FDCAN_TTMLM_Register is record
      --  Cycle Count Max
      CCM            : FDCAN_TTMLM_CCM_Field := 16#0#;
      --  Cycle Start Synchronization
      CSS            : FDCAN_TTMLM_CSS_Field := 16#0#;
      --  Tx Enable Window
      TXEW           : FDCAN_TTMLM_TXEW_Field := 16#0#;
      --  unspecified
      Reserved_12_15 : STM32H753.UInt4 := 16#0#;
      --  Expected Number of Tx Triggers
      ENTT           : FDCAN_TTMLM_ENTT_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : STM32H753.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TTMLM_Register use record
      CCM            at 0 range 0 .. 5;
      CSS            at 0 range 6 .. 7;
      TXEW           at 0 range 8 .. 11;
      Reserved_12_15 at 0 range 12 .. 15;
      ENTT           at 0 range 16 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype FDCAN_TURCF_NCL_Field is STM32H753.UInt16;
   subtype FDCAN_TURCF_DC_Field is STM32H753.UInt14;

   --  FDCAN TUR Configuration Register
   type FDCAN_TURCF_Register is record
      --  Numerator Configuration Low.
      NCL            : FDCAN_TURCF_NCL_Field := 16#0#;
      --  Denominator Configuration.
      DC             : FDCAN_TURCF_DC_Field := 16#0#;
      --  unspecified
      Reserved_30_30 : STM32H753.Bit := 16#0#;
      --  Enable Local Time
      ELT            : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TURCF_Register use record
      NCL            at 0 range 0 .. 15;
      DC             at 0 range 16 .. 29;
      Reserved_30_30 at 0 range 30 .. 30;
      ELT            at 0 range 31 .. 31;
   end record;

   subtype FDCAN_TTOCN_SWS_Field is STM32H753.UInt2;
   subtype FDCAN_TTOCN_TMC_Field is STM32H753.UInt2;

   --  FDCAN TT Operation Control Register
   type FDCAN_TTOCN_Register is record
      --  Set Global time
      SGT            : Boolean := False;
      --  External Clock Synchronization
      ECS            : Boolean := False;
      --  Stop Watch Polarity
      SWP            : Boolean := False;
      --  Stop Watch Source.
      SWS            : FDCAN_TTOCN_SWS_Field := 16#0#;
      --  Register Time Mark Interrupt Pulse Enable
      RTIE           : Boolean := False;
      --  Register Time Mark Compare
      TMC            : FDCAN_TTOCN_TMC_Field := 16#0#;
      --  Trigger Time Mark Interrupt Pulse Enable
      TTIE           : Boolean := False;
      --  Gap Control Select
      GCS            : Boolean := False;
      --  Finish Gap.
      FGP            : Boolean := False;
      --  Time Mark Gap
      TMG            : Boolean := False;
      --  Next is Gap
      NIG            : Boolean := False;
      --  External Synchronization Control
      ESCN           : Boolean := False;
      --  unspecified
      Reserved_14_14 : STM32H753.Bit := 16#0#;
      --  TT Operation Control Register Locked
      LCKC           : Boolean := False;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TTOCN_Register use record
      SGT            at 0 range 0 .. 0;
      ECS            at 0 range 1 .. 1;
      SWP            at 0 range 2 .. 2;
      SWS            at 0 range 3 .. 4;
      RTIE           at 0 range 5 .. 5;
      TMC            at 0 range 6 .. 7;
      TTIE           at 0 range 8 .. 8;
      GCS            at 0 range 9 .. 9;
      FGP            at 0 range 10 .. 10;
      TMG            at 0 range 11 .. 11;
      NIG            at 0 range 12 .. 12;
      ESCN           at 0 range 13 .. 13;
      Reserved_14_14 at 0 range 14 .. 14;
      LCKC           at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype CAN_TTGTP_NCL_Field is STM32H753.UInt16;
   subtype CAN_TTGTP_CTP_Field is STM32H753.UInt16;

   --  FDCAN TT Global Time Preset Register
   type CAN_TTGTP_Register is record
      --  Time Preset
      NCL : CAN_TTGTP_NCL_Field := 16#0#;
      --  Cycle Time Target Phase
      CTP : CAN_TTGTP_CTP_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CAN_TTGTP_Register use record
      NCL at 0 range 0 .. 15;
      CTP at 0 range 16 .. 31;
   end record;

   subtype FDCAN_TTTMK_TM_Field is STM32H753.UInt16;
   subtype FDCAN_TTTMK_TICC_Field is STM32H753.UInt7;

   --  FDCAN TT Time Mark Register
   type FDCAN_TTTMK_Register is record
      --  Time Mark
      TM             : FDCAN_TTTMK_TM_Field := 16#0#;
      --  Time Mark Cycle Code
      TICC           : FDCAN_TTTMK_TICC_Field := 16#0#;
      --  unspecified
      Reserved_23_30 : STM32H753.Byte := 16#0#;
      --  TT Time Mark Register Locked
      LCKM           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TTTMK_Register use record
      TM             at 0 range 0 .. 15;
      TICC           at 0 range 16 .. 22;
      Reserved_23_30 at 0 range 23 .. 30;
      LCKM           at 0 range 31 .. 31;
   end record;

   --  FDCAN_TTIR_SE array
   type FDCAN_TTIR_SE_Field_Array is array (1 .. 2) of Boolean
     with Component_Size => 1, Size => 2;

   --  Type definition for FDCAN_TTIR_SE
   type FDCAN_TTIR_SE_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SE as a value
            Val : STM32H753.UInt2;
         when True =>
            --  SE as an array
            Arr : FDCAN_TTIR_SE_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 2;

   for FDCAN_TTIR_SE_Field use record
      Val at 0 range 0 .. 1;
      Arr at 0 range 0 .. 1;
   end record;

   --  FDCAN TT Interrupt Register
   type FDCAN_TTIR_Register is record
      --  Start of Basic Cycle
      SBC            : Boolean := False;
      --  Start of Matrix Cycle
      SMC            : Boolean := False;
      --  Change of Synchronization Mode
      CSM            : Boolean := False;
      --  Start of Gap
      SOG            : Boolean := False;
      --  Register Time Mark Interrupt.
      RTMI           : Boolean := False;
      --  Trigger Time Mark Event Internal
      TTMI           : Boolean := False;
      --  Stop Watch Event
      SWE            : Boolean := False;
      --  Global Time Wrap
      GTW            : Boolean := False;
      --  Global Time Discontinuity
      GTD            : Boolean := False;
      --  Global Time Error
      GTE            : Boolean := False;
      --  Tx Count Underflow
      TXU            : Boolean := False;
      --  Tx Count Overflow
      TXO            : Boolean := False;
      --  Scheduling Error 1
      SE             : FDCAN_TTIR_SE_Field :=
                        (As_Array => False, Val => 16#0#);
      --  Error Level Changed.
      ELC            : Boolean := False;
      --  Initialization Watch Trigger
      IWTG           : Boolean := False;
      --  Watch Trigger
      WT             : Boolean := False;
      --  Application Watchdog
      AW             : Boolean := False;
      --  Configuration Error
      CER            : Boolean := False;
      --  unspecified
      Reserved_19_31 : STM32H753.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TTIR_Register use record
      SBC            at 0 range 0 .. 0;
      SMC            at 0 range 1 .. 1;
      CSM            at 0 range 2 .. 2;
      SOG            at 0 range 3 .. 3;
      RTMI           at 0 range 4 .. 4;
      TTMI           at 0 range 5 .. 5;
      SWE            at 0 range 6 .. 6;
      GTW            at 0 range 7 .. 7;
      GTD            at 0 range 8 .. 8;
      GTE            at 0 range 9 .. 9;
      TXU            at 0 range 10 .. 10;
      TXO            at 0 range 11 .. 11;
      SE             at 0 range 12 .. 13;
      ELC            at 0 range 14 .. 14;
      IWTG           at 0 range 15 .. 15;
      WT             at 0 range 16 .. 16;
      AW             at 0 range 17 .. 17;
      CER            at 0 range 18 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   --  FDCAN TT Interrupt Enable Register
   type FDCAN_TTIE_Register is record
      --  Start of Basic Cycle Interrupt Enable
      SBCE           : Boolean := False;
      --  Start of Matrix Cycle Interrupt Enable
      SMCE           : Boolean := False;
      --  Change of Synchronization Mode Interrupt Enable
      CSME           : Boolean := False;
      --  Start of Gap Interrupt Enable
      SOGE           : Boolean := False;
      --  Register Time Mark Interrupt Enable
      RTMIE          : Boolean := False;
      --  Trigger Time Mark Event Internal Interrupt Enable
      TTMIE          : Boolean := False;
      --  Stop Watch Event Interrupt Enable
      SWEE           : Boolean := False;
      --  Global Time Wrap Interrupt Enable
      GTWE           : Boolean := False;
      --  Global Time Discontinuity Interrupt Enable
      GTDE           : Boolean := False;
      --  Global Time Error Interrupt Enable
      GTEE           : Boolean := False;
      --  Tx Count Underflow Interrupt Enable
      TXUE           : Boolean := False;
      --  Tx Count Overflow Interrupt Enable
      TXOE           : Boolean := False;
      --  Scheduling Error 1 Interrupt Enable
      SE1E           : Boolean := False;
      --  Scheduling Error 2 Interrupt Enable
      SE2E           : Boolean := False;
      --  Change Error Level Interrupt Enable
      ELCE           : Boolean := False;
      --  Initialization Watch Trigger Interrupt Enable
      IWTGE          : Boolean := False;
      --  Watch Trigger Interrupt Enable
      WTE            : Boolean := False;
      --  Application Watchdog Interrupt Enable
      AWE            : Boolean := False;
      --  Configuration Error Interrupt Enable
      CERE           : Boolean := False;
      --  unspecified
      Reserved_19_31 : STM32H753.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TTIE_Register use record
      SBCE           at 0 range 0 .. 0;
      SMCE           at 0 range 1 .. 1;
      CSME           at 0 range 2 .. 2;
      SOGE           at 0 range 3 .. 3;
      RTMIE          at 0 range 4 .. 4;
      TTMIE          at 0 range 5 .. 5;
      SWEE           at 0 range 6 .. 6;
      GTWE           at 0 range 7 .. 7;
      GTDE           at 0 range 8 .. 8;
      GTEE           at 0 range 9 .. 9;
      TXUE           at 0 range 10 .. 10;
      TXOE           at 0 range 11 .. 11;
      SE1E           at 0 range 12 .. 12;
      SE2E           at 0 range 13 .. 13;
      ELCE           at 0 range 14 .. 14;
      IWTGE          at 0 range 15 .. 15;
      WTE            at 0 range 16 .. 16;
      AWE            at 0 range 17 .. 17;
      CERE           at 0 range 18 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   --  FDCAN TT Interrupt Line Select Register
   type FDCAN_TTILS_Register is record
      --  Start of Basic Cycle Interrupt Line
      SBCL           : Boolean := False;
      --  Start of Matrix Cycle Interrupt Line
      SMCL           : Boolean := False;
      --  Change of Synchronization Mode Interrupt Line
      CSML           : Boolean := False;
      --  Start of Gap Interrupt Line
      SOGL           : Boolean := False;
      --  Register Time Mark Interrupt Line
      RTMIL          : Boolean := False;
      --  Trigger Time Mark Event Internal Interrupt Line
      TTMIL          : Boolean := False;
      --  Stop Watch Event Interrupt Line
      SWEL           : Boolean := False;
      --  Global Time Wrap Interrupt Line
      GTWL           : Boolean := False;
      --  Global Time Discontinuity Interrupt Line
      GTDL           : Boolean := False;
      --  Global Time Error Interrupt Line
      GTEL           : Boolean := False;
      --  Tx Count Underflow Interrupt Line
      TXUL           : Boolean := False;
      --  Tx Count Overflow Interrupt Line
      TXOL           : Boolean := False;
      --  Scheduling Error 1 Interrupt Line
      SE1L           : Boolean := False;
      --  Scheduling Error 2 Interrupt Line
      SE2L           : Boolean := False;
      --  Change Error Level Interrupt Line
      ELCL           : Boolean := False;
      --  Initialization Watch Trigger Interrupt Line
      IWTGL          : Boolean := False;
      --  Watch Trigger Interrupt Line
      WTL            : Boolean := False;
      --  Application Watchdog Interrupt Line
      AWL            : Boolean := False;
      --  Configuration Error Interrupt Line
      CERL           : Boolean := False;
      --  unspecified
      Reserved_19_31 : STM32H753.UInt13 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TTILS_Register use record
      SBCL           at 0 range 0 .. 0;
      SMCL           at 0 range 1 .. 1;
      CSML           at 0 range 2 .. 2;
      SOGL           at 0 range 3 .. 3;
      RTMIL          at 0 range 4 .. 4;
      TTMIL          at 0 range 5 .. 5;
      SWEL           at 0 range 6 .. 6;
      GTWL           at 0 range 7 .. 7;
      GTDL           at 0 range 8 .. 8;
      GTEL           at 0 range 9 .. 9;
      TXUL           at 0 range 10 .. 10;
      TXOL           at 0 range 11 .. 11;
      SE1L           at 0 range 12 .. 12;
      SE2L           at 0 range 13 .. 13;
      ELCL           at 0 range 14 .. 14;
      IWTGL          at 0 range 15 .. 15;
      WTL            at 0 range 16 .. 16;
      AWL            at 0 range 17 .. 17;
      CERL           at 0 range 18 .. 18;
      Reserved_19_31 at 0 range 19 .. 31;
   end record;

   subtype FDCAN_TTOST_EL_Field is STM32H753.UInt2;
   subtype FDCAN_TTOST_MS_Field is STM32H753.UInt2;
   subtype FDCAN_TTOST_SYS_Field is STM32H753.UInt2;
   subtype FDCAN_TTOST_RTO_Field is STM32H753.Byte;
   subtype FDCAN_TTOST_TMP_Field is STM32H753.UInt3;

   --  FDCAN TT Operation Status Register
   type FDCAN_TTOST_Register is record
      --  Error Level
      EL             : FDCAN_TTOST_EL_Field := 16#0#;
      --  Master State.
      MS             : FDCAN_TTOST_MS_Field := 16#0#;
      --  Synchronization State
      SYS            : FDCAN_TTOST_SYS_Field := 16#0#;
      --  Quality of Global Time Phase
      GTP            : Boolean := False;
      --  Quality of Clock Speed
      QCS            : Boolean := False;
      --  Reference Trigger Offset
      RTO            : FDCAN_TTOST_RTO_Field := 16#0#;
      --  unspecified
      Reserved_16_21 : STM32H753.UInt6 := 16#0#;
      --  Wait for Global Time Discontinuity
      WGTD           : Boolean := False;
      --  Gap Finished Indicator.
      GFI            : Boolean := False;
      --  Time Master Priority
      TMP            : FDCAN_TTOST_TMP_Field := 16#0#;
      --  Gap Started Indicator.
      GSI            : Boolean := False;
      --  Wait for Event
      WFE            : Boolean := False;
      --  Application Watchdog Event
      AWE            : Boolean := False;
      --  Wait for External Clock Synchronization
      WECS           : Boolean := False;
      --  Schedule Phase Lock
      SPL            : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TTOST_Register use record
      EL             at 0 range 0 .. 1;
      MS             at 0 range 2 .. 3;
      SYS            at 0 range 4 .. 5;
      GTP            at 0 range 6 .. 6;
      QCS            at 0 range 7 .. 7;
      RTO            at 0 range 8 .. 15;
      Reserved_16_21 at 0 range 16 .. 21;
      WGTD           at 0 range 22 .. 22;
      GFI            at 0 range 23 .. 23;
      TMP            at 0 range 24 .. 26;
      GSI            at 0 range 27 .. 27;
      WFE            at 0 range 28 .. 28;
      AWE            at 0 range 29 .. 29;
      WECS           at 0 range 30 .. 30;
      SPL            at 0 range 31 .. 31;
   end record;

   subtype FDCAN_TURNA_NAV_Field is STM32H753.UInt18;

   --  FDCAN TUR Numerator Actual Register
   type FDCAN_TURNA_Register is record
      --  Read-only. Numerator Actual Value
      NAV            : FDCAN_TURNA_NAV_Field;
      --  unspecified
      Reserved_18_31 : STM32H753.UInt14;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TURNA_Register use record
      NAV            at 0 range 0 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   subtype FDCAN_TTLGT_LT_Field is STM32H753.UInt16;
   subtype FDCAN_TTLGT_GT_Field is STM32H753.UInt16;

   --  FDCAN TT Local and Global Time Register
   type FDCAN_TTLGT_Register is record
      --  Read-only. Local Time
      LT : FDCAN_TTLGT_LT_Field;
      --  Read-only. Global Time
      GT : FDCAN_TTLGT_GT_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TTLGT_Register use record
      LT at 0 range 0 .. 15;
      GT at 0 range 16 .. 31;
   end record;

   subtype FDCAN_TTCTC_CT_Field is STM32H753.UInt16;
   subtype FDCAN_TTCTC_CC_Field is STM32H753.UInt6;

   --  FDCAN TT Cycle Time and Count Register
   type FDCAN_TTCTC_Register is record
      --  Read-only. Cycle Time
      CT             : FDCAN_TTCTC_CT_Field;
      --  Read-only. Cycle Count
      CC             : FDCAN_TTCTC_CC_Field;
      --  unspecified
      Reserved_22_31 : STM32H753.UInt10;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TTCTC_Register use record
      CT             at 0 range 0 .. 15;
      CC             at 0 range 16 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   subtype FDCAN_TTCPT_CT_Field is STM32H753.UInt6;
   subtype FDCAN_TTCPT_SWV_Field is STM32H753.UInt16;

   --  FDCAN TT Capture Time Register
   type FDCAN_TTCPT_Register is record
      --  Read-only. Cycle Count Value
      CT            : FDCAN_TTCPT_CT_Field;
      --  unspecified
      Reserved_6_15 : STM32H753.UInt10;
      --  Read-only. Stop Watch Value
      SWV           : FDCAN_TTCPT_SWV_Field;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TTCPT_Register use record
      CT            at 0 range 0 .. 5;
      Reserved_6_15 at 0 range 6 .. 15;
      SWV           at 0 range 16 .. 31;
   end record;

   subtype FDCAN_TTCSM_CSM_Field is STM32H753.UInt16;

   --  FDCAN TT Cycle Sync Mark Register
   type FDCAN_TTCSM_Register is record
      --  Read-only. Cycle Sync Mark
      CSM            : FDCAN_TTCSM_CSM_Field;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TTCSM_Register use record
      CSM            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype FDCAN_TTTS_SWTDEL_Field is STM32H753.UInt2;
   subtype FDCAN_TTTS_EVTSEL_Field is STM32H753.UInt2;

   --  FDCAN TT Trigger Select Register
   type FDCAN_TTTS_Register is record
      --  Stop watch trigger input selection
      SWTDEL        : FDCAN_TTTS_SWTDEL_Field := 16#0#;
      --  unspecified
      Reserved_2_3  : STM32H753.UInt2 := 16#0#;
      --  Event trigger input selection
      EVTSEL        : FDCAN_TTTS_EVTSEL_Field := 16#0#;
      --  unspecified
      Reserved_6_31 : STM32H753.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for FDCAN_TTTS_Register use record
      SWTDEL        at 0 range 0 .. 1;
      Reserved_2_3  at 0 range 2 .. 3;
      EVTSEL        at 0 range 4 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  CCU registers
   type CAN_CCU_Peripheral is record
      --  Clock Calibration Unit Core Release Register
      CREL  : aliased CREL_Register;
      --  Calibration Configuration Register
      CCFG  : aliased CCFG_Register;
      --  Calibration Status Register
      CSTAT : aliased CSTAT_Register;
      --  Calibration Watchdog Register
      CWD   : aliased CWD_Register;
      --  Clock Calibration Unit Interrupt Register
      IR    : aliased IR_Register;
      --  Clock Calibration Unit Interrupt Enable Register
      IE    : aliased IE_Register;
   end record
     with Volatile;

   for CAN_CCU_Peripheral use record
      CREL  at 16#0# range 0 .. 31;
      CCFG  at 16#4# range 0 .. 31;
      CSTAT at 16#8# range 0 .. 31;
      CWD   at 16#C# range 0 .. 31;
      IR    at 16#10# range 0 .. 31;
      IE    at 16#14# range 0 .. 31;
   end record;

   --  CCU registers
   CAN_CCU_Periph : aliased CAN_CCU_Peripheral
     with Import, Address => CAN_CCU_Base;

   --  FDCAN1
   type FDCAN_Peripheral is record
      --  FDCAN Core Release Register
      FDCAN_CREL   : aliased FDCAN_CREL_Register;
      --  FDCAN Core Release Register
      FDCAN_ENDN   : aliased STM32H753.UInt32;
      --  FDCAN Data Bit Timing and Prescaler Register
      FDCAN_DBTP   : aliased FDCAN_DBTP_Register;
      --  FDCAN Test Register
      FDCAN_TEST   : aliased FDCAN_TEST_Register;
      --  FDCAN RAM Watchdog Register
      FDCAN_RWD    : aliased FDCAN_RWD_Register;
      --  FDCAN CC Control Register
      FDCAN_CCCR   : aliased FDCAN_CCCR_Register;
      --  FDCAN Nominal Bit Timing and Prescaler Register
      FDCAN_NBTP   : aliased FDCAN_NBTP_Register;
      --  FDCAN Timestamp Counter Configuration Register
      FDCAN_TSCC   : aliased FDCAN_TSCC_Register;
      --  FDCAN Timestamp Counter Value Register
      FDCAN_TSCV   : aliased FDCAN_TSCV_Register;
      --  FDCAN Timeout Counter Configuration Register
      FDCAN_TOCC   : aliased FDCAN_TOCC_Register;
      --  FDCAN Timeout Counter Value Register
      FDCAN_TOCV   : aliased FDCAN_TOCV_Register;
      --  FDCAN Error Counter Register
      FDCAN_ECR    : aliased FDCAN_ECR_Register;
      --  FDCAN Protocol Status Register
      FDCAN_PSR    : aliased FDCAN_PSR_Register;
      --  FDCAN Transmitter Delay Compensation Register
      FDCAN_TDCR   : aliased FDCAN_TDCR_Register;
      --  FDCAN Interrupt Register
      FDCAN_IR     : aliased FDCAN_IR_Register;
      --  FDCAN Interrupt Enable Register
      FDCAN_IE     : aliased FDCAN_IE_Register;
      --  FDCAN Interrupt Line Select Register
      FDCAN_ILS    : aliased FDCAN_ILS_Register;
      --  FDCAN Interrupt Line Enable Register
      FDCAN_ILE    : aliased FDCAN_ILE_Register;
      --  FDCAN Global Filter Configuration Register
      FDCAN_GFC    : aliased FDCAN_GFC_Register;
      --  FDCAN Standard ID Filter Configuration Register
      FDCAN_SIDFC  : aliased FDCAN_SIDFC_Register;
      --  FDCAN Extended ID Filter Configuration Register
      FDCAN_XIDFC  : aliased FDCAN_XIDFC_Register;
      --  FDCAN Extended ID and Mask Register
      FDCAN_XIDAM  : aliased FDCAN_XIDAM_Register;
      --  FDCAN High Priority Message Status Register
      FDCAN_HPMS   : aliased FDCAN_HPMS_Register;
      --  FDCAN New Data 1 Register
      FDCAN_NDAT1  : aliased FDCAN_NDAT1_Register;
      --  FDCAN New Data 2 Register
      FDCAN_NDAT2  : aliased FDCAN_NDAT2_Register;
      --  FDCAN Rx FIFO 0 Configuration Register
      FDCAN_RXF0C  : aliased FDCAN_RXF0C_Register;
      --  FDCAN Rx FIFO 0 Status Register
      FDCAN_RXF0S  : aliased FDCAN_RXF0S_Register;
      --  CAN Rx FIFO 0 Acknowledge Register
      FDCAN_RXF0A  : aliased FDCAN_RXF0A_Register;
      --  FDCAN Rx Buffer Configuration Register
      FDCAN_RXBC   : aliased FDCAN_RXBC_Register;
      --  FDCAN Rx FIFO 1 Configuration Register
      FDCAN_RXF1C  : aliased FDCAN_RXF1C_Register;
      --  FDCAN Rx FIFO 1 Status Register
      FDCAN_RXF1S  : aliased FDCAN_RXF1S_Register;
      --  FDCAN Rx FIFO 1 Acknowledge Register
      FDCAN_RXF1A  : aliased FDCAN_RXF1A_Register;
      --  FDCAN Rx Buffer Element Size Configuration Register
      FDCAN_RXESC  : aliased FDCAN_RXESC_Register;
      --  FDCAN Tx Buffer Configuration Register
      FDCAN_TXBC   : aliased FDCAN_TXBC_Register;
      --  FDCAN Tx FIFO/Queue Status Register
      FDCAN_TXFQS  : aliased FDCAN_TXFQS_Register;
      --  FDCAN Tx Buffer Element Size Configuration Register
      FDCAN_TXESC  : aliased FDCAN_TXESC_Register;
      --  FDCAN Tx Buffer Request Pending Register
      FDCAN_TXBRP  : aliased STM32H753.UInt32;
      --  FDCAN Tx Buffer Add Request Register
      FDCAN_TXBAR  : aliased STM32H753.UInt32;
      --  FDCAN Tx Buffer Cancellation Request Register
      FDCAN_TXBCR  : aliased STM32H753.UInt32;
      --  FDCAN Tx Buffer Transmission Occurred Register
      FDCAN_TXBTO  : aliased STM32H753.UInt32;
      --  FDCAN Tx Buffer Cancellation Finished Register
      FDCAN_TXBCF  : aliased STM32H753.UInt32;
      --  FDCAN Tx Buffer Transmission Interrupt Enable Register
      FDCAN_TXBTIE : aliased STM32H753.UInt32;
      --  FDCAN Tx Buffer Cancellation Finished Interrupt Enable Register
      FDCAN_TXBCIE : aliased STM32H753.UInt32;
      --  FDCAN Tx Event FIFO Configuration Register
      FDCAN_TXEFC  : aliased FDCAN_TXEFC_Register;
      --  FDCAN Tx Event FIFO Status Register
      FDCAN_TXEFS  : aliased FDCAN_TXEFS_Register;
      --  FDCAN Tx Event FIFO Acknowledge Register
      FDCAN_TXEFA  : aliased FDCAN_TXEFA_Register;
      --  FDCAN TT Trigger Memory Configuration Register
      FDCAN_TTTMC  : aliased FDCAN_TTTMC_Register;
      --  FDCAN TT Reference Message Configuration Register
      FDCAN_TTRMC  : aliased FDCAN_TTRMC_Register;
      --  FDCAN TT Operation Configuration Register
      FDCAN_TTOCF  : aliased FDCAN_TTOCF_Register;
      --  FDCAN TT Matrix Limits Register
      FDCAN_TTMLM  : aliased FDCAN_TTMLM_Register;
      --  FDCAN TUR Configuration Register
      FDCAN_TURCF  : aliased FDCAN_TURCF_Register;
      --  FDCAN TT Operation Control Register
      FDCAN_TTOCN  : aliased FDCAN_TTOCN_Register;
      --  FDCAN TT Global Time Preset Register
      CAN_TTGTP    : aliased CAN_TTGTP_Register;
      --  FDCAN TT Time Mark Register
      FDCAN_TTTMK  : aliased FDCAN_TTTMK_Register;
      --  FDCAN TT Interrupt Register
      FDCAN_TTIR   : aliased FDCAN_TTIR_Register;
      --  FDCAN TT Interrupt Enable Register
      FDCAN_TTIE   : aliased FDCAN_TTIE_Register;
      --  FDCAN TT Interrupt Line Select Register
      FDCAN_TTILS  : aliased FDCAN_TTILS_Register;
      --  FDCAN TT Operation Status Register
      FDCAN_TTOST  : aliased FDCAN_TTOST_Register;
      --  FDCAN TUR Numerator Actual Register
      FDCAN_TURNA  : aliased FDCAN_TURNA_Register;
      --  FDCAN TT Local and Global Time Register
      FDCAN_TTLGT  : aliased FDCAN_TTLGT_Register;
      --  FDCAN TT Cycle Time and Count Register
      FDCAN_TTCTC  : aliased FDCAN_TTCTC_Register;
      --  FDCAN TT Capture Time Register
      FDCAN_TTCPT  : aliased FDCAN_TTCPT_Register;
      --  FDCAN TT Cycle Sync Mark Register
      FDCAN_TTCSM  : aliased FDCAN_TTCSM_Register;
      --  FDCAN TT Trigger Select Register
      FDCAN_TTTS   : aliased FDCAN_TTTS_Register;
   end record
     with Volatile;

   for FDCAN_Peripheral use record
      FDCAN_CREL   at 16#0# range 0 .. 31;
      FDCAN_ENDN   at 16#4# range 0 .. 31;
      FDCAN_DBTP   at 16#C# range 0 .. 31;
      FDCAN_TEST   at 16#10# range 0 .. 31;
      FDCAN_RWD    at 16#14# range 0 .. 31;
      FDCAN_CCCR   at 16#18# range 0 .. 31;
      FDCAN_NBTP   at 16#1C# range 0 .. 31;
      FDCAN_TSCC   at 16#20# range 0 .. 31;
      FDCAN_TSCV   at 16#24# range 0 .. 31;
      FDCAN_TOCC   at 16#28# range 0 .. 31;
      FDCAN_TOCV   at 16#2C# range 0 .. 31;
      FDCAN_ECR    at 16#40# range 0 .. 31;
      FDCAN_PSR    at 16#44# range 0 .. 31;
      FDCAN_TDCR   at 16#48# range 0 .. 31;
      FDCAN_IR     at 16#50# range 0 .. 31;
      FDCAN_IE     at 16#54# range 0 .. 31;
      FDCAN_ILS    at 16#58# range 0 .. 31;
      FDCAN_ILE    at 16#5C# range 0 .. 31;
      FDCAN_GFC    at 16#80# range 0 .. 31;
      FDCAN_SIDFC  at 16#84# range 0 .. 31;
      FDCAN_XIDFC  at 16#88# range 0 .. 31;
      FDCAN_XIDAM  at 16#90# range 0 .. 31;
      FDCAN_HPMS   at 16#94# range 0 .. 31;
      FDCAN_NDAT1  at 16#98# range 0 .. 31;
      FDCAN_NDAT2  at 16#9C# range 0 .. 31;
      FDCAN_RXF0C  at 16#A0# range 0 .. 31;
      FDCAN_RXF0S  at 16#A4# range 0 .. 31;
      FDCAN_RXF0A  at 16#A8# range 0 .. 31;
      FDCAN_RXBC   at 16#AC# range 0 .. 31;
      FDCAN_RXF1C  at 16#B0# range 0 .. 31;
      FDCAN_RXF1S  at 16#B4# range 0 .. 31;
      FDCAN_RXF1A  at 16#B8# range 0 .. 31;
      FDCAN_RXESC  at 16#BC# range 0 .. 31;
      FDCAN_TXBC   at 16#C0# range 0 .. 31;
      FDCAN_TXFQS  at 16#C4# range 0 .. 31;
      FDCAN_TXESC  at 16#C8# range 0 .. 31;
      FDCAN_TXBRP  at 16#CC# range 0 .. 31;
      FDCAN_TXBAR  at 16#D0# range 0 .. 31;
      FDCAN_TXBCR  at 16#D4# range 0 .. 31;
      FDCAN_TXBTO  at 16#D8# range 0 .. 31;
      FDCAN_TXBCF  at 16#DC# range 0 .. 31;
      FDCAN_TXBTIE at 16#E0# range 0 .. 31;
      FDCAN_TXBCIE at 16#E4# range 0 .. 31;
      FDCAN_TXEFC  at 16#F0# range 0 .. 31;
      FDCAN_TXEFS  at 16#F4# range 0 .. 31;
      FDCAN_TXEFA  at 16#F8# range 0 .. 31;
      FDCAN_TTTMC  at 16#100# range 0 .. 31;
      FDCAN_TTRMC  at 16#104# range 0 .. 31;
      FDCAN_TTOCF  at 16#108# range 0 .. 31;
      FDCAN_TTMLM  at 16#10C# range 0 .. 31;
      FDCAN_TURCF  at 16#110# range 0 .. 31;
      FDCAN_TTOCN  at 16#114# range 0 .. 31;
      CAN_TTGTP    at 16#118# range 0 .. 31;
      FDCAN_TTTMK  at 16#11C# range 0 .. 31;
      FDCAN_TTIR   at 16#120# range 0 .. 31;
      FDCAN_TTIE   at 16#124# range 0 .. 31;
      FDCAN_TTILS  at 16#128# range 0 .. 31;
      FDCAN_TTOST  at 16#12C# range 0 .. 31;
      FDCAN_TURNA  at 16#130# range 0 .. 31;
      FDCAN_TTLGT  at 16#134# range 0 .. 31;
      FDCAN_TTCTC  at 16#138# range 0 .. 31;
      FDCAN_TTCPT  at 16#13C# range 0 .. 31;
      FDCAN_TTCSM  at 16#140# range 0 .. 31;
      FDCAN_TTTS   at 16#300# range 0 .. 31;
   end record;

   --  FDCAN1
   FDCAN1_Periph : aliased FDCAN_Peripheral
     with Import, Address => FDCAN1_Base;

   --  FDCAN1
   FDCAN2_Periph : aliased FDCAN_Peripheral
     with Import, Address => FDCAN2_Base;

end STM32H753.FDCAN;
