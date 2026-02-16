pragma Style_Checks (Off);

--  This spec has been automatically generated from STM32H753.svd


with System;

package STM32H753.RTC is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   subtype RTC_TR_SU_Field is STM32H753.UInt4;
   subtype RTC_TR_ST_Field is STM32H753.UInt3;
   subtype RTC_TR_MNU_Field is STM32H753.UInt4;
   subtype RTC_TR_MNT_Field is STM32H753.UInt3;
   subtype RTC_TR_HU_Field is STM32H753.UInt4;
   subtype RTC_TR_HT_Field is STM32H753.UInt2;

   --  The RTC_TR is the calendar time shadow register. This register must be
   --  written in initialization mode only. Refer to Calendar initialization
   --  and configuration on page9 and Reading the calendar on page10.This
   --  register is write protected. The write access procedure is described in
   --  RTC register write protection on page9.
   type RTC_TR_Register is record
      --  Second units in BCD format
      SU             : RTC_TR_SU_Field := 16#0#;
      --  Second tens in BCD format
      ST             : RTC_TR_ST_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : STM32H753.Bit := 16#0#;
      --  Minute units in BCD format
      MNU            : RTC_TR_MNU_Field := 16#0#;
      --  Minute tens in BCD format
      MNT            : RTC_TR_MNT_Field := 16#0#;
      --  unspecified
      Reserved_15_15 : STM32H753.Bit := 16#0#;
      --  Hour units in BCD format
      HU             : RTC_TR_HU_Field := 16#0#;
      --  Hour tens in BCD format
      HT             : RTC_TR_HT_Field := 16#0#;
      --  AM/PM notation
      PM             : Boolean := False;
      --  unspecified
      Reserved_23_31 : STM32H753.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_TR_Register use record
      SU             at 0 range 0 .. 3;
      ST             at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      MNU            at 0 range 8 .. 11;
      MNT            at 0 range 12 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      HU             at 0 range 16 .. 19;
      HT             at 0 range 20 .. 21;
      PM             at 0 range 22 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   subtype RTC_DR_DU_Field is STM32H753.UInt4;
   subtype RTC_DR_DT_Field is STM32H753.UInt2;
   subtype RTC_DR_MU_Field is STM32H753.UInt4;
   subtype RTC_DR_WDU_Field is STM32H753.UInt3;
   subtype RTC_DR_YU_Field is STM32H753.UInt4;
   subtype RTC_DR_YT_Field is STM32H753.UInt4;

   --  The RTC_DR is the calendar date shadow register. This register must be
   --  written in initialization mode only. Refer to Calendar initialization
   --  and configuration on page9 and Reading the calendar on page10.This
   --  register is write protected. The write access procedure is described in
   --  RTC register write protection on page9.
   type RTC_DR_Register is record
      --  Date units in BCD format
      DU             : RTC_DR_DU_Field := 16#1#;
      --  Date tens in BCD format
      DT             : RTC_DR_DT_Field := 16#0#;
      --  unspecified
      Reserved_6_7   : STM32H753.UInt2 := 16#0#;
      --  Month units in BCD format
      MU             : RTC_DR_MU_Field := 16#1#;
      --  Month tens in BCD format
      MT             : Boolean := False;
      --  Week day units
      WDU            : RTC_DR_WDU_Field := 16#1#;
      --  Year units in BCD format
      YU             : RTC_DR_YU_Field := 16#0#;
      --  Year tens in BCD format
      YT             : RTC_DR_YT_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : STM32H753.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_DR_Register use record
      DU             at 0 range 0 .. 3;
      DT             at 0 range 4 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      MU             at 0 range 8 .. 11;
      MT             at 0 range 12 .. 12;
      WDU            at 0 range 13 .. 15;
      YU             at 0 range 16 .. 19;
      YT             at 0 range 20 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype RTC_CR_WUCKSEL_Field is STM32H753.UInt3;
   subtype RTC_CR_OSEL_Field is STM32H753.UInt2;

   --  RTC control register
   type RTC_CR_Register is record
      --  Wakeup clock selection
      WUCKSEL        : RTC_CR_WUCKSEL_Field := 16#0#;
      --  Time-stamp event active edge TSE must be reset when TSEDGE is changed
      --  to avoid unwanted TSF setting.
      TSEDGE         : Boolean := False;
      --  RTC_REFIN reference clock detection enable (50 or 60Hz) Note:
      --  PREDIV_S must be 0x00FF.
      REFCKON        : Boolean := False;
      --  Bypass the shadow registers Note: If the frequency of the APB clock
      --  is less than seven times the frequency of RTCCLK, BYPSHAD must be set
      --  to 1.
      BYPSHAD        : Boolean := False;
      --  Hour format
      FMT            : Boolean := False;
      --  unspecified
      Reserved_7_7   : STM32H753.Bit := 16#0#;
      --  Alarm A enable
      ALRAE          : Boolean := False;
      --  Alarm B enable
      ALRBE          : Boolean := False;
      --  Wakeup timer enable
      WUTE           : Boolean := False;
      --  timestamp enable
      TSE            : Boolean := False;
      --  Alarm A interrupt enable
      ALRAIE         : Boolean := False;
      --  Alarm B interrupt enable
      ALRBIE         : Boolean := False;
      --  Wakeup timer interrupt enable
      WUTIE          : Boolean := False;
      --  Time-stamp interrupt enable
      TSIE           : Boolean := False;
      --  Write-only. Add 1 hour (summer time change) When this bit is set
      --  outside initialization mode, 1 hour is added to the calendar time.
      --  This bit is always read as 0.
      ADD1H          : Boolean := False;
      --  Write-only. Subtract 1 hour (winter time change) When this bit is set
      --  outside initialization mode, 1 hour is subtracted to the calendar
      --  time if the current hour is not 0. This bit is always read as 0.
      --  Setting this bit has no effect when current hour is 0.
      SUB1H          : Boolean := False;
      --  Backup This bit can be written by the user to memorize whether the
      --  daylight saving time change has been performed or not.
      BKP            : Boolean := False;
      --  Calibration output selection When COE=1, this bit selects which
      --  signal is output on RTC_CALIB. These frequencies are valid for RTCCLK
      --  at 32.768 kHz and prescalers at their default values (PREDIV_A=127
      --  and PREDIV_S=255). Refer to Section24.3.15: Calibration clock output
      COSEL          : Boolean := False;
      --  Output polarity This bit is used to configure the polarity of
      --  RTC_ALARM output
      POL            : Boolean := False;
      --  Output selection These bits are used to select the flag to be routed
      --  to RTC_ALARM output
      OSEL           : RTC_CR_OSEL_Field := 16#0#;
      --  Calibration output enable This bit enables the RTC_CALIB output
      COE            : Boolean := False;
      --  timestamp on internal event enable
      ITSE           : Boolean := False;
      --  unspecified
      Reserved_25_31 : STM32H753.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_CR_Register use record
      WUCKSEL        at 0 range 0 .. 2;
      TSEDGE         at 0 range 3 .. 3;
      REFCKON        at 0 range 4 .. 4;
      BYPSHAD        at 0 range 5 .. 5;
      FMT            at 0 range 6 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      ALRAE          at 0 range 8 .. 8;
      ALRBE          at 0 range 9 .. 9;
      WUTE           at 0 range 10 .. 10;
      TSE            at 0 range 11 .. 11;
      ALRAIE         at 0 range 12 .. 12;
      ALRBIE         at 0 range 13 .. 13;
      WUTIE          at 0 range 14 .. 14;
      TSIE           at 0 range 15 .. 15;
      ADD1H          at 0 range 16 .. 16;
      SUB1H          at 0 range 17 .. 17;
      BKP            at 0 range 18 .. 18;
      COSEL          at 0 range 19 .. 19;
      POL            at 0 range 20 .. 20;
      OSEL           at 0 range 21 .. 22;
      COE            at 0 range 23 .. 23;
      ITSE           at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   --  This register is write protected (except for RTC_ISR[13:8] bits). The
   --  write access procedure is described in RTC register write protection on
   --  page9.
   type RTC_ISR_Register is record
      --  Read-only. Alarm A write flag This bit is set by hardware when Alarm
      --  A values can be changed, after the ALRAE bit has been set to 0 in
      --  RTC_CR. It is cleared by hardware in initialization mode.
      ALRAWF         : Boolean := True;
      --  Read-only. Alarm B write flag This bit is set by hardware when Alarm
      --  B values can be changed, after the ALRBE bit has been set to 0 in
      --  RTC_CR. It is cleared by hardware in initialization mode.
      ALRBWF         : Boolean := True;
      --  Read-only. Wakeup timer write flag This bit is set by hardware up to
      --  2 RTCCLK cycles after the WUTE bit has been set to 0 in RTC_CR, and
      --  is cleared up to 2 RTCCLK cycles after the WUTE bit has been set to
      --  1. The wakeup timer values can be changed when WUTE bit is cleared
      --  and WUTWF is set.
      WUTWF          : Boolean := True;
      --  Read-only. Shift operation pending This flag is set by hardware as
      --  soon as a shift operation is initiated by a write to the RTC_SHIFTR
      --  register. It is cleared by hardware when the corresponding shift
      --  operation has been executed. Writing to the SHPF bit has no effect.
      SHPF           : Boolean := False;
      --  Read-only. Initialization status flag This bit is set by hardware
      --  when the calendar year field is different from 0 (Backup domain reset
      --  state).
      INITS          : Boolean := False;
      --  Registers synchronization flag This bit is set by hardware each time
      --  the calendar registers are copied into the shadow registers
      --  (RTC_SSRx, RTC_TRx and RTC_DRx). This bit is cleared by hardware in
      --  initialization mode, while a shift operation is pending (SHPF=1), or
      --  when in bypass shadow register mode (BYPSHAD=1). This bit can also be
      --  cleared by software. It is cleared either by software or by hardware
      --  in initialization mode.
      RSF            : Boolean := False;
      --  Read-only. Initialization flag When this bit is set to 1, the RTC is
      --  in initialization state, and the time, date and prescaler registers
      --  can be updated.
      INITF          : Boolean := False;
      --  Initialization mode
      INIT           : Boolean := False;
      --  Alarm A flag This flag is set by hardware when the time/date
      --  registers (RTC_TR and RTC_DR) match the Alarm A register
      --  (RTC_ALRMAR). This flag is cleared by software by writing 0.
      ALRAF          : Boolean := False;
      --  Alarm B flag This flag is set by hardware when the time/date
      --  registers (RTC_TR and RTC_DR) match the Alarm B register
      --  (RTC_ALRMBR). This flag is cleared by software by writing 0.
      ALRBF          : Boolean := False;
      --  Wakeup timer flag This flag is set by hardware when the wakeup
      --  auto-reload counter reaches 0. This flag is cleared by software by
      --  writing 0. This flag must be cleared by software at least 1.5 RTCCLK
      --  periods before WUTF is set to 1 again.
      WUTF           : Boolean := False;
      --  Time-stamp flag This flag is set by hardware when a time-stamp event
      --  occurs. This flag is cleared by software by writing 0.
      TSF            : Boolean := False;
      --  Time-stamp overflow flag This flag is set by hardware when a
      --  time-stamp event occurs while TSF is already set. This flag is
      --  cleared by software by writing 0. It is recommended to check and then
      --  clear TSOVF only after clearing the TSF bit. Otherwise, an overflow
      --  might not be noticed if a time-stamp event occurs immediately before
      --  the TSF bit is cleared.
      TSOVF          : Boolean := False;
      --  RTC_TAMP1 detection flag This flag is set by hardware when a tamper
      --  detection event is detected on the RTC_TAMP1 input. It is cleared by
      --  software writing 0
      TAMP1F         : Boolean := False;
      --  RTC_TAMP2 detection flag This flag is set by hardware when a tamper
      --  detection event is detected on the RTC_TAMP2 input. It is cleared by
      --  software writing 0
      TAMP2F         : Boolean := False;
      --  RTC_TAMP3 detection flag This flag is set by hardware when a tamper
      --  detection event is detected on the RTC_TAMP3 input. It is cleared by
      --  software writing 0
      TAMP3F         : Boolean := False;
      --  Read-only. Recalibration pending Flag The RECALPF status flag is
      --  automatically set to 1 when software writes to the RTC_CALR register,
      --  indicating that the RTC_CALR register is blocked. When the new
      --  calibration settings are taken into account, this bit returns to 0.
      --  Refer to Re-calibration on-the-fly.
      RECALPF        : Boolean := False;
      --  Internal tTime-stamp flag
      ITSF           : Boolean := False;
      --  unspecified
      Reserved_18_31 : STM32H753.UInt14 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_ISR_Register use record
      ALRAWF         at 0 range 0 .. 0;
      ALRBWF         at 0 range 1 .. 1;
      WUTWF          at 0 range 2 .. 2;
      SHPF           at 0 range 3 .. 3;
      INITS          at 0 range 4 .. 4;
      RSF            at 0 range 5 .. 5;
      INITF          at 0 range 6 .. 6;
      INIT           at 0 range 7 .. 7;
      ALRAF          at 0 range 8 .. 8;
      ALRBF          at 0 range 9 .. 9;
      WUTF           at 0 range 10 .. 10;
      TSF            at 0 range 11 .. 11;
      TSOVF          at 0 range 12 .. 12;
      TAMP1F         at 0 range 13 .. 13;
      TAMP2F         at 0 range 14 .. 14;
      TAMP3F         at 0 range 15 .. 15;
      RECALPF        at 0 range 16 .. 16;
      ITSF           at 0 range 17 .. 17;
      Reserved_18_31 at 0 range 18 .. 31;
   end record;

   subtype RTC_PRER_PREDIV_S_Field is STM32H753.UInt15;
   subtype RTC_PRER_PREDIV_A_Field is STM32H753.UInt7;

   --  This register must be written in initialization mode only. The
   --  initialization must be performed in two separate write accesses. Refer
   --  to Calendar initialization and configuration on page9.This register is
   --  write protected. The write access procedure is described in RTC register
   --  write protection on page9.
   type RTC_PRER_Register is record
      --  Synchronous prescaler factor This is the synchronous division factor:
      --  ck_spre frequency = ck_apre frequency/(PREDIV_S+1)
      PREDIV_S       : RTC_PRER_PREDIV_S_Field := 16#FF#;
      --  unspecified
      Reserved_15_15 : STM32H753.Bit := 16#0#;
      --  Asynchronous prescaler factor This is the asynchronous division
      --  factor: ck_apre frequency = RTCCLK frequency/(PREDIV_A+1)
      PREDIV_A       : RTC_PRER_PREDIV_A_Field := 16#7F#;
      --  unspecified
      Reserved_23_31 : STM32H753.UInt9 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_PRER_Register use record
      PREDIV_S       at 0 range 0 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      PREDIV_A       at 0 range 16 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   subtype RTC_WUTR_WUT_Field is STM32H753.UInt16;

   --  This register can be written only when WUTWF is set to 1 in RTC_ISR.This
   --  register is write protected. The write access procedure is described in
   --  RTC register write protection on page9.
   type RTC_WUTR_Register is record
      --  Wakeup auto-reload value bits When the wakeup timer is enabled (WUTE
      --  set to 1), the WUTF flag is set every (WUT[15:0] + 1) ck_wut cycles.
      --  The ck_wut period is selected through WUCKSEL[2:0] bits of the RTC_CR
      --  register When WUCKSEL[2] = 1, the wakeup timer becomes 17-bits and
      --  WUCKSEL[1] effectively becomes WUT[16] the most-significant bit to be
      --  reloaded into the timer. The first assertion of WUTF occurs (WUT+1)
      --  ck_wut cycles after WUTE is set. Setting WUT[15:0] to 0x0000 with
      --  WUCKSEL[2:0] =011 (RTCCLK/2) is forbidden.
      WUT            : RTC_WUTR_WUT_Field := 16#FFFF#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_WUTR_Register use record
      WUT            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype RTC_ALRMAR_SU_Field is STM32H753.UInt4;
   subtype RTC_ALRMAR_ST_Field is STM32H753.UInt3;
   subtype RTC_ALRMAR_MNU_Field is STM32H753.UInt4;
   subtype RTC_ALRMAR_MNT_Field is STM32H753.UInt3;
   subtype RTC_ALRMAR_HU_Field is STM32H753.UInt4;
   subtype RTC_ALRMAR_HT_Field is STM32H753.UInt2;
   subtype RTC_ALRMAR_DU_Field is STM32H753.UInt4;
   subtype RTC_ALRMAR_DT_Field is STM32H753.UInt2;

   --  This register can be written only when ALRAWF is set to 1 in RTC_ISR, or
   --  in initialization mode.This register is write protected. The write
   --  access procedure is described in RTC register write protection on page9.
   type RTC_ALRMAR_Register is record
      --  Second units in BCD format.
      SU    : RTC_ALRMAR_SU_Field := 16#0#;
      --  Second tens in BCD format.
      ST    : RTC_ALRMAR_ST_Field := 16#0#;
      --  Alarm A seconds mask
      MSK1  : Boolean := False;
      --  Minute units in BCD format.
      MNU   : RTC_ALRMAR_MNU_Field := 16#0#;
      --  Minute tens in BCD format.
      MNT   : RTC_ALRMAR_MNT_Field := 16#0#;
      --  Alarm A minutes mask
      MSK2  : Boolean := False;
      --  Hour units in BCD format.
      HU    : RTC_ALRMAR_HU_Field := 16#0#;
      --  Hour tens in BCD format.
      HT    : RTC_ALRMAR_HT_Field := 16#0#;
      --  AM/PM notation
      PM    : Boolean := False;
      --  Alarm A hours mask
      MSK3  : Boolean := False;
      --  Date units or day in BCD format.
      DU    : RTC_ALRMAR_DU_Field := 16#0#;
      --  Date tens in BCD format.
      DT    : RTC_ALRMAR_DT_Field := 16#0#;
      --  Week day selection
      WDSEL : Boolean := False;
      --  Alarm A date mask
      MSK4  : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_ALRMAR_Register use record
      SU    at 0 range 0 .. 3;
      ST    at 0 range 4 .. 6;
      MSK1  at 0 range 7 .. 7;
      MNU   at 0 range 8 .. 11;
      MNT   at 0 range 12 .. 14;
      MSK2  at 0 range 15 .. 15;
      HU    at 0 range 16 .. 19;
      HT    at 0 range 20 .. 21;
      PM    at 0 range 22 .. 22;
      MSK3  at 0 range 23 .. 23;
      DU    at 0 range 24 .. 27;
      DT    at 0 range 28 .. 29;
      WDSEL at 0 range 30 .. 30;
      MSK4  at 0 range 31 .. 31;
   end record;

   subtype RTC_ALRMBR_SU_Field is STM32H753.UInt4;
   subtype RTC_ALRMBR_ST_Field is STM32H753.UInt3;
   subtype RTC_ALRMBR_MNU_Field is STM32H753.UInt4;
   subtype RTC_ALRMBR_MNT_Field is STM32H753.UInt3;
   subtype RTC_ALRMBR_HU_Field is STM32H753.UInt4;
   subtype RTC_ALRMBR_HT_Field is STM32H753.UInt2;
   subtype RTC_ALRMBR_DU_Field is STM32H753.UInt4;
   subtype RTC_ALRMBR_DT_Field is STM32H753.UInt2;

   --  This register can be written only when ALRBWF is set to 1 in RTC_ISR, or
   --  in initialization mode.This register is write protected. The write
   --  access procedure is described in RTC register write protection on page9.
   type RTC_ALRMBR_Register is record
      --  Second units in BCD format
      SU    : RTC_ALRMBR_SU_Field := 16#0#;
      --  Second tens in BCD format
      ST    : RTC_ALRMBR_ST_Field := 16#0#;
      --  Alarm B seconds mask
      MSK1  : Boolean := False;
      --  Minute units in BCD format
      MNU   : RTC_ALRMBR_MNU_Field := 16#0#;
      --  Minute tens in BCD format
      MNT   : RTC_ALRMBR_MNT_Field := 16#0#;
      --  Alarm B minutes mask
      MSK2  : Boolean := False;
      --  Hour units in BCD format
      HU    : RTC_ALRMBR_HU_Field := 16#0#;
      --  Hour tens in BCD format
      HT    : RTC_ALRMBR_HT_Field := 16#0#;
      --  AM/PM notation
      PM    : Boolean := False;
      --  Alarm B hours mask
      MSK3  : Boolean := False;
      --  Date units or day in BCD format
      DU    : RTC_ALRMBR_DU_Field := 16#0#;
      --  Date tens in BCD format
      DT    : RTC_ALRMBR_DT_Field := 16#0#;
      --  Week day selection
      WDSEL : Boolean := False;
      --  Alarm B date mask
      MSK4  : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_ALRMBR_Register use record
      SU    at 0 range 0 .. 3;
      ST    at 0 range 4 .. 6;
      MSK1  at 0 range 7 .. 7;
      MNU   at 0 range 8 .. 11;
      MNT   at 0 range 12 .. 14;
      MSK2  at 0 range 15 .. 15;
      HU    at 0 range 16 .. 19;
      HT    at 0 range 20 .. 21;
      PM    at 0 range 22 .. 22;
      MSK3  at 0 range 23 .. 23;
      DU    at 0 range 24 .. 27;
      DT    at 0 range 28 .. 29;
      WDSEL at 0 range 30 .. 30;
      MSK4  at 0 range 31 .. 31;
   end record;

   subtype RTC_WPR_KEY_Field is STM32H753.Byte;

   --  RTC write protection register
   type RTC_WPR_Register is record
      --  Write-only. Write protection key This byte is written by software.
      --  Reading this byte always returns 0x00. Refer to RTC register write
      --  protection for a description of how to unlock RTC register write
      --  protection.
      KEY           : RTC_WPR_KEY_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : STM32H753.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_WPR_Register use record
      KEY           at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype RTC_SSR_SS_Field is STM32H753.UInt16;

   --  RTC sub second register
   type RTC_SSR_Register is record
      --  Read-only. Sub second value SS[15:0] is the value in the synchronous
      --  prescaler counter. The fraction of a second is given by the formula
      --  below: Second fraction = (PREDIV_S - SS) / (PREDIV_S + 1) Note: SS
      --  can be larger than PREDIV_S only after a shift operation. In that
      --  case, the correct time/date is one second less than as indicated by
      --  RTC_TR/RTC_DR.
      SS             : RTC_SSR_SS_Field;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_SSR_Register use record
      SS             at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype RTC_SHIFTR_SUBFS_Field is STM32H753.UInt15;

   --  This register is write protected. The write access procedure is
   --  described in RTC register write protection on page9.
   type RTC_SHIFTR_Register is record
      --  Write-only. Subtract a fraction of a second These bits are write only
      --  and is always read as zero. Writing to this bit has no effect when a
      --  shift operation is pending (when SHPF=1, in RTC_ISR). The value which
      --  is written to SUBFS is added to the synchronous prescaler counter.
      --  Since this counter counts down, this operation effectively subtracts
      --  from (delays) the clock by: Delay (seconds) = SUBFS / (PREDIV_S + 1)
      --  A fraction of a second can effectively be added to the clock
      --  (advancing the clock) when the ADD1S function is used in conjunction
      --  with SUBFS, effectively advancing the clock by: Advance (seconds) =
      --  (1 - (SUBFS / (PREDIV_S + 1))). Note: Writing to SUBFS causes RSF to
      --  be cleared. Software can then wait until RSF=1 to be sure that the
      --  shadow registers have been updated with the shifted time.
      SUBFS          : RTC_SHIFTR_SUBFS_Field := 16#0#;
      --  unspecified
      Reserved_15_30 : STM32H753.UInt16 := 16#0#;
      --  Write-only. Add one second This bit is write only and is always read
      --  as zero. Writing to this bit has no effect when a shift operation is
      --  pending (when SHPF=1, in RTC_ISR). This function is intended to be
      --  used with SUBFS (see description below) in order to effectively add a
      --  fraction of a second to the clock in an atomic operation.
      ADD1S          : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_SHIFTR_Register use record
      SUBFS          at 0 range 0 .. 14;
      Reserved_15_30 at 0 range 15 .. 30;
      ADD1S          at 0 range 31 .. 31;
   end record;

   subtype RTC_TSTR_SU_Field is STM32H753.UInt4;
   subtype RTC_TSTR_ST_Field is STM32H753.UInt3;
   subtype RTC_TSTR_MNU_Field is STM32H753.UInt4;
   subtype RTC_TSTR_MNT_Field is STM32H753.UInt3;
   subtype RTC_TSTR_HU_Field is STM32H753.UInt4;
   subtype RTC_TSTR_HT_Field is STM32H753.UInt2;

   --  The content of this register is valid only when TSF is set to 1 in
   --  RTC_ISR. It is cleared when TSF bit is reset.
   type RTC_TSTR_Register is record
      --  Read-only. Second units in BCD format.
      SU             : RTC_TSTR_SU_Field;
      --  Read-only. Second tens in BCD format.
      ST             : RTC_TSTR_ST_Field;
      --  unspecified
      Reserved_7_7   : STM32H753.Bit;
      --  Read-only. Minute units in BCD format.
      MNU            : RTC_TSTR_MNU_Field;
      --  Read-only. Minute tens in BCD format.
      MNT            : RTC_TSTR_MNT_Field;
      --  unspecified
      Reserved_15_15 : STM32H753.Bit;
      --  Read-only. Hour units in BCD format.
      HU             : RTC_TSTR_HU_Field;
      --  Read-only. Hour tens in BCD format.
      HT             : RTC_TSTR_HT_Field;
      --  Read-only. AM/PM notation
      PM             : Boolean;
      --  unspecified
      Reserved_23_31 : STM32H753.UInt9;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_TSTR_Register use record
      SU             at 0 range 0 .. 3;
      ST             at 0 range 4 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      MNU            at 0 range 8 .. 11;
      MNT            at 0 range 12 .. 14;
      Reserved_15_15 at 0 range 15 .. 15;
      HU             at 0 range 16 .. 19;
      HT             at 0 range 20 .. 21;
      PM             at 0 range 22 .. 22;
      Reserved_23_31 at 0 range 23 .. 31;
   end record;

   subtype RTC_TSDR_DU_Field is STM32H753.UInt4;
   subtype RTC_TSDR_DT_Field is STM32H753.UInt2;
   subtype RTC_TSDR_MU_Field is STM32H753.UInt4;
   subtype RTC_TSDR_WDU_Field is STM32H753.UInt3;

   --  The content of this register is valid only when TSF is set to 1 in
   --  RTC_ISR. It is cleared when TSF bit is reset.
   type RTC_TSDR_Register is record
      --  Read-only. Date units in BCD format
      DU             : RTC_TSDR_DU_Field;
      --  Read-only. Date tens in BCD format
      DT             : RTC_TSDR_DT_Field;
      --  unspecified
      Reserved_6_7   : STM32H753.UInt2;
      --  Read-only. Month units in BCD format
      MU             : RTC_TSDR_MU_Field;
      --  Read-only. Month tens in BCD format
      MT             : Boolean;
      --  Read-only. Week day units
      WDU            : RTC_TSDR_WDU_Field;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_TSDR_Register use record
      DU             at 0 range 0 .. 3;
      DT             at 0 range 4 .. 5;
      Reserved_6_7   at 0 range 6 .. 7;
      MU             at 0 range 8 .. 11;
      MT             at 0 range 12 .. 12;
      WDU            at 0 range 13 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype RTC_TSSSR_SS_Field is STM32H753.UInt16;

   --  The content of this register is valid only when RTC_ISR/TSF is set. It
   --  is cleared when the RTC_ISR/TSF bit is reset.
   type RTC_TSSSR_Register is record
      --  Read-only. Sub second value SS[15:0] is the value of the synchronous
      --  prescaler counter when the timestamp event occurred.
      SS             : RTC_TSSSR_SS_Field;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_TSSSR_Register use record
      SS             at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype RTC_CALR_CALM_Field is STM32H753.UInt9;

   --  This register is write protected. The write access procedure is
   --  described in RTC register write protection on page9.
   type RTC_CALR_Register is record
      --  Calibration minus The frequency of the calendar is reduced by masking
      --  CALM out of 220 RTCCLK pulses (32 seconds if the input frequency is
      --  32768 Hz). This decreases the frequency of the calendar with a
      --  resolution of 0.9537 ppm. To increase the frequency of the calendar,
      --  this feature should be used in conjunction with CALP. See
      --  Section24.3.12: RTC smooth digital calibration on page13.
      CALM           : RTC_CALR_CALM_Field := 16#0#;
      --  unspecified
      Reserved_9_12  : STM32H753.UInt4 := 16#0#;
      --  Use a 16-second calibration cycle period When CALW16 is set to 1, the
      --  16-second calibration cycle period is selected.This bit must not be
      --  set to 1 if CALW8=1. Note: CALM[0] is stuck at 0 when CALW16= 1.
      --  Refer to Section24.3.12: RTC smooth digital calibration.
      CALW16         : Boolean := False;
      --  Use an 8-second calibration cycle period When CALW8 is set to 1, the
      --  8-second calibration cycle period is selected. Note: CALM[1:0] are
      --  stuck at 00; when CALW8= 1. Refer to Section24.3.12: RTC smooth
      --  digital calibration.
      CALW8          : Boolean := False;
      --  Increase frequency of RTC by 488.5 ppm This feature is intended to be
      --  used in conjunction with CALM, which lowers the frequency of the
      --  calendar with a fine resolution. if the input frequency is 32768 Hz,
      --  the number of RTCCLK pulses added during a 32-second window is
      --  calculated as follows: (512 * CALP) - CALM. Refer to Section24.3.12:
      --  RTC smooth digital calibration.
      CALP           : Boolean := False;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_CALR_Register use record
      CALM           at 0 range 0 .. 8;
      Reserved_9_12  at 0 range 9 .. 12;
      CALW16         at 0 range 13 .. 13;
      CALW8          at 0 range 14 .. 14;
      CALP           at 0 range 15 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype RTC_TAMPCR_TAMPFREQ_Field is STM32H753.UInt3;
   subtype RTC_TAMPCR_TAMPFLT_Field is STM32H753.UInt2;
   subtype RTC_TAMPCR_TAMPPRCH_Field is STM32H753.UInt2;

   --  RTC tamper and alternate function configuration register
   type RTC_TAMPCR_Register is record
      --  RTC_TAMP1 input detection enable
      TAMP1E         : Boolean := False;
      --  Active level for RTC_TAMP1 input If TAMPFLT != 00 if TAMPFLT = 00:
      TAMP1TRG       : Boolean := False;
      --  Tamper interrupt enable
      TAMPIE         : Boolean := False;
      --  RTC_TAMP2 input detection enable
      TAMP2E         : Boolean := False;
      --  Active level for RTC_TAMP2 input if TAMPFLT != 00: if TAMPFLT = 00:
      TAMP2TRG       : Boolean := False;
      --  RTC_TAMP3 detection enable
      TAMP3E         : Boolean := False;
      --  Active level for RTC_TAMP3 input if TAMPFLT != 00: if TAMPFLT = 00:
      TAMP3TRG       : Boolean := False;
      --  Activate timestamp on tamper detection event TAMPTS is valid even if
      --  TSE=0 in the RTC_CR register.
      TAMPTS         : Boolean := False;
      --  Tamper sampling frequency Determines the frequency at which each of
      --  the RTC_TAMPx inputs are sampled.
      TAMPFREQ       : RTC_TAMPCR_TAMPFREQ_Field := 16#0#;
      --  RTC_TAMPx filter count These bits determines the number of
      --  consecutive samples at the specified level (TAMP*TRG) needed to
      --  activate a Tamper event. TAMPFLT is valid for each of the RTC_TAMPx
      --  inputs.
      TAMPFLT        : RTC_TAMPCR_TAMPFLT_Field := 16#0#;
      --  RTC_TAMPx precharge duration These bit determines the duration of
      --  time during which the pull-up/is activated before each sample.
      --  TAMPPRCH is valid for each of the RTC_TAMPx inputs.
      TAMPPRCH       : RTC_TAMPCR_TAMPPRCH_Field := 16#0#;
      --  RTC_TAMPx pull-up disable This bit determines if each of the
      --  RTC_TAMPx pins are pre-charged before each sample.
      TAMPPUDIS      : Boolean := False;
      --  Tamper 1 interrupt enable
      TAMP1IE        : Boolean := False;
      --  Tamper 1 no erase
      TAMP1NOERASE   : Boolean := False;
      --  Tamper 1 mask flag
      TAMP1MF        : Boolean := False;
      --  Tamper 2 interrupt enable
      TAMP2IE        : Boolean := False;
      --  Tamper 2 no erase
      TAMP2NOERASE   : Boolean := False;
      --  Tamper 2 mask flag
      TAMP2MF        : Boolean := False;
      --  Tamper 3 interrupt enable
      TAMP3IE        : Boolean := False;
      --  Tamper 3 no erase
      TAMP3NOERASE   : Boolean := False;
      --  Tamper 3 mask flag
      TAMP3MF        : Boolean := False;
      --  unspecified
      Reserved_25_31 : STM32H753.UInt7 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_TAMPCR_Register use record
      TAMP1E         at 0 range 0 .. 0;
      TAMP1TRG       at 0 range 1 .. 1;
      TAMPIE         at 0 range 2 .. 2;
      TAMP2E         at 0 range 3 .. 3;
      TAMP2TRG       at 0 range 4 .. 4;
      TAMP3E         at 0 range 5 .. 5;
      TAMP3TRG       at 0 range 6 .. 6;
      TAMPTS         at 0 range 7 .. 7;
      TAMPFREQ       at 0 range 8 .. 10;
      TAMPFLT        at 0 range 11 .. 12;
      TAMPPRCH       at 0 range 13 .. 14;
      TAMPPUDIS      at 0 range 15 .. 15;
      TAMP1IE        at 0 range 16 .. 16;
      TAMP1NOERASE   at 0 range 17 .. 17;
      TAMP1MF        at 0 range 18 .. 18;
      TAMP2IE        at 0 range 19 .. 19;
      TAMP2NOERASE   at 0 range 20 .. 20;
      TAMP2MF        at 0 range 21 .. 21;
      TAMP3IE        at 0 range 22 .. 22;
      TAMP3NOERASE   at 0 range 23 .. 23;
      TAMP3MF        at 0 range 24 .. 24;
      Reserved_25_31 at 0 range 25 .. 31;
   end record;

   subtype RTC_ALRMASSR_SS_Field is STM32H753.UInt15;
   subtype RTC_ALRMASSR_MASKSS_Field is STM32H753.UInt4;

   --  This register can be written only when ALRAE is reset in RTC_CR
   --  register, or in initialization mode.This register is write protected.
   --  The write access procedure is described in RTC register write protection
   --  on page9
   type RTC_ALRMASSR_Register is record
      --  Sub seconds value This value is compared with the contents of the
      --  synchronous prescaler counter to determine if Alarm A is to be
      --  activated. Only bits 0 up MASKSS-1 are compared.
      SS             : RTC_ALRMASSR_SS_Field := 16#0#;
      --  unspecified
      Reserved_15_23 : STM32H753.UInt9 := 16#0#;
      --  Mask the most-significant bits starting at this bit ... The overflow
      --  bits of the synchronous counter (bits 15) is never compared. This bit
      --  can be different from 0 only after a shift operation.
      MASKSS         : RTC_ALRMASSR_MASKSS_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : STM32H753.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_ALRMASSR_Register use record
      SS             at 0 range 0 .. 14;
      Reserved_15_23 at 0 range 15 .. 23;
      MASKSS         at 0 range 24 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   subtype RTC_ALRMBSSR_SS_Field is STM32H753.UInt15;
   subtype RTC_ALRMBSSR_MASKSS_Field is STM32H753.UInt4;

   --  This register can be written only when ALRBE is reset in RTC_CR
   --  register, or in initialization mode.This register is write protected.The
   --  write access procedure is described in Section: RTC register write
   --  protection.
   type RTC_ALRMBSSR_Register is record
      --  Sub seconds value This value is compared with the contents of the
      --  synchronous prescaler counter to determine if Alarm B is to be
      --  activated. Only bits 0 up to MASKSS-1 are compared.
      SS             : RTC_ALRMBSSR_SS_Field := 16#0#;
      --  unspecified
      Reserved_15_23 : STM32H753.UInt9 := 16#0#;
      --  Mask the most-significant bits starting at this bit ... The overflow
      --  bits of the synchronous counter (bits 15) is never compared. This bit
      --  can be different from 0 only after a shift operation.
      MASKSS         : RTC_ALRMBSSR_MASKSS_Field := 16#0#;
      --  unspecified
      Reserved_28_31 : STM32H753.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_ALRMBSSR_Register use record
      SS             at 0 range 0 .. 14;
      Reserved_15_23 at 0 range 15 .. 23;
      MASKSS         at 0 range 24 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  RTC option register
   type RTC_OR_Register is record
      --  RTC_ALARM output type on PC13
      RTC_ALARM_TYPE : Boolean := False;
      --  RTC_OUT remap
      RTC_OUT_RMP    : Boolean := False;
      --  unspecified
      Reserved_2_31  : STM32H753.UInt30 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RTC_OR_Register use record
      RTC_ALARM_TYPE at 0 range 0 .. 0;
      RTC_OUT_RMP    at 0 range 1 .. 1;
      Reserved_2_31  at 0 range 2 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  RTC
   type RTC_Peripheral is record
      --  The RTC_TR is the calendar time shadow register. This register must
      --  be written in initialization mode only. Refer to Calendar
      --  initialization and configuration on page9 and Reading the calendar on
      --  page10.This register is write protected. The write access procedure
      --  is described in RTC register write protection on page9.
      RTC_TR       : aliased RTC_TR_Register;
      --  The RTC_DR is the calendar date shadow register. This register must
      --  be written in initialization mode only. Refer to Calendar
      --  initialization and configuration on page9 and Reading the calendar on
      --  page10.This register is write protected. The write access procedure
      --  is described in RTC register write protection on page9.
      RTC_DR       : aliased RTC_DR_Register;
      --  RTC control register
      RTC_CR       : aliased RTC_CR_Register;
      --  This register is write protected (except for RTC_ISR[13:8] bits). The
      --  write access procedure is described in RTC register write protection
      --  on page9.
      RTC_ISR      : aliased RTC_ISR_Register;
      --  This register must be written in initialization mode only. The
      --  initialization must be performed in two separate write accesses.
      --  Refer to Calendar initialization and configuration on page9.This
      --  register is write protected. The write access procedure is described
      --  in RTC register write protection on page9.
      RTC_PRER     : aliased RTC_PRER_Register;
      --  This register can be written only when WUTWF is set to 1 in
      --  RTC_ISR.This register is write protected. The write access procedure
      --  is described in RTC register write protection on page9.
      RTC_WUTR     : aliased RTC_WUTR_Register;
      --  This register can be written only when ALRAWF is set to 1 in RTC_ISR,
      --  or in initialization mode.This register is write protected. The write
      --  access procedure is described in RTC register write protection on
      --  page9.
      RTC_ALRMAR   : aliased RTC_ALRMAR_Register;
      --  This register can be written only when ALRBWF is set to 1 in RTC_ISR,
      --  or in initialization mode.This register is write protected. The write
      --  access procedure is described in RTC register write protection on
      --  page9.
      RTC_ALRMBR   : aliased RTC_ALRMBR_Register;
      --  RTC write protection register
      RTC_WPR      : aliased RTC_WPR_Register;
      --  RTC sub second register
      RTC_SSR      : aliased RTC_SSR_Register;
      --  This register is write protected. The write access procedure is
      --  described in RTC register write protection on page9.
      RTC_SHIFTR   : aliased RTC_SHIFTR_Register;
      --  The content of this register is valid only when TSF is set to 1 in
      --  RTC_ISR. It is cleared when TSF bit is reset.
      RTC_TSTR     : aliased RTC_TSTR_Register;
      --  The content of this register is valid only when TSF is set to 1 in
      --  RTC_ISR. It is cleared when TSF bit is reset.
      RTC_TSDR     : aliased RTC_TSDR_Register;
      --  The content of this register is valid only when RTC_ISR/TSF is set.
      --  It is cleared when the RTC_ISR/TSF bit is reset.
      RTC_TSSSR    : aliased RTC_TSSSR_Register;
      --  This register is write protected. The write access procedure is
      --  described in RTC register write protection on page9.
      RTC_CALR     : aliased RTC_CALR_Register;
      --  RTC tamper and alternate function configuration register
      RTC_TAMPCR   : aliased RTC_TAMPCR_Register;
      --  This register can be written only when ALRAE is reset in RTC_CR
      --  register, or in initialization mode.This register is write protected.
      --  The write access procedure is described in RTC register write
      --  protection on page9
      RTC_ALRMASSR : aliased RTC_ALRMASSR_Register;
      --  This register can be written only when ALRBE is reset in RTC_CR
      --  register, or in initialization mode.This register is write
      --  protected.The write access procedure is described in Section: RTC
      --  register write protection.
      RTC_ALRMBSSR : aliased RTC_ALRMBSSR_Register;
      --  RTC option register
      RTC_OR       : aliased RTC_OR_Register;
      --  RTC backup registers
      RTC_BKP0R    : aliased STM32H753.UInt32;
      --  RTC backup registers
      RTC_BKP1R    : aliased STM32H753.UInt32;
      --  RTC backup registers
      RTC_BKP2R    : aliased STM32H753.UInt32;
      --  RTC backup registers
      RTC_BKP3R    : aliased STM32H753.UInt32;
      --  RTC backup registers
      RTC_BKP4R    : aliased STM32H753.UInt32;
      --  RTC backup registers
      RTC_BKP5R    : aliased STM32H753.UInt32;
      --  RTC backup registers
      RTC_BKP6R    : aliased STM32H753.UInt32;
      --  RTC backup registers
      RTC_BKP7R    : aliased STM32H753.UInt32;
      --  RTC backup registers
      RTC_BKP8R    : aliased STM32H753.UInt32;
      --  RTC backup registers
      RTC_BKP9R    : aliased STM32H753.UInt32;
      --  RTC backup registers
      RTC_BKP10R   : aliased STM32H753.UInt32;
      --  RTC backup registers
      RTC_BKP11R   : aliased STM32H753.UInt32;
      --  RTC backup registers
      RTC_BKP12R   : aliased STM32H753.UInt32;
      --  RTC backup registers
      RTC_BKP13R   : aliased STM32H753.UInt32;
      --  RTC backup registers
      RTC_BKP14R   : aliased STM32H753.UInt32;
      --  RTC backup registers
      RTC_BKP15R   : aliased STM32H753.UInt32;
      --  RTC backup registers
      RTC_BKP16R   : aliased STM32H753.UInt32;
      --  RTC backup registers
      RTC_BKP17R   : aliased STM32H753.UInt32;
      --  RTC backup registers
      RTC_BKP18R   : aliased STM32H753.UInt32;
      --  RTC backup registers
      RTC_BKP19R   : aliased STM32H753.UInt32;
      --  RTC backup registers
      RTC_BKP20R   : aliased STM32H753.UInt32;
      --  RTC backup registers
      RTC_BKP21R   : aliased STM32H753.UInt32;
      --  RTC backup registers
      RTC_BKP22R   : aliased STM32H753.UInt32;
      --  RTC backup registers
      RTC_BKP23R   : aliased STM32H753.UInt32;
      --  RTC backup registers
      RTC_BKP24R   : aliased STM32H753.UInt32;
      --  RTC backup registers
      RTC_BKP25R   : aliased STM32H753.UInt32;
      --  RTC backup registers
      RTC_BKP26R   : aliased STM32H753.UInt32;
      --  RTC backup registers
      RTC_BKP27R   : aliased STM32H753.UInt32;
      --  RTC backup registers
      RTC_BKP28R   : aliased STM32H753.UInt32;
      --  RTC backup registers
      RTC_BKP29R   : aliased STM32H753.UInt32;
      --  RTC backup registers
      RTC_BKP30R   : aliased STM32H753.UInt32;
      --  RTC backup registers
      RTC_BKP31R   : aliased STM32H753.UInt32;
   end record
     with Volatile;

   for RTC_Peripheral use record
      RTC_TR       at 16#0# range 0 .. 31;
      RTC_DR       at 16#4# range 0 .. 31;
      RTC_CR       at 16#8# range 0 .. 31;
      RTC_ISR      at 16#C# range 0 .. 31;
      RTC_PRER     at 16#10# range 0 .. 31;
      RTC_WUTR     at 16#14# range 0 .. 31;
      RTC_ALRMAR   at 16#1C# range 0 .. 31;
      RTC_ALRMBR   at 16#20# range 0 .. 31;
      RTC_WPR      at 16#24# range 0 .. 31;
      RTC_SSR      at 16#28# range 0 .. 31;
      RTC_SHIFTR   at 16#2C# range 0 .. 31;
      RTC_TSTR     at 16#30# range 0 .. 31;
      RTC_TSDR     at 16#34# range 0 .. 31;
      RTC_TSSSR    at 16#38# range 0 .. 31;
      RTC_CALR     at 16#3C# range 0 .. 31;
      RTC_TAMPCR   at 16#40# range 0 .. 31;
      RTC_ALRMASSR at 16#44# range 0 .. 31;
      RTC_ALRMBSSR at 16#48# range 0 .. 31;
      RTC_OR       at 16#4C# range 0 .. 31;
      RTC_BKP0R    at 16#50# range 0 .. 31;
      RTC_BKP1R    at 16#54# range 0 .. 31;
      RTC_BKP2R    at 16#58# range 0 .. 31;
      RTC_BKP3R    at 16#5C# range 0 .. 31;
      RTC_BKP4R    at 16#60# range 0 .. 31;
      RTC_BKP5R    at 16#64# range 0 .. 31;
      RTC_BKP6R    at 16#68# range 0 .. 31;
      RTC_BKP7R    at 16#6C# range 0 .. 31;
      RTC_BKP8R    at 16#70# range 0 .. 31;
      RTC_BKP9R    at 16#74# range 0 .. 31;
      RTC_BKP10R   at 16#78# range 0 .. 31;
      RTC_BKP11R   at 16#7C# range 0 .. 31;
      RTC_BKP12R   at 16#80# range 0 .. 31;
      RTC_BKP13R   at 16#84# range 0 .. 31;
      RTC_BKP14R   at 16#88# range 0 .. 31;
      RTC_BKP15R   at 16#8C# range 0 .. 31;
      RTC_BKP16R   at 16#90# range 0 .. 31;
      RTC_BKP17R   at 16#94# range 0 .. 31;
      RTC_BKP18R   at 16#98# range 0 .. 31;
      RTC_BKP19R   at 16#9C# range 0 .. 31;
      RTC_BKP20R   at 16#A0# range 0 .. 31;
      RTC_BKP21R   at 16#A4# range 0 .. 31;
      RTC_BKP22R   at 16#A8# range 0 .. 31;
      RTC_BKP23R   at 16#AC# range 0 .. 31;
      RTC_BKP24R   at 16#B0# range 0 .. 31;
      RTC_BKP25R   at 16#B4# range 0 .. 31;
      RTC_BKP26R   at 16#B8# range 0 .. 31;
      RTC_BKP27R   at 16#BC# range 0 .. 31;
      RTC_BKP28R   at 16#C0# range 0 .. 31;
      RTC_BKP29R   at 16#C4# range 0 .. 31;
      RTC_BKP30R   at 16#C8# range 0 .. 31;
      RTC_BKP31R   at 16#CC# range 0 .. 31;
   end record;

   --  RTC
   RTC_Periph : aliased RTC_Peripheral
     with Import, Address => RTC_Base;

end STM32H753.RTC;
