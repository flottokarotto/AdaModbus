pragma Style_Checks (Off);

--  This spec has been automatically generated from STM32H753.svd


with System;

package STM32H753.Flash is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   subtype ACR_LATENCY_Field is STM32H753.UInt4;
   subtype ACR_WRHIGHFREQ_Field is STM32H753.UInt2;

   --  Access control register
   type ACR_Register is record
      --  Read latency
      LATENCY       : ACR_LATENCY_Field := 16#7#;
      --  Flash signal delay
      WRHIGHFREQ    : ACR_WRHIGHFREQ_Field := 16#3#;
      --  unspecified
      Reserved_6_31 : STM32H753.UInt26 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ACR_Register use record
      LATENCY       at 0 range 0 .. 3;
      WRHIGHFREQ    at 0 range 4 .. 5;
      Reserved_6_31 at 0 range 6 .. 31;
   end record;

   subtype CR1_PSIZE1_Field is STM32H753.UInt2;
   subtype CR1_SNB1_Field is STM32H753.UInt3;

   --  FLASH control register for bank 1
   type CR1_Register is record
      --  Bank 1 configuration lock bit
      LOCK1          : Boolean := False;
      --  Bank 1 program enable bit
      PG1            : Boolean := False;
      --  Bank 1 sector erase request
      SER1           : Boolean := False;
      --  Bank 1 erase request
      BER1           : Boolean := False;
      --  Bank 1 program size
      PSIZE1         : CR1_PSIZE1_Field := 16#0#;
      --  Bank 1 write forcing control bit
      FW1            : Boolean := False;
      --  Bank 1 bank or sector erase start control bit
      START1         : Boolean := False;
      --  Bank 1 sector erase selection number
      SNB1           : CR1_SNB1_Field := 16#0#;
      --  unspecified
      Reserved_11_14 : STM32H753.UInt4 := 16#0#;
      --  Bank 1 CRC control bit
      CRC_EN         : Boolean := False;
      --  Bank 1 end-of-program interrupt control bit
      EOPIE1         : Boolean := False;
      --  Bank 1 write protection error interrupt enable bit
      WRPERRIE1      : Boolean := False;
      --  Bank 1 programming sequence error interrupt enable bit
      PGSERRIE1      : Boolean := False;
      --  Bank 1 strobe error interrupt enable bit
      STRBERRIE1     : Boolean := False;
      --  unspecified
      Reserved_20_20 : STM32H753.Bit := 16#0#;
      --  Bank 1 inconsistency error interrupt enable bit
      INCERRIE1      : Boolean := False;
      --  Bank 1 write/erase error interrupt enable bit
      OPERRIE1       : Boolean := False;
      --  Bank 1 read protection error interrupt enable bit
      RDPERRIE1      : Boolean := False;
      --  Bank 1 secure error interrupt enable bit
      RDSERRIE1      : Boolean := False;
      --  Bank 1 ECC single correction error interrupt enable bit
      SNECCERR1      : Boolean := False;
      --  Bank 1 ECC double detection error interrupt enable bit
      DBECCERRIE1    : Boolean := False;
      --  Bank 1 end of CRC calculation interrupt enable bit
      CRCENDIE1      : Boolean := False;
      --  Bank 1 CRC read error interrupt enable bit When CRCRDERRIE1 bit is
      --  set to 1, an interrupt is generated when a protected area (PCROP or
      --  secure-only) has been detected during the last CRC computation on
      --  bank 1. CRCRDERRIE1 can be programmed only when LOCK1 is cleared to
      --  0.
      CRCRDERRIE1    : Boolean := False;
      --  unspecified
      Reserved_29_31 : STM32H753.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR1_Register use record
      LOCK1          at 0 range 0 .. 0;
      PG1            at 0 range 1 .. 1;
      SER1           at 0 range 2 .. 2;
      BER1           at 0 range 3 .. 3;
      PSIZE1         at 0 range 4 .. 5;
      FW1            at 0 range 6 .. 6;
      START1         at 0 range 7 .. 7;
      SNB1           at 0 range 8 .. 10;
      Reserved_11_14 at 0 range 11 .. 14;
      CRC_EN         at 0 range 15 .. 15;
      EOPIE1         at 0 range 16 .. 16;
      WRPERRIE1      at 0 range 17 .. 17;
      PGSERRIE1      at 0 range 18 .. 18;
      STRBERRIE1     at 0 range 19 .. 19;
      Reserved_20_20 at 0 range 20 .. 20;
      INCERRIE1      at 0 range 21 .. 21;
      OPERRIE1       at 0 range 22 .. 22;
      RDPERRIE1      at 0 range 23 .. 23;
      RDSERRIE1      at 0 range 24 .. 24;
      SNECCERR1      at 0 range 25 .. 25;
      DBECCERRIE1    at 0 range 26 .. 26;
      CRCENDIE1      at 0 range 27 .. 27;
      CRCRDERRIE1    at 0 range 28 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   --  FLASH status register for bank 1
   type SR1_Register is record
      --  Bank 1 ongoing program flag
      BSY1           : Boolean := False;
      --  Bank 1 write buffer not empty flag
      WBNE1          : Boolean := False;
      --  Bank 1 wait queue flag
      QW1            : Boolean := False;
      --  Bank 1 CRC busy flag
      CRC_BUSY1      : Boolean := False;
      --  unspecified
      Reserved_4_15  : STM32H753.UInt12 := 16#0#;
      --  Bank 1 end-of-program flag
      EOP1           : Boolean := False;
      --  Bank 1 write protection error flag
      WRPERR1        : Boolean := False;
      --  Bank 1 programming sequence error flag
      PGSERR1        : Boolean := False;
      --  Bank 1 strobe error flag
      STRBERR1       : Boolean := False;
      --  unspecified
      Reserved_20_20 : STM32H753.Bit := 16#0#;
      --  Bank 1 inconsistency error flag
      INCERR1        : Boolean := False;
      --  Bank 1 write/erase error flag
      OPERR1         : Boolean := False;
      --  Bank 1 read protection error flag
      RDPERR1        : Boolean := False;
      --  Bank 1 secure error flag
      RDSERR1        : Boolean := False;
      --  Bank 1 single correction error flag
      SNECCERR       : Boolean := False;
      --  Bank 1 ECC double detection error flag
      DBECCERR1      : Boolean := False;
      --  Bank 1 CRC-complete flag
      CRCEND1        : Boolean := False;
      --  Read-only. Bank 1 CRC read error flag CRCRDERR1 flag is raised when a
      --  word is found read protected during a CRC operation on bank 1. An
      --  interrupt is generated if CRCRDIE1 and CRCEND1 are set to 1. Writing
      --  1 to CLR_CRCRDERR1 bit in FLASH_CCR1 register clears CRCRDERR1. Note:
      --  This flag is valid only when CRCEND1 bit is set to 1
      CRCRDERR1      : Boolean := False;
      --  unspecified
      Reserved_29_31 : STM32H753.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR1_Register use record
      BSY1           at 0 range 0 .. 0;
      WBNE1          at 0 range 1 .. 1;
      QW1            at 0 range 2 .. 2;
      CRC_BUSY1      at 0 range 3 .. 3;
      Reserved_4_15  at 0 range 4 .. 15;
      EOP1           at 0 range 16 .. 16;
      WRPERR1        at 0 range 17 .. 17;
      PGSERR1        at 0 range 18 .. 18;
      STRBERR1       at 0 range 19 .. 19;
      Reserved_20_20 at 0 range 20 .. 20;
      INCERR1        at 0 range 21 .. 21;
      OPERR1         at 0 range 22 .. 22;
      RDPERR1        at 0 range 23 .. 23;
      RDSERR1        at 0 range 24 .. 24;
      SNECCERR       at 0 range 25 .. 25;
      DBECCERR1      at 0 range 26 .. 26;
      CRCEND1        at 0 range 27 .. 27;
      CRCRDERR1      at 0 range 28 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   --  FLASH clear control register for bank 1
   type CCR1_Register is record
      --  unspecified
      Reserved_0_15  : STM32H753.UInt16 := 16#0#;
      --  Bank 1 EOP1 flag clear bit
      CLR_EOP1       : Boolean := False;
      --  Bank 1 WRPERR1 flag clear bit
      CLR_WRPERR1    : Boolean := False;
      --  Bank 1 PGSERR1 flag clear bi
      CLR_PGSERR1    : Boolean := False;
      --  Bank 1 STRBERR1 flag clear bit
      CLR_STRBERR1   : Boolean := False;
      --  unspecified
      Reserved_20_20 : STM32H753.Bit := 16#0#;
      --  Bank 1 INCERR1 flag clear bit
      CLR_INCERR1    : Boolean := False;
      --  Bank 1 OPERR1 flag clear bit
      CLR_OPERR1     : Boolean := False;
      --  Bank 1 RDPERR1 flag clear bit
      CLR_RDPERR1    : Boolean := False;
      --  Bank 1 RDSERR1 flag clear bit
      CLR_RDSERR1    : Boolean := False;
      --  Bank 1 SNECCERR1 flag clear bit
      CLR_SNECCERR1  : Boolean := False;
      --  Bank 1 DBECCERR1 flag clear bit
      CLR_DBECCERR1  : Boolean := False;
      --  Bank 1 CRCEND1 flag clear bit
      CLR_CRCEND1    : Boolean := False;
      --  Write-only. Bank 1 CRCRDERR1 flag clear bit Setting this bit to 1
      --  resets to 0 CRCRDERR1 flag in FLASH_SR1 register.
      CLR_CRCRDERR1  : Boolean := False;
      --  unspecified
      Reserved_29_31 : STM32H753.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCR1_Register use record
      Reserved_0_15  at 0 range 0 .. 15;
      CLR_EOP1       at 0 range 16 .. 16;
      CLR_WRPERR1    at 0 range 17 .. 17;
      CLR_PGSERR1    at 0 range 18 .. 18;
      CLR_STRBERR1   at 0 range 19 .. 19;
      Reserved_20_20 at 0 range 20 .. 20;
      CLR_INCERR1    at 0 range 21 .. 21;
      CLR_OPERR1     at 0 range 22 .. 22;
      CLR_RDPERR1    at 0 range 23 .. 23;
      CLR_RDSERR1    at 0 range 24 .. 24;
      CLR_SNECCERR1  at 0 range 25 .. 25;
      CLR_DBECCERR1  at 0 range 26 .. 26;
      CLR_CRCEND1    at 0 range 27 .. 27;
      CLR_CRCRDERR1  at 0 range 28 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   --  FLASH option control register
   type OPTCR_Register is record
      --  FLASH_OPTCR lock option configuration bit
      OPTLOCK        : Boolean := False;
      --  Option byte start change option configuration bit
      OPTSTART       : Boolean := False;
      --  unspecified
      Reserved_2_3   : STM32H753.UInt2 := 16#0#;
      --  Flash mass erase enable bit
      MER            : Boolean := False;
      --  unspecified
      Reserved_5_29  : STM32H753.UInt25 := 16#0#;
      --  Option byte change error interrupt enable bit
      OPTCHANGEERRIE : Boolean := False;
      --  Bank swapping configuration bit
      SWAP_BANK      : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OPTCR_Register use record
      OPTLOCK        at 0 range 0 .. 0;
      OPTSTART       at 0 range 1 .. 1;
      Reserved_2_3   at 0 range 2 .. 3;
      MER            at 0 range 4 .. 4;
      Reserved_5_29  at 0 range 5 .. 29;
      OPTCHANGEERRIE at 0 range 30 .. 30;
      SWAP_BANK      at 0 range 31 .. 31;
   end record;

   subtype OPTSR_CUR_BOR_LEV_Field is STM32H753.UInt2;
   subtype OPTSR_CUR_RDP_Field is STM32H753.Byte;
   subtype OPTSR_CUR_ST_RAM_SIZE_Field is STM32H753.UInt2;

   --  FLASH option status register
   type OPTSR_CUR_Register is record
      --  Option byte change ongoing flag
      OPT_BUSY       : Boolean := False;
      --  unspecified
      Reserved_1_1   : STM32H753.Bit := 16#0#;
      --  Brownout level option status bit
      BOR_LEV        : OPTSR_CUR_BOR_LEV_Field := 16#0#;
      --  IWDG1 control option status bit
      IWDG1_SW       : Boolean := False;
      --  unspecified
      Reserved_5_5   : STM32H753.Bit := 16#0#;
      --  D1 DStop entry reset option status bit
      NRST_STOP_D1   : Boolean := False;
      --  D1 DStandby entry reset option status bit
      NRST_STBY_D1   : Boolean := False;
      --  Readout protection level option status byte
      RDP            : OPTSR_CUR_RDP_Field := 16#0#;
      --  unspecified
      Reserved_16_16 : STM32H753.Bit := 16#0#;
      --  IWDG Stop mode freeze option status bit
      IWDG_FZ_STOP   : Boolean := False;
      --  IWDG Standby mode freeze option status bit
      IWDG_FZ_SDBY   : Boolean := False;
      --  DTCM RAM size option status
      ST_RAM_SIZE    : OPTSR_CUR_ST_RAM_SIZE_Field := 16#0#;
      --  Security enable option status bit
      SECURITY       : Boolean := False;
      --  unspecified
      Reserved_22_28 : STM32H753.UInt7 := 16#0#;
      --  I/O high-speed at low-voltage status bit (PRODUCT_BELOW_25V)
      IO_HSLV        : Boolean := False;
      --  Option byte change error flag
      OPTCHANGEERR   : Boolean := False;
      --  Bank swapping option status bit
      SWAP_BANK_OPT  : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OPTSR_CUR_Register use record
      OPT_BUSY       at 0 range 0 .. 0;
      Reserved_1_1   at 0 range 1 .. 1;
      BOR_LEV        at 0 range 2 .. 3;
      IWDG1_SW       at 0 range 4 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      NRST_STOP_D1   at 0 range 6 .. 6;
      NRST_STBY_D1   at 0 range 7 .. 7;
      RDP            at 0 range 8 .. 15;
      Reserved_16_16 at 0 range 16 .. 16;
      IWDG_FZ_STOP   at 0 range 17 .. 17;
      IWDG_FZ_SDBY   at 0 range 18 .. 18;
      ST_RAM_SIZE    at 0 range 19 .. 20;
      SECURITY       at 0 range 21 .. 21;
      Reserved_22_28 at 0 range 22 .. 28;
      IO_HSLV        at 0 range 29 .. 29;
      OPTCHANGEERR   at 0 range 30 .. 30;
      SWAP_BANK_OPT  at 0 range 31 .. 31;
   end record;

   subtype OPTSR_PRG_BOR_LEV_Field is STM32H753.UInt2;
   subtype OPTSR_PRG_RDP_Field is STM32H753.Byte;
   subtype OPTSR_PRG_ST_RAM_SIZE_Field is STM32H753.UInt2;

   --  FLASH option status register
   type OPTSR_PRG_Register is record
      --  unspecified
      Reserved_0_1   : STM32H753.UInt2 := 16#0#;
      --  BOR reset level option configuration bits
      BOR_LEV        : OPTSR_PRG_BOR_LEV_Field := 16#0#;
      --  IWDG1 option configuration bit
      IWDG1_SW       : Boolean := False;
      --  unspecified
      Reserved_5_5   : STM32H753.Bit := 16#0#;
      --  Option byte erase after D1 DStop option configuration bit
      NRST_STOP_D1   : Boolean := False;
      --  Option byte erase after D1 DStandby option configuration bit
      NRST_STBY_D1   : Boolean := False;
      --  Readout protection level option configuration byte
      RDP            : OPTSR_PRG_RDP_Field := 16#0#;
      --  unspecified
      Reserved_16_16 : STM32H753.Bit := 16#0#;
      --  IWDG Stop mode freeze option configuration bit
      IWDG_FZ_STOP   : Boolean := False;
      --  IWDG Standby mode freeze option configuration bit
      IWDG_FZ_SDBY   : Boolean := False;
      --  DTCM size select option configuration bits
      ST_RAM_SIZE    : OPTSR_PRG_ST_RAM_SIZE_Field := 16#0#;
      --  Security option configuration bit
      SECURITY       : Boolean := False;
      --  unspecified
      Reserved_22_28 : STM32H753.UInt7 := 16#0#;
      --  I/O high-speed at low-voltage (PRODUCT_BELOW_25V)
      IO_HSLV        : Boolean := False;
      --  unspecified
      Reserved_30_30 : STM32H753.Bit := 16#0#;
      --  Bank swapping option configuration bit
      SWAP_BANK_OPT  : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OPTSR_PRG_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      BOR_LEV        at 0 range 2 .. 3;
      IWDG1_SW       at 0 range 4 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      NRST_STOP_D1   at 0 range 6 .. 6;
      NRST_STBY_D1   at 0 range 7 .. 7;
      RDP            at 0 range 8 .. 15;
      Reserved_16_16 at 0 range 16 .. 16;
      IWDG_FZ_STOP   at 0 range 17 .. 17;
      IWDG_FZ_SDBY   at 0 range 18 .. 18;
      ST_RAM_SIZE    at 0 range 19 .. 20;
      SECURITY       at 0 range 21 .. 21;
      Reserved_22_28 at 0 range 22 .. 28;
      IO_HSLV        at 0 range 29 .. 29;
      Reserved_30_30 at 0 range 30 .. 30;
      SWAP_BANK_OPT  at 0 range 31 .. 31;
   end record;

   --  FLASH option clear control register
   type OPTCCR_Register is record
      --  unspecified
      Reserved_0_29    : STM32H753.UInt30 := 16#0#;
      --  Write-only. OPTCHANGEERR reset bit
      CLR_OPTCHANGEERR : Boolean := False;
      --  unspecified
      Reserved_31_31   : STM32H753.Bit := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OPTCCR_Register use record
      Reserved_0_29    at 0 range 0 .. 29;
      CLR_OPTCHANGEERR at 0 range 30 .. 30;
      Reserved_31_31   at 0 range 31 .. 31;
   end record;

   subtype PRAR_CUR1_PROT_AREA_START1_Field is STM32H753.UInt12;
   subtype PRAR_CUR1_PROT_AREA_END1_Field is STM32H753.UInt12;

   --  FLASH protection address for bank 1
   type PRAR_CUR1_Register is record
      --  Read-only. Bank 1 lowest PCROP protected address
      PROT_AREA_START1 : PRAR_CUR1_PROT_AREA_START1_Field;
      --  unspecified
      Reserved_12_15   : STM32H753.UInt4;
      --  Read-only. Bank 1 highest PCROP protected address
      PROT_AREA_END1   : PRAR_CUR1_PROT_AREA_END1_Field;
      --  unspecified
      Reserved_28_30   : STM32H753.UInt3;
      --  Read-only. Bank 1 PCROP protected erase enable option status bit
      DMEP1            : Boolean;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PRAR_CUR1_Register use record
      PROT_AREA_START1 at 0 range 0 .. 11;
      Reserved_12_15   at 0 range 12 .. 15;
      PROT_AREA_END1   at 0 range 16 .. 27;
      Reserved_28_30   at 0 range 28 .. 30;
      DMEP1            at 0 range 31 .. 31;
   end record;

   subtype PRAR_PRG1_PROT_AREA_START1_Field is STM32H753.UInt12;
   subtype PRAR_PRG1_PROT_AREA_END1_Field is STM32H753.UInt12;

   --  FLASH protection address for bank 1
   type PRAR_PRG1_Register is record
      --  Bank 1 lowest PCROP protected address configuration
      PROT_AREA_START1 : PRAR_PRG1_PROT_AREA_START1_Field := 16#0#;
      --  unspecified
      Reserved_12_15   : STM32H753.UInt4 := 16#0#;
      --  Bank 1 highest PCROP protected address configuration
      PROT_AREA_END1   : PRAR_PRG1_PROT_AREA_END1_Field := 16#0#;
      --  unspecified
      Reserved_28_30   : STM32H753.UInt3 := 16#0#;
      --  Bank 1 PCROP protected erase enable option configuration bit
      DMEP1            : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PRAR_PRG1_Register use record
      PROT_AREA_START1 at 0 range 0 .. 11;
      Reserved_12_15   at 0 range 12 .. 15;
      PROT_AREA_END1   at 0 range 16 .. 27;
      Reserved_28_30   at 0 range 28 .. 30;
      DMEP1            at 0 range 31 .. 31;
   end record;

   subtype SCAR_CUR1_SEC_AREA_START1_Field is STM32H753.UInt12;
   subtype SCAR_CUR1_SEC_AREA_END1_Field is STM32H753.UInt12;

   --  FLASH secure address for bank 1
   type SCAR_CUR1_Register is record
      --  Bank 1 lowest secure protected address
      SEC_AREA_START1 : SCAR_CUR1_SEC_AREA_START1_Field := 16#0#;
      --  unspecified
      Reserved_12_15  : STM32H753.UInt4 := 16#0#;
      --  Bank 1 highest secure protected address
      SEC_AREA_END1   : SCAR_CUR1_SEC_AREA_END1_Field := 16#0#;
      --  unspecified
      Reserved_28_30  : STM32H753.UInt3 := 16#0#;
      --  Bank 1 secure protected erase enable option status bit
      DMES1           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SCAR_CUR1_Register use record
      SEC_AREA_START1 at 0 range 0 .. 11;
      Reserved_12_15  at 0 range 12 .. 15;
      SEC_AREA_END1   at 0 range 16 .. 27;
      Reserved_28_30  at 0 range 28 .. 30;
      DMES1           at 0 range 31 .. 31;
   end record;

   subtype SCAR_PRG1_SEC_AREA_START1_Field is STM32H753.UInt12;
   subtype SCAR_PRG1_SEC_AREA_END1_Field is STM32H753.UInt12;

   --  FLASH secure address for bank 1
   type SCAR_PRG1_Register is record
      --  Bank 1 lowest secure protected address configuration
      SEC_AREA_START1 : SCAR_PRG1_SEC_AREA_START1_Field := 16#0#;
      --  unspecified
      Reserved_12_15  : STM32H753.UInt4 := 16#0#;
      --  Bank 1 highest secure protected address configuration
      SEC_AREA_END1   : SCAR_PRG1_SEC_AREA_END1_Field := 16#0#;
      --  unspecified
      Reserved_28_30  : STM32H753.UInt3 := 16#0#;
      --  Bank 1 secure protected erase enable option configuration bit
      DMES1           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SCAR_PRG1_Register use record
      SEC_AREA_START1 at 0 range 0 .. 11;
      Reserved_12_15  at 0 range 12 .. 15;
      SEC_AREA_END1   at 0 range 16 .. 27;
      Reserved_28_30  at 0 range 28 .. 30;
      DMES1           at 0 range 31 .. 31;
   end record;

   subtype WPSN_CUR1R_WRPSn1_Field is STM32H753.Byte;

   --  FLASH write sector protection for bank 1
   type WPSN_CUR1R_Register is record
      --  Read-only. Bank 1 sector write protection option status byte
      WRPSn1        : WPSN_CUR1R_WRPSn1_Field;
      --  unspecified
      Reserved_8_31 : STM32H753.UInt24;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for WPSN_CUR1R_Register use record
      WRPSn1        at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype WPSN_PRG1R_WRPSn1_Field is STM32H753.Byte;

   --  FLASH write sector protection for bank 1
   type WPSN_PRG1R_Register is record
      --  Bank 1 sector write protection configuration byte
      WRPSn1        : WPSN_PRG1R_WRPSn1_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : STM32H753.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for WPSN_PRG1R_Register use record
      WRPSn1        at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  BOOT_CURR_BOOT_ADD array element
   subtype BOOT_CURR_BOOT_ADD_Element is STM32H753.UInt16;

   --  BOOT_CURR_BOOT_ADD array
   type BOOT_CURR_BOOT_ADD_Field_Array is array (0 .. 1)
     of BOOT_CURR_BOOT_ADD_Element
     with Component_Size => 16, Size => 32;

   --  FLASH register with boot address
   type BOOT_CURR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  BOOT_ADD as a value
            Val : STM32H753.UInt32;
         when True =>
            --  BOOT_ADD as an array
            Arr : BOOT_CURR_BOOT_ADD_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for BOOT_CURR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   --  BOOT_PRGR_BOOT_ADD array element
   subtype BOOT_PRGR_BOOT_ADD_Element is STM32H753.UInt16;

   --  BOOT_PRGR_BOOT_ADD array
   type BOOT_PRGR_BOOT_ADD_Field_Array is array (0 .. 1)
     of BOOT_PRGR_BOOT_ADD_Element
     with Component_Size => 16, Size => 32;

   --  FLASH register with boot address
   type BOOT_PRGR_Register
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  BOOT_ADD as a value
            Val : STM32H753.UInt32;
         when True =>
            --  BOOT_ADD as an array
            Arr : BOOT_PRGR_BOOT_ADD_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 32, Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for BOOT_PRGR_Register use record
      Val at 0 range 0 .. 31;
      Arr at 0 range 0 .. 31;
   end record;

   subtype CRCCR_CRC_SECT_Field is STM32H753.UInt3;
   subtype CRCCR_CRC_BURST_Field is STM32H753.UInt2;

   --  FLASH CRC control register for bank 1
   type CRCCR_Register is record
      --  Bank 1 CRC sector number
      CRC_SECT       : CRCCR_CRC_SECT_Field := 16#0#;
      --  unspecified
      Reserved_3_6   : STM32H753.UInt4 := 16#0#;
      --  Bank 1 CRC select bit
      ALL_BANK       : Boolean := False;
      --  Bank 1 CRC sector mode select bit
      CRC_BY_SECT    : Boolean := False;
      --  Bank 1 CRC sector select bit
      ADD_SECT       : Boolean := False;
      --  Bank 1 CRC sector list clear bit
      CLEAN_SECT     : Boolean := False;
      --  unspecified
      Reserved_11_15 : STM32H753.UInt5 := 16#0#;
      --  Bank 1 CRC start bit
      START_CRC      : Boolean := False;
      --  Bank 1 CRC clear bit
      CLEAN_CRC      : Boolean := False;
      --  unspecified
      Reserved_18_19 : STM32H753.UInt2 := 16#0#;
      --  Bank 1 CRC burst size
      CRC_BURST      : CRCCR_CRC_BURST_Field := 16#0#;
      --  unspecified
      Reserved_22_31 : STM32H753.UInt10 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CRCCR_Register use record
      CRC_SECT       at 0 range 0 .. 2;
      Reserved_3_6   at 0 range 3 .. 6;
      ALL_BANK       at 0 range 7 .. 7;
      CRC_BY_SECT    at 0 range 8 .. 8;
      ADD_SECT       at 0 range 9 .. 9;
      CLEAN_SECT     at 0 range 10 .. 10;
      Reserved_11_15 at 0 range 11 .. 15;
      START_CRC      at 0 range 16 .. 16;
      CLEAN_CRC      at 0 range 17 .. 17;
      Reserved_18_19 at 0 range 18 .. 19;
      CRC_BURST      at 0 range 20 .. 21;
      Reserved_22_31 at 0 range 22 .. 31;
   end record;

   subtype CRCSADD1R_CRC_START_ADDR_Field is STM32H753.UInt18;

   --  FLASH CRC start address register for bank 1
   type CRCSADD1R_Register is record
      --  unspecified
      Reserved_0_1   : STM32H753.UInt2 := 16#0#;
      --  CRC start address on bank 1
      CRC_START_ADDR : CRCSADD1R_CRC_START_ADDR_Field := 16#0#;
      --  unspecified
      Reserved_20_31 : STM32H753.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CRCSADD1R_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      CRC_START_ADDR at 0 range 2 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   subtype CRCEADD1R_CRC_END_ADDR_Field is STM32H753.UInt18;

   --  FLASH CRC end address register for bank 1
   type CRCEADD1R_Register is record
      --  unspecified
      Reserved_0_1   : STM32H753.UInt2 := 16#0#;
      --  CRC end address on bank 1
      CRC_END_ADDR   : CRCEADD1R_CRC_END_ADDR_Field := 16#0#;
      --  unspecified
      Reserved_20_31 : STM32H753.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CRCEADD1R_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      CRC_END_ADDR   at 0 range 2 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   subtype ECC_FA1R_FAIL_ECC_ADDR1_Field is STM32H753.UInt15;

   --  FLASH ECC fail address for bank 1
   type ECC_FA1R_Register is record
      --  Read-only. Bank 1 ECC error address
      FAIL_ECC_ADDR1 : ECC_FA1R_FAIL_ECC_ADDR1_Field;
      --  unspecified
      Reserved_15_31 : STM32H753.UInt17;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ECC_FA1R_Register use record
      FAIL_ECC_ADDR1 at 0 range 0 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   subtype CR2_PSIZE2_Field is STM32H753.UInt2;
   subtype CR2_SNB2_Field is STM32H753.UInt3;

   --  FLASH control register for bank 2
   type CR2_Register is record
      --  Bank 2 configuration lock bit
      LOCK2          : Boolean := True;
      --  Bank 2 program enable bit
      PG2            : Boolean := False;
      --  Bank 2 sector erase request
      SER2           : Boolean := False;
      --  Bank 2 erase request
      BER2           : Boolean := False;
      --  Bank 2 program size
      PSIZE2         : CR2_PSIZE2_Field := 16#3#;
      --  Bank 2 write forcing control bit
      FW2            : Boolean := False;
      --  Bank 2 bank or sector erase start control bit
      START2         : Boolean := False;
      --  Bank 2 sector erase selection number
      SNB2           : CR2_SNB2_Field := 16#0#;
      --  unspecified
      Reserved_11_13 : STM32H753.UInt3 := 16#0#;
      --  Bank 2 special sector selection bit
      SPSS2          : Boolean := False;
      --  Bank 2 CRC control bit
      CRC_EN         : Boolean := False;
      --  Bank 2 end-of-program interrupt control bit
      EOPIE2         : Boolean := False;
      --  Bank 2 write protection error interrupt enable bit
      WRPERRIE2      : Boolean := False;
      --  Bank 2 programming sequence error interrupt enable bit
      PGSERRIE2      : Boolean := False;
      --  Bank 2 strobe error interrupt enable bit
      STRBERRIE2     : Boolean := False;
      --  unspecified
      Reserved_20_20 : STM32H753.Bit := 16#0#;
      --  Bank 2 inconsistency error interrupt enable bit
      INCERRIE2      : Boolean := False;
      --  Bank 2 write/erase error interrupt enable bit
      OPERRIE2       : Boolean := False;
      --  Bank 2 read protection error interrupt enable bit
      RDPERRIE2      : Boolean := False;
      --  Bank 2 secure error interrupt enable bit
      RDSERRIE2      : Boolean := False;
      --  Bank 2 ECC single correction error interrupt enable bit
      SNECCERRIE2    : Boolean := False;
      --  Bank 2 ECC double detection error interrupt enable bit
      DBECCERRIE2    : Boolean := False;
      --  Bank 2 end of CRC calculation interrupt enable bit
      CRCENDIE2      : Boolean := False;
      --  Bank 2 CRC read error interrupt enable bit
      CRCRDERRIE2    : Boolean := False;
      --  unspecified
      Reserved_29_31 : STM32H753.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR2_Register use record
      LOCK2          at 0 range 0 .. 0;
      PG2            at 0 range 1 .. 1;
      SER2           at 0 range 2 .. 2;
      BER2           at 0 range 3 .. 3;
      PSIZE2         at 0 range 4 .. 5;
      FW2            at 0 range 6 .. 6;
      START2         at 0 range 7 .. 7;
      SNB2           at 0 range 8 .. 10;
      Reserved_11_13 at 0 range 11 .. 13;
      SPSS2          at 0 range 14 .. 14;
      CRC_EN         at 0 range 15 .. 15;
      EOPIE2         at 0 range 16 .. 16;
      WRPERRIE2      at 0 range 17 .. 17;
      PGSERRIE2      at 0 range 18 .. 18;
      STRBERRIE2     at 0 range 19 .. 19;
      Reserved_20_20 at 0 range 20 .. 20;
      INCERRIE2      at 0 range 21 .. 21;
      OPERRIE2       at 0 range 22 .. 22;
      RDPERRIE2      at 0 range 23 .. 23;
      RDSERRIE2      at 0 range 24 .. 24;
      SNECCERRIE2    at 0 range 25 .. 25;
      DBECCERRIE2    at 0 range 26 .. 26;
      CRCENDIE2      at 0 range 27 .. 27;
      CRCRDERRIE2    at 0 range 28 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   --  FLASH status register for bank 2
   type SR2_Register is record
      --  Bank 2 ongoing program flag
      BSY2           : Boolean := False;
      --  Bank 2 write buffer not empty flag
      WBNE2          : Boolean := False;
      --  Bank 2 wait queue flag
      QW2            : Boolean := False;
      --  Bank 2 CRC busy flag
      CRC_BUSY2      : Boolean := False;
      --  unspecified
      Reserved_4_15  : STM32H753.UInt12 := 16#0#;
      --  Bank 2 end-of-program flag
      EOP2           : Boolean := False;
      --  Bank 2 write protection error flag
      WRPERR2        : Boolean := False;
      --  Bank 2 programming sequence error flag
      PGSERR2        : Boolean := False;
      --  Bank 2 strobe error flag
      STRBERR2       : Boolean := False;
      --  unspecified
      Reserved_20_20 : STM32H753.Bit := 16#0#;
      --  Bank 2 inconsistency error flag
      INCERR2        : Boolean := False;
      --  Bank 2 write/erase error flag
      OPERR2         : Boolean := False;
      --  Bank 2 read protection error flag
      RDPERR2        : Boolean := False;
      --  Bank 2 secure error flag
      RDSERR2        : Boolean := False;
      --  Bank 2 single correction error flag
      SNECCERR2      : Boolean := False;
      --  Bank 2 ECC double detection error flag
      DBECCERR2      : Boolean := False;
      --  Bank 2 CRC-complete flag
      CRCEND2        : Boolean := False;
      --  unspecified
      Reserved_28_31 : STM32H753.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR2_Register use record
      BSY2           at 0 range 0 .. 0;
      WBNE2          at 0 range 1 .. 1;
      QW2            at 0 range 2 .. 2;
      CRC_BUSY2      at 0 range 3 .. 3;
      Reserved_4_15  at 0 range 4 .. 15;
      EOP2           at 0 range 16 .. 16;
      WRPERR2        at 0 range 17 .. 17;
      PGSERR2        at 0 range 18 .. 18;
      STRBERR2       at 0 range 19 .. 19;
      Reserved_20_20 at 0 range 20 .. 20;
      INCERR2        at 0 range 21 .. 21;
      OPERR2         at 0 range 22 .. 22;
      RDPERR2        at 0 range 23 .. 23;
      RDSERR2        at 0 range 24 .. 24;
      SNECCERR2      at 0 range 25 .. 25;
      DBECCERR2      at 0 range 26 .. 26;
      CRCEND2        at 0 range 27 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  FLASH clear control register for bank 2
   type CCR2_Register is record
      --  unspecified
      Reserved_0_15  : STM32H753.UInt16 := 16#0#;
      --  Bank 1 EOP1 flag clear bit
      CLR_EOP2       : Boolean := False;
      --  Bank 2 WRPERR1 flag clear bit
      CLR_WRPERR2    : Boolean := False;
      --  Bank 2 PGSERR1 flag clear bi
      CLR_PGSERR2    : Boolean := False;
      --  Bank 2 STRBERR1 flag clear bit
      CLR_STRBERR2   : Boolean := False;
      --  unspecified
      Reserved_20_20 : STM32H753.Bit := 16#0#;
      --  Bank 2 INCERR1 flag clear bit
      CLR_INCERR2    : Boolean := False;
      --  Bank 2 OPERR1 flag clear bit
      CLR_OPERR2     : Boolean := False;
      --  Bank 2 RDPERR1 flag clear bit
      CLR_RDPERR2    : Boolean := False;
      --  Bank 1 RDSERR1 flag clear bit
      CLR_RDSERR1    : Boolean := False;
      --  Bank 2 SNECCERR1 flag clear bit
      CLR_SNECCERR2  : Boolean := False;
      --  Bank 1 DBECCERR1 flag clear bit
      CLR_DBECCERR1  : Boolean := False;
      --  Bank 2 CRCEND1 flag clear bit
      CLR_CRCEND2    : Boolean := False;
      --  unspecified
      Reserved_28_31 : STM32H753.UInt4 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CCR2_Register use record
      Reserved_0_15  at 0 range 0 .. 15;
      CLR_EOP2       at 0 range 16 .. 16;
      CLR_WRPERR2    at 0 range 17 .. 17;
      CLR_PGSERR2    at 0 range 18 .. 18;
      CLR_STRBERR2   at 0 range 19 .. 19;
      Reserved_20_20 at 0 range 20 .. 20;
      CLR_INCERR2    at 0 range 21 .. 21;
      CLR_OPERR2     at 0 range 22 .. 22;
      CLR_RDPERR2    at 0 range 23 .. 23;
      CLR_RDSERR1    at 0 range 24 .. 24;
      CLR_SNECCERR2  at 0 range 25 .. 25;
      CLR_DBECCERR1  at 0 range 26 .. 26;
      CLR_CRCEND2    at 0 range 27 .. 27;
      Reserved_28_31 at 0 range 28 .. 31;
   end record;

   --  FLASH option status register
   type OPTSR_PRG_Register_1 is record
      --  unspecified
      Reserved_0_1   : STM32H753.UInt2 := 16#0#;
      --  BOR reset level option configuration bits
      BOR_LEV        : OPTSR_PRG_BOR_LEV_Field := 16#0#;
      --  IWDG1 option configuration bit
      IWDG1_SW       : Boolean := False;
      --  unspecified
      Reserved_5_5   : STM32H753.Bit := 16#0#;
      --  Option byte erase after D1 DStop option configuration bit
      nRST_STOP      : Boolean := False;
      --  Option byte erase after D1 DStandby option configuration bit
      nRST_STDY      : Boolean := False;
      --  Readout protection level option configuration byte
      RDP            : OPTSR_PRG_RDP_Field := 16#0#;
      --  unspecified
      Reserved_16_16 : STM32H753.Bit := 16#0#;
      --  IWDG Stop mode freeze option configuration bit
      FZ_IWDG_STOP   : Boolean := False;
      --  IWDG Standby mode freeze option configuration bit
      FZ_IWDG_SDBY   : Boolean := False;
      --  DTCM size select option configuration bits
      ST_RAM_SIZE    : OPTSR_PRG_ST_RAM_SIZE_Field := 16#0#;
      --  Security option configuration bit
      SECURITY       : Boolean := False;
      --  unspecified
      Reserved_22_28 : STM32H753.UInt7 := 16#0#;
      --  I/O high-speed at low-voltage (PRODUCT_BELOW_25V)
      IO_HSLV        : Boolean := False;
      --  unspecified
      Reserved_30_30 : STM32H753.Bit := 16#0#;
      --  Bank swapping option configuration bit
      SWAP_BANK_OPT  : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for OPTSR_PRG_Register_1 use record
      Reserved_0_1   at 0 range 0 .. 1;
      BOR_LEV        at 0 range 2 .. 3;
      IWDG1_SW       at 0 range 4 .. 4;
      Reserved_5_5   at 0 range 5 .. 5;
      nRST_STOP      at 0 range 6 .. 6;
      nRST_STDY      at 0 range 7 .. 7;
      RDP            at 0 range 8 .. 15;
      Reserved_16_16 at 0 range 16 .. 16;
      FZ_IWDG_STOP   at 0 range 17 .. 17;
      FZ_IWDG_SDBY   at 0 range 18 .. 18;
      ST_RAM_SIZE    at 0 range 19 .. 20;
      SECURITY       at 0 range 21 .. 21;
      Reserved_22_28 at 0 range 22 .. 28;
      IO_HSLV        at 0 range 29 .. 29;
      Reserved_30_30 at 0 range 30 .. 30;
      SWAP_BANK_OPT  at 0 range 31 .. 31;
   end record;

   subtype PRAR_CUR2_PROT_AREA_START2_Field is STM32H753.UInt12;
   subtype PRAR_CUR2_PROT_AREA_END2_Field is STM32H753.UInt12;

   --  FLASH protection address for bank 1
   type PRAR_CUR2_Register is record
      --  Read-only. Bank 2 lowest PCROP protected address
      PROT_AREA_START2 : PRAR_CUR2_PROT_AREA_START2_Field;
      --  unspecified
      Reserved_12_15   : STM32H753.UInt4;
      --  Read-only. Bank 2 highest PCROP protected address
      PROT_AREA_END2   : PRAR_CUR2_PROT_AREA_END2_Field;
      --  unspecified
      Reserved_28_30   : STM32H753.UInt3;
      --  Read-only. Bank 2 PCROP protected erase enable option status bit
      DMEP2            : Boolean;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PRAR_CUR2_Register use record
      PROT_AREA_START2 at 0 range 0 .. 11;
      Reserved_12_15   at 0 range 12 .. 15;
      PROT_AREA_END2   at 0 range 16 .. 27;
      Reserved_28_30   at 0 range 28 .. 30;
      DMEP2            at 0 range 31 .. 31;
   end record;

   subtype PRAR_PRG2_PROT_AREA_START2_Field is STM32H753.UInt12;
   subtype PRAR_PRG2_PROT_AREA_END2_Field is STM32H753.UInt12;

   --  FLASH protection address for bank 2
   type PRAR_PRG2_Register is record
      --  Bank 2 lowest PCROP protected address configuration
      PROT_AREA_START2 : PRAR_PRG2_PROT_AREA_START2_Field := 16#0#;
      --  unspecified
      Reserved_12_15   : STM32H753.UInt4 := 16#0#;
      --  Bank 2 highest PCROP protected address configuration
      PROT_AREA_END2   : PRAR_PRG2_PROT_AREA_END2_Field := 16#0#;
      --  unspecified
      Reserved_28_30   : STM32H753.UInt3 := 16#0#;
      --  Bank 2 PCROP protected erase enable option configuration bit
      DMEP2            : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for PRAR_PRG2_Register use record
      PROT_AREA_START2 at 0 range 0 .. 11;
      Reserved_12_15   at 0 range 12 .. 15;
      PROT_AREA_END2   at 0 range 16 .. 27;
      Reserved_28_30   at 0 range 28 .. 30;
      DMEP2            at 0 range 31 .. 31;
   end record;

   subtype SCAR_CUR2_SEC_AREA_START2_Field is STM32H753.UInt12;
   subtype SCAR_CUR2_SEC_AREA_END2_Field is STM32H753.UInt12;

   --  FLASH secure address for bank 2
   type SCAR_CUR2_Register is record
      --  Bank 2 lowest secure protected address
      SEC_AREA_START2 : SCAR_CUR2_SEC_AREA_START2_Field := 16#0#;
      --  unspecified
      Reserved_12_15  : STM32H753.UInt4 := 16#0#;
      --  Bank 2 highest secure protected address
      SEC_AREA_END2   : SCAR_CUR2_SEC_AREA_END2_Field := 16#0#;
      --  unspecified
      Reserved_28_30  : STM32H753.UInt3 := 16#0#;
      --  Bank 2 secure protected erase enable option status bit
      DMES2           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SCAR_CUR2_Register use record
      SEC_AREA_START2 at 0 range 0 .. 11;
      Reserved_12_15  at 0 range 12 .. 15;
      SEC_AREA_END2   at 0 range 16 .. 27;
      Reserved_28_30  at 0 range 28 .. 30;
      DMES2           at 0 range 31 .. 31;
   end record;

   subtype SCAR_PRG2_SEC_AREA_START2_Field is STM32H753.UInt12;
   subtype SCAR_PRG2_SEC_AREA_END2_Field is STM32H753.UInt12;

   --  FLASH secure address for bank 2
   type SCAR_PRG2_Register is record
      --  Bank 2 lowest secure protected address configuration
      SEC_AREA_START2 : SCAR_PRG2_SEC_AREA_START2_Field := 16#0#;
      --  unspecified
      Reserved_12_15  : STM32H753.UInt4 := 16#0#;
      --  Bank 2 highest secure protected address configuration
      SEC_AREA_END2   : SCAR_PRG2_SEC_AREA_END2_Field := 16#0#;
      --  unspecified
      Reserved_28_30  : STM32H753.UInt3 := 16#0#;
      --  Bank 2 secure protected erase enable option configuration bit
      DMES2           : Boolean := False;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SCAR_PRG2_Register use record
      SEC_AREA_START2 at 0 range 0 .. 11;
      Reserved_12_15  at 0 range 12 .. 15;
      SEC_AREA_END2   at 0 range 16 .. 27;
      Reserved_28_30  at 0 range 28 .. 30;
      DMES2           at 0 range 31 .. 31;
   end record;

   subtype WPSN_CUR2R_WRPSn2_Field is STM32H753.Byte;

   --  FLASH write sector protection for bank 2
   type WPSN_CUR2R_Register is record
      --  Read-only. Bank 2 sector write protection option status byte
      WRPSn2        : WPSN_CUR2R_WRPSn2_Field;
      --  unspecified
      Reserved_8_31 : STM32H753.UInt24;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for WPSN_CUR2R_Register use record
      WRPSn2        at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype WPSN_PRG2R_WRPSn2_Field is STM32H753.Byte;

   --  FLASH write sector protection for bank 2
   type WPSN_PRG2R_Register is record
      --  Bank 2 sector write protection configuration byte
      WRPSn2        : WPSN_PRG2R_WRPSn2_Field := 16#0#;
      --  unspecified
      Reserved_8_31 : STM32H753.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for WPSN_PRG2R_Register use record
      WRPSn2        at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype CRCSADD2R_CRC_START_ADDR_Field is STM32H753.UInt18;

   --  FLASH CRC start address register for bank 2
   type CRCSADD2R_Register is record
      --  unspecified
      Reserved_0_1   : STM32H753.UInt2 := 16#0#;
      --  CRC start address on bank 2
      CRC_START_ADDR : CRCSADD2R_CRC_START_ADDR_Field := 16#0#;
      --  unspecified
      Reserved_20_31 : STM32H753.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CRCSADD2R_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      CRC_START_ADDR at 0 range 2 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   subtype CRCEADD2R_CRC_END_ADDR_Field is STM32H753.UInt18;

   --  FLASH CRC end address register for bank 2
   type CRCEADD2R_Register is record
      --  unspecified
      Reserved_0_1   : STM32H753.UInt2 := 16#0#;
      --  CRC end address on bank 2
      CRC_END_ADDR   : CRCEADD2R_CRC_END_ADDR_Field := 16#0#;
      --  unspecified
      Reserved_20_31 : STM32H753.UInt12 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CRCEADD2R_Register use record
      Reserved_0_1   at 0 range 0 .. 1;
      CRC_END_ADDR   at 0 range 2 .. 19;
      Reserved_20_31 at 0 range 20 .. 31;
   end record;

   subtype ECC_FA2R_FAIL_ECC_ADDR2_Field is STM32H753.UInt15;

   --  FLASH ECC fail address for bank 2
   type ECC_FA2R_Register is record
      --  Read-only. Bank 2 ECC error address
      FAIL_ECC_ADDR2 : ECC_FA2R_FAIL_ECC_ADDR2_Field;
      --  unspecified
      Reserved_15_31 : STM32H753.UInt17;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for ECC_FA2R_Register use record
      FAIL_ECC_ADDR2 at 0 range 0 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  Flash
   type Flash_Peripheral is record
      --  Access control register
      ACR         : aliased ACR_Register;
      --  FLASH key register for bank 1
      KEYR1       : aliased STM32H753.UInt32;
      --  FLASH option key register
      OPTKEYR     : aliased STM32H753.UInt32;
      --  FLASH control register for bank 1
      CR1         : aliased CR1_Register;
      --  FLASH status register for bank 1
      SR1         : aliased SR1_Register;
      --  FLASH clear control register for bank 1
      CCR1        : aliased CCR1_Register;
      --  FLASH option control register
      OPTCR       : aliased OPTCR_Register;
      --  FLASH option status register
      OPTSR_CUR   : aliased OPTSR_CUR_Register;
      --  FLASH option status register
      OPTSR_PRG   : aliased OPTSR_PRG_Register;
      --  FLASH option clear control register
      OPTCCR      : aliased OPTCCR_Register;
      --  FLASH protection address for bank 1
      PRAR_CUR1   : aliased PRAR_CUR1_Register;
      --  FLASH protection address for bank 1
      PRAR_PRG1   : aliased PRAR_PRG1_Register;
      --  FLASH secure address for bank 1
      SCAR_CUR1   : aliased SCAR_CUR1_Register;
      --  FLASH secure address for bank 1
      SCAR_PRG1   : aliased SCAR_PRG1_Register;
      --  FLASH write sector protection for bank 1
      WPSN_CUR1R  : aliased WPSN_CUR1R_Register;
      --  FLASH write sector protection for bank 1
      WPSN_PRG1R  : aliased WPSN_PRG1R_Register;
      --  FLASH register with boot address
      BOOT_CURR   : aliased BOOT_CURR_Register;
      --  FLASH register with boot address
      BOOT_PRGR   : aliased BOOT_PRGR_Register;
      --  FLASH CRC control register for bank 1
      CRCCR1      : aliased CRCCR_Register;
      --  FLASH CRC start address register for bank 1
      CRCSADD1R   : aliased CRCSADD1R_Register;
      --  FLASH CRC end address register for bank 1
      CRCEADD1R   : aliased CRCEADD1R_Register;
      --  FLASH CRC data register
      CRCDATAR    : aliased STM32H753.UInt32;
      --  FLASH ECC fail address for bank 1
      ECC_FA1R    : aliased ECC_FA1R_Register;
      --  Access control register
      ACR_1       : aliased ACR_Register;
      --  FLASH key register for bank 2
      KEYR2       : aliased STM32H753.UInt32;
      --  FLASH option key register
      OPTKEYR_1   : aliased STM32H753.UInt32;
      --  FLASH control register for bank 2
      CR2         : aliased CR2_Register;
      --  FLASH status register for bank 2
      SR2         : aliased SR2_Register;
      --  FLASH clear control register for bank 2
      CCR2        : aliased CCR2_Register;
      --  FLASH option control register
      OPTCR_1     : aliased OPTCR_Register;
      --  FLASH option status register
      OPTSR_CUR_1 : aliased OPTSR_CUR_Register;
      --  FLASH option status register
      OPTSR_PRG_1 : aliased OPTSR_PRG_Register_1;
      --  FLASH option clear control register
      OPTCCR_1    : aliased OPTCCR_Register;
      --  FLASH protection address for bank 1
      PRAR_CUR2   : aliased PRAR_CUR2_Register;
      --  FLASH protection address for bank 2
      PRAR_PRG2   : aliased PRAR_PRG2_Register;
      --  FLASH secure address for bank 2
      SCAR_CUR2   : aliased SCAR_CUR2_Register;
      --  FLASH secure address for bank 2
      SCAR_PRG2   : aliased SCAR_PRG2_Register;
      --  FLASH write sector protection for bank 2
      WPSN_CUR2R  : aliased WPSN_CUR2R_Register;
      --  FLASH write sector protection for bank 2
      WPSN_PRG2R  : aliased WPSN_PRG2R_Register;
      --  FLASH register with boot address
      BOOT_CURR_1 : aliased BOOT_CURR_Register;
      --  FLASH register with boot address
      BOOT_PRGR_1 : aliased BOOT_PRGR_Register;
      --  FLASH CRC control register for bank 1
      CRCCR2      : aliased CRCCR_Register;
      --  FLASH CRC start address register for bank 2
      CRCSADD2R   : aliased CRCSADD2R_Register;
      --  FLASH CRC end address register for bank 2
      CRCEADD2R   : aliased CRCEADD2R_Register;
      --  FLASH CRC data register
      CRCDATAR_1  : aliased STM32H753.UInt32;
      --  FLASH ECC fail address for bank 2
      ECC_FA2R    : aliased ECC_FA2R_Register;
   end record
     with Volatile;

   for Flash_Peripheral use record
      ACR         at 16#0# range 0 .. 31;
      KEYR1       at 16#4# range 0 .. 31;
      OPTKEYR     at 16#8# range 0 .. 31;
      CR1         at 16#C# range 0 .. 31;
      SR1         at 16#10# range 0 .. 31;
      CCR1        at 16#14# range 0 .. 31;
      OPTCR       at 16#18# range 0 .. 31;
      OPTSR_CUR   at 16#1C# range 0 .. 31;
      OPTSR_PRG   at 16#20# range 0 .. 31;
      OPTCCR      at 16#24# range 0 .. 31;
      PRAR_CUR1   at 16#28# range 0 .. 31;
      PRAR_PRG1   at 16#2C# range 0 .. 31;
      SCAR_CUR1   at 16#30# range 0 .. 31;
      SCAR_PRG1   at 16#34# range 0 .. 31;
      WPSN_CUR1R  at 16#38# range 0 .. 31;
      WPSN_PRG1R  at 16#3C# range 0 .. 31;
      BOOT_CURR   at 16#40# range 0 .. 31;
      BOOT_PRGR   at 16#44# range 0 .. 31;
      CRCCR1      at 16#50# range 0 .. 31;
      CRCSADD1R   at 16#54# range 0 .. 31;
      CRCEADD1R   at 16#58# range 0 .. 31;
      CRCDATAR    at 16#5C# range 0 .. 31;
      ECC_FA1R    at 16#60# range 0 .. 31;
      ACR_1       at 16#100# range 0 .. 31;
      KEYR2       at 16#104# range 0 .. 31;
      OPTKEYR_1   at 16#108# range 0 .. 31;
      CR2         at 16#10C# range 0 .. 31;
      SR2         at 16#110# range 0 .. 31;
      CCR2        at 16#114# range 0 .. 31;
      OPTCR_1     at 16#118# range 0 .. 31;
      OPTSR_CUR_1 at 16#11C# range 0 .. 31;
      OPTSR_PRG_1 at 16#120# range 0 .. 31;
      OPTCCR_1    at 16#124# range 0 .. 31;
      PRAR_CUR2   at 16#128# range 0 .. 31;
      PRAR_PRG2   at 16#12C# range 0 .. 31;
      SCAR_CUR2   at 16#130# range 0 .. 31;
      SCAR_PRG2   at 16#134# range 0 .. 31;
      WPSN_CUR2R  at 16#138# range 0 .. 31;
      WPSN_PRG2R  at 16#13C# range 0 .. 31;
      BOOT_CURR_1 at 16#140# range 0 .. 31;
      BOOT_PRGR_1 at 16#144# range 0 .. 31;
      CRCCR2      at 16#150# range 0 .. 31;
      CRCSADD2R   at 16#154# range 0 .. 31;
      CRCEADD2R   at 16#158# range 0 .. 31;
      CRCDATAR_1  at 16#15C# range 0 .. 31;
      ECC_FA2R    at 16#160# range 0 .. 31;
   end record;

   --  Flash
   Flash_Periph : aliased Flash_Peripheral
     with Import, Address => Flash_Base;

end STM32H753.Flash;
