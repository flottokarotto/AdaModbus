pragma Style_Checks (Off);

--  This spec has been automatically generated from STM32H753.svd


with Interfaces;
with System;

--  STM32H753
package STM32H753 is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Base type --
   ---------------

   type UInt32 is new Interfaces.Unsigned_32;
   type UInt16 is new Interfaces.Unsigned_16;
   type Byte is new Interfaces.Unsigned_8;
   type Bit is mod 2**1
     with Size => 1;
   type UInt2 is mod 2**2
     with Size => 2;
   type UInt3 is mod 2**3
     with Size => 3;
   type UInt4 is mod 2**4
     with Size => 4;
   type UInt5 is mod 2**5
     with Size => 5;
   type UInt6 is mod 2**6
     with Size => 6;
   type UInt7 is mod 2**7
     with Size => 7;
   type UInt9 is mod 2**9
     with Size => 9;
   type UInt10 is mod 2**10
     with Size => 10;
   type UInt11 is mod 2**11
     with Size => 11;
   type UInt12 is mod 2**12
     with Size => 12;
   type UInt13 is mod 2**13
     with Size => 13;
   type UInt14 is mod 2**14
     with Size => 14;
   type UInt15 is mod 2**15
     with Size => 15;
   type UInt17 is mod 2**17
     with Size => 17;
   type UInt18 is mod 2**18
     with Size => 18;
   type UInt19 is mod 2**19
     with Size => 19;
   type UInt20 is mod 2**20
     with Size => 20;
   type UInt21 is mod 2**21
     with Size => 21;
   type UInt22 is mod 2**22
     with Size => 22;
   type UInt23 is mod 2**23
     with Size => 23;
   type UInt24 is mod 2**24
     with Size => 24;
   type UInt25 is mod 2**25
     with Size => 25;
   type UInt26 is mod 2**26
     with Size => 26;
   type UInt27 is mod 2**27
     with Size => 27;
   type UInt28 is mod 2**28
     with Size => 28;
   type UInt29 is mod 2**29
     with Size => 29;
   type UInt30 is mod 2**30
     with Size => 30;
   type UInt31 is mod 2**31
     with Size => 31;

   --------------------
   -- Base addresses --
   --------------------

   COMP1_Base : constant System.Address := System'To_Address (16#58003800#);
   CRS_Base : constant System.Address := System'To_Address (16#40008400#);
   DAC_Base : constant System.Address := System'To_Address (16#40007400#);
   BDMA_Base : constant System.Address := System'To_Address (16#58025400#);
   DMA2D_Base : constant System.Address := System'To_Address (16#52001000#);
   DMAMUX1_Base : constant System.Address := System'To_Address (16#40020800#);
   DMAMUX2_Base : constant System.Address := System'To_Address (16#58025800#);
   FMC_Base : constant System.Address := System'To_Address (16#52004000#);
   CEC_Base : constant System.Address := System'To_Address (16#40006C00#);
   HSEM_Base : constant System.Address := System'To_Address (16#58026400#);
   I2C1_Base : constant System.Address := System'To_Address (16#40005400#);
   I2C2_Base : constant System.Address := System'To_Address (16#40005800#);
   I2C3_Base : constant System.Address := System'To_Address (16#40005C00#);
   I2C4_Base : constant System.Address := System'To_Address (16#58001C00#);
   GPIOA_Base : constant System.Address := System'To_Address (16#58020000#);
   GPIOB_Base : constant System.Address := System'To_Address (16#58020400#);
   GPIOC_Base : constant System.Address := System'To_Address (16#58020800#);
   GPIOD_Base : constant System.Address := System'To_Address (16#58020C00#);
   GPIOE_Base : constant System.Address := System'To_Address (16#58021000#);
   GPIOF_Base : constant System.Address := System'To_Address (16#58021400#);
   GPIOG_Base : constant System.Address := System'To_Address (16#58021800#);
   GPIOH_Base : constant System.Address := System'To_Address (16#58021C00#);
   GPIOI_Base : constant System.Address := System'To_Address (16#58022000#);
   GPIOJ_Base : constant System.Address := System'To_Address (16#58022400#);
   GPIOK_Base : constant System.Address := System'To_Address (16#58022800#);
   JPEG_Base : constant System.Address := System'To_Address (16#52003000#);
   MDMA_Base : constant System.Address := System'To_Address (16#52000000#);
   QUADSPI_Base : constant System.Address := System'To_Address (16#52005000#);
   RNG_Base : constant System.Address := System'To_Address (16#48021800#);
   RTC_Base : constant System.Address := System'To_Address (16#58004000#);
   SAI4_Base : constant System.Address := System'To_Address (16#58005400#);
   SAI1_Base : constant System.Address := System'To_Address (16#40015800#);
   SAI2_Base : constant System.Address := System'To_Address (16#40015C00#);
   SAI3_Base : constant System.Address := System'To_Address (16#40016000#);
   SDMMC1_Base : constant System.Address := System'To_Address (16#52007000#);
   SDMMC2_Base : constant System.Address := System'To_Address (16#48022400#);
   VREFBUF_Base : constant System.Address := System'To_Address (16#58003C00#);
   IWDG_Base : constant System.Address := System'To_Address (16#58004800#);
   WWDG_Base : constant System.Address := System'To_Address (16#50003000#);
   PWR_Base : constant System.Address := System'To_Address (16#58024800#);
   SPI1_Base : constant System.Address := System'To_Address (16#40013000#);
   SPI2_Base : constant System.Address := System'To_Address (16#40003800#);
   SPI3_Base : constant System.Address := System'To_Address (16#40003C00#);
   SPI4_Base : constant System.Address := System'To_Address (16#40013400#);
   SPI5_Base : constant System.Address := System'To_Address (16#40015000#);
   SPI6_Base : constant System.Address := System'To_Address (16#58001400#);
   LTDC_Base : constant System.Address := System'To_Address (16#50001000#);
   SPDIFRX_Base : constant System.Address := System'To_Address (16#40004000#);
   ADC3_Base : constant System.Address := System'To_Address (16#58026000#);
   ADC1_Base : constant System.Address := System'To_Address (16#40022000#);
   ADC2_Base : constant System.Address := System'To_Address (16#40022100#);
   ADC3_Common_Base : constant System.Address := System'To_Address (16#58026300#);
   ADC12_Common_Base : constant System.Address := System'To_Address (16#40022300#);
   CRC_Base : constant System.Address := System'To_Address (16#58024C00#);
   RCC_Base : constant System.Address := System'To_Address (16#58024400#);
   LPTIM1_Base : constant System.Address := System'To_Address (16#40002400#);
   LPTIM2_Base : constant System.Address := System'To_Address (16#58002400#);
   LPTIM3_Base : constant System.Address := System'To_Address (16#58002800#);
   LPTIM4_Base : constant System.Address := System'To_Address (16#58002C00#);
   LPTIM5_Base : constant System.Address := System'To_Address (16#58003000#);
   LPUART1_Base : constant System.Address := System'To_Address (16#58000C00#);
   SYSCFG_Base : constant System.Address := System'To_Address (16#58000400#);
   EXTI_Base : constant System.Address := System'To_Address (16#58000000#);
   RAMECC1_Base : constant System.Address := System'To_Address (16#52009000#);
   RAMECC2_Base : constant System.Address := System'To_Address (16#48023000#);
   RAMECC3_Base : constant System.Address := System'To_Address (16#58027000#);
   DELAY_Block_SDMMC1_Base : constant System.Address := System'To_Address (16#52008000#);
   DELAY_Block_QUADSPI_Base : constant System.Address := System'To_Address (16#52006000#);
   DELAY_Block_SDMMC2_Base : constant System.Address := System'To_Address (16#48022800#);
   Flash_Base : constant System.Address := System'To_Address (16#52002000#);
   AXI_Base : constant System.Address := System'To_Address (16#51000000#);
   HASH_Base : constant System.Address := System'To_Address (16#48021400#);
   CRYP_Base : constant System.Address := System'To_Address (16#48021000#);
   DCMI_Base : constant System.Address := System'To_Address (16#48020000#);
   OTG1_HS_GLOBAL_Base : constant System.Address := System'To_Address (16#40040000#);
   OTG2_HS_GLOBAL_Base : constant System.Address := System'To_Address (16#40080000#);
   OTG1_HS_HOST_Base : constant System.Address := System'To_Address (16#40040400#);
   OTG2_HS_HOST_Base : constant System.Address := System'To_Address (16#40080400#);
   OTG1_HS_DEVICE_Base : constant System.Address := System'To_Address (16#40040800#);
   OTG2_HS_DEVICE_Base : constant System.Address := System'To_Address (16#40080800#);
   OTG1_HS_PWRCLK_Base : constant System.Address := System'To_Address (16#40040E00#);
   OTG2_HS_PWRCLK_Base : constant System.Address := System'To_Address (16#40080E00#);
   Ethernet_DMA_Base : constant System.Address := System'To_Address (16#40029000#);
   Ethernet_MTL_Base : constant System.Address := System'To_Address (16#40028C00#);
   Ethernet_MAC_Base : constant System.Address := System'To_Address (16#40028000#);
   DMA1_Base : constant System.Address := System'To_Address (16#40020000#);
   DMA2_Base : constant System.Address := System'To_Address (16#40020400#);
   HRTIM_Master_Base : constant System.Address := System'To_Address (16#40017400#);
   HRTIM_TIMA_Base : constant System.Address := System'To_Address (16#40017480#);
   HRTIM_TIMB_Base : constant System.Address := System'To_Address (16#40017500#);
   HRTIM_TIMC_Base : constant System.Address := System'To_Address (16#40017580#);
   HRTIM_TIMD_Base : constant System.Address := System'To_Address (16#40017600#);
   HRTIM_TIME_Base : constant System.Address := System'To_Address (16#40017680#);
   HRTIM_Common_Base : constant System.Address := System'To_Address (16#40017780#);
   DFSDM_Base : constant System.Address := System'To_Address (16#40017000#);
   TIM16_Base : constant System.Address := System'To_Address (16#40014400#);
   TIM17_Base : constant System.Address := System'To_Address (16#40014800#);
   TIM15_Base : constant System.Address := System'To_Address (16#40014000#);
   USART1_Base : constant System.Address := System'To_Address (16#40011000#);
   USART2_Base : constant System.Address := System'To_Address (16#40004400#);
   USART3_Base : constant System.Address := System'To_Address (16#40004800#);
   UART4_Base : constant System.Address := System'To_Address (16#40004C00#);
   UART5_Base : constant System.Address := System'To_Address (16#40005000#);
   USART6_Base : constant System.Address := System'To_Address (16#40011400#);
   UART7_Base : constant System.Address := System'To_Address (16#40007800#);
   UART8_Base : constant System.Address := System'To_Address (16#40007C00#);
   TIM1_Base : constant System.Address := System'To_Address (16#40010000#);
   TIM8_Base : constant System.Address := System'To_Address (16#40010400#);
   FDCAN1_Base : constant System.Address := System'To_Address (16#4000A000#);
   FDCAN2_Base : constant System.Address := System'To_Address (16#4000A400#);
   CAN_CCU_Base : constant System.Address := System'To_Address (16#4000A800#);
   MDIOS_Base : constant System.Address := System'To_Address (16#40009400#);
   OPAMP_Base : constant System.Address := System'To_Address (16#40009000#);
   SWPMI_Base : constant System.Address := System'To_Address (16#40008800#);
   TIM2_Base : constant System.Address := System'To_Address (16#40000000#);
   TIM3_Base : constant System.Address := System'To_Address (16#40000400#);
   TIM4_Base : constant System.Address := System'To_Address (16#40000800#);
   TIM5_Base : constant System.Address := System'To_Address (16#40000C00#);
   TIM12_Base : constant System.Address := System'To_Address (16#40001800#);
   TIM13_Base : constant System.Address := System'To_Address (16#40001C00#);
   TIM14_Base : constant System.Address := System'To_Address (16#40002000#);
   TIM6_Base : constant System.Address := System'To_Address (16#40001000#);
   TIM7_Base : constant System.Address := System'To_Address (16#40001400#);
   DBGMCU_Base : constant System.Address := System'To_Address (16#5C001000#);

end STM32H753;
