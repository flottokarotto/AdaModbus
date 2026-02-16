pragma Style_Checks (Off);

--  This spec has been automatically generated from STM32H753.svd


with System;

package STM32H753.JPEG is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   --  JPEG codec control register
   type CONFR0_Register is record
      --  Write-only. Start This bit start or stop the encoding or decoding
      --  process. Read this register always return 0.
      START         : Boolean := False;
      --  unspecified
      Reserved_1_31 : STM32H753.UInt31 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CONFR0_Register use record
      START         at 0 range 0 .. 0;
      Reserved_1_31 at 0 range 1 .. 31;
   end record;

   subtype CONFR1_NF_Field is STM32H753.UInt2;
   subtype CONFR1_COLORSPACE_Field is STM32H753.UInt2;
   subtype CONFR1_NS_Field is STM32H753.UInt2;
   subtype CONFR1_YSIZE_Field is STM32H753.UInt16;

   --  JPEG codec configuration register 1
   type CONFR1_Register is record
      --  Number of color components This field defines the number of color
      --  components minus 1.
      NF            : CONFR1_NF_Field := 16#0#;
      --  unspecified
      Reserved_2_2  : STM32H753.Bit := 16#0#;
      --  Decoding Enable This bit selects the coding or decoding process
      DE            : Boolean := False;
      --  Color Space This filed defines the number of quantization tables
      --  minus 1 to insert in the output stream.
      COLORSPACE    : CONFR1_COLORSPACE_Field := 16#0#;
      --  Number of components for Scan This field defines the number of
      --  components minus 1 for scan header marker segment.
      NS            : CONFR1_NS_Field := 16#0#;
      --  Header Processing This bit enable the header processing
      --  (generation/parsing).
      HDR           : Boolean := False;
      --  unspecified
      Reserved_9_15 : STM32H753.UInt7 := 16#0#;
      --  Y Size This field defines the number of lines in source image.
      YSIZE         : CONFR1_YSIZE_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CONFR1_Register use record
      NF            at 0 range 0 .. 1;
      Reserved_2_2  at 0 range 2 .. 2;
      DE            at 0 range 3 .. 3;
      COLORSPACE    at 0 range 4 .. 5;
      NS            at 0 range 6 .. 7;
      HDR           at 0 range 8 .. 8;
      Reserved_9_15 at 0 range 9 .. 15;
      YSIZE         at 0 range 16 .. 31;
   end record;

   subtype CONFR2_NMCU_Field is STM32H753.UInt26;

   --  JPEG codec configuration register 2
   type CONFR2_Register is record
      --  Number of MCU For encoding: this field defines the number of MCU
      --  units minus 1 to encode. For decoding: this field indicates the
      --  number of complete MCU units minus 1 to be decoded (this field is
      --  updated after the JPEG header parsing). If the decoded image size has
      --  not a X or Y size multiple of 8 or 16 (depending on the sub-sampling
      --  process), the resulting incomplete or empty MCU must be added to this
      --  value to get the total number of MCU generated.
      NMCU           : CONFR2_NMCU_Field := 16#0#;
      --  unspecified
      Reserved_26_31 : STM32H753.UInt6 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CONFR2_Register use record
      NMCU           at 0 range 0 .. 25;
      Reserved_26_31 at 0 range 26 .. 31;
   end record;

   subtype CONFR3_XSIZE_Field is STM32H753.UInt16;

   --  JPEG codec configuration register 3
   type CONFR3_Register is record
      --  unspecified
      Reserved_0_15 : STM32H753.UInt16 := 16#0#;
      --  X size This field defines the number of pixels per line.
      XSIZE         : CONFR3_XSIZE_Field := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CONFR3_Register use record
      Reserved_0_15 at 0 range 0 .. 15;
      XSIZE         at 0 range 16 .. 31;
   end record;

   subtype CONFRN_QT_Field is STM32H753.UInt2;
   subtype CONFRN_NB_Field is STM32H753.UInt4;
   subtype CONFRN_VSF_Field is STM32H753.UInt4;
   subtype CONFRN_HSF_Field is STM32H753.UInt4;

   --  JPEG codec configuration register 4-7
   type CONFRN_Register is record
      --  Huffman DC Selects the Huffman table for encoding the DC
      --  coefficients.
      HD             : Boolean := False;
      --  Huffman AC Selects the Huffman table for encoding the AC
      --  coefficients.
      HA             : Boolean := False;
      --  Quantization Table Selects quantization table associated with a color
      --  component.
      QT             : CONFRN_QT_Field := 16#0#;
      --  Number of Block Number of data units minus 1 that belong to a
      --  particular color in the MCU.
      NB             : CONFRN_NB_Field := 16#0#;
      --  Vertical Sampling Factor Vertical sampling factor for component i.
      VSF            : CONFRN_VSF_Field := 16#0#;
      --  Horizontal Sampling Factor Horizontal sampling factor for component
      --  i.
      HSF            : CONFRN_HSF_Field := 16#0#;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CONFRN_Register use record
      HD             at 0 range 0 .. 0;
      HA             at 0 range 1 .. 1;
      QT             at 0 range 2 .. 3;
      NB             at 0 range 4 .. 7;
      VSF            at 0 range 8 .. 11;
      HSF            at 0 range 12 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  JPEG control register
   type CR_Register is record
      --  JPEG Core Enable Enable the JPEG codec Core.
      JCEN           : Boolean := False;
      --  Input FIFO Threshold Interrupt Enable This bit enables the interrupt
      --  generation when input FIFO reach the threshold.
      IFTIE          : Boolean := False;
      --  Input FIFO Not Full Interrupt Enable This bit enables the interrupt
      --  generation when input FIFO is not empty.
      IFNFIE         : Boolean := False;
      --  Output FIFO Threshold Interrupt Enable This bit enables the interrupt
      --  generation when output FIFO reach the threshold.
      OFTIE          : Boolean := False;
      --  Output FIFO Not Empty Interrupt Enable This bit enables the interrupt
      --  generation when output FIFO is not empty.
      OFNEIE         : Boolean := False;
      --  End of Conversion Interrupt Enable This bit enables the interrupt
      --  generation on the end of conversion.
      EOCIE          : Boolean := False;
      --  Header Parsing Done Interrupt Enable This bit enables the interrupt
      --  generation on the Header Parsing Operation.
      HPDIE          : Boolean := False;
      --  unspecified
      Reserved_7_10  : STM32H753.UInt4 := 16#0#;
      --  Input DMA Enable Enable the DMA request generation for the input
      --  FIFO.
      IDMAEN         : Boolean := False;
      --  Output DMA Enable Enable the DMA request generation for the output
      --  FIFO.
      ODMAEN         : Boolean := False;
      --  Input FIFO Flush This bit flush the input FIFO. This bit is always
      --  read as 0.
      IFF            : Boolean := False;
      --  Output FIFO Flush This bit flush the output FIFO. This bit is always
      --  read as 0.
      OFF            : Boolean := False;
      --  unspecified
      Reserved_15_31 : STM32H753.UInt17 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CR_Register use record
      JCEN           at 0 range 0 .. 0;
      IFTIE          at 0 range 1 .. 1;
      IFNFIE         at 0 range 2 .. 2;
      OFTIE          at 0 range 3 .. 3;
      OFNEIE         at 0 range 4 .. 4;
      EOCIE          at 0 range 5 .. 5;
      HPDIE          at 0 range 6 .. 6;
      Reserved_7_10  at 0 range 7 .. 10;
      IDMAEN         at 0 range 11 .. 11;
      ODMAEN         at 0 range 12 .. 12;
      IFF            at 0 range 13 .. 13;
      OFF            at 0 range 14 .. 14;
      Reserved_15_31 at 0 range 15 .. 31;
   end record;

   --  JPEG status register
   type SR_Register is record
      --  unspecified
      Reserved_0_0  : STM32H753.Bit;
      --  Read-only. Input FIFO Threshold Flag This bit is set when the input
      --  FIFO is not full and is bellow its threshold.
      IFTF          : Boolean;
      --  Read-only. Input FIFO Not Full Flag This bit is set when the input
      --  FIFO is not full (a data can be written).
      IFNFF         : Boolean;
      --  Read-only. Output FIFO Threshold Flag This bit is set when the output
      --  FIFO is not empty and has reach its threshold.
      OFTF          : Boolean;
      --  Read-only. Output FIFO Not Empty Flag This bit is set when the output
      --  FIFO is not empty (a data is available).
      OFNEF         : Boolean;
      --  Read-only. End of Conversion Flag This bit is set when the JPEG codec
      --  core has finished the encoding or the decoding process and than last
      --  data has been sent to the output FIFO.
      EOCF          : Boolean;
      --  Read-only. Header Parsing Done Flag This bit is set in decode mode
      --  when the JPEG codec has finished the parsing of the headers and the
      --  internal registers have been updated.
      HPDF          : Boolean;
      --  Read-only. Codec Operation Flag This bit is set when when a JPEG
      --  codec operation is on going (encoding or decoding).
      COF           : Boolean;
      --  unspecified
      Reserved_8_31 : STM32H753.UInt24;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for SR_Register use record
      Reserved_0_0  at 0 range 0 .. 0;
      IFTF          at 0 range 1 .. 1;
      IFNFF         at 0 range 2 .. 2;
      OFTF          at 0 range 3 .. 3;
      OFNEF         at 0 range 4 .. 4;
      EOCF          at 0 range 5 .. 5;
      HPDF          at 0 range 6 .. 6;
      COF           at 0 range 7 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  JPEG clear flag register
   type CFR_Register is record
      --  unspecified
      Reserved_0_4  : STM32H753.UInt5 := 16#0#;
      --  Clear End of Conversion Flag Writing 1 clears the End of Conversion
      --  Flag of the JPEG Status Register.
      CEOCF         : Boolean := False;
      --  Clear Header Parsing Done Flag Writing 1 clears the Header Parsing
      --  Done Flag of the JPEG Status Register.
      CHPDF         : Boolean := False;
      --  unspecified
      Reserved_7_31 : STM32H753.UInt25 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CFR_Register use record
      Reserved_0_4  at 0 range 0 .. 4;
      CEOCF         at 0 range 5 .. 5;
      CHPDF         at 0 range 6 .. 6;
      Reserved_7_31 at 0 range 7 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  JPEG
   type JPEG_Peripheral is record
      --  JPEG codec control register
      CONFR0  : aliased CONFR0_Register;
      --  JPEG codec configuration register 1
      CONFR1  : aliased CONFR1_Register;
      --  JPEG codec configuration register 2
      CONFR2  : aliased CONFR2_Register;
      --  JPEG codec configuration register 3
      CONFR3  : aliased CONFR3_Register;
      --  JPEG codec configuration register 4-7
      CONFRN1 : aliased CONFRN_Register;
      --  JPEG codec configuration register 4-7
      CONFRN2 : aliased CONFRN_Register;
      --  JPEG codec configuration register 4-7
      CONFRN3 : aliased CONFRN_Register;
      --  JPEG codec configuration register 4-7
      CONFRN4 : aliased CONFRN_Register;
      --  JPEG control register
      CR      : aliased CR_Register;
      --  JPEG status register
      SR      : aliased SR_Register;
      --  JPEG clear flag register
      CFR     : aliased CFR_Register;
      --  JPEG data input register
      DIR     : aliased STM32H753.UInt32;
      --  JPEG data output register
      DOR     : aliased STM32H753.UInt32;
   end record
     with Volatile;

   for JPEG_Peripheral use record
      CONFR0  at 16#0# range 0 .. 31;
      CONFR1  at 16#4# range 0 .. 31;
      CONFR2  at 16#8# range 0 .. 31;
      CONFR3  at 16#C# range 0 .. 31;
      CONFRN1 at 16#10# range 0 .. 31;
      CONFRN2 at 16#14# range 0 .. 31;
      CONFRN3 at 16#18# range 0 .. 31;
      CONFRN4 at 16#1C# range 0 .. 31;
      CR      at 16#30# range 0 .. 31;
      SR      at 16#34# range 0 .. 31;
      CFR     at 16#38# range 0 .. 31;
      DIR     at 16#40# range 0 .. 31;
      DOR     at 16#44# range 0 .. 31;
   end record;

   --  JPEG
   JPEG_Periph : aliased JPEG_Peripheral
     with Import, Address => JPEG_Base;

end STM32H753.JPEG;
