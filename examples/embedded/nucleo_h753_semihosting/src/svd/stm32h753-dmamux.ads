pragma Style_Checks (Off);

--  This spec has been automatically generated from STM32H753.svd


with System;

package STM32H753.DMAMUX is
   pragma Preelaborate;
   pragma No_Elaboration_Code_All;

   ---------------
   -- Registers --
   ---------------

   subtype DMAMUX_C0CR_DMAREQ_ID_Field is STM32H753.UInt7;

   --  Synchronization overrun interrupt enable
   type DMAMUX_C0CR_SOIE_Field is
     (--  Interrupt disabled
      B_0x0,
      --  Interrupt enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C0CR_SOIE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Event generation enable
   type DMAMUX_C0CR_EGE_Field is
     (--  Event generation disabled
      B_0x0,
      --  Event generation enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C0CR_EGE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Synchronization enable
   type DMAMUX_C0CR_SE_Field is
     (--  Synchronization disabled
      B_0x0,
      --  Synchronization enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C0CR_SE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Synchronization polarity Defines the edge polarity of the selected
   --  synchronization input:
   type DMAMUX_C0CR_SPOL_Field is
     (--  No event, i.e. no synchronization nor detection.
      B_0x0,
      --  Rising edge
      B_0x1,
      --  Falling edge
      B_0x2,
      --  Rising and falling edges
      B_0x3)
     with Size => 2;
   for DMAMUX_C0CR_SPOL_Field use
     (B_0x0 => 0,
      B_0x1 => 1,
      B_0x2 => 2,
      B_0x3 => 3);

   subtype DMAMUX_C0CR_NBREQ_Field is STM32H753.UInt5;
   subtype DMAMUX_C0CR_SYNC_ID_Field is STM32H753.UInt3;

   type DMAMUX_C0CR_Register is record
      --  DMA request identification Selects the input DMA request. See the
      --  DMAMUX table about assignments of multiplexer inputs to resources.
      DMAREQ_ID      : DMAMUX_C0CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : STM32H753.Bit := 16#0#;
      --  Synchronization overrun interrupt enable
      SOIE           : DMAMUX_C0CR_SOIE_Field := STM32H753.DMAMUX.B_0x0;
      --  Event generation enable
      EGE            : DMAMUX_C0CR_EGE_Field := STM32H753.DMAMUX.B_0x0;
      --  unspecified
      Reserved_10_15 : STM32H753.UInt6 := 16#0#;
      --  Synchronization enable
      SE             : DMAMUX_C0CR_SE_Field := STM32H753.DMAMUX.B_0x0;
      --  Synchronization polarity Defines the edge polarity of the selected
      --  synchronization input:
      SPOL           : DMAMUX_C0CR_SPOL_Field := STM32H753.DMAMUX.B_0x0;
      --  Number of DMA requests minus 1 to forward Defines the number of DMA
      --  requests to forward to the DMA controller after a synchronization
      --  event, and/or the number of DMA requests before an output event is
      --  generated. This field shall only be written when both SE and EGE bits
      --  are low.
      NBREQ          : DMAMUX_C0CR_NBREQ_Field := 16#0#;
      --  Synchronization identification Selects the synchronization input (see
      --  inputs to resources).
      SYNC_ID        : DMAMUX_C0CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_27_31 : STM32H753.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_C0CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 26;
      Reserved_27_31 at 0 range 27 .. 31;
   end record;

   subtype DMAMUX_C1CR_DMAREQ_ID_Field is STM32H753.UInt7;

   --  Synchronization overrun interrupt enable
   type DMAMUX_C1CR_SOIE_Field is
     (--  Interrupt disabled
      B_0x0,
      --  Interrupt enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C1CR_SOIE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Event generation enable
   type DMAMUX_C1CR_EGE_Field is
     (--  Event generation disabled
      B_0x0,
      --  Event generation enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C1CR_EGE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Synchronization enable
   type DMAMUX_C1CR_SE_Field is
     (--  Synchronization disabled
      B_0x0,
      --  Synchronization enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C1CR_SE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Synchronization polarity Defines the edge polarity of the selected
   --  synchronization input:
   type DMAMUX_C1CR_SPOL_Field is
     (--  No event, i.e. no synchronization nor detection.
      B_0x0,
      --  Rising edge
      B_0x1,
      --  Falling edge
      B_0x2,
      --  Rising and falling edges
      B_0x3)
     with Size => 2;
   for DMAMUX_C1CR_SPOL_Field use
     (B_0x0 => 0,
      B_0x1 => 1,
      B_0x2 => 2,
      B_0x3 => 3);

   subtype DMAMUX_C1CR_NBREQ_Field is STM32H753.UInt5;
   subtype DMAMUX_C1CR_SYNC_ID_Field is STM32H753.UInt3;

   type DMAMUX_C1CR_Register is record
      --  DMA request identification Selects the input DMA request. See the
      --  DMAMUX table about assignments of multiplexer inputs to resources.
      DMAREQ_ID      : DMAMUX_C1CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : STM32H753.Bit := 16#0#;
      --  Synchronization overrun interrupt enable
      SOIE           : DMAMUX_C1CR_SOIE_Field := STM32H753.DMAMUX.B_0x0;
      --  Event generation enable
      EGE            : DMAMUX_C1CR_EGE_Field := STM32H753.DMAMUX.B_0x0;
      --  unspecified
      Reserved_10_15 : STM32H753.UInt6 := 16#0#;
      --  Synchronization enable
      SE             : DMAMUX_C1CR_SE_Field := STM32H753.DMAMUX.B_0x0;
      --  Synchronization polarity Defines the edge polarity of the selected
      --  synchronization input:
      SPOL           : DMAMUX_C1CR_SPOL_Field := STM32H753.DMAMUX.B_0x0;
      --  Number of DMA requests minus 1 to forward Defines the number of DMA
      --  requests to forward to the DMA controller after a synchronization
      --  event, and/or the number of DMA requests before an output event is
      --  generated. This field shall only be written when both SE and EGE bits
      --  are low.
      NBREQ          : DMAMUX_C1CR_NBREQ_Field := 16#0#;
      --  Synchronization identification Selects the synchronization input (see
      --  inputs to resources).
      SYNC_ID        : DMAMUX_C1CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_27_31 : STM32H753.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_C1CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 26;
      Reserved_27_31 at 0 range 27 .. 31;
   end record;

   subtype DMAMUX_C2CR_DMAREQ_ID_Field is STM32H753.UInt7;

   --  Synchronization overrun interrupt enable
   type DMAMUX_C2CR_SOIE_Field is
     (--  Interrupt disabled
      B_0x0,
      --  Interrupt enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C2CR_SOIE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Event generation enable
   type DMAMUX_C2CR_EGE_Field is
     (--  Event generation disabled
      B_0x0,
      --  Event generation enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C2CR_EGE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Synchronization enable
   type DMAMUX_C2CR_SE_Field is
     (--  Synchronization disabled
      B_0x0,
      --  Synchronization enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C2CR_SE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Synchronization polarity Defines the edge polarity of the selected
   --  synchronization input:
   type DMAMUX_C2CR_SPOL_Field is
     (--  No event, i.e. no synchronization nor detection.
      B_0x0,
      --  Rising edge
      B_0x1,
      --  Falling edge
      B_0x2,
      --  Rising and falling edges
      B_0x3)
     with Size => 2;
   for DMAMUX_C2CR_SPOL_Field use
     (B_0x0 => 0,
      B_0x1 => 1,
      B_0x2 => 2,
      B_0x3 => 3);

   subtype DMAMUX_C2CR_NBREQ_Field is STM32H753.UInt5;
   subtype DMAMUX_C2CR_SYNC_ID_Field is STM32H753.UInt3;

   type DMAMUX_C2CR_Register is record
      --  DMA request identification Selects the input DMA request. See the
      --  DMAMUX table about assignments of multiplexer inputs to resources.
      DMAREQ_ID      : DMAMUX_C2CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : STM32H753.Bit := 16#0#;
      --  Synchronization overrun interrupt enable
      SOIE           : DMAMUX_C2CR_SOIE_Field := STM32H753.DMAMUX.B_0x0;
      --  Event generation enable
      EGE            : DMAMUX_C2CR_EGE_Field := STM32H753.DMAMUX.B_0x0;
      --  unspecified
      Reserved_10_15 : STM32H753.UInt6 := 16#0#;
      --  Synchronization enable
      SE             : DMAMUX_C2CR_SE_Field := STM32H753.DMAMUX.B_0x0;
      --  Synchronization polarity Defines the edge polarity of the selected
      --  synchronization input:
      SPOL           : DMAMUX_C2CR_SPOL_Field := STM32H753.DMAMUX.B_0x0;
      --  Number of DMA requests minus 1 to forward Defines the number of DMA
      --  requests to forward to the DMA controller after a synchronization
      --  event, and/or the number of DMA requests before an output event is
      --  generated. This field shall only be written when both SE and EGE bits
      --  are low.
      NBREQ          : DMAMUX_C2CR_NBREQ_Field := 16#0#;
      --  Synchronization identification Selects the synchronization input (see
      --  inputs to resources).
      SYNC_ID        : DMAMUX_C2CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_27_31 : STM32H753.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_C2CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 26;
      Reserved_27_31 at 0 range 27 .. 31;
   end record;

   subtype DMAMUX_C3CR_DMAREQ_ID_Field is STM32H753.UInt7;

   --  Synchronization overrun interrupt enable
   type DMAMUX_C3CR_SOIE_Field is
     (--  Interrupt disabled
      B_0x0,
      --  Interrupt enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C3CR_SOIE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Event generation enable
   type DMAMUX_C3CR_EGE_Field is
     (--  Event generation disabled
      B_0x0,
      --  Event generation enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C3CR_EGE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Synchronization enable
   type DMAMUX_C3CR_SE_Field is
     (--  Synchronization disabled
      B_0x0,
      --  Synchronization enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C3CR_SE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Synchronization polarity Defines the edge polarity of the selected
   --  synchronization input:
   type DMAMUX_C3CR_SPOL_Field is
     (--  No event, i.e. no synchronization nor detection.
      B_0x0,
      --  Rising edge
      B_0x1,
      --  Falling edge
      B_0x2,
      --  Rising and falling edges
      B_0x3)
     with Size => 2;
   for DMAMUX_C3CR_SPOL_Field use
     (B_0x0 => 0,
      B_0x1 => 1,
      B_0x2 => 2,
      B_0x3 => 3);

   subtype DMAMUX_C3CR_NBREQ_Field is STM32H753.UInt5;
   subtype DMAMUX_C3CR_SYNC_ID_Field is STM32H753.UInt3;

   type DMAMUX_C3CR_Register is record
      --  DMA request identification Selects the input DMA request. See the
      --  DMAMUX table about assignments of multiplexer inputs to resources.
      DMAREQ_ID      : DMAMUX_C3CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : STM32H753.Bit := 16#0#;
      --  Synchronization overrun interrupt enable
      SOIE           : DMAMUX_C3CR_SOIE_Field := STM32H753.DMAMUX.B_0x0;
      --  Event generation enable
      EGE            : DMAMUX_C3CR_EGE_Field := STM32H753.DMAMUX.B_0x0;
      --  unspecified
      Reserved_10_15 : STM32H753.UInt6 := 16#0#;
      --  Synchronization enable
      SE             : DMAMUX_C3CR_SE_Field := STM32H753.DMAMUX.B_0x0;
      --  Synchronization polarity Defines the edge polarity of the selected
      --  synchronization input:
      SPOL           : DMAMUX_C3CR_SPOL_Field := STM32H753.DMAMUX.B_0x0;
      --  Number of DMA requests minus 1 to forward Defines the number of DMA
      --  requests to forward to the DMA controller after a synchronization
      --  event, and/or the number of DMA requests before an output event is
      --  generated. This field shall only be written when both SE and EGE bits
      --  are low.
      NBREQ          : DMAMUX_C3CR_NBREQ_Field := 16#0#;
      --  Synchronization identification Selects the synchronization input (see
      --  inputs to resources).
      SYNC_ID        : DMAMUX_C3CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_27_31 : STM32H753.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_C3CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 26;
      Reserved_27_31 at 0 range 27 .. 31;
   end record;

   subtype DMAMUX_C4CR_DMAREQ_ID_Field is STM32H753.UInt7;

   --  Synchronization overrun interrupt enable
   type DMAMUX_C4CR_SOIE_Field is
     (--  Interrupt disabled
      B_0x0,
      --  Interrupt enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C4CR_SOIE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Event generation enable
   type DMAMUX_C4CR_EGE_Field is
     (--  Event generation disabled
      B_0x0,
      --  Event generation enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C4CR_EGE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Synchronization enable
   type DMAMUX_C4CR_SE_Field is
     (--  Synchronization disabled
      B_0x0,
      --  Synchronization enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C4CR_SE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Synchronization polarity Defines the edge polarity of the selected
   --  synchronization input:
   type DMAMUX_C4CR_SPOL_Field is
     (--  No event, i.e. no synchronization nor detection.
      B_0x0,
      --  Rising edge
      B_0x1,
      --  Falling edge
      B_0x2,
      --  Rising and falling edges
      B_0x3)
     with Size => 2;
   for DMAMUX_C4CR_SPOL_Field use
     (B_0x0 => 0,
      B_0x1 => 1,
      B_0x2 => 2,
      B_0x3 => 3);

   subtype DMAMUX_C4CR_NBREQ_Field is STM32H753.UInt5;
   subtype DMAMUX_C4CR_SYNC_ID_Field is STM32H753.UInt3;

   type DMAMUX_C4CR_Register is record
      --  DMA request identification Selects the input DMA request. See the
      --  DMAMUX table about assignments of multiplexer inputs to resources.
      DMAREQ_ID      : DMAMUX_C4CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : STM32H753.Bit := 16#0#;
      --  Synchronization overrun interrupt enable
      SOIE           : DMAMUX_C4CR_SOIE_Field := STM32H753.DMAMUX.B_0x0;
      --  Event generation enable
      EGE            : DMAMUX_C4CR_EGE_Field := STM32H753.DMAMUX.B_0x0;
      --  unspecified
      Reserved_10_15 : STM32H753.UInt6 := 16#0#;
      --  Synchronization enable
      SE             : DMAMUX_C4CR_SE_Field := STM32H753.DMAMUX.B_0x0;
      --  Synchronization polarity Defines the edge polarity of the selected
      --  synchronization input:
      SPOL           : DMAMUX_C4CR_SPOL_Field := STM32H753.DMAMUX.B_0x0;
      --  Number of DMA requests minus 1 to forward Defines the number of DMA
      --  requests to forward to the DMA controller after a synchronization
      --  event, and/or the number of DMA requests before an output event is
      --  generated. This field shall only be written when both SE and EGE bits
      --  are low.
      NBREQ          : DMAMUX_C4CR_NBREQ_Field := 16#0#;
      --  Synchronization identification Selects the synchronization input (see
      --  inputs to resources).
      SYNC_ID        : DMAMUX_C4CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_27_31 : STM32H753.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_C4CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 26;
      Reserved_27_31 at 0 range 27 .. 31;
   end record;

   subtype DMAMUX_C5CR_DMAREQ_ID_Field is STM32H753.UInt7;

   --  Synchronization overrun interrupt enable
   type DMAMUX_C5CR_SOIE_Field is
     (--  Interrupt disabled
      B_0x0,
      --  Interrupt enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C5CR_SOIE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Event generation enable
   type DMAMUX_C5CR_EGE_Field is
     (--  Event generation disabled
      B_0x0,
      --  Event generation enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C5CR_EGE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Synchronization enable
   type DMAMUX_C5CR_SE_Field is
     (--  Synchronization disabled
      B_0x0,
      --  Synchronization enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C5CR_SE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Synchronization polarity Defines the edge polarity of the selected
   --  synchronization input:
   type DMAMUX_C5CR_SPOL_Field is
     (--  No event, i.e. no synchronization nor detection.
      B_0x0,
      --  Rising edge
      B_0x1,
      --  Falling edge
      B_0x2,
      --  Rising and falling edges
      B_0x3)
     with Size => 2;
   for DMAMUX_C5CR_SPOL_Field use
     (B_0x0 => 0,
      B_0x1 => 1,
      B_0x2 => 2,
      B_0x3 => 3);

   subtype DMAMUX_C5CR_NBREQ_Field is STM32H753.UInt5;
   subtype DMAMUX_C5CR_SYNC_ID_Field is STM32H753.UInt3;

   type DMAMUX_C5CR_Register is record
      --  DMA request identification Selects the input DMA request. See the
      --  DMAMUX table about assignments of multiplexer inputs to resources.
      DMAREQ_ID      : DMAMUX_C5CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : STM32H753.Bit := 16#0#;
      --  Synchronization overrun interrupt enable
      SOIE           : DMAMUX_C5CR_SOIE_Field := STM32H753.DMAMUX.B_0x0;
      --  Event generation enable
      EGE            : DMAMUX_C5CR_EGE_Field := STM32H753.DMAMUX.B_0x0;
      --  unspecified
      Reserved_10_15 : STM32H753.UInt6 := 16#0#;
      --  Synchronization enable
      SE             : DMAMUX_C5CR_SE_Field := STM32H753.DMAMUX.B_0x0;
      --  Synchronization polarity Defines the edge polarity of the selected
      --  synchronization input:
      SPOL           : DMAMUX_C5CR_SPOL_Field := STM32H753.DMAMUX.B_0x0;
      --  Number of DMA requests minus 1 to forward Defines the number of DMA
      --  requests to forward to the DMA controller after a synchronization
      --  event, and/or the number of DMA requests before an output event is
      --  generated. This field shall only be written when both SE and EGE bits
      --  are low.
      NBREQ          : DMAMUX_C5CR_NBREQ_Field := 16#0#;
      --  Synchronization identification Selects the synchronization input (see
      --  inputs to resources).
      SYNC_ID        : DMAMUX_C5CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_27_31 : STM32H753.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_C5CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 26;
      Reserved_27_31 at 0 range 27 .. 31;
   end record;

   subtype DMAMUX_C6CR_DMAREQ_ID_Field is STM32H753.UInt7;

   --  Synchronization overrun interrupt enable
   type DMAMUX_C6CR_SOIE_Field is
     (--  Interrupt disabled
      B_0x0,
      --  Interrupt enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C6CR_SOIE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Event generation enable
   type DMAMUX_C6CR_EGE_Field is
     (--  Event generation disabled
      B_0x0,
      --  Event generation enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C6CR_EGE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Synchronization enable
   type DMAMUX_C6CR_SE_Field is
     (--  Synchronization disabled
      B_0x0,
      --  Synchronization enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C6CR_SE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Synchronization polarity Defines the edge polarity of the selected
   --  synchronization input:
   type DMAMUX_C6CR_SPOL_Field is
     (--  No event, i.e. no synchronization nor detection.
      B_0x0,
      --  Rising edge
      B_0x1,
      --  Falling edge
      B_0x2,
      --  Rising and falling edges
      B_0x3)
     with Size => 2;
   for DMAMUX_C6CR_SPOL_Field use
     (B_0x0 => 0,
      B_0x1 => 1,
      B_0x2 => 2,
      B_0x3 => 3);

   subtype DMAMUX_C6CR_NBREQ_Field is STM32H753.UInt5;
   subtype DMAMUX_C6CR_SYNC_ID_Field is STM32H753.UInt3;

   type DMAMUX_C6CR_Register is record
      --  DMA request identification Selects the input DMA request. See the
      --  DMAMUX table about assignments of multiplexer inputs to resources.
      DMAREQ_ID      : DMAMUX_C6CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : STM32H753.Bit := 16#0#;
      --  Synchronization overrun interrupt enable
      SOIE           : DMAMUX_C6CR_SOIE_Field := STM32H753.DMAMUX.B_0x0;
      --  Event generation enable
      EGE            : DMAMUX_C6CR_EGE_Field := STM32H753.DMAMUX.B_0x0;
      --  unspecified
      Reserved_10_15 : STM32H753.UInt6 := 16#0#;
      --  Synchronization enable
      SE             : DMAMUX_C6CR_SE_Field := STM32H753.DMAMUX.B_0x0;
      --  Synchronization polarity Defines the edge polarity of the selected
      --  synchronization input:
      SPOL           : DMAMUX_C6CR_SPOL_Field := STM32H753.DMAMUX.B_0x0;
      --  Number of DMA requests minus 1 to forward Defines the number of DMA
      --  requests to forward to the DMA controller after a synchronization
      --  event, and/or the number of DMA requests before an output event is
      --  generated. This field shall only be written when both SE and EGE bits
      --  are low.
      NBREQ          : DMAMUX_C6CR_NBREQ_Field := 16#0#;
      --  Synchronization identification Selects the synchronization input (see
      --  inputs to resources).
      SYNC_ID        : DMAMUX_C6CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_27_31 : STM32H753.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_C6CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 26;
      Reserved_27_31 at 0 range 27 .. 31;
   end record;

   subtype DMAMUX_C7CR_DMAREQ_ID_Field is STM32H753.UInt7;

   --  Synchronization overrun interrupt enable
   type DMAMUX_C7CR_SOIE_Field is
     (--  Interrupt disabled
      B_0x0,
      --  Interrupt enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C7CR_SOIE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Event generation enable
   type DMAMUX_C7CR_EGE_Field is
     (--  Event generation disabled
      B_0x0,
      --  Event generation enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C7CR_EGE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Synchronization enable
   type DMAMUX_C7CR_SE_Field is
     (--  Synchronization disabled
      B_0x0,
      --  Synchronization enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C7CR_SE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Synchronization polarity Defines the edge polarity of the selected
   --  synchronization input:
   type DMAMUX_C7CR_SPOL_Field is
     (--  No event, i.e. no synchronization nor detection.
      B_0x0,
      --  Rising edge
      B_0x1,
      --  Falling edge
      B_0x2,
      --  Rising and falling edges
      B_0x3)
     with Size => 2;
   for DMAMUX_C7CR_SPOL_Field use
     (B_0x0 => 0,
      B_0x1 => 1,
      B_0x2 => 2,
      B_0x3 => 3);

   subtype DMAMUX_C7CR_NBREQ_Field is STM32H753.UInt5;
   subtype DMAMUX_C7CR_SYNC_ID_Field is STM32H753.UInt3;

   type DMAMUX_C7CR_Register is record
      --  DMA request identification Selects the input DMA request. See the
      --  DMAMUX table about assignments of multiplexer inputs to resources.
      DMAREQ_ID      : DMAMUX_C7CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : STM32H753.Bit := 16#0#;
      --  Synchronization overrun interrupt enable
      SOIE           : DMAMUX_C7CR_SOIE_Field := STM32H753.DMAMUX.B_0x0;
      --  Event generation enable
      EGE            : DMAMUX_C7CR_EGE_Field := STM32H753.DMAMUX.B_0x0;
      --  unspecified
      Reserved_10_15 : STM32H753.UInt6 := 16#0#;
      --  Synchronization enable
      SE             : DMAMUX_C7CR_SE_Field := STM32H753.DMAMUX.B_0x0;
      --  Synchronization polarity Defines the edge polarity of the selected
      --  synchronization input:
      SPOL           : DMAMUX_C7CR_SPOL_Field := STM32H753.DMAMUX.B_0x0;
      --  Number of DMA requests minus 1 to forward Defines the number of DMA
      --  requests to forward to the DMA controller after a synchronization
      --  event, and/or the number of DMA requests before an output event is
      --  generated. This field shall only be written when both SE and EGE bits
      --  are low.
      NBREQ          : DMAMUX_C7CR_NBREQ_Field := 16#0#;
      --  Synchronization identification Selects the synchronization input (see
      --  inputs to resources).
      SYNC_ID        : DMAMUX_C7CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_27_31 : STM32H753.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_C7CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 26;
      Reserved_27_31 at 0 range 27 .. 31;
   end record;

   subtype DMAMUX_C8CR_DMAREQ_ID_Field is STM32H753.UInt7;

   --  Synchronization overrun interrupt enable
   type DMAMUX_C8CR_SOIE_Field is
     (--  Interrupt disabled
      B_0x0,
      --  Interrupt enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C8CR_SOIE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Event generation enable
   type DMAMUX_C8CR_EGE_Field is
     (--  Event generation disabled
      B_0x0,
      --  Event generation enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C8CR_EGE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Synchronization enable
   type DMAMUX_C8CR_SE_Field is
     (--  Synchronization disabled
      B_0x0,
      --  Synchronization enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C8CR_SE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Synchronization polarity Defines the edge polarity of the selected
   --  synchronization input:
   type DMAMUX_C8CR_SPOL_Field is
     (--  No event, i.e. no synchronization nor detection.
      B_0x0,
      --  Rising edge
      B_0x1,
      --  Falling edge
      B_0x2,
      --  Rising and falling edges
      B_0x3)
     with Size => 2;
   for DMAMUX_C8CR_SPOL_Field use
     (B_0x0 => 0,
      B_0x1 => 1,
      B_0x2 => 2,
      B_0x3 => 3);

   subtype DMAMUX_C8CR_NBREQ_Field is STM32H753.UInt5;
   subtype DMAMUX_C8CR_SYNC_ID_Field is STM32H753.UInt3;

   type DMAMUX_C8CR_Register is record
      --  DMA request identification Selects the input DMA request. See the
      --  DMAMUX table about assignments of multiplexer inputs to resources.
      DMAREQ_ID      : DMAMUX_C8CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : STM32H753.Bit := 16#0#;
      --  Synchronization overrun interrupt enable
      SOIE           : DMAMUX_C8CR_SOIE_Field := STM32H753.DMAMUX.B_0x0;
      --  Event generation enable
      EGE            : DMAMUX_C8CR_EGE_Field := STM32H753.DMAMUX.B_0x0;
      --  unspecified
      Reserved_10_15 : STM32H753.UInt6 := 16#0#;
      --  Synchronization enable
      SE             : DMAMUX_C8CR_SE_Field := STM32H753.DMAMUX.B_0x0;
      --  Synchronization polarity Defines the edge polarity of the selected
      --  synchronization input:
      SPOL           : DMAMUX_C8CR_SPOL_Field := STM32H753.DMAMUX.B_0x0;
      --  Number of DMA requests minus 1 to forward Defines the number of DMA
      --  requests to forward to the DMA controller after a synchronization
      --  event, and/or the number of DMA requests before an output event is
      --  generated. This field shall only be written when both SE and EGE bits
      --  are low.
      NBREQ          : DMAMUX_C8CR_NBREQ_Field := 16#0#;
      --  Synchronization identification Selects the synchronization input (see
      --  inputs to resources).
      SYNC_ID        : DMAMUX_C8CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_27_31 : STM32H753.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_C8CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 26;
      Reserved_27_31 at 0 range 27 .. 31;
   end record;

   subtype DMAMUX_C9CR_DMAREQ_ID_Field is STM32H753.UInt7;

   --  Synchronization overrun interrupt enable
   type DMAMUX_C9CR_SOIE_Field is
     (--  Interrupt disabled
      B_0x0,
      --  Interrupt enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C9CR_SOIE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Event generation enable
   type DMAMUX_C9CR_EGE_Field is
     (--  Event generation disabled
      B_0x0,
      --  Event generation enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C9CR_EGE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Synchronization enable
   type DMAMUX_C9CR_SE_Field is
     (--  Synchronization disabled
      B_0x0,
      --  Synchronization enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C9CR_SE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Synchronization polarity Defines the edge polarity of the selected
   --  synchronization input:
   type DMAMUX_C9CR_SPOL_Field is
     (--  No event, i.e. no synchronization nor detection.
      B_0x0,
      --  Rising edge
      B_0x1,
      --  Falling edge
      B_0x2,
      --  Rising and falling edges
      B_0x3)
     with Size => 2;
   for DMAMUX_C9CR_SPOL_Field use
     (B_0x0 => 0,
      B_0x1 => 1,
      B_0x2 => 2,
      B_0x3 => 3);

   subtype DMAMUX_C9CR_NBREQ_Field is STM32H753.UInt5;
   subtype DMAMUX_C9CR_SYNC_ID_Field is STM32H753.UInt3;

   type DMAMUX_C9CR_Register is record
      --  DMA request identification Selects the input DMA request. See the
      --  DMAMUX table about assignments of multiplexer inputs to resources.
      DMAREQ_ID      : DMAMUX_C9CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : STM32H753.Bit := 16#0#;
      --  Synchronization overrun interrupt enable
      SOIE           : DMAMUX_C9CR_SOIE_Field := STM32H753.DMAMUX.B_0x0;
      --  Event generation enable
      EGE            : DMAMUX_C9CR_EGE_Field := STM32H753.DMAMUX.B_0x0;
      --  unspecified
      Reserved_10_15 : STM32H753.UInt6 := 16#0#;
      --  Synchronization enable
      SE             : DMAMUX_C9CR_SE_Field := STM32H753.DMAMUX.B_0x0;
      --  Synchronization polarity Defines the edge polarity of the selected
      --  synchronization input:
      SPOL           : DMAMUX_C9CR_SPOL_Field := STM32H753.DMAMUX.B_0x0;
      --  Number of DMA requests minus 1 to forward Defines the number of DMA
      --  requests to forward to the DMA controller after a synchronization
      --  event, and/or the number of DMA requests before an output event is
      --  generated. This field shall only be written when both SE and EGE bits
      --  are low.
      NBREQ          : DMAMUX_C9CR_NBREQ_Field := 16#0#;
      --  Synchronization identification Selects the synchronization input (see
      --  inputs to resources).
      SYNC_ID        : DMAMUX_C9CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_27_31 : STM32H753.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_C9CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 26;
      Reserved_27_31 at 0 range 27 .. 31;
   end record;

   subtype DMAMUX_C10CR_DMAREQ_ID_Field is STM32H753.UInt7;

   --  Synchronization overrun interrupt enable
   type DMAMUX_C10CR_SOIE_Field is
     (--  Interrupt disabled
      B_0x0,
      --  Interrupt enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C10CR_SOIE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Event generation enable
   type DMAMUX_C10CR_EGE_Field is
     (--  Event generation disabled
      B_0x0,
      --  Event generation enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C10CR_EGE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Synchronization enable
   type DMAMUX_C10CR_SE_Field is
     (--  Synchronization disabled
      B_0x0,
      --  Synchronization enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C10CR_SE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Synchronization polarity Defines the edge polarity of the selected
   --  synchronization input:
   type DMAMUX_C10CR_SPOL_Field is
     (--  No event, i.e. no synchronization nor detection.
      B_0x0,
      --  Rising edge
      B_0x1,
      --  Falling edge
      B_0x2,
      --  Rising and falling edges
      B_0x3)
     with Size => 2;
   for DMAMUX_C10CR_SPOL_Field use
     (B_0x0 => 0,
      B_0x1 => 1,
      B_0x2 => 2,
      B_0x3 => 3);

   subtype DMAMUX_C10CR_NBREQ_Field is STM32H753.UInt5;
   subtype DMAMUX_C10CR_SYNC_ID_Field is STM32H753.UInt3;

   type DMAMUX_C10CR_Register is record
      --  DMA request identification Selects the input DMA request. See the
      --  DMAMUX table about assignments of multiplexer inputs to resources.
      DMAREQ_ID      : DMAMUX_C10CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : STM32H753.Bit := 16#0#;
      --  Synchronization overrun interrupt enable
      SOIE           : DMAMUX_C10CR_SOIE_Field := STM32H753.DMAMUX.B_0x0;
      --  Event generation enable
      EGE            : DMAMUX_C10CR_EGE_Field := STM32H753.DMAMUX.B_0x0;
      --  unspecified
      Reserved_10_15 : STM32H753.UInt6 := 16#0#;
      --  Synchronization enable
      SE             : DMAMUX_C10CR_SE_Field := STM32H753.DMAMUX.B_0x0;
      --  Synchronization polarity Defines the edge polarity of the selected
      --  synchronization input:
      SPOL           : DMAMUX_C10CR_SPOL_Field := STM32H753.DMAMUX.B_0x0;
      --  Number of DMA requests minus 1 to forward Defines the number of DMA
      --  requests to forward to the DMA controller after a synchronization
      --  event, and/or the number of DMA requests before an output event is
      --  generated. This field shall only be written when both SE and EGE bits
      --  are low.
      NBREQ          : DMAMUX_C10CR_NBREQ_Field := 16#0#;
      --  Synchronization identification Selects the synchronization input (see
      --  inputs to resources).
      SYNC_ID        : DMAMUX_C10CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_27_31 : STM32H753.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_C10CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 26;
      Reserved_27_31 at 0 range 27 .. 31;
   end record;

   subtype DMAMUX_C11CR_DMAREQ_ID_Field is STM32H753.UInt7;

   --  Synchronization overrun interrupt enable
   type DMAMUX_C11CR_SOIE_Field is
     (--  Interrupt disabled
      B_0x0,
      --  Interrupt enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C11CR_SOIE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Event generation enable
   type DMAMUX_C11CR_EGE_Field is
     (--  Event generation disabled
      B_0x0,
      --  Event generation enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C11CR_EGE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Synchronization enable
   type DMAMUX_C11CR_SE_Field is
     (--  Synchronization disabled
      B_0x0,
      --  Synchronization enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C11CR_SE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Synchronization polarity Defines the edge polarity of the selected
   --  synchronization input:
   type DMAMUX_C11CR_SPOL_Field is
     (--  No event, i.e. no synchronization nor detection.
      B_0x0,
      --  Rising edge
      B_0x1,
      --  Falling edge
      B_0x2,
      --  Rising and falling edges
      B_0x3)
     with Size => 2;
   for DMAMUX_C11CR_SPOL_Field use
     (B_0x0 => 0,
      B_0x1 => 1,
      B_0x2 => 2,
      B_0x3 => 3);

   subtype DMAMUX_C11CR_NBREQ_Field is STM32H753.UInt5;
   subtype DMAMUX_C11CR_SYNC_ID_Field is STM32H753.UInt3;

   type DMAMUX_C11CR_Register is record
      --  DMA request identification Selects the input DMA request. See the
      --  DMAMUX table about assignments of multiplexer inputs to resources.
      DMAREQ_ID      : DMAMUX_C11CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : STM32H753.Bit := 16#0#;
      --  Synchronization overrun interrupt enable
      SOIE           : DMAMUX_C11CR_SOIE_Field := STM32H753.DMAMUX.B_0x0;
      --  Event generation enable
      EGE            : DMAMUX_C11CR_EGE_Field := STM32H753.DMAMUX.B_0x0;
      --  unspecified
      Reserved_10_15 : STM32H753.UInt6 := 16#0#;
      --  Synchronization enable
      SE             : DMAMUX_C11CR_SE_Field := STM32H753.DMAMUX.B_0x0;
      --  Synchronization polarity Defines the edge polarity of the selected
      --  synchronization input:
      SPOL           : DMAMUX_C11CR_SPOL_Field := STM32H753.DMAMUX.B_0x0;
      --  Number of DMA requests minus 1 to forward Defines the number of DMA
      --  requests to forward to the DMA controller after a synchronization
      --  event, and/or the number of DMA requests before an output event is
      --  generated. This field shall only be written when both SE and EGE bits
      --  are low.
      NBREQ          : DMAMUX_C11CR_NBREQ_Field := 16#0#;
      --  Synchronization identification Selects the synchronization input (see
      --  inputs to resources).
      SYNC_ID        : DMAMUX_C11CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_27_31 : STM32H753.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_C11CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 26;
      Reserved_27_31 at 0 range 27 .. 31;
   end record;

   subtype DMAMUX_C12CR_DMAREQ_ID_Field is STM32H753.UInt7;

   --  Synchronization overrun interrupt enable
   type DMAMUX_C12CR_SOIE_Field is
     (--  Interrupt disabled
      B_0x0,
      --  Interrupt enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C12CR_SOIE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Event generation enable
   type DMAMUX_C12CR_EGE_Field is
     (--  Event generation disabled
      B_0x0,
      --  Event generation enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C12CR_EGE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Synchronization enable
   type DMAMUX_C12CR_SE_Field is
     (--  Synchronization disabled
      B_0x0,
      --  Synchronization enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C12CR_SE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Synchronization polarity Defines the edge polarity of the selected
   --  synchronization input:
   type DMAMUX_C12CR_SPOL_Field is
     (--  No event, i.e. no synchronization nor detection.
      B_0x0,
      --  Rising edge
      B_0x1,
      --  Falling edge
      B_0x2,
      --  Rising and falling edges
      B_0x3)
     with Size => 2;
   for DMAMUX_C12CR_SPOL_Field use
     (B_0x0 => 0,
      B_0x1 => 1,
      B_0x2 => 2,
      B_0x3 => 3);

   subtype DMAMUX_C12CR_NBREQ_Field is STM32H753.UInt5;
   subtype DMAMUX_C12CR_SYNC_ID_Field is STM32H753.UInt3;

   type DMAMUX_C12CR_Register is record
      --  DMA request identification Selects the input DMA request. See the
      --  DMAMUX table about assignments of multiplexer inputs to resources.
      DMAREQ_ID      : DMAMUX_C12CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : STM32H753.Bit := 16#0#;
      --  Synchronization overrun interrupt enable
      SOIE           : DMAMUX_C12CR_SOIE_Field := STM32H753.DMAMUX.B_0x0;
      --  Event generation enable
      EGE            : DMAMUX_C12CR_EGE_Field := STM32H753.DMAMUX.B_0x0;
      --  unspecified
      Reserved_10_15 : STM32H753.UInt6 := 16#0#;
      --  Synchronization enable
      SE             : DMAMUX_C12CR_SE_Field := STM32H753.DMAMUX.B_0x0;
      --  Synchronization polarity Defines the edge polarity of the selected
      --  synchronization input:
      SPOL           : DMAMUX_C12CR_SPOL_Field := STM32H753.DMAMUX.B_0x0;
      --  Number of DMA requests minus 1 to forward Defines the number of DMA
      --  requests to forward to the DMA controller after a synchronization
      --  event, and/or the number of DMA requests before an output event is
      --  generated. This field shall only be written when both SE and EGE bits
      --  are low.
      NBREQ          : DMAMUX_C12CR_NBREQ_Field := 16#0#;
      --  Synchronization identification Selects the synchronization input (see
      --  inputs to resources).
      SYNC_ID        : DMAMUX_C12CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_27_31 : STM32H753.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_C12CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 26;
      Reserved_27_31 at 0 range 27 .. 31;
   end record;

   subtype DMAMUX_C13CR_DMAREQ_ID_Field is STM32H753.UInt7;

   --  Synchronization overrun interrupt enable
   type DMAMUX_C13CR_SOIE_Field is
     (--  Interrupt disabled
      B_0x0,
      --  Interrupt enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C13CR_SOIE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Event generation enable
   type DMAMUX_C13CR_EGE_Field is
     (--  Event generation disabled
      B_0x0,
      --  Event generation enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C13CR_EGE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Synchronization enable
   type DMAMUX_C13CR_SE_Field is
     (--  Synchronization disabled
      B_0x0,
      --  Synchronization enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C13CR_SE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Synchronization polarity Defines the edge polarity of the selected
   --  synchronization input:
   type DMAMUX_C13CR_SPOL_Field is
     (--  No event, i.e. no synchronization nor detection.
      B_0x0,
      --  Rising edge
      B_0x1,
      --  Falling edge
      B_0x2,
      --  Rising and falling edges
      B_0x3)
     with Size => 2;
   for DMAMUX_C13CR_SPOL_Field use
     (B_0x0 => 0,
      B_0x1 => 1,
      B_0x2 => 2,
      B_0x3 => 3);

   subtype DMAMUX_C13CR_NBREQ_Field is STM32H753.UInt5;
   subtype DMAMUX_C13CR_SYNC_ID_Field is STM32H753.UInt3;

   type DMAMUX_C13CR_Register is record
      --  DMA request identification Selects the input DMA request. See the
      --  DMAMUX table about assignments of multiplexer inputs to resources.
      DMAREQ_ID      : DMAMUX_C13CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : STM32H753.Bit := 16#0#;
      --  Synchronization overrun interrupt enable
      SOIE           : DMAMUX_C13CR_SOIE_Field := STM32H753.DMAMUX.B_0x0;
      --  Event generation enable
      EGE            : DMAMUX_C13CR_EGE_Field := STM32H753.DMAMUX.B_0x0;
      --  unspecified
      Reserved_10_15 : STM32H753.UInt6 := 16#0#;
      --  Synchronization enable
      SE             : DMAMUX_C13CR_SE_Field := STM32H753.DMAMUX.B_0x0;
      --  Synchronization polarity Defines the edge polarity of the selected
      --  synchronization input:
      SPOL           : DMAMUX_C13CR_SPOL_Field := STM32H753.DMAMUX.B_0x0;
      --  Number of DMA requests minus 1 to forward Defines the number of DMA
      --  requests to forward to the DMA controller after a synchronization
      --  event, and/or the number of DMA requests before an output event is
      --  generated. This field shall only be written when both SE and EGE bits
      --  are low.
      NBREQ          : DMAMUX_C13CR_NBREQ_Field := 16#0#;
      --  Synchronization identification Selects the synchronization input (see
      --  inputs to resources).
      SYNC_ID        : DMAMUX_C13CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_27_31 : STM32H753.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_C13CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 26;
      Reserved_27_31 at 0 range 27 .. 31;
   end record;

   subtype DMAMUX_C14CR_DMAREQ_ID_Field is STM32H753.UInt7;

   --  Synchronization overrun interrupt enable
   type DMAMUX_C14CR_SOIE_Field is
     (--  Interrupt disabled
      B_0x0,
      --  Interrupt enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C14CR_SOIE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Event generation enable
   type DMAMUX_C14CR_EGE_Field is
     (--  Event generation disabled
      B_0x0,
      --  Event generation enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C14CR_EGE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Synchronization enable
   type DMAMUX_C14CR_SE_Field is
     (--  Synchronization disabled
      B_0x0,
      --  Synchronization enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C14CR_SE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Synchronization polarity Defines the edge polarity of the selected
   --  synchronization input:
   type DMAMUX_C14CR_SPOL_Field is
     (--  No event, i.e. no synchronization nor detection.
      B_0x0,
      --  Rising edge
      B_0x1,
      --  Falling edge
      B_0x2,
      --  Rising and falling edges
      B_0x3)
     with Size => 2;
   for DMAMUX_C14CR_SPOL_Field use
     (B_0x0 => 0,
      B_0x1 => 1,
      B_0x2 => 2,
      B_0x3 => 3);

   subtype DMAMUX_C14CR_NBREQ_Field is STM32H753.UInt5;
   subtype DMAMUX_C14CR_SYNC_ID_Field is STM32H753.UInt3;

   type DMAMUX_C14CR_Register is record
      --  DMA request identification Selects the input DMA request. See the
      --  DMAMUX table about assignments of multiplexer inputs to resources.
      DMAREQ_ID      : DMAMUX_C14CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : STM32H753.Bit := 16#0#;
      --  Synchronization overrun interrupt enable
      SOIE           : DMAMUX_C14CR_SOIE_Field := STM32H753.DMAMUX.B_0x0;
      --  Event generation enable
      EGE            : DMAMUX_C14CR_EGE_Field := STM32H753.DMAMUX.B_0x0;
      --  unspecified
      Reserved_10_15 : STM32H753.UInt6 := 16#0#;
      --  Synchronization enable
      SE             : DMAMUX_C14CR_SE_Field := STM32H753.DMAMUX.B_0x0;
      --  Synchronization polarity Defines the edge polarity of the selected
      --  synchronization input:
      SPOL           : DMAMUX_C14CR_SPOL_Field := STM32H753.DMAMUX.B_0x0;
      --  Number of DMA requests minus 1 to forward Defines the number of DMA
      --  requests to forward to the DMA controller after a synchronization
      --  event, and/or the number of DMA requests before an output event is
      --  generated. This field shall only be written when both SE and EGE bits
      --  are low.
      NBREQ          : DMAMUX_C14CR_NBREQ_Field := 16#0#;
      --  Synchronization identification Selects the synchronization input (see
      --  inputs to resources).
      SYNC_ID        : DMAMUX_C14CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_27_31 : STM32H753.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_C14CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 26;
      Reserved_27_31 at 0 range 27 .. 31;
   end record;

   subtype DMAMUX_C15CR_DMAREQ_ID_Field is STM32H753.UInt7;

   --  Synchronization overrun interrupt enable
   type DMAMUX_C15CR_SOIE_Field is
     (--  Interrupt disabled
      B_0x0,
      --  Interrupt enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C15CR_SOIE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Event generation enable
   type DMAMUX_C15CR_EGE_Field is
     (--  Event generation disabled
      B_0x0,
      --  Event generation enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C15CR_EGE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Synchronization enable
   type DMAMUX_C15CR_SE_Field is
     (--  Synchronization disabled
      B_0x0,
      --  Synchronization enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_C15CR_SE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  Synchronization polarity Defines the edge polarity of the selected
   --  synchronization input:
   type DMAMUX_C15CR_SPOL_Field is
     (--  No event, i.e. no synchronization nor detection.
      B_0x0,
      --  Rising edge
      B_0x1,
      --  Falling edge
      B_0x2,
      --  Rising and falling edges
      B_0x3)
     with Size => 2;
   for DMAMUX_C15CR_SPOL_Field use
     (B_0x0 => 0,
      B_0x1 => 1,
      B_0x2 => 2,
      B_0x3 => 3);

   subtype DMAMUX_C15CR_NBREQ_Field is STM32H753.UInt5;
   subtype DMAMUX_C15CR_SYNC_ID_Field is STM32H753.UInt3;

   type DMAMUX_C15CR_Register is record
      --  DMA request identification Selects the input DMA request. See the
      --  DMAMUX table about assignments of multiplexer inputs to resources.
      DMAREQ_ID      : DMAMUX_C15CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_7_7   : STM32H753.Bit := 16#0#;
      --  Synchronization overrun interrupt enable
      SOIE           : DMAMUX_C15CR_SOIE_Field := STM32H753.DMAMUX.B_0x0;
      --  Event generation enable
      EGE            : DMAMUX_C15CR_EGE_Field := STM32H753.DMAMUX.B_0x0;
      --  unspecified
      Reserved_10_15 : STM32H753.UInt6 := 16#0#;
      --  Synchronization enable
      SE             : DMAMUX_C15CR_SE_Field := STM32H753.DMAMUX.B_0x0;
      --  Synchronization polarity Defines the edge polarity of the selected
      --  synchronization input:
      SPOL           : DMAMUX_C15CR_SPOL_Field := STM32H753.DMAMUX.B_0x0;
      --  Number of DMA requests minus 1 to forward Defines the number of DMA
      --  requests to forward to the DMA controller after a synchronization
      --  event, and/or the number of DMA requests before an output event is
      --  generated. This field shall only be written when both SE and EGE bits
      --  are low.
      NBREQ          : DMAMUX_C15CR_NBREQ_Field := 16#0#;
      --  Synchronization identification Selects the synchronization input (see
      --  inputs to resources).
      SYNC_ID        : DMAMUX_C15CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_27_31 : STM32H753.UInt5 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_C15CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 6;
      Reserved_7_7   at 0 range 7 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 26;
      Reserved_27_31 at 0 range 27 .. 31;
   end record;

   --  DMAMUX_CSR_SOF array
   type DMAMUX_CSR_SOF_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for DMAMUX_CSR_SOF
   type DMAMUX_CSR_SOF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SOF as a value
            Val : STM32H753.UInt16;
         when True =>
            --  SOF as an array
            Arr : DMAMUX_CSR_SOF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for DMAMUX_CSR_SOF_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   type DMAMUX_CSR_Register is record
      --  Read-only. Synchronization overrun event flag The flag is set when a
      --  synchronization event occurs on a DMA request line multiplexer
      --  channel x, while the DMA request counter value is lower than NBREQ.
      --  The flag is cleared by writing 1 to the corresponding CSOFx bit in
      --  DMAMUX_CFR register. For DMAMUX2 bits 15:8 are reserved, keep them at
      --  reset value.
      SOF            : DMAMUX_CSR_SOF_Field;
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_CSR_Register use record
      SOF            at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   --  DMAMUX_CFR_CSOF array
   type DMAMUX_CFR_CSOF_Field_Array is array (0 .. 15) of Boolean
     with Component_Size => 1, Size => 16;

   --  Type definition for DMAMUX_CFR_CSOF
   type DMAMUX_CFR_CSOF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CSOF as a value
            Val : STM32H753.UInt16;
         when True =>
            --  CSOF as an array
            Arr : DMAMUX_CFR_CSOF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 16;

   for DMAMUX_CFR_CSOF_Field use record
      Val at 0 range 0 .. 15;
      Arr at 0 range 0 .. 15;
   end record;

   type DMAMUX_CFR_Register is record
      --  Write-only. Clear synchronization overrun event flag Writing 1 in
      --  each bit clears the corresponding overrun flag SOFx in the DMAMUX_CSR
      --  register.
      CSOF           : DMAMUX_CFR_CSOF_Field :=
                        (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_16_31 : STM32H753.UInt16 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_CFR_Register use record
      CSOF           at 0 range 0 .. 15;
      Reserved_16_31 at 0 range 16 .. 31;
   end record;

   subtype DMAMUX_RG0CR_SIG_ID_Field is STM32H753.UInt3;

   --  Trigger overrun interrupt enable
   type DMAMUX_RG0CR_OIE_Field is
     (--  Interrupt on a trigger overrun event occurrence is disabled
      B_0x0,
      --  Interrupt on a trigger overrun event occurrence is enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_RG0CR_OIE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  DMA request generator channel x enable
   type DMAMUX_RG0CR_GE_Field is
     (--  DMA request generator channel x disabled
      B_0x0,
      --  DMA request generator channel x enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_RG0CR_GE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  DMA request generator trigger polarity Defines the edge polarity of the
   --  selected trigger input
   type DMAMUX_RG0CR_GPOL_Field is
     (--  No event, i.e. no trigger detection nor generation.
      B_0x0,
      --  Rising edge
      B_0x1,
      --  Falling edge
      B_0x2,
      --  Rising and falling edges
      B_0x3)
     with Size => 2;
   for DMAMUX_RG0CR_GPOL_Field use
     (B_0x0 => 0,
      B_0x1 => 1,
      B_0x2 => 2,
      B_0x3 => 3);

   subtype DMAMUX_RG0CR_GNBREQ_Field is STM32H753.UInt5;

   type DMAMUX_RG0CR_Register is record
      --  Signal identification Selects the DMA request trigger input used for
      --  the channel x of the DMA request generator
      SIG_ID         : DMAMUX_RG0CR_SIG_ID_Field := 16#0#;
      --  unspecified
      Reserved_3_7   : STM32H753.UInt5 := 16#0#;
      --  Trigger overrun interrupt enable
      OIE            : DMAMUX_RG0CR_OIE_Field := STM32H753.DMAMUX.B_0x0;
      --  unspecified
      Reserved_9_15  : STM32H753.UInt7 := 16#0#;
      --  DMA request generator channel x enable
      GE             : DMAMUX_RG0CR_GE_Field := STM32H753.DMAMUX.B_0x0;
      --  DMA request generator trigger polarity Defines the edge polarity of
      --  the selected trigger input
      GPOL           : DMAMUX_RG0CR_GPOL_Field := STM32H753.DMAMUX.B_0x0;
      --  Number of DMA requests to be generated (minus 1) Defines the number
      --  of DMA requests to be generated after a trigger event. The actual
      --  number of generated DMA requests is GNBREQ +1. Note: This field must
      --  be written only when GE bit is disabled.
      GNBREQ         : DMAMUX_RG0CR_GNBREQ_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : STM32H753.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_RG0CR_Register use record
      SIG_ID         at 0 range 0 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      OIE            at 0 range 8 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      GE             at 0 range 16 .. 16;
      GPOL           at 0 range 17 .. 18;
      GNBREQ         at 0 range 19 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype DMAMUX_RG1CR_SIG_ID_Field is STM32H753.UInt3;

   --  Trigger overrun interrupt enable
   type DMAMUX_RG1CR_OIE_Field is
     (--  Interrupt on a trigger overrun event occurrence is disabled
      B_0x0,
      --  Interrupt on a trigger overrun event occurrence is enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_RG1CR_OIE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  DMA request generator channel x enable
   type DMAMUX_RG1CR_GE_Field is
     (--  DMA request generator channel x disabled
      B_0x0,
      --  DMA request generator channel x enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_RG1CR_GE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  DMA request generator trigger polarity Defines the edge polarity of the
   --  selected trigger input
   type DMAMUX_RG1CR_GPOL_Field is
     (--  No event, i.e. no trigger detection nor generation.
      B_0x0,
      --  Rising edge
      B_0x1,
      --  Falling edge
      B_0x2,
      --  Rising and falling edges
      B_0x3)
     with Size => 2;
   for DMAMUX_RG1CR_GPOL_Field use
     (B_0x0 => 0,
      B_0x1 => 1,
      B_0x2 => 2,
      B_0x3 => 3);

   subtype DMAMUX_RG1CR_GNBREQ_Field is STM32H753.UInt5;

   type DMAMUX_RG1CR_Register is record
      --  Signal identification Selects the DMA request trigger input used for
      --  the channel x of the DMA request generator
      SIG_ID         : DMAMUX_RG1CR_SIG_ID_Field := 16#0#;
      --  unspecified
      Reserved_3_7   : STM32H753.UInt5 := 16#0#;
      --  Trigger overrun interrupt enable
      OIE            : DMAMUX_RG1CR_OIE_Field := STM32H753.DMAMUX.B_0x0;
      --  unspecified
      Reserved_9_15  : STM32H753.UInt7 := 16#0#;
      --  DMA request generator channel x enable
      GE             : DMAMUX_RG1CR_GE_Field := STM32H753.DMAMUX.B_0x0;
      --  DMA request generator trigger polarity Defines the edge polarity of
      --  the selected trigger input
      GPOL           : DMAMUX_RG1CR_GPOL_Field := STM32H753.DMAMUX.B_0x0;
      --  Number of DMA requests to be generated (minus 1) Defines the number
      --  of DMA requests to be generated after a trigger event. The actual
      --  number of generated DMA requests is GNBREQ +1. Note: This field must
      --  be written only when GE bit is disabled.
      GNBREQ         : DMAMUX_RG1CR_GNBREQ_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : STM32H753.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_RG1CR_Register use record
      SIG_ID         at 0 range 0 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      OIE            at 0 range 8 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      GE             at 0 range 16 .. 16;
      GPOL           at 0 range 17 .. 18;
      GNBREQ         at 0 range 19 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype DMAMUX_RG2CR_SIG_ID_Field is STM32H753.UInt3;

   --  Trigger overrun interrupt enable
   type DMAMUX_RG2CR_OIE_Field is
     (--  Interrupt on a trigger overrun event occurrence is disabled
      B_0x0,
      --  Interrupt on a trigger overrun event occurrence is enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_RG2CR_OIE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  DMA request generator channel x enable
   type DMAMUX_RG2CR_GE_Field is
     (--  DMA request generator channel x disabled
      B_0x0,
      --  DMA request generator channel x enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_RG2CR_GE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  DMA request generator trigger polarity Defines the edge polarity of the
   --  selected trigger input
   type DMAMUX_RG2CR_GPOL_Field is
     (--  No event, i.e. no trigger detection nor generation.
      B_0x0,
      --  Rising edge
      B_0x1,
      --  Falling edge
      B_0x2,
      --  Rising and falling edges
      B_0x3)
     with Size => 2;
   for DMAMUX_RG2CR_GPOL_Field use
     (B_0x0 => 0,
      B_0x1 => 1,
      B_0x2 => 2,
      B_0x3 => 3);

   subtype DMAMUX_RG2CR_GNBREQ_Field is STM32H753.UInt5;

   type DMAMUX_RG2CR_Register is record
      --  Signal identification Selects the DMA request trigger input used for
      --  the channel x of the DMA request generator
      SIG_ID         : DMAMUX_RG2CR_SIG_ID_Field := 16#0#;
      --  unspecified
      Reserved_3_7   : STM32H753.UInt5 := 16#0#;
      --  Trigger overrun interrupt enable
      OIE            : DMAMUX_RG2CR_OIE_Field := STM32H753.DMAMUX.B_0x0;
      --  unspecified
      Reserved_9_15  : STM32H753.UInt7 := 16#0#;
      --  DMA request generator channel x enable
      GE             : DMAMUX_RG2CR_GE_Field := STM32H753.DMAMUX.B_0x0;
      --  DMA request generator trigger polarity Defines the edge polarity of
      --  the selected trigger input
      GPOL           : DMAMUX_RG2CR_GPOL_Field := STM32H753.DMAMUX.B_0x0;
      --  Number of DMA requests to be generated (minus 1) Defines the number
      --  of DMA requests to be generated after a trigger event. The actual
      --  number of generated DMA requests is GNBREQ +1. Note: This field must
      --  be written only when GE bit is disabled.
      GNBREQ         : DMAMUX_RG2CR_GNBREQ_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : STM32H753.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_RG2CR_Register use record
      SIG_ID         at 0 range 0 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      OIE            at 0 range 8 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      GE             at 0 range 16 .. 16;
      GPOL           at 0 range 17 .. 18;
      GNBREQ         at 0 range 19 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype DMAMUX_RG3CR_SIG_ID_Field is STM32H753.UInt3;

   --  Trigger overrun interrupt enable
   type DMAMUX_RG3CR_OIE_Field is
     (--  Interrupt on a trigger overrun event occurrence is disabled
      B_0x0,
      --  Interrupt on a trigger overrun event occurrence is enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_RG3CR_OIE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  DMA request generator channel x enable
   type DMAMUX_RG3CR_GE_Field is
     (--  DMA request generator channel x disabled
      B_0x0,
      --  DMA request generator channel x enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_RG3CR_GE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  DMA request generator trigger polarity Defines the edge polarity of the
   --  selected trigger input
   type DMAMUX_RG3CR_GPOL_Field is
     (--  No event, i.e. no trigger detection nor generation.
      B_0x0,
      --  Rising edge
      B_0x1,
      --  Falling edge
      B_0x2,
      --  Rising and falling edges
      B_0x3)
     with Size => 2;
   for DMAMUX_RG3CR_GPOL_Field use
     (B_0x0 => 0,
      B_0x1 => 1,
      B_0x2 => 2,
      B_0x3 => 3);

   subtype DMAMUX_RG3CR_GNBREQ_Field is STM32H753.UInt5;

   type DMAMUX_RG3CR_Register is record
      --  Signal identification Selects the DMA request trigger input used for
      --  the channel x of the DMA request generator
      SIG_ID         : DMAMUX_RG3CR_SIG_ID_Field := 16#0#;
      --  unspecified
      Reserved_3_7   : STM32H753.UInt5 := 16#0#;
      --  Trigger overrun interrupt enable
      OIE            : DMAMUX_RG3CR_OIE_Field := STM32H753.DMAMUX.B_0x0;
      --  unspecified
      Reserved_9_15  : STM32H753.UInt7 := 16#0#;
      --  DMA request generator channel x enable
      GE             : DMAMUX_RG3CR_GE_Field := STM32H753.DMAMUX.B_0x0;
      --  DMA request generator trigger polarity Defines the edge polarity of
      --  the selected trigger input
      GPOL           : DMAMUX_RG3CR_GPOL_Field := STM32H753.DMAMUX.B_0x0;
      --  Number of DMA requests to be generated (minus 1) Defines the number
      --  of DMA requests to be generated after a trigger event. The actual
      --  number of generated DMA requests is GNBREQ +1. Note: This field must
      --  be written only when GE bit is disabled.
      GNBREQ         : DMAMUX_RG3CR_GNBREQ_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : STM32H753.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_RG3CR_Register use record
      SIG_ID         at 0 range 0 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      OIE            at 0 range 8 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      GE             at 0 range 16 .. 16;
      GPOL           at 0 range 17 .. 18;
      GNBREQ         at 0 range 19 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype DMAMUX_RG4CR_SIG_ID_Field is STM32H753.UInt3;

   --  Trigger overrun interrupt enable
   type DMAMUX_RG4CR_OIE_Field is
     (--  Interrupt on a trigger overrun event occurrence is disabled
      B_0x0,
      --  Interrupt on a trigger overrun event occurrence is enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_RG4CR_OIE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  DMA request generator channel x enable
   type DMAMUX_RG4CR_GE_Field is
     (--  DMA request generator channel x disabled
      B_0x0,
      --  DMA request generator channel x enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_RG4CR_GE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  DMA request generator trigger polarity Defines the edge polarity of the
   --  selected trigger input
   type DMAMUX_RG4CR_GPOL_Field is
     (--  No event, i.e. no trigger detection nor generation.
      B_0x0,
      --  Rising edge
      B_0x1,
      --  Falling edge
      B_0x2,
      --  Rising and falling edges
      B_0x3)
     with Size => 2;
   for DMAMUX_RG4CR_GPOL_Field use
     (B_0x0 => 0,
      B_0x1 => 1,
      B_0x2 => 2,
      B_0x3 => 3);

   subtype DMAMUX_RG4CR_GNBREQ_Field is STM32H753.UInt5;

   type DMAMUX_RG4CR_Register is record
      --  Signal identification Selects the DMA request trigger input used for
      --  the channel x of the DMA request generator
      SIG_ID         : DMAMUX_RG4CR_SIG_ID_Field := 16#0#;
      --  unspecified
      Reserved_3_7   : STM32H753.UInt5 := 16#0#;
      --  Trigger overrun interrupt enable
      OIE            : DMAMUX_RG4CR_OIE_Field := STM32H753.DMAMUX.B_0x0;
      --  unspecified
      Reserved_9_15  : STM32H753.UInt7 := 16#0#;
      --  DMA request generator channel x enable
      GE             : DMAMUX_RG4CR_GE_Field := STM32H753.DMAMUX.B_0x0;
      --  DMA request generator trigger polarity Defines the edge polarity of
      --  the selected trigger input
      GPOL           : DMAMUX_RG4CR_GPOL_Field := STM32H753.DMAMUX.B_0x0;
      --  Number of DMA requests to be generated (minus 1) Defines the number
      --  of DMA requests to be generated after a trigger event. The actual
      --  number of generated DMA requests is GNBREQ +1. Note: This field must
      --  be written only when GE bit is disabled.
      GNBREQ         : DMAMUX_RG4CR_GNBREQ_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : STM32H753.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_RG4CR_Register use record
      SIG_ID         at 0 range 0 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      OIE            at 0 range 8 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      GE             at 0 range 16 .. 16;
      GPOL           at 0 range 17 .. 18;
      GNBREQ         at 0 range 19 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype DMAMUX_RG5CR_SIG_ID_Field is STM32H753.UInt3;

   --  Trigger overrun interrupt enable
   type DMAMUX_RG5CR_OIE_Field is
     (--  Interrupt on a trigger overrun event occurrence is disabled
      B_0x0,
      --  Interrupt on a trigger overrun event occurrence is enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_RG5CR_OIE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  DMA request generator channel x enable
   type DMAMUX_RG5CR_GE_Field is
     (--  DMA request generator channel x disabled
      B_0x0,
      --  DMA request generator channel x enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_RG5CR_GE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  DMA request generator trigger polarity Defines the edge polarity of the
   --  selected trigger input
   type DMAMUX_RG5CR_GPOL_Field is
     (--  No event, i.e. no trigger detection nor generation.
      B_0x0,
      --  Rising edge
      B_0x1,
      --  Falling edge
      B_0x2,
      --  Rising and falling edges
      B_0x3)
     with Size => 2;
   for DMAMUX_RG5CR_GPOL_Field use
     (B_0x0 => 0,
      B_0x1 => 1,
      B_0x2 => 2,
      B_0x3 => 3);

   subtype DMAMUX_RG5CR_GNBREQ_Field is STM32H753.UInt5;

   type DMAMUX_RG5CR_Register is record
      --  Signal identification Selects the DMA request trigger input used for
      --  the channel x of the DMA request generator
      SIG_ID         : DMAMUX_RG5CR_SIG_ID_Field := 16#0#;
      --  unspecified
      Reserved_3_7   : STM32H753.UInt5 := 16#0#;
      --  Trigger overrun interrupt enable
      OIE            : DMAMUX_RG5CR_OIE_Field := STM32H753.DMAMUX.B_0x0;
      --  unspecified
      Reserved_9_15  : STM32H753.UInt7 := 16#0#;
      --  DMA request generator channel x enable
      GE             : DMAMUX_RG5CR_GE_Field := STM32H753.DMAMUX.B_0x0;
      --  DMA request generator trigger polarity Defines the edge polarity of
      --  the selected trigger input
      GPOL           : DMAMUX_RG5CR_GPOL_Field := STM32H753.DMAMUX.B_0x0;
      --  Number of DMA requests to be generated (minus 1) Defines the number
      --  of DMA requests to be generated after a trigger event. The actual
      --  number of generated DMA requests is GNBREQ +1. Note: This field must
      --  be written only when GE bit is disabled.
      GNBREQ         : DMAMUX_RG5CR_GNBREQ_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : STM32H753.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_RG5CR_Register use record
      SIG_ID         at 0 range 0 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      OIE            at 0 range 8 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      GE             at 0 range 16 .. 16;
      GPOL           at 0 range 17 .. 18;
      GNBREQ         at 0 range 19 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype DMAMUX_RG6CR_SIG_ID_Field is STM32H753.UInt3;

   --  Trigger overrun interrupt enable
   type DMAMUX_RG6CR_OIE_Field is
     (--  Interrupt on a trigger overrun event occurrence is disabled
      B_0x0,
      --  Interrupt on a trigger overrun event occurrence is enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_RG6CR_OIE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  DMA request generator channel x enable
   type DMAMUX_RG6CR_GE_Field is
     (--  DMA request generator channel x disabled
      B_0x0,
      --  DMA request generator channel x enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_RG6CR_GE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  DMA request generator trigger polarity Defines the edge polarity of the
   --  selected trigger input
   type DMAMUX_RG6CR_GPOL_Field is
     (--  No event, i.e. no trigger detection nor generation.
      B_0x0,
      --  Rising edge
      B_0x1,
      --  Falling edge
      B_0x2,
      --  Rising and falling edges
      B_0x3)
     with Size => 2;
   for DMAMUX_RG6CR_GPOL_Field use
     (B_0x0 => 0,
      B_0x1 => 1,
      B_0x2 => 2,
      B_0x3 => 3);

   subtype DMAMUX_RG6CR_GNBREQ_Field is STM32H753.UInt5;

   type DMAMUX_RG6CR_Register is record
      --  Signal identification Selects the DMA request trigger input used for
      --  the channel x of the DMA request generator
      SIG_ID         : DMAMUX_RG6CR_SIG_ID_Field := 16#0#;
      --  unspecified
      Reserved_3_7   : STM32H753.UInt5 := 16#0#;
      --  Trigger overrun interrupt enable
      OIE            : DMAMUX_RG6CR_OIE_Field := STM32H753.DMAMUX.B_0x0;
      --  unspecified
      Reserved_9_15  : STM32H753.UInt7 := 16#0#;
      --  DMA request generator channel x enable
      GE             : DMAMUX_RG6CR_GE_Field := STM32H753.DMAMUX.B_0x0;
      --  DMA request generator trigger polarity Defines the edge polarity of
      --  the selected trigger input
      GPOL           : DMAMUX_RG6CR_GPOL_Field := STM32H753.DMAMUX.B_0x0;
      --  Number of DMA requests to be generated (minus 1) Defines the number
      --  of DMA requests to be generated after a trigger event. The actual
      --  number of generated DMA requests is GNBREQ +1. Note: This field must
      --  be written only when GE bit is disabled.
      GNBREQ         : DMAMUX_RG6CR_GNBREQ_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : STM32H753.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_RG6CR_Register use record
      SIG_ID         at 0 range 0 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      OIE            at 0 range 8 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      GE             at 0 range 16 .. 16;
      GPOL           at 0 range 17 .. 18;
      GNBREQ         at 0 range 19 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype DMAMUX_RG7CR_SIG_ID_Field is STM32H753.UInt3;

   --  Trigger overrun interrupt enable
   type DMAMUX_RG7CR_OIE_Field is
     (--  Interrupt on a trigger overrun event occurrence is disabled
      B_0x0,
      --  Interrupt on a trigger overrun event occurrence is enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_RG7CR_OIE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  DMA request generator channel x enable
   type DMAMUX_RG7CR_GE_Field is
     (--  DMA request generator channel x disabled
      B_0x0,
      --  DMA request generator channel x enabled
      B_0x1)
     with Size => 1;
   for DMAMUX_RG7CR_GE_Field use
     (B_0x0 => 0,
      B_0x1 => 1);

   --  DMA request generator trigger polarity Defines the edge polarity of the
   --  selected trigger input
   type DMAMUX_RG7CR_GPOL_Field is
     (--  No event, i.e. no trigger detection nor generation.
      B_0x0,
      --  Rising edge
      B_0x1,
      --  Falling edge
      B_0x2,
      --  Rising and falling edges
      B_0x3)
     with Size => 2;
   for DMAMUX_RG7CR_GPOL_Field use
     (B_0x0 => 0,
      B_0x1 => 1,
      B_0x2 => 2,
      B_0x3 => 3);

   subtype DMAMUX_RG7CR_GNBREQ_Field is STM32H753.UInt5;

   type DMAMUX_RG7CR_Register is record
      --  Signal identification Selects the DMA request trigger input used for
      --  the channel x of the DMA request generator
      SIG_ID         : DMAMUX_RG7CR_SIG_ID_Field := 16#0#;
      --  unspecified
      Reserved_3_7   : STM32H753.UInt5 := 16#0#;
      --  Trigger overrun interrupt enable
      OIE            : DMAMUX_RG7CR_OIE_Field := STM32H753.DMAMUX.B_0x0;
      --  unspecified
      Reserved_9_15  : STM32H753.UInt7 := 16#0#;
      --  DMA request generator channel x enable
      GE             : DMAMUX_RG7CR_GE_Field := STM32H753.DMAMUX.B_0x0;
      --  DMA request generator trigger polarity Defines the edge polarity of
      --  the selected trigger input
      GPOL           : DMAMUX_RG7CR_GPOL_Field := STM32H753.DMAMUX.B_0x0;
      --  Number of DMA requests to be generated (minus 1) Defines the number
      --  of DMA requests to be generated after a trigger event. The actual
      --  number of generated DMA requests is GNBREQ +1. Note: This field must
      --  be written only when GE bit is disabled.
      GNBREQ         : DMAMUX_RG7CR_GNBREQ_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : STM32H753.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_RG7CR_Register use record
      SIG_ID         at 0 range 0 .. 2;
      Reserved_3_7   at 0 range 3 .. 7;
      OIE            at 0 range 8 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      GE             at 0 range 16 .. 16;
      GPOL           at 0 range 17 .. 18;
      GNBREQ         at 0 range 19 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  DMAMUX_RGSR_OF array
   type DMAMUX_RGSR_OF_Field_Array is array (0 .. 7) of Boolean
     with Component_Size => 1, Size => 8;

   --  Type definition for DMAMUX_RGSR_OF
   type DMAMUX_RGSR_OF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  OF as a value
            Val : STM32H753.Byte;
         when True =>
            --  OF as an array
            Arr : DMAMUX_RGSR_OF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 8;

   for DMAMUX_RGSR_OF_Field use record
      Val at 0 range 0 .. 7;
      Arr at 0 range 0 .. 7;
   end record;

   type DMAMUX_RGSR_Register is record
      --  Read-only. [:0]: Trigger overrun event flag The flag is set when a
      --  new trigger event occurs on DMA request generator channel x, before
      --  the request counter underrun (the internal request counter programmed
      --  via the GNBREQ field of the DMAMUX_RGxCR register). The flag is
      --  cleared by writing 1 to the corresponding COFx bit in the
      --  DMAMUX_RGCFR register.
      OF_k          : DMAMUX_RGSR_OF_Field;
      --  unspecified
      Reserved_8_31 : STM32H753.UInt24;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_RGSR_Register use record
      OF_k          at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  DMAMUX_RGCFR_COF array
   type DMAMUX_RGCFR_COF_Field_Array is array (0 .. 7) of Boolean
     with Component_Size => 1, Size => 8;

   --  Type definition for DMAMUX_RGCFR_COF
   type DMAMUX_RGCFR_COF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  COF as a value
            Val : STM32H753.Byte;
         when True =>
            --  COF as an array
            Arr : DMAMUX_RGCFR_COF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 8;

   for DMAMUX_RGCFR_COF_Field use record
      Val at 0 range 0 .. 7;
      Arr at 0 range 0 .. 7;
   end record;

   type DMAMUX_RGCFR_Register is record
      --  Write-only. Clear trigger overrun event flag Writing 1 in each bit
      --  clears the corresponding overrun flag OFx in the DMAMUX_RGSR
      --  register.
      COF           : DMAMUX_RGCFR_COF_Field :=
                       (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_8_31 : STM32H753.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for DMAMUX_RGCFR_Register use record
      COF           at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype C0CR_DMAREQ_ID_Field is STM32H753.UInt5;
   subtype C0CR_SPOL_Field is STM32H753.UInt2;
   subtype C0CR_NBREQ_Field is STM32H753.UInt5;
   subtype C0CR_SYNC_ID_Field is STM32H753.UInt5;

   --  DMAMux - DMA request line multiplexer channel x control register
   type C0CR_Register is record
      --  Input DMA request line selected
      DMAREQ_ID      : C0CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : STM32H753.UInt3 := 16#0#;
      --  Interrupt enable at synchronization event overrun
      SOIE           : Boolean := False;
      --  Event generation enable/disable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : STM32H753.UInt6 := 16#0#;
      --  Synchronous operating mode enable/disable
      SE             : Boolean := False;
      --  Synchronization event type selector Defines the synchronization event
      --  on the selected synchronization input:
      SPOL           : C0CR_SPOL_Field := 16#0#;
      --  Number of DMA requests to forward Defines the number of DMA requests
      --  forwarded before output event is generated. In synchronous mode, it
      --  also defines the number of DMA requests to forward after a
      --  synchronization event, then stop forwarding. The actual number of DMA
      --  requests forwarded is NBREQ+1. Note: This field can only be written
      --  when both SE and EGE bits are reset.
      NBREQ          : C0CR_NBREQ_Field := 16#0#;
      --  Synchronization input selected
      SYNC_ID        : C0CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : STM32H753.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C0CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C1CR_DMAREQ_ID_Field is STM32H753.UInt5;
   subtype C1CR_SPOL_Field is STM32H753.UInt2;
   subtype C1CR_NBREQ_Field is STM32H753.UInt5;
   subtype C1CR_SYNC_ID_Field is STM32H753.UInt5;

   --  DMAMux - DMA request line multiplexer channel x control register
   type C1CR_Register is record
      --  Input DMA request line selected
      DMAREQ_ID      : C1CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : STM32H753.UInt3 := 16#0#;
      --  Interrupt enable at synchronization event overrun
      SOIE           : Boolean := False;
      --  Event generation enable/disable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : STM32H753.UInt6 := 16#0#;
      --  Synchronous operating mode enable/disable
      SE             : Boolean := False;
      --  Synchronization event type selector Defines the synchronization event
      --  on the selected synchronization input:
      SPOL           : C1CR_SPOL_Field := 16#0#;
      --  Number of DMA requests to forward Defines the number of DMA requests
      --  forwarded before output event is generated. In synchronous mode, it
      --  also defines the number of DMA requests to forward after a
      --  synchronization event, then stop forwarding. The actual number of DMA
      --  requests forwarded is NBREQ+1. Note: This field can only be written
      --  when both SE and EGE bits are reset.
      NBREQ          : C1CR_NBREQ_Field := 16#0#;
      --  Synchronization input selected
      SYNC_ID        : C1CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : STM32H753.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C1CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C2CR_DMAREQ_ID_Field is STM32H753.UInt5;
   subtype C2CR_SPOL_Field is STM32H753.UInt2;
   subtype C2CR_NBREQ_Field is STM32H753.UInt5;
   subtype C2CR_SYNC_ID_Field is STM32H753.UInt5;

   --  DMAMux - DMA request line multiplexer channel x control register
   type C2CR_Register is record
      --  Input DMA request line selected
      DMAREQ_ID      : C2CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : STM32H753.UInt3 := 16#0#;
      --  Interrupt enable at synchronization event overrun
      SOIE           : Boolean := False;
      --  Event generation enable/disable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : STM32H753.UInt6 := 16#0#;
      --  Synchronous operating mode enable/disable
      SE             : Boolean := False;
      --  Synchronization event type selector Defines the synchronization event
      --  on the selected synchronization input:
      SPOL           : C2CR_SPOL_Field := 16#0#;
      --  Number of DMA requests to forward Defines the number of DMA requests
      --  forwarded before output event is generated. In synchronous mode, it
      --  also defines the number of DMA requests to forward after a
      --  synchronization event, then stop forwarding. The actual number of DMA
      --  requests forwarded is NBREQ+1. Note: This field can only be written
      --  when both SE and EGE bits are reset.
      NBREQ          : C2CR_NBREQ_Field := 16#0#;
      --  Synchronization input selected
      SYNC_ID        : C2CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : STM32H753.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C2CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C3CR_DMAREQ_ID_Field is STM32H753.UInt5;
   subtype C3CR_SPOL_Field is STM32H753.UInt2;
   subtype C3CR_NBREQ_Field is STM32H753.UInt5;
   subtype C3CR_SYNC_ID_Field is STM32H753.UInt5;

   --  DMAMux - DMA request line multiplexer channel x control register
   type C3CR_Register is record
      --  Input DMA request line selected
      DMAREQ_ID      : C3CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : STM32H753.UInt3 := 16#0#;
      --  Interrupt enable at synchronization event overrun
      SOIE           : Boolean := False;
      --  Event generation enable/disable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : STM32H753.UInt6 := 16#0#;
      --  Synchronous operating mode enable/disable
      SE             : Boolean := False;
      --  Synchronization event type selector Defines the synchronization event
      --  on the selected synchronization input:
      SPOL           : C3CR_SPOL_Field := 16#0#;
      --  Number of DMA requests to forward Defines the number of DMA requests
      --  forwarded before output event is generated. In synchronous mode, it
      --  also defines the number of DMA requests to forward after a
      --  synchronization event, then stop forwarding. The actual number of DMA
      --  requests forwarded is NBREQ+1. Note: This field can only be written
      --  when both SE and EGE bits are reset.
      NBREQ          : C3CR_NBREQ_Field := 16#0#;
      --  Synchronization input selected
      SYNC_ID        : C3CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : STM32H753.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C3CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C4CR_DMAREQ_ID_Field is STM32H753.UInt5;
   subtype C4CR_SPOL_Field is STM32H753.UInt2;
   subtype C4CR_NBREQ_Field is STM32H753.UInt5;
   subtype C4CR_SYNC_ID_Field is STM32H753.UInt5;

   --  DMAMux - DMA request line multiplexer channel x control register
   type C4CR_Register is record
      --  Input DMA request line selected
      DMAREQ_ID      : C4CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : STM32H753.UInt3 := 16#0#;
      --  Interrupt enable at synchronization event overrun
      SOIE           : Boolean := False;
      --  Event generation enable/disable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : STM32H753.UInt6 := 16#0#;
      --  Synchronous operating mode enable/disable
      SE             : Boolean := False;
      --  Synchronization event type selector Defines the synchronization event
      --  on the selected synchronization input:
      SPOL           : C4CR_SPOL_Field := 16#0#;
      --  Number of DMA requests to forward Defines the number of DMA requests
      --  forwarded before output event is generated. In synchronous mode, it
      --  also defines the number of DMA requests to forward after a
      --  synchronization event, then stop forwarding. The actual number of DMA
      --  requests forwarded is NBREQ+1. Note: This field can only be written
      --  when both SE and EGE bits are reset.
      NBREQ          : C4CR_NBREQ_Field := 16#0#;
      --  Synchronization input selected
      SYNC_ID        : C4CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : STM32H753.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C4CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C5CR_DMAREQ_ID_Field is STM32H753.UInt5;
   subtype C5CR_SPOL_Field is STM32H753.UInt2;
   subtype C5CR_NBREQ_Field is STM32H753.UInt5;
   subtype C5CR_SYNC_ID_Field is STM32H753.UInt5;

   --  DMAMux - DMA request line multiplexer channel x control register
   type C5CR_Register is record
      --  Input DMA request line selected
      DMAREQ_ID      : C5CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : STM32H753.UInt3 := 16#0#;
      --  Interrupt enable at synchronization event overrun
      SOIE           : Boolean := False;
      --  Event generation enable/disable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : STM32H753.UInt6 := 16#0#;
      --  Synchronous operating mode enable/disable
      SE             : Boolean := False;
      --  Synchronization event type selector Defines the synchronization event
      --  on the selected synchronization input:
      SPOL           : C5CR_SPOL_Field := 16#0#;
      --  Number of DMA requests to forward Defines the number of DMA requests
      --  forwarded before output event is generated. In synchronous mode, it
      --  also defines the number of DMA requests to forward after a
      --  synchronization event, then stop forwarding. The actual number of DMA
      --  requests forwarded is NBREQ+1. Note: This field can only be written
      --  when both SE and EGE bits are reset.
      NBREQ          : C5CR_NBREQ_Field := 16#0#;
      --  Synchronization input selected
      SYNC_ID        : C5CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : STM32H753.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C5CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C6CR_DMAREQ_ID_Field is STM32H753.UInt5;
   subtype C6CR_SPOL_Field is STM32H753.UInt2;
   subtype C6CR_NBREQ_Field is STM32H753.UInt5;
   subtype C6CR_SYNC_ID_Field is STM32H753.UInt5;

   --  DMAMux - DMA request line multiplexer channel x control register
   type C6CR_Register is record
      --  Input DMA request line selected
      DMAREQ_ID      : C6CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : STM32H753.UInt3 := 16#0#;
      --  Interrupt enable at synchronization event overrun
      SOIE           : Boolean := False;
      --  Event generation enable/disable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : STM32H753.UInt6 := 16#0#;
      --  Synchronous operating mode enable/disable
      SE             : Boolean := False;
      --  Synchronization event type selector Defines the synchronization event
      --  on the selected synchronization input:
      SPOL           : C6CR_SPOL_Field := 16#0#;
      --  Number of DMA requests to forward Defines the number of DMA requests
      --  forwarded before output event is generated. In synchronous mode, it
      --  also defines the number of DMA requests to forward after a
      --  synchronization event, then stop forwarding. The actual number of DMA
      --  requests forwarded is NBREQ+1. Note: This field can only be written
      --  when both SE and EGE bits are reset.
      NBREQ          : C6CR_NBREQ_Field := 16#0#;
      --  Synchronization input selected
      SYNC_ID        : C6CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : STM32H753.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C6CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   subtype C7CR_DMAREQ_ID_Field is STM32H753.UInt5;
   subtype C7CR_SPOL_Field is STM32H753.UInt2;
   subtype C7CR_NBREQ_Field is STM32H753.UInt5;
   subtype C7CR_SYNC_ID_Field is STM32H753.UInt5;

   --  DMAMux - DMA request line multiplexer channel x control register
   type C7CR_Register is record
      --  Input DMA request line selected
      DMAREQ_ID      : C7CR_DMAREQ_ID_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : STM32H753.UInt3 := 16#0#;
      --  Interrupt enable at synchronization event overrun
      SOIE           : Boolean := False;
      --  Event generation enable/disable
      EGE            : Boolean := False;
      --  unspecified
      Reserved_10_15 : STM32H753.UInt6 := 16#0#;
      --  Synchronous operating mode enable/disable
      SE             : Boolean := False;
      --  Synchronization event type selector Defines the synchronization event
      --  on the selected synchronization input:
      SPOL           : C7CR_SPOL_Field := 16#0#;
      --  Number of DMA requests to forward Defines the number of DMA requests
      --  forwarded before output event is generated. In synchronous mode, it
      --  also defines the number of DMA requests to forward after a
      --  synchronization event, then stop forwarding. The actual number of DMA
      --  requests forwarded is NBREQ+1. Note: This field can only be written
      --  when both SE and EGE bits are reset.
      NBREQ          : C7CR_NBREQ_Field := 16#0#;
      --  Synchronization input selected
      SYNC_ID        : C7CR_SYNC_ID_Field := 16#0#;
      --  unspecified
      Reserved_29_31 : STM32H753.UInt3 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for C7CR_Register use record
      DMAREQ_ID      at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      SOIE           at 0 range 8 .. 8;
      EGE            at 0 range 9 .. 9;
      Reserved_10_15 at 0 range 10 .. 15;
      SE             at 0 range 16 .. 16;
      SPOL           at 0 range 17 .. 18;
      NBREQ          at 0 range 19 .. 23;
      SYNC_ID        at 0 range 24 .. 28;
      Reserved_29_31 at 0 range 29 .. 31;
   end record;

   --  CSR_SOF array
   type CSR_SOF_Field_Array is array (0 .. 7) of Boolean
     with Component_Size => 1, Size => 8;

   --  Type definition for CSR_SOF
   type CSR_SOF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  SOF as a value
            Val : STM32H753.Byte;
         when True =>
            --  SOF as an array
            Arr : CSR_SOF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 8;

   for CSR_SOF_Field use record
      Val at 0 range 0 .. 7;
      Arr at 0 range 0 .. 7;
   end record;

   type CSR_Register is record
      --  Read-only. Synchronization overrun event flag
      SOF           : CSR_SOF_Field;
      --  unspecified
      Reserved_8_31 : STM32H753.UInt24;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CSR_Register use record
      SOF           at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  CFR_CSOF array
   type CFR_CSOF_Field_Array is array (0 .. 7) of Boolean
     with Component_Size => 1, Size => 8;

   --  Type definition for CFR_CSOF
   type CFR_CSOF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  CSOF as a value
            Val : STM32H753.Byte;
         when True =>
            --  CSOF as an array
            Arr : CFR_CSOF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 8;

   for CFR_CSOF_Field use record
      Val at 0 range 0 .. 7;
      Arr at 0 range 0 .. 7;
   end record;

   type CFR_Register is record
      --  Write-only. Clear synchronization overrun event flag Writing 1 in
      --  each bit clears the corresponding overrun flag SOFx in the DMAMUX_CSR
      --  register.
      CSOF          : CFR_CSOF_Field := (As_Array => False, Val => 16#0#);
      --  unspecified
      Reserved_8_31 : STM32H753.UInt24 := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for CFR_Register use record
      CSOF          at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   subtype RG0CR_SIG_ID_Field is STM32H753.UInt5;
   subtype RG0CR_GPOL_Field is STM32H753.UInt2;
   subtype RG0CR_GNBREQ_Field is STM32H753.UInt5;

   --  DMAMux - DMA request generator channel x control register
   type RG0CR_Register is record
      --  DMA request trigger input selected
      SIG_ID         : RG0CR_SIG_ID_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : STM32H753.UInt3 := 16#0#;
      --  Interrupt enable at trigger event overrun
      OIE            : Boolean := False;
      --  unspecified
      Reserved_9_15  : STM32H753.UInt7 := 16#0#;
      --  DMA request generator channel enable/disable
      GE             : Boolean := False;
      --  DMA request generator trigger event type selection Defines the
      --  trigger event on the selected DMA request trigger input
      GPOL           : RG0CR_GPOL_Field := 16#0#;
      --  Number of DMA requests to generate Defines the number of DMA requests
      --  generated after a trigger event, then stop generating. The actual
      --  number of generated DMA requests is GNBREQ+1. Note: This field can
      --  only be written when GE bit is reset.
      GNBREQ         : RG0CR_GNBREQ_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : STM32H753.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RG0CR_Register use record
      SIG_ID         at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      OIE            at 0 range 8 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      GE             at 0 range 16 .. 16;
      GPOL           at 0 range 17 .. 18;
      GNBREQ         at 0 range 19 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype RG1CR_SIG_ID_Field is STM32H753.UInt5;
   subtype RG1CR_GPOL_Field is STM32H753.UInt2;
   subtype RG1CR_GNBREQ_Field is STM32H753.UInt5;

   --  DMAMux - DMA request generator channel x control register
   type RG1CR_Register is record
      --  DMA request trigger input selected
      SIG_ID         : RG1CR_SIG_ID_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : STM32H753.UInt3 := 16#0#;
      --  Interrupt enable at trigger event overrun
      OIE            : Boolean := False;
      --  unspecified
      Reserved_9_15  : STM32H753.UInt7 := 16#0#;
      --  DMA request generator channel enable/disable
      GE             : Boolean := False;
      --  DMA request generator trigger event type selection Defines the
      --  trigger event on the selected DMA request trigger input
      GPOL           : RG1CR_GPOL_Field := 16#0#;
      --  Number of DMA requests to generate Defines the number of DMA requests
      --  generated after a trigger event, then stop generating. The actual
      --  number of generated DMA requests is GNBREQ+1. Note: This field can
      --  only be written when GE bit is reset.
      GNBREQ         : RG1CR_GNBREQ_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : STM32H753.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RG1CR_Register use record
      SIG_ID         at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      OIE            at 0 range 8 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      GE             at 0 range 16 .. 16;
      GPOL           at 0 range 17 .. 18;
      GNBREQ         at 0 range 19 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype RG2CR_SIG_ID_Field is STM32H753.UInt5;
   subtype RG2CR_GPOL_Field is STM32H753.UInt2;
   subtype RG2CR_GNBREQ_Field is STM32H753.UInt5;

   --  DMAMux - DMA request generator channel x control register
   type RG2CR_Register is record
      --  DMA request trigger input selected
      SIG_ID         : RG2CR_SIG_ID_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : STM32H753.UInt3 := 16#0#;
      --  Interrupt enable at trigger event overrun
      OIE            : Boolean := False;
      --  unspecified
      Reserved_9_15  : STM32H753.UInt7 := 16#0#;
      --  DMA request generator channel enable/disable
      GE             : Boolean := False;
      --  DMA request generator trigger event type selection Defines the
      --  trigger event on the selected DMA request trigger input
      GPOL           : RG2CR_GPOL_Field := 16#0#;
      --  Number of DMA requests to generate Defines the number of DMA requests
      --  generated after a trigger event, then stop generating. The actual
      --  number of generated DMA requests is GNBREQ+1. Note: This field can
      --  only be written when GE bit is reset.
      GNBREQ         : RG2CR_GNBREQ_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : STM32H753.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RG2CR_Register use record
      SIG_ID         at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      OIE            at 0 range 8 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      GE             at 0 range 16 .. 16;
      GPOL           at 0 range 17 .. 18;
      GNBREQ         at 0 range 19 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype RG3CR_SIG_ID_Field is STM32H753.UInt5;
   subtype RG3CR_GPOL_Field is STM32H753.UInt2;
   subtype RG3CR_GNBREQ_Field is STM32H753.UInt5;

   --  DMAMux - DMA request generator channel x control register
   type RG3CR_Register is record
      --  DMA request trigger input selected
      SIG_ID         : RG3CR_SIG_ID_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : STM32H753.UInt3 := 16#0#;
      --  Interrupt enable at trigger event overrun
      OIE            : Boolean := False;
      --  unspecified
      Reserved_9_15  : STM32H753.UInt7 := 16#0#;
      --  DMA request generator channel enable/disable
      GE             : Boolean := False;
      --  DMA request generator trigger event type selection Defines the
      --  trigger event on the selected DMA request trigger input
      GPOL           : RG3CR_GPOL_Field := 16#0#;
      --  Number of DMA requests to generate Defines the number of DMA requests
      --  generated after a trigger event, then stop generating. The actual
      --  number of generated DMA requests is GNBREQ+1. Note: This field can
      --  only be written when GE bit is reset.
      GNBREQ         : RG3CR_GNBREQ_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : STM32H753.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RG3CR_Register use record
      SIG_ID         at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      OIE            at 0 range 8 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      GE             at 0 range 16 .. 16;
      GPOL           at 0 range 17 .. 18;
      GNBREQ         at 0 range 19 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype RG4CR_SIG_ID_Field is STM32H753.UInt5;
   subtype RG4CR_GPOL_Field is STM32H753.UInt2;
   subtype RG4CR_GNBREQ_Field is STM32H753.UInt5;

   --  DMAMux - DMA request generator channel x control register
   type RG4CR_Register is record
      --  DMA request trigger input selected
      SIG_ID         : RG4CR_SIG_ID_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : STM32H753.UInt3 := 16#0#;
      --  Interrupt enable at trigger event overrun
      OIE            : Boolean := False;
      --  unspecified
      Reserved_9_15  : STM32H753.UInt7 := 16#0#;
      --  DMA request generator channel enable/disable
      GE             : Boolean := False;
      --  DMA request generator trigger event type selection Defines the
      --  trigger event on the selected DMA request trigger input
      GPOL           : RG4CR_GPOL_Field := 16#0#;
      --  Number of DMA requests to generate Defines the number of DMA requests
      --  generated after a trigger event, then stop generating. The actual
      --  number of generated DMA requests is GNBREQ+1. Note: This field can
      --  only be written when GE bit is reset.
      GNBREQ         : RG4CR_GNBREQ_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : STM32H753.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RG4CR_Register use record
      SIG_ID         at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      OIE            at 0 range 8 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      GE             at 0 range 16 .. 16;
      GPOL           at 0 range 17 .. 18;
      GNBREQ         at 0 range 19 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype RG5CR_SIG_ID_Field is STM32H753.UInt5;
   subtype RG5CR_GPOL_Field is STM32H753.UInt2;
   subtype RG5CR_GNBREQ_Field is STM32H753.UInt5;

   --  DMAMux - DMA request generator channel x control register
   type RG5CR_Register is record
      --  DMA request trigger input selected
      SIG_ID         : RG5CR_SIG_ID_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : STM32H753.UInt3 := 16#0#;
      --  Interrupt enable at trigger event overrun
      OIE            : Boolean := False;
      --  unspecified
      Reserved_9_15  : STM32H753.UInt7 := 16#0#;
      --  DMA request generator channel enable/disable
      GE             : Boolean := False;
      --  DMA request generator trigger event type selection Defines the
      --  trigger event on the selected DMA request trigger input
      GPOL           : RG5CR_GPOL_Field := 16#0#;
      --  Number of DMA requests to generate Defines the number of DMA requests
      --  generated after a trigger event, then stop generating. The actual
      --  number of generated DMA requests is GNBREQ+1. Note: This field can
      --  only be written when GE bit is reset.
      GNBREQ         : RG5CR_GNBREQ_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : STM32H753.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RG5CR_Register use record
      SIG_ID         at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      OIE            at 0 range 8 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      GE             at 0 range 16 .. 16;
      GPOL           at 0 range 17 .. 18;
      GNBREQ         at 0 range 19 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype RG6CR_SIG_ID_Field is STM32H753.UInt5;
   subtype RG6CR_GPOL_Field is STM32H753.UInt2;
   subtype RG6CR_GNBREQ_Field is STM32H753.UInt5;

   --  DMAMux - DMA request generator channel x control register
   type RG6CR_Register is record
      --  DMA request trigger input selected
      SIG_ID         : RG6CR_SIG_ID_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : STM32H753.UInt3 := 16#0#;
      --  Interrupt enable at trigger event overrun
      OIE            : Boolean := False;
      --  unspecified
      Reserved_9_15  : STM32H753.UInt7 := 16#0#;
      --  DMA request generator channel enable/disable
      GE             : Boolean := False;
      --  DMA request generator trigger event type selection Defines the
      --  trigger event on the selected DMA request trigger input
      GPOL           : RG6CR_GPOL_Field := 16#0#;
      --  Number of DMA requests to generate Defines the number of DMA requests
      --  generated after a trigger event, then stop generating. The actual
      --  number of generated DMA requests is GNBREQ+1. Note: This field can
      --  only be written when GE bit is reset.
      GNBREQ         : RG6CR_GNBREQ_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : STM32H753.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RG6CR_Register use record
      SIG_ID         at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      OIE            at 0 range 8 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      GE             at 0 range 16 .. 16;
      GPOL           at 0 range 17 .. 18;
      GNBREQ         at 0 range 19 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   subtype RG7CR_SIG_ID_Field is STM32H753.UInt5;
   subtype RG7CR_GPOL_Field is STM32H753.UInt2;
   subtype RG7CR_GNBREQ_Field is STM32H753.UInt5;

   --  DMAMux - DMA request generator channel x control register
   type RG7CR_Register is record
      --  DMA request trigger input selected
      SIG_ID         : RG7CR_SIG_ID_Field := 16#0#;
      --  unspecified
      Reserved_5_7   : STM32H753.UInt3 := 16#0#;
      --  Interrupt enable at trigger event overrun
      OIE            : Boolean := False;
      --  unspecified
      Reserved_9_15  : STM32H753.UInt7 := 16#0#;
      --  DMA request generator channel enable/disable
      GE             : Boolean := False;
      --  DMA request generator trigger event type selection Defines the
      --  trigger event on the selected DMA request trigger input
      GPOL           : RG7CR_GPOL_Field := 16#0#;
      --  Number of DMA requests to generate Defines the number of DMA requests
      --  generated after a trigger event, then stop generating. The actual
      --  number of generated DMA requests is GNBREQ+1. Note: This field can
      --  only be written when GE bit is reset.
      GNBREQ         : RG7CR_GNBREQ_Field := 16#0#;
      --  unspecified
      Reserved_24_31 : STM32H753.Byte := 16#0#;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RG7CR_Register use record
      SIG_ID         at 0 range 0 .. 4;
      Reserved_5_7   at 0 range 5 .. 7;
      OIE            at 0 range 8 .. 8;
      Reserved_9_15  at 0 range 9 .. 15;
      GE             at 0 range 16 .. 16;
      GPOL           at 0 range 17 .. 18;
      GNBREQ         at 0 range 19 .. 23;
      Reserved_24_31 at 0 range 24 .. 31;
   end record;

   --  RGSR_OF array
   type RGSR_OF_Field_Array is array (0 .. 7) of Boolean
     with Component_Size => 1, Size => 8;

   --  Type definition for RGSR_OF
   type RGSR_OF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  OF as a value
            Val : STM32H753.Byte;
         when True =>
            --  OF as an array
            Arr : RGSR_OF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 8;

   for RGSR_OF_Field use record
      Val at 0 range 0 .. 7;
      Arr at 0 range 0 .. 7;
   end record;

   type RGSR_Register is record
      --  Read-only. Trigger overrun event flag
      OF_k          : RGSR_OF_Field;
      --  unspecified
      Reserved_8_31 : STM32H753.UInt24;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RGSR_Register use record
      OF_k          at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   --  RGCFR_COF array
   type RGCFR_COF_Field_Array is array (0 .. 7) of Boolean
     with Component_Size => 1, Size => 8;

   --  Type definition for RGCFR_COF
   type RGCFR_COF_Field
     (As_Array : Boolean := False)
   is record
      case As_Array is
         when False =>
            --  COF as a value
            Val : STM32H753.Byte;
         when True =>
            --  COF as an array
            Arr : RGCFR_COF_Field_Array;
      end case;
   end record
     with Unchecked_Union, Size => 8;

   for RGCFR_COF_Field use record
      Val at 0 range 0 .. 7;
      Arr at 0 range 0 .. 7;
   end record;

   type RGCFR_Register is record
      --  Read-only. Clear trigger overrun event flag
      COF           : RGCFR_COF_Field;
      --  unspecified
      Reserved_8_31 : STM32H753.UInt24;
   end record
     with Volatile_Full_Access, Object_Size => 32,
          Bit_Order => System.Low_Order_First;

   for RGCFR_Register use record
      COF           at 0 range 0 .. 7;
      Reserved_8_31 at 0 range 8 .. 31;
   end record;

   -----------------
   -- Peripherals --
   -----------------

   --  DMAMUX
   type DMAMUX1_Peripheral is record
      DMAMUX_C0CR  : aliased DMAMUX_C0CR_Register;
      DMAMUX_C1CR  : aliased DMAMUX_C1CR_Register;
      DMAMUX_C2CR  : aliased DMAMUX_C2CR_Register;
      DMAMUX_C3CR  : aliased DMAMUX_C3CR_Register;
      DMAMUX_C4CR  : aliased DMAMUX_C4CR_Register;
      DMAMUX_C5CR  : aliased DMAMUX_C5CR_Register;
      DMAMUX_C6CR  : aliased DMAMUX_C6CR_Register;
      DMAMUX_C7CR  : aliased DMAMUX_C7CR_Register;
      DMAMUX_C8CR  : aliased DMAMUX_C8CR_Register;
      DMAMUX_C9CR  : aliased DMAMUX_C9CR_Register;
      DMAMUX_C10CR : aliased DMAMUX_C10CR_Register;
      DMAMUX_C11CR : aliased DMAMUX_C11CR_Register;
      DMAMUX_C12CR : aliased DMAMUX_C12CR_Register;
      DMAMUX_C13CR : aliased DMAMUX_C13CR_Register;
      DMAMUX_C14CR : aliased DMAMUX_C14CR_Register;
      DMAMUX_C15CR : aliased DMAMUX_C15CR_Register;
      DMAMUX_CSR   : aliased DMAMUX_CSR_Register;
      DMAMUX_CFR   : aliased DMAMUX_CFR_Register;
      DMAMUX_RG0CR : aliased DMAMUX_RG0CR_Register;
      DMAMUX_RG1CR : aliased DMAMUX_RG1CR_Register;
      DMAMUX_RG2CR : aliased DMAMUX_RG2CR_Register;
      DMAMUX_RG3CR : aliased DMAMUX_RG3CR_Register;
      DMAMUX_RG4CR : aliased DMAMUX_RG4CR_Register;
      DMAMUX_RG5CR : aliased DMAMUX_RG5CR_Register;
      DMAMUX_RG6CR : aliased DMAMUX_RG6CR_Register;
      DMAMUX_RG7CR : aliased DMAMUX_RG7CR_Register;
      DMAMUX_RGSR  : aliased DMAMUX_RGSR_Register;
      DMAMUX_RGCFR : aliased DMAMUX_RGCFR_Register;
   end record
     with Volatile;

   for DMAMUX1_Peripheral use record
      DMAMUX_C0CR  at 16#0# range 0 .. 31;
      DMAMUX_C1CR  at 16#4# range 0 .. 31;
      DMAMUX_C2CR  at 16#8# range 0 .. 31;
      DMAMUX_C3CR  at 16#C# range 0 .. 31;
      DMAMUX_C4CR  at 16#10# range 0 .. 31;
      DMAMUX_C5CR  at 16#14# range 0 .. 31;
      DMAMUX_C6CR  at 16#18# range 0 .. 31;
      DMAMUX_C7CR  at 16#1C# range 0 .. 31;
      DMAMUX_C8CR  at 16#20# range 0 .. 31;
      DMAMUX_C9CR  at 16#24# range 0 .. 31;
      DMAMUX_C10CR at 16#28# range 0 .. 31;
      DMAMUX_C11CR at 16#2C# range 0 .. 31;
      DMAMUX_C12CR at 16#30# range 0 .. 31;
      DMAMUX_C13CR at 16#34# range 0 .. 31;
      DMAMUX_C14CR at 16#38# range 0 .. 31;
      DMAMUX_C15CR at 16#3C# range 0 .. 31;
      DMAMUX_CSR   at 16#80# range 0 .. 31;
      DMAMUX_CFR   at 16#84# range 0 .. 31;
      DMAMUX_RG0CR at 16#100# range 0 .. 31;
      DMAMUX_RG1CR at 16#104# range 0 .. 31;
      DMAMUX_RG2CR at 16#108# range 0 .. 31;
      DMAMUX_RG3CR at 16#10C# range 0 .. 31;
      DMAMUX_RG4CR at 16#110# range 0 .. 31;
      DMAMUX_RG5CR at 16#114# range 0 .. 31;
      DMAMUX_RG6CR at 16#118# range 0 .. 31;
      DMAMUX_RG7CR at 16#11C# range 0 .. 31;
      DMAMUX_RGSR  at 16#140# range 0 .. 31;
      DMAMUX_RGCFR at 16#144# range 0 .. 31;
   end record;

   --  DMAMUX
   DMAMUX1_Periph : aliased DMAMUX1_Peripheral
     with Import, Address => DMAMUX1_Base;

   --  DMAMUX
   type DMAMUX2_Peripheral is record
      --  DMAMux - DMA request line multiplexer channel x control register
      C0CR  : aliased C0CR_Register;
      --  DMAMux - DMA request line multiplexer channel x control register
      C1CR  : aliased C1CR_Register;
      --  DMAMux - DMA request line multiplexer channel x control register
      C2CR  : aliased C2CR_Register;
      --  DMAMux - DMA request line multiplexer channel x control register
      C3CR  : aliased C3CR_Register;
      --  DMAMux - DMA request line multiplexer channel x control register
      C4CR  : aliased C4CR_Register;
      --  DMAMux - DMA request line multiplexer channel x control register
      C5CR  : aliased C5CR_Register;
      --  DMAMux - DMA request line multiplexer channel x control register
      C6CR  : aliased C6CR_Register;
      --  DMAMux - DMA request line multiplexer channel x control register
      C7CR  : aliased C7CR_Register;
      CSR   : aliased CSR_Register;
      CFR   : aliased CFR_Register;
      --  DMAMux - DMA request generator channel x control register
      RG0CR : aliased RG0CR_Register;
      --  DMAMux - DMA request generator channel x control register
      RG1CR : aliased RG1CR_Register;
      --  DMAMux - DMA request generator channel x control register
      RG2CR : aliased RG2CR_Register;
      --  DMAMux - DMA request generator channel x control register
      RG3CR : aliased RG3CR_Register;
      --  DMAMux - DMA request generator channel x control register
      RG4CR : aliased RG4CR_Register;
      --  DMAMux - DMA request generator channel x control register
      RG5CR : aliased RG5CR_Register;
      --  DMAMux - DMA request generator channel x control register
      RG6CR : aliased RG6CR_Register;
      --  DMAMux - DMA request generator channel x control register
      RG7CR : aliased RG7CR_Register;
      RGSR  : aliased RGSR_Register;
      RGCFR : aliased RGCFR_Register;
   end record
     with Volatile;

   for DMAMUX2_Peripheral use record
      C0CR  at 16#0# range 0 .. 31;
      C1CR  at 16#4# range 0 .. 31;
      C2CR  at 16#8# range 0 .. 31;
      C3CR  at 16#C# range 0 .. 31;
      C4CR  at 16#10# range 0 .. 31;
      C5CR  at 16#14# range 0 .. 31;
      C6CR  at 16#18# range 0 .. 31;
      C7CR  at 16#1C# range 0 .. 31;
      CSR   at 16#80# range 0 .. 31;
      CFR   at 16#84# range 0 .. 31;
      RG0CR at 16#100# range 0 .. 31;
      RG1CR at 16#104# range 0 .. 31;
      RG2CR at 16#108# range 0 .. 31;
      RG3CR at 16#10C# range 0 .. 31;
      RG4CR at 16#110# range 0 .. 31;
      RG5CR at 16#114# range 0 .. 31;
      RG6CR at 16#118# range 0 .. 31;
      RG7CR at 16#11C# range 0 .. 31;
      RGSR  at 16#140# range 0 .. 31;
      RGCFR at 16#144# range 0 .. 31;
   end record;

   --  DMAMUX
   DMAMUX2_Periph : aliased DMAMUX2_Peripheral
     with Import, Address => DMAMUX2_Base;

end STM32H753.DMAMUX;
