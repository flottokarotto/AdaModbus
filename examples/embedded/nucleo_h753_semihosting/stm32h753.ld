/*
 * STM32H753ZI Linker Script for NUCLEO-H753ZI Board
 *
 * Self-written, guided by:
 *   STM32CubeH7/Projects/STM32H743I-EVAL/Applications/LwIP/
 *   LwIP_TCP_Echo_Server/STM32H743I_EVAL/STM32H743XIHx_FLASH.ld
 *   and the STM32H753ZI reference manual (RM0433) memory map.
 *
 * Key design decisions:
 *   - .data in DTCM (0x20000000): fastest CPU access (zero wait state)
 *   - .bss in SRAM1 (0x24000000): DMA-accessible for ETH and LwIP pools
 *     (DTCM is CPU-only, NOT accessible by Ethernet DMA controller!)
 *   - ETH DMA sections in SRAM2 (0x30000000): required by HAL ETH driver
 *     (.RxDescripSection, .TxDescripSection, .Rx_PoolSection)
 *   - LwIP heap at 0x30004000 (SRAM2, set via LWIP_RAM_HEAP_POINTER)
 *   - Stack 16 KB, Heap 64 KB in DTCM
 *
 * Memory Map:
 *   Flash: 2 MB (0x08000000 - 0x08200000)
 *   DTCM:  128 KB @ 0x20000000 (fast, CPU-only, no DMA)
 *   SRAM1: 256 KB @ 0x24000000 (AXI SRAM, DMA-accessible)
 *   SRAM2: 128 KB @ 0x30000000 (D2 SRAM, ETH DMA-accessible)
 */

MEMORY
{
  FLASH (rx)      : ORIGIN = 0x08000000, LENGTH = 2M
  DTCM  (rwx)     : ORIGIN = 0x20000000, LENGTH = 128K
  SRAM1 (rwx)     : ORIGIN = 0x24000000, LENGTH = 256K
  SRAM2 (rwx)     : ORIGIN = 0x30000000, LENGTH = 128K
}

/* Stack and heap sizes */
__stack_size = DEFINED(__stack_size) ? __stack_size : 16K;
__heap_size = DEFINED(__heap_size) ? __heap_size : 64K;

ENTRY(Reset_Handler)

SECTIONS
{
  /* Vector table at start of flash */
  .isr_vector :
  {
    . = ALIGN(4);
    KEEP(*(.isr_vector))
    . = ALIGN(4);
  } >FLASH

  /* Program code */
  .text :
  {
    . = ALIGN(4);
    *(.text)
    *(.text*)
    *(.rodata)
    *(.rodata*)
    . = ALIGN(4);
    _etext = .;
  } >FLASH

  /* ARM exception handling */
  .ARM.extab :
  {
    *(.ARM.extab*)
  } >FLASH

  .ARM.exidx :
  {
    __exidx_start = .;
    *(.ARM.exidx*)
    __exidx_end = .;
  } >FLASH

  /* Used for data initialization */
  _sidata = .;

  /* Initialized data in DTCM (fast access) */
  .data : AT (_sidata)
  {
    . = ALIGN(4);
    _sdata = .;
    *(.data)
    *(.data*)
    . = ALIGN(4);
    _edata = .;
  } >DTCM

  /* Uninitialized data in SRAM1 (DMA-accessible).
   * LwIP memory pools and general .bss end up here.
   * DTCM (0x20000000) is NOT accessible by Ethernet DMA. */
  .bss (NOLOAD) :
  {
    . = ALIGN(4);
    _sbss = .;
    *(.bss)
    *(.bss*)
    *(COMMON)
    . = ALIGN(4);
    _ebss = .;
  } >SRAM1

  /* Heap in DTCM */
  .heap :
  {
    . = ALIGN(8);
    __heap_start = .;
    . = . + __heap_size;
    __heap_end = .;
    . = ALIGN(8);
  } >DTCM

  /* Stack at end of DTCM */
  .stack :
  {
    . = ALIGN(8);
    __stack_end = .;
    . = . + __stack_size;
    __stack_start = .;
    . = ALIGN(8);
  } >DTCM

  /* Ethernet DMA descriptors in SRAM2 (D2 SRAM, 0x30000000).
   * Must be in DMA-accessible memory.
   * Section names match STM32CubeH7 LwIP examples. */
  .RxDescripSection (NOLOAD) :
  {
    . = ALIGN(4);
    *(.RxDescripSection)
    . = ALIGN(4);
  } >SRAM2

  .TxDescripSection (NOLOAD) :
  {
    . = ALIGN(4);
    *(.TxDescripSection)
    . = ALIGN(4);
  } >SRAM2

  .Rx_PoolSection (NOLOAD) :
  {
    . = ALIGN(4);
    *(.Rx_PoolSection)
    . = ALIGN(4);
  } >SRAM2

  /* Discard debug info for smaller binary */
  /DISCARD/ :
  {
    *(.comment)
    *(.note*)
  }

  /* Symbols for startup code */
  PROVIDE(_stack = __stack_start);
  PROVIDE(_estack = __stack_start);
}
