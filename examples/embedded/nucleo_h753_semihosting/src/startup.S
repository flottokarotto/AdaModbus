/**
 * startup.S - Minimal STM32H753 Vector Table and Boot Code
 *
 * Self-written, based on the pattern from:
 *   examples/embedded/nucleo_h753_pv_charger/src/startup.S
 *   (same hardware, same boot requirements)
 *
 * Does NOT use GNAT's crt0/runtime startup -- we bypass it entirely
 * because the Light runtime's default startup doesn't handle the
 * STM32H7's multi-bank SRAM layout (.bss in SRAM1, .data in DTCM).
 *
 * Reset_Handler performs:
 *   1. Set MSP to _estack (top of DTCM)
 *   2. Enable FPU (Cortex-M7 has hardware FPU, needed by Ada)
 *   3. Copy .data from Flash to DTCM
 *   4. Zero .bss in SRAM1
 *   5. Branch to main (Ada elaboration + application)
 *
 * SysTick_Handler calls both:
 *   - Ada's stm32h7_hal__systick_handler (LED blink, timing)
 *   - ST HAL's HAL_IncTick (HAL_Delay, ETH timeouts)
 *
 * HardFault_Handler turns on red LED (PB14) for visual crash indication.
 */

    .syntax unified
    .cpu cortex-m7
    .thumb

/* Vector Table - must be at 0x08000000 */
    .section .isr_vector, "a", %progbits
    .type g_pfnVectors, %object

g_pfnVectors:
    .word _estack               /* Initial SP */
    .word Reset_Handler         /* Reset */
    .word Default_Handler       /* NMI */
    .word HardFault_Handler      /* HardFault */
    .word Default_Handler       /* MemManage */
    .word Default_Handler       /* BusFault */
    .word Default_Handler       /* UsageFault */
    .word 0, 0, 0, 0            /* Reserved */
    .word Default_Handler       /* SVCall */
    .word Default_Handler       /* Debug */
    .word 0                     /* Reserved */
    .word Default_Handler       /* PendSV */
    .word SysTick_Handler       /* SysTick */
    /* IRQ 0-97 - all default except what we need */
    .rept 98
    .word Default_Handler
    .endr

    .size g_pfnVectors, .-g_pfnVectors

/* Reset Handler */
    .section .text.Reset_Handler
    .weak Reset_Handler
    .type Reset_Handler, %function
Reset_Handler:
    /* Set stack pointer */
    ldr r0, =_estack
    msr msp, r0

    /* Enable FPU (CP10/CP11) */
    ldr r0, =0xE000ED88
    ldr r1, [r0]
    orr r1, r1, #(0xF << 20)
    str r1, [r0]
    dsb
    isb

    /* Copy .data from Flash to RAM */
    ldr r0, =_sdata
    ldr r1, =_edata
    ldr r2, =_sidata
copy_data:
    cmp r0, r1
    bge done_data
    ldr r3, [r2], #4
    str r3, [r0], #4
    b copy_data
done_data:

    /* Zero .bss */
    ldr r0, =_sbss
    ldr r1, =_ebss
    mov r2, #0
zero_bss:
    cmp r0, r1
    bge done_bss
    str r2, [r0], #4
    b zero_bss
done_bss:

    /* Call main (includes Ada elaboration) */
    bl main
    b .
    .size Reset_Handler, .-Reset_Handler

/* Hard Fault Handler - turn on red LED (PB14) */
    .section .text.HardFault_Handler
    .type HardFault_Handler, %function
HardFault_Handler:
    /* Red LED PB14 ON: GPIOB_BSRR (0x58020418) bit 14 */
    ldr r0, =0x58020418
    mov r1, #(1 << 14)
    str r1, [r0]
    b .
    .size HardFault_Handler, .-HardFault_Handler

/* Default Handler */
    .section .text.Default_Handler
    .weak Default_Handler
    .type Default_Handler, %function
Default_Handler:
    b .
    .size Default_Handler, .-Default_Handler

/* SysTick Handler - calls both Ada handler and ST HAL tick */
    .section .text.SysTick_Handler
    .weak SysTick_Handler
    .type SysTick_Handler, %function
SysTick_Handler:
    push {lr}
    bl stm32h7_hal__systick_handler
    bl HAL_IncTick
    pop {pc}
    .size SysTick_Handler, .-SysTick_Handler

    .end
