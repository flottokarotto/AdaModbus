/**
 * startup.S - Minimal STM32H753 Vector Table
 *
 * Only provides the vector table and Reset_Handler.
 * Delegates to GNAT runtime's _start for actual initialization.
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
    .word Default_Handler       /* HardFault */
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

/* Default Handler */
    .section .text.Default_Handler
    .weak Default_Handler
    .type Default_Handler, %function
Default_Handler:
    b .
    .size Default_Handler, .-Default_Handler

/* SysTick Handler - calls Ada handler */
    .section .text.SysTick_Handler
    .weak SysTick_Handler
    .type SysTick_Handler, %function
SysTick_Handler:
    push {lr}
    bl stm32h7_hal__systick_handler
    pop {pc}
    .size SysTick_Handler, .-SysTick_Handler

    .end
