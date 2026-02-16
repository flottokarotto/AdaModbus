/**
 * cc.h - Compiler/Platform abstraction for LwIP on ARM Cortex-M7
 * Copyright (c) 2026 Florian Fischer
 * SPDX-License-Identifier: MIT
 *
 * Self-written, based on LwIP's doc/sys_arch.txt requirements.
 * Defines the platform types (u8_t, u16_t, etc.), byte order,
 * struct packing macros, and diagnostic hooks that LwIP needs
 * from each target platform.  Standard for any LwIP port.
 */

#ifndef CC_H
#define CC_H

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

/* Types */
typedef uint8_t     u8_t;
typedef int8_t      s8_t;
typedef uint16_t    u16_t;
typedef int16_t     s16_t;
typedef uint32_t    u32_t;
typedef int32_t     s32_t;
typedef uintptr_t   mem_ptr_t;

/* Printf formatters */
#define U16_F       "hu"
#define S16_F       "hd"
#define X16_F       "hx"
#define U32_F       "u"
#define S32_F       "d"
#define X32_F       "x"
#define SZT_F       "zu"

/* Compiler hints */
#define PACK_STRUCT_FIELD(x)    x
#define PACK_STRUCT_STRUCT      __attribute__((packed))
#define PACK_STRUCT_BEGIN
#define PACK_STRUCT_END

/* Byte order - STM32 is little endian */
#ifndef BYTE_ORDER
#define BYTE_ORDER LITTLE_ENDIAN
#endif

/* Platform specific diagnostics */
#define LWIP_PLATFORM_DIAG(x)   do { printf x; } while(0)
#define LWIP_PLATFORM_ASSERT(x) do { printf("Assert: %s\n", x); while(1); } while(0)

/* Random number generator */
#define LWIP_RAND()             ((u32_t)rand())

#endif /* CC_H */
