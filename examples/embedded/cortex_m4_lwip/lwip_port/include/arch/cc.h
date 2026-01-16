/*
 * LwIP Architecture Configuration for Cortex-M
 * Copyright (c) 2026 Florian Fischer
 * SPDX-License-Identifier: MIT
 */

#ifndef CC_H
#define CC_H

#include <stdint.h>
#include <stdio.h>

/* ========== Types ========== */
typedef uint8_t   u8_t;
typedef int8_t    s8_t;
typedef uint16_t  u16_t;
typedef int16_t   s16_t;
typedef uint32_t  u32_t;
typedef int32_t   s32_t;

typedef uintptr_t mem_ptr_t;

/* ========== Byte Order ========== */
/* ARM Cortex-M ist Little-Endian */
#ifndef BYTE_ORDER
#define BYTE_ORDER LITTLE_ENDIAN
#endif

/* ========== Structure Packing ========== */
#define PACK_STRUCT_FIELD(x)    x
#define PACK_STRUCT_STRUCT      __attribute__((packed))
#define PACK_STRUCT_BEGIN
#define PACK_STRUCT_END

/* ========== Printf Formatters ========== */
#define X8_F   "02x"
#define U16_F  "u"
#define S16_F  "d"
#define X16_F  "x"
#define U32_F  "u"
#define S32_F  "d"
#define X32_F  "x"
#define SZT_F  "u"

/* ========== Diagnostics ========== */
#ifndef LWIP_PLATFORM_DIAG
#define LWIP_PLATFORM_DIAG(x)   do { } while(0)
#endif

#ifndef LWIP_PLATFORM_ASSERT
#define LWIP_PLATFORM_ASSERT(x) do { while(1); } while(0)
#endif

/* ========== Random Number ========== */
#ifndef LWIP_RAND
#define LWIP_RAND() ((u32_t)rand())
#endif

#endif /* CC_H */
