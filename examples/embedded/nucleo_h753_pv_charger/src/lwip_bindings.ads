--  LwIP_Bindings - Ada Bindings for LwIP TCP/IP Stack
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Low-level bindings to LwIP for STM32H7.
--  Uses raw API (callback-based) for minimal footprint.

with System;
with Interfaces; use Interfaces;
with Interfaces.C; use Interfaces.C;

package LwIP_Bindings is

   pragma Preelaborate;

   -----------------------
   --  Error Codes      --
   -----------------------

   type Err_T is new Interfaces.C.int;

   ERR_OK      : constant Err_T := 0;
   ERR_MEM     : constant Err_T := -1;
   ERR_BUF     : constant Err_T := -2;
   ERR_TIMEOUT : constant Err_T := -3;
   ERR_RTE     : constant Err_T := -4;
   ERR_INPROGRESS : constant Err_T := -5;
   ERR_VAL     : constant Err_T := -6;
   ERR_WOULDBLOCK : constant Err_T := -7;
   ERR_USE     : constant Err_T := -8;
   ERR_ALREADY : constant Err_T := -9;
   ERR_ISCONN  : constant Err_T := -10;
   ERR_CONN    : constant Err_T := -11;
   ERR_IF      : constant Err_T := -12;
   ERR_ABRT    : constant Err_T := -13;
   ERR_RST     : constant Err_T := -14;
   ERR_CLSD    : constant Err_T := -15;
   ERR_ARG     : constant Err_T := -16;

   -----------------------
   --  IP Address       --
   -----------------------

   type IP4_Addr_T is record
      Addr : Unsigned_32;
   end record
     with Convention => C;

   function Make_IP4_Addr (A, B, C, D : Unsigned_8) return IP4_Addr_T;

   IP_ADDR_ANY : constant IP4_Addr_T := (Addr => 0);

   -----------------------
   --  Network Interface --
   -----------------------

   type Netif_T is limited private;
   type Netif_Ptr is access all Netif_T;

   -----------------------
   --  TCP PCB          --
   -----------------------

   type TCP_PCB_T is limited private;
   type TCP_PCB_Ptr is access all TCP_PCB_T;

   -----------------------
   --  Pbuf (Packet Buffer) --
   -----------------------

   type Pbuf_T is limited private;
   type Pbuf_Ptr is access all Pbuf_T;

   function Pbuf_Get_Payload (P : Pbuf_Ptr) return System.Address;
   function Pbuf_Get_Len (P : Pbuf_Ptr) return Unsigned_16;
   function Pbuf_Get_Tot_Len (P : Pbuf_Ptr) return Unsigned_16;

   -----------------------
   --  LwIP Core        --
   -----------------------

   --  Initialize the lwIP stack
   procedure LwIP_Init
     with Import, Convention => C, External_Name => "lwip_init";

   --  Process timeouts (call periodically from main loop)
   procedure Sys_Check_Timeouts
     with Import, Convention => C, External_Name => "sys_check_timeouts";

   -----------------------
   --  Network Interface --
   -----------------------

   --  Netif init callback type
   type Netif_Init_Fn is access function (N : Netif_Ptr) return Err_T
     with Convention => C;

   --  Netif input function type
   type Netif_Input_Fn is access function (P : Pbuf_Ptr; N : Netif_Ptr) return Err_T
     with Convention => C;

   --  Add network interface (simplified binding)
   procedure Netif_Add
     (Netif_Struct : Netif_Ptr;
      IP_Addr      : IP4_Addr_T;
      Netmask      : IP4_Addr_T;
      Gateway      : IP4_Addr_T)
     with Import, Convention => C, External_Name => "ada_netif_add";

   --  Set default network interface
   procedure Netif_Set_Default (N : Netif_Ptr)
     with Import, Convention => C, External_Name => "netif_set_default";

   --  Bring interface up
   procedure Netif_Set_Up (N : Netif_Ptr)
     with Import, Convention => C, External_Name => "netif_set_up";

   --  Poll for received packets
   procedure Ethernetif_Input (N : Netif_Ptr)
     with Import, Convention => C, External_Name => "ethernetif_input";

   --  Check link status
   function Ethernetif_Link_Status return int
     with Import, Convention => C, External_Name => "ethernetif_link_status";

   -----------------------
   --  TCP Functions    --
   -----------------------

   --  Create new TCP PCB
   function TCP_New return TCP_PCB_Ptr
     with Import, Convention => C, External_Name => "tcp_new";

   --  Connect to remote host
   function TCP_Connect
     (PCB       : TCP_PCB_Ptr;
      IP_Addr   : access IP4_Addr_T;
      Port      : Unsigned_16;
      Connected : System.Address) return Err_T
     with Import, Convention => C, External_Name => "tcp_connect";

   --  Close connection
   function TCP_Close (PCB : TCP_PCB_Ptr) return Err_T
     with Import, Convention => C, External_Name => "tcp_close";

   --  Abort connection (immediate)
   procedure TCP_Abort (PCB : TCP_PCB_Ptr)
     with Import, Convention => C, External_Name => "tcp_abort";

   --  Write data to send buffer
   function TCP_Write
     (PCB    : TCP_PCB_Ptr;
      Data   : System.Address;
      Len    : Unsigned_16;
      Apiflags : Unsigned_8) return Err_T
     with Import, Convention => C, External_Name => "tcp_write";

   --  Trigger sending of data
   function TCP_Output (PCB : TCP_PCB_Ptr) return Err_T
     with Import, Convention => C, External_Name => "tcp_output";

   --  Acknowledge received data
   procedure TCP_Recved (PCB : TCP_PCB_Ptr; Len : Unsigned_16)
     with Import, Convention => C, External_Name => "tcp_recved";

   --  Set callback argument
   procedure TCP_Arg (PCB : TCP_PCB_Ptr; Arg : System.Address)
     with Import, Convention => C, External_Name => "tcp_arg";

   --  Set receive callback
   procedure TCP_Recv (PCB : TCP_PCB_Ptr; Recv : System.Address)
     with Import, Convention => C, External_Name => "tcp_recv";

   --  Set sent callback
   procedure TCP_Sent (PCB : TCP_PCB_Ptr; Sent : System.Address)
     with Import, Convention => C, External_Name => "tcp_sent";

   --  Set error callback
   procedure TCP_Err (PCB : TCP_PCB_Ptr; Err_Fn : System.Address)
     with Import, Convention => C, External_Name => "tcp_err";

   --  Set poll callback
   procedure TCP_Poll (PCB : TCP_PCB_Ptr; Poll_Fn : System.Address; Interval : Unsigned_8)
     with Import, Convention => C, External_Name => "tcp_poll";

   --  TCP write flags
   TCP_WRITE_FLAG_COPY : constant Unsigned_8 := 16#01#;
   TCP_WRITE_FLAG_MORE : constant Unsigned_8 := 16#02#;

   -----------------------
   --  Pbuf Functions   --
   -----------------------

   --  Free pbuf
   function Pbuf_Free (P : Pbuf_Ptr) return Unsigned_8
     with Import, Convention => C, External_Name => "pbuf_free";

   --  Copy data from pbuf chain
   function Pbuf_Copy_Partial
     (P      : Pbuf_Ptr;
      Data   : System.Address;
      Len    : Unsigned_16;
      Offset : Unsigned_16) return Unsigned_16
     with Import, Convention => C, External_Name => "pbuf_copy_partial";

private

   --  Opaque types (actual size defined by LwIP)
   --  Sizes must be >= sizeof(struct netif) and sizeof(struct tcp_pcb)
   --  Generous sizing to account for LwIP configuration variations
   type Netif_Data is array (1 .. 512) of Unsigned_8;
   type Netif_T is record
      Data : Netif_Data;
   end record
     with Convention => C;

   type TCP_PCB_Data is array (1 .. 512) of Unsigned_8;
   type TCP_PCB_T is record
      Data : TCP_PCB_Data;
   end record
     with Convention => C;

   type Pbuf_Data is array (1 .. 32) of Unsigned_8;
   type Pbuf_T is record
      Next    : Pbuf_Ptr;
      Payload : System.Address;
      Tot_Len : Unsigned_16;
      Len     : Unsigned_16;
      Pbuf_Type : Unsigned_8;
      Flags   : Unsigned_8;
      Ref     : Unsigned_16;
   end record
     with Convention => C;

end LwIP_Bindings;
