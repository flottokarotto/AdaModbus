--  LwIP_Bindings - Ada bindings for LwIP initialization
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  Provides Ada interface to LwIP stack initialization and network
--  interface management.
--
--  Usage:
--    1. Call LwIP_Init once at startup
--    2. Call Netif_Add to configure network interface
--    3. Call Netif_Set_Default and Netif_Set_Up
--    4. In main loop, call Sys_Check_Timeouts periodically

with Interfaces; use Interfaces;
with Interfaces.C; use Interfaces.C;
with System;

package LwIP_Bindings is

   -----------------------
   --  IP Address Type  --
   -----------------------

   type IP4_Addr is record
      Addr : Unsigned_32;
   end record
     with Convention => C;

   --  Create IP address from octets
   function Make_IP4_Addr (A, B, C, D : Unsigned_8) return IP4_Addr;

   --  Common addresses
   IP_ADDR_ANY       : constant IP4_Addr := (Addr => 0);
   IP_ADDR_BROADCAST : constant IP4_Addr := (Addr => 16#FFFFFFFF#);

   --------------------------
   --  Network Interface   --
   --------------------------

   --  Opaque netif structure (actual size is larger, this is just a handle)
   type Netif is limited private;
   type Netif_Access is access all Netif;

   --  Netif init function type (called by netif_add)
   type Netif_Init_Fn is access function (N : Netif_Access) return int
     with Convention => C;

   --  Netif input function type (for receiving packets)
   type Netif_Input_Fn is access function
     (P : System.Address; N : Netif_Access) return int
     with Convention => C;

   -----------------------
   --  LwIP Functions   --
   -----------------------

   --  Initialize the lwIP stack
   procedure LwIP_Init
     with Import, Convention => C, External_Name => "lwip_init";

   --  Add a network interface
   function Netif_Add
     (N        : Netif_Access;
      IP_Addr  : access IP4_Addr;
      Netmask  : access IP4_Addr;
      Gateway  : access IP4_Addr;
      State    : System.Address;     --  Driver-specific state
      Init_Fn  : Netif_Init_Fn;
      Input_Fn : Netif_Input_Fn) return Netif_Access
     with Import, Convention => C, External_Name => "netif_add";

   --  Set the default network interface
   procedure Netif_Set_Default (N : Netif_Access)
     with Import, Convention => C, External_Name => "netif_set_default";

   --  Bring interface up
   procedure Netif_Set_Up (N : Netif_Access)
     with Import, Convention => C, External_Name => "netif_set_up";

   --  Bring interface down
   procedure Netif_Set_Down (N : Netif_Access)
     with Import, Convention => C, External_Name => "netif_set_down";

   --  Check if interface is up
   function Netif_Is_Up (N : Netif_Access) return int
     with Import, Convention => C, External_Name => "netif_is_up";

   --  Process timeouts (call periodically from main loop)
   procedure Sys_Check_Timeouts
     with Import, Convention => C, External_Name => "sys_check_timeouts";

   --  Get current time in milliseconds
   function Sys_Now return Unsigned_32
     with Import, Convention => C, External_Name => "sys_now";

   --  Increment system tick (call from timer interrupt)
   procedure Sys_Tick_Increment
     with Import, Convention => C, External_Name => "sys_tick_increment";

   -----------------------
   --  DHCP Functions   --
   -----------------------

   --  Start DHCP client on interface
   function DHCP_Start (N : Netif_Access) return int
     with Import, Convention => C, External_Name => "dhcp_start";

   --  Stop DHCP client
   procedure DHCP_Stop (N : Netif_Access)
     with Import, Convention => C, External_Name => "dhcp_stop";

   --  Check if DHCP has obtained an address
   function DHCP_Supplied_Address (N : Netif_Access) return int
     with Import, Convention => C, External_Name => "dhcp_supplied_address";

   -----------------------
   --  Utility          --
   -----------------------

   --  Convert host to network byte order
   function Htons (N : Unsigned_16) return Unsigned_16
     with Import, Convention => C, External_Name => "lwip_htons";

   function Htonl (N : Unsigned_32) return Unsigned_32
     with Import, Convention => C, External_Name => "lwip_htonl";

   function Ntohs (N : Unsigned_16) return Unsigned_16
     with Import, Convention => C, External_Name => "lwip_ntohs";

   function Ntohl (N : Unsigned_32) return Unsigned_32
     with Import, Convention => C, External_Name => "lwip_ntohl";

private

   --  Netif structure placeholder
   --  Actual LwIP netif is ~140 bytes, we reserve enough space
   type Netif_Data is array (1 .. 160) of Unsigned_8;

   type Netif is record
      Data : Netif_Data;
   end record
     with Convention => C;

end LwIP_Bindings;
