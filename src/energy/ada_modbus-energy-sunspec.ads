--  Ada_Modbus.Energy.SunSpec - SunSpec Modbus Profile
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  SunSpec Alliance defines standardized Modbus registers for:
--  - Solar inverters (Models 101-103)
--  - Battery storage (Models 124, 802)
--  - Smart meters (Models 201-204)
--  - DER controls (Models 701-714)
--
--  Reference: https://sunspec.org/sunspec-modbus-specifications/
--
--  SunSpec devices use a "SunS" identifier at the base address,
--  followed by concatenated model blocks. Each model has a header:
--    - Register 0: Model ID (uint16)
--    - Register 1: Model Length (uint16, excluding header)

with Ada_Modbus.Protocol;

package Ada_Modbus.Energy.SunSpec
  with SPARK_Mode => On
is

   --  Standard SunSpec base addresses (holding registers)
   Default_Base_Address : constant Register_Address := 40_000;
   Alt_Base_Address     : constant Register_Address := 50_000;

   --  SunSpec "SunS" identifier (0x53756E53)
   SunS_ID_High : constant Register_Value := 16#5375#;  --  "Su"
   SunS_ID_Low  : constant Register_Value := 16#6E53#;  --  "nS"

   --  End of model marker
   End_Model_ID : constant Register_Value := 16#FFFF#;

   --  "Not Implemented" markers in SunSpec
   --  Different data types use different markers
   Not_Implemented       : constant Register_Value := 16#FFFF#;  --  uint16
   Not_Implemented_Int16 : constant Register_Value := 16#8000#;  --  int16
   Not_Implemented_Acc32 : constant Register_Value := 16#0000#;  --  acc32 (0)

   --  Common SunSpec Model IDs
   Model_Common         : constant := 1;
   Model_Inverter_1P    : constant := 101;  --  Single phase
   Model_Inverter_SP    : constant := 102;  --  Split phase
   Model_Inverter_3P    : constant := 103;  --  Three phase
   Model_Nameplate      : constant := 120;
   Model_Basic_Settings : constant := 121;
   Model_Measurements   : constant := 122;
   Model_Immed_Controls : constant := 123;  --  Immediate controls
   Model_Storage        : constant := 124;  --  Basic storage
   Model_MPPT           : constant := 160;  --  Multiple MPPT
   Model_Meter_1P       : constant := 201;
   Model_Meter_SP       : constant := 202;
   Model_Meter_3P_WYE   : constant := 203;
   Model_Meter_3P_Delta : constant := 204;

   --  SunSpec data types
   type Model_ID is new Interfaces.Unsigned_16;
   type Model_Length is new Interfaces.Unsigned_16;

   --  Scale factors (sunssf type: -10 to +10)
   type Scale_Factor is range -10 .. 10;

   --  Lookup table for scale factors (avoids floating-point exponentiation)
   Scale_Multipliers : constant array (Scale_Factor) of Float :=
     [-10 => 1.0E-10,
      -9  => 1.0E-9,
      -8  => 1.0E-8,
      -7  => 1.0E-7,
      -6  => 1.0E-6,
      -5  => 1.0E-5,
      -4  => 1.0E-4,
      -3  => 1.0E-3,
      -2  => 1.0E-2,
      -1  => 1.0E-1,
       0  => 1.0,
       1  => 1.0E1,
       2  => 1.0E2,
       3  => 1.0E3,
       4  => 1.0E4,
       5  => 1.0E5,
       6  => 1.0E6,
       7  => 1.0E7,
       8  => 1.0E8,
       9  => 1.0E9,
       10 => 1.0E10];

   --  Convert unsigned register to signed scale factor
   --  SunSpec scale factors are signed 16-bit stored as unsigned
   function To_Scale_Factor (Value : Register_Value) return Scale_Factor
     with Inline;

   --  Convert unsigned register to signed 16-bit integer
   --  For SunSpec int16 values stored as unsigned
   function To_Signed_16 (Value : Register_Value) return Integer
     with Inline,
          Post => To_Signed_16'Result in -32768 .. 32767;

   --  Check if a register value is "implemented" (not a marker value)
   --  Checks for common SunSpec "not implemented" markers
   function Is_Implemented (Value : Register_Value) return Boolean
     with Inline;

   --  Check if a signed int16 value is "implemented"
   function Is_Implemented_Int16 (Value : Register_Value) return Boolean
     with Inline;

   --  Apply scale factor to a register value (unsigned)
   function Apply_Scale (Value : Register_Value; SF : Scale_Factor) return Float
     with Inline;

   --  Apply scale factor to a signed register value (e.g., temperature)
   --  SunSpec int16 values are stored as unsigned but represent signed data
   function Apply_Scale_Signed
     (Value : Register_Value; SF : Scale_Factor) return Float
     with Inline;

   --  SunSpec string (max 16 registers = 32 chars)
   subtype SunSpec_String is String (1 .. 32);

   --  Decode SunSpec string from registers (padded with NUL or spaces)
   procedure Decode_String
     (Registers : Register_Array;
      Result    : out SunSpec_String;
      Length    : out Natural)
     with Pre => Registers'Length <= 16;

   --  Model header info
   type Model_Header is record
      ID     : Model_ID;
      Length : Model_Length;
   end record;

   --  Check for SunSpec identifier at given address
   --  Encodes a request to read the 2-register SunS identifier
   procedure Encode_Check_SunSpec_Request
     (Base_Address : Register_Address;
      Buffer       : out Protocol.PDU_Buffer;
      Length       : out Natural)
     with Post => Length <= Protocol.Max_PDU_Size;

   --  Decode SunSpec identifier check response
   procedure Decode_Check_SunSpec_Response
     (Buffer   : Protocol.PDU_Buffer;
      Length   : Natural;
      Is_Valid : out Boolean;
      Result   : out Status)
     with Pre => Length <= Protocol.Max_PDU_Size;

   --  Read model header at specified offset from base
   procedure Encode_Read_Model_Header_Request
     (Base_Address : Register_Address;
      Offset       : Register_Address;
      Buffer       : out Protocol.PDU_Buffer;
      Length       : out Natural)
     with Post => Length <= Protocol.Max_PDU_Size;

   --  Decode model header response
   procedure Decode_Model_Header_Response
     (Buffer : Protocol.PDU_Buffer;
      Length : Natural;
      Header : out Model_Header;
      Result : out Status)
     with Pre => Length <= Protocol.Max_PDU_Size;

   ---------------------
   -- Model Discovery --
   ---------------------

   --  Information about a discovered model
   type Model_Info is record
      ID       : Model_ID;      --  Model ID (1, 101, 103, 124, 160, 201, etc.)
      Length   : Model_Length;  --  Model data length (excluding header)
      Offset   : Register_Address;  --  Offset from base address
   end record;

   --  Model discovery state for iteration
   type Model_Iterator is record
      Base_Address   : Register_Address;  --  SunSpec base (40000 or 50000)
      Current_Offset : Register_Address;  --  Current position in model list
      Max_Offset     : Register_Address;  --  Safety limit
      Is_Valid       : Boolean;           --  True if iterator is valid
   end record;

   --  Initialize model iterator (call after verifying SunSpec identifier)
   procedure Init_Model_Iterator
     (Iterator     : out Model_Iterator;
      Base_Address : Register_Address := Default_Base_Address;
      Max_Offset   : Register_Address := 1000);

   --  Calculate next model address from header response
   --  Returns updated iterator with next model position
   --  Call this after successfully reading a model header
   procedure Advance_Model_Iterator
     (Iterator : in out Model_Iterator;
      Length   : Model_Length);

   --  Check if current position is end marker (0xFFFF)
   function Is_End_Model (Header : Model_Header) return Boolean
     with Inline;

   --  Get current read address for model header
   function Get_Header_Address (Iterator : Model_Iterator) return Register_Address
     with Inline;

   --  Get model data start address (header address + 2)
   function Get_Data_Address
     (Base_Address : Register_Address;
      Model_Offset : Register_Address) return Register_Address
     with Inline;

   --  Helper: Check if model ID is an Inverter model (101-103)
   function Is_Inverter_Model (ID : Model_ID) return Boolean
     with Inline;

   --  Helper: Check if model ID is a Meter model (201-204)
   function Is_Meter_Model (ID : Model_ID) return Boolean
     with Inline;

end Ada_Modbus.Energy.SunSpec;
