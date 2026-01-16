--  Ada_Modbus.Slave_Generic - Implementation
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT

with Ada_Modbus.Utilities; use Ada_Modbus.Utilities;
with Ada_Modbus.Protocol.RTU;
with Ada_Modbus.Protocol.ASCII;
with Ada_Modbus.Protocol.TCP;

package body Ada_Modbus.Slave_Generic
  with SPARK_Mode => On
is

   ------------------------------
   -- Build_Exception_Response --
   ------------------------------

   procedure Build_Exception_Response
     (FC           : Function_Code;
      Exception_St : Status;
      Response     : out Protocol.PDU_Buffer;
      Response_Len : out Natural)
   is
   begin
      Response := [others => 0];
      --  Exception function code = original FC + 0x80
      Response (0) := Byte (FC) or 16#80#;
      Response (1) := Protocol.To_Exception_Byte (Exception_St);
      Response_Len := 2;
   end Build_Exception_Response;

   -----------------
   -- Process_PDU --
   -----------------

   procedure Process_PDU
     (Request_PDU  : Protocol.PDU_Buffer;
      Request_Len  : Natural;
      Response_PDU : out Protocol.PDU_Buffer;
      Response_Len : out Natural)
   is
      FC : constant Function_Code := Function_Code (Request_PDU (0));
      Result : Status;
   begin
      Response_PDU := [others => 0];
      Response_Len := 0;

      if Request_Len < 1 then
         Build_Exception_Response (FC, Exception_Illegal_Function, Response_PDU, Response_Len);
         return;
      end if;

      case FC is
         when FC_Read_Coils =>
            if Request_Len < 5 then
               Build_Exception_Response (FC, Exception_Illegal_Value, Response_PDU, Response_Len);
               return;
            end if;

            declare
               Start_Addr : constant Coil_Address :=
                 Coil_Address (From_Big_Endian (Request_PDU (1), Request_PDU (2)));
               Qty_Raw    : constant Natural :=
                 Natural (From_Big_Endian (Request_PDU (3), Request_PDU (4)));
               Byte_Count : Natural;
            begin
               if Qty_Raw < 1 or else Qty_Raw > 2000 then
                  Build_Exception_Response (FC, Exception_Illegal_Value, Response_PDU, Response_Len);
                  return;
               end if;

               declare
                  Quantity : constant Coil_Count := Coil_Count (Qty_Raw);
                  Values   : Coil_Array (0 .. Natural (Quantity) - 1);
               begin
                  Result := Read_Coils (Start_Addr, Quantity, Values);

                  if Result /= Success then
                     Build_Exception_Response (FC, Result, Response_PDU, Response_Len);
                     return;
                  end if;

                  Byte_Count := (Natural (Quantity) + 7) / 8;
                  Response_PDU (0) := Byte (FC);
                  Response_PDU (1) := Byte (Byte_Count);

                  for I in 0 .. Byte_Count - 1 loop
                     declare
                        B : Byte := 0;
                     begin
                        for Bit in 0 .. 7 loop
                           declare
                              Coil_Idx : constant Natural := I * 8 + Bit;
                           begin
                              if Coil_Idx < Natural (Quantity) and then Values (Coil_Idx) then
                                 B := B or Byte (2 ** Bit);
                              end if;
                           end;
                        end loop;
                        Response_PDU (2 + I) := B;
                     end;
                  end loop;

                  Response_Len := 2 + Byte_Count;
               end;
            end;

         when FC_Read_Discrete_Inputs =>
            if Request_Len < 5 then
               Build_Exception_Response (FC, Exception_Illegal_Value, Response_PDU, Response_Len);
               return;
            end if;

            declare
               Start_Addr : constant Coil_Address :=
                 Coil_Address (From_Big_Endian (Request_PDU (1), Request_PDU (2)));
               Qty_Raw    : constant Natural :=
                 Natural (From_Big_Endian (Request_PDU (3), Request_PDU (4)));
               Byte_Count : Natural;
            begin
               if Qty_Raw < 1 or else Qty_Raw > 2000 then
                  Build_Exception_Response (FC, Exception_Illegal_Value, Response_PDU, Response_Len);
                  return;
               end if;

               declare
                  Quantity : constant Coil_Count := Coil_Count (Qty_Raw);
                  Values   : Coil_Array (0 .. Natural (Quantity) - 1);
               begin
                  Result := Read_Discrete_Inputs (Start_Addr, Quantity, Values);

                  if Result /= Success then
                     Build_Exception_Response (FC, Result, Response_PDU, Response_Len);
                     return;
                  end if;

                  Byte_Count := (Natural (Quantity) + 7) / 8;
                  Response_PDU (0) := Byte (FC);
                  Response_PDU (1) := Byte (Byte_Count);

                  for I in 0 .. Byte_Count - 1 loop
                     declare
                        B : Byte := 0;
                     begin
                        for Bit in 0 .. 7 loop
                           declare
                              Coil_Idx : constant Natural := I * 8 + Bit;
                           begin
                              if Coil_Idx < Natural (Quantity) and then Values (Coil_Idx) then
                                 B := B or Byte (2 ** Bit);
                              end if;
                           end;
                        end loop;
                        Response_PDU (2 + I) := B;
                     end;
                  end loop;

                  Response_Len := 2 + Byte_Count;
               end;
            end;

         when FC_Read_Holding_Registers =>
            if Request_Len < 5 then
               Build_Exception_Response (FC, Exception_Illegal_Value, Response_PDU, Response_Len);
               return;
            end if;

            declare
               Start_Addr : constant Register_Address :=
                 Register_Address (From_Big_Endian (Request_PDU (1), Request_PDU (2)));
               Qty_Raw    : constant Natural :=
                 Natural (From_Big_Endian (Request_PDU (3), Request_PDU (4)));
            begin
               if Qty_Raw < 1 or else Qty_Raw > 125 then
                  Build_Exception_Response (FC, Exception_Illegal_Value, Response_PDU, Response_Len);
                  return;
               end if;

               declare
                  Quantity : constant Register_Count := Register_Count (Qty_Raw);
                  Values   : Register_Array (0 .. Natural (Quantity) - 1);
                  Byte_Count : constant Natural := Natural (Quantity) * 2;
               begin
                  Result := Read_Holding_Registers (Start_Addr, Quantity, Values);

                  if Result /= Success then
                     Build_Exception_Response (FC, Result, Response_PDU, Response_Len);
                     return;
                  end if;

                  Response_PDU (0) := Byte (FC);
                  Response_PDU (1) := Byte (Byte_Count);

                  for I in 0 .. Natural (Quantity) - 1 loop
                     declare
                        Reg_Bytes : constant Byte_Array := To_Big_Endian (Values (I));
                     begin
                        Response_PDU (2 + I * 2) := Reg_Bytes (0);
                        Response_PDU (3 + I * 2) := Reg_Bytes (1);
                     end;
                  end loop;

                  Response_Len := 2 + Byte_Count;
               end;
            end;

         when FC_Read_Input_Registers =>
            if Request_Len < 5 then
               Build_Exception_Response (FC, Exception_Illegal_Value, Response_PDU, Response_Len);
               return;
            end if;

            declare
               Start_Addr : constant Register_Address :=
                 Register_Address (From_Big_Endian (Request_PDU (1), Request_PDU (2)));
               Qty_Raw    : constant Natural :=
                 Natural (From_Big_Endian (Request_PDU (3), Request_PDU (4)));
            begin
               if Qty_Raw < 1 or else Qty_Raw > 125 then
                  Build_Exception_Response (FC, Exception_Illegal_Value, Response_PDU, Response_Len);
                  return;
               end if;

               declare
                  Quantity : constant Register_Count := Register_Count (Qty_Raw);
                  Values   : Register_Array (0 .. Natural (Quantity) - 1);
                  Byte_Count : constant Natural := Natural (Quantity) * 2;
               begin
                  Result := Read_Input_Registers (Start_Addr, Quantity, Values);

                  if Result /= Success then
                     Build_Exception_Response (FC, Result, Response_PDU, Response_Len);
                     return;
                  end if;

                  Response_PDU (0) := Byte (FC);
                  Response_PDU (1) := Byte (Byte_Count);

                  for I in 0 .. Natural (Quantity) - 1 loop
                     declare
                        Reg_Bytes : constant Byte_Array := To_Big_Endian (Values (I));
                     begin
                        Response_PDU (2 + I * 2) := Reg_Bytes (0);
                        Response_PDU (3 + I * 2) := Reg_Bytes (1);
                     end;
                  end loop;

                  Response_Len := 2 + Byte_Count;
               end;
            end;

         when FC_Write_Single_Coil =>
            if Request_Len < 5 then
               Build_Exception_Response (FC, Exception_Illegal_Value, Response_PDU, Response_Len);
               return;
            end if;

            declare
               Address : constant Coil_Address :=
                 Coil_Address (From_Big_Endian (Request_PDU (1), Request_PDU (2)));
               Val_Raw : constant Register_Value :=
                 From_Big_Endian (Request_PDU (3), Request_PDU (4));
               Value   : Coil_Value;
            begin
               if Val_Raw = 16#FF00# then
                  Value := True;
               elsif Val_Raw = 16#0000# then
                  Value := False;
               else
                  Build_Exception_Response (FC, Exception_Illegal_Value, Response_PDU, Response_Len);
                  return;
               end if;

               Result := Write_Single_Coil (Address, Value);

               if Result /= Success then
                  Build_Exception_Response (FC, Result, Response_PDU, Response_Len);
                  return;
               end if;

               Response_PDU (0 .. 4) := Request_PDU (0 .. 4);
               Response_Len := 5;
            end;

         when FC_Write_Single_Register =>
            if Request_Len < 5 then
               Build_Exception_Response (FC, Exception_Illegal_Value, Response_PDU, Response_Len);
               return;
            end if;

            declare
               Address : constant Register_Address :=
                 Register_Address (From_Big_Endian (Request_PDU (1), Request_PDU (2)));
               Value   : constant Register_Value :=
                 From_Big_Endian (Request_PDU (3), Request_PDU (4));
            begin
               Result := Write_Single_Register (Address, Value);

               if Result /= Success then
                  Build_Exception_Response (FC, Result, Response_PDU, Response_Len);
                  return;
               end if;

               Response_PDU (0 .. 4) := Request_PDU (0 .. 4);
               Response_Len := 5;
            end;

         when FC_Write_Multiple_Coils =>
            if Request_Len < 6 then
               Build_Exception_Response (FC, Exception_Illegal_Value, Response_PDU, Response_Len);
               return;
            end if;

            declare
               Start_Addr : constant Coil_Address :=
                 Coil_Address (From_Big_Endian (Request_PDU (1), Request_PDU (2)));
               Qty_Raw    : constant Natural :=
                 Natural (From_Big_Endian (Request_PDU (3), Request_PDU (4)));
               Byte_Count : constant Natural := Natural (Request_PDU (5));
            begin
               if Qty_Raw < 1 or else Qty_Raw > 1968 then
                  Build_Exception_Response (FC, Exception_Illegal_Value, Response_PDU, Response_Len);
                  return;
               end if;

               if Byte_Count /= (Qty_Raw + 7) / 8 then
                  Build_Exception_Response (FC, Exception_Illegal_Value, Response_PDU, Response_Len);
                  return;
               end if;

               if Request_Len < 6 + Byte_Count then
                  Build_Exception_Response (FC, Exception_Illegal_Value, Response_PDU, Response_Len);
                  return;
               end if;

               declare
                  Quantity : constant Coil_Count := Coil_Count (Qty_Raw);
                  Values   : Coil_Array (0 .. Natural (Quantity) - 1);
               begin
                  for I in 0 .. Natural (Quantity) - 1 loop
                     declare
                        Byte_Idx : constant Natural := I / 8;
                        Bit_Idx  : constant Natural := I mod 8;
                        B        : constant Byte := Request_PDU (6 + Byte_Idx);
                     begin
                        Values (I) := (B and Byte (2 ** Bit_Idx)) /= 0;
                     end;
                  end loop;

                  Result := Write_Multiple_Coils (Start_Addr, Values);

                  if Result /= Success then
                     Build_Exception_Response (FC, Result, Response_PDU, Response_Len);
                     return;
                  end if;

                  Response_PDU (0 .. 4) := Request_PDU (0 .. 4);
                  Response_Len := 5;
               end;
            end;

         when FC_Write_Multiple_Registers =>
            if Request_Len < 6 then
               Build_Exception_Response (FC, Exception_Illegal_Value, Response_PDU, Response_Len);
               return;
            end if;

            declare
               Start_Addr : constant Register_Address :=
                 Register_Address (From_Big_Endian (Request_PDU (1), Request_PDU (2)));
               Qty_Raw    : constant Natural :=
                 Natural (From_Big_Endian (Request_PDU (3), Request_PDU (4)));
               Byte_Count : constant Natural := Natural (Request_PDU (5));
            begin
               if Qty_Raw < 1 or else Qty_Raw > 123 then
                  Build_Exception_Response (FC, Exception_Illegal_Value, Response_PDU, Response_Len);
                  return;
               end if;

               if Byte_Count /= Qty_Raw * 2 then
                  Build_Exception_Response (FC, Exception_Illegal_Value, Response_PDU, Response_Len);
                  return;
               end if;

               if Request_Len < 6 + Byte_Count then
                  Build_Exception_Response (FC, Exception_Illegal_Value, Response_PDU, Response_Len);
                  return;
               end if;

               declare
                  Quantity : constant Register_Count := Register_Count (Qty_Raw);
                  Values   : Register_Array (0 .. Natural (Quantity) - 1);
               begin
                  for I in 0 .. Natural (Quantity) - 1 loop
                     Values (I) := From_Big_Endian
                       (Request_PDU (6 + I * 2), Request_PDU (7 + I * 2));
                  end loop;

                  Result := Write_Multiple_Registers (Start_Addr, Values);

                  if Result /= Success then
                     Build_Exception_Response (FC, Result, Response_PDU, Response_Len);
                     return;
                  end if;

                  Response_PDU (0 .. 4) := Request_PDU (0 .. 4);
                  Response_Len := 5;
               end;
            end;

         when FC_Read_Exception_Status =>
            declare
               Exception_Status : Byte;
            begin
               Result := Read_Exception_Status (Exception_Status);

               if Result /= Success then
                  Build_Exception_Response (FC, Result, Response_PDU, Response_Len);
                  return;
               end if;

               Response_PDU (0) := Byte (FC);
               Response_PDU (1) := Exception_Status;
               Response_Len := 2;
            end;

         when FC_Diagnostics =>
            if Request_Len < 5 then
               Build_Exception_Response (FC, Exception_Illegal_Value, Response_PDU, Response_Len);
               return;
            end if;

            declare
               Sub_Func : constant Interfaces.Unsigned_16 :=
                 Interfaces.Unsigned_16 (From_Big_Endian (Request_PDU (1), Request_PDU (2)));
               Data_In  : constant Interfaces.Unsigned_16 :=
                 Interfaces.Unsigned_16 (From_Big_Endian (Request_PDU (3), Request_PDU (4)));
               Data_Out : Interfaces.Unsigned_16;
               Out_Bytes : Byte_Array (0 .. 1);
            begin
               Result := Diagnostics (Sub_Func, Data_In, Data_Out);

               if Result /= Success then
                  Build_Exception_Response (FC, Result, Response_PDU, Response_Len);
                  return;
               end if;

               Response_PDU (0) := Byte (FC);
               Response_PDU (1) := Request_PDU (1);
               Response_PDU (2) := Request_PDU (2);
               Out_Bytes := To_Big_Endian (Register_Value (Data_Out));
               Response_PDU (3) := Out_Bytes (0);
               Response_PDU (4) := Out_Bytes (1);
               Response_Len := 5;
            end;

         when FC_Report_Server_Id =>
            declare
               Server_Id     : Byte;
               Run_Indicator : Boolean;
               Add_Data      : Byte_Array (0 .. 249);
               Add_Data_Len  : Natural;
            begin
               Add_Data := [others => 0];
               Result := Report_Server_Id (Server_Id, Run_Indicator, Add_Data, Add_Data_Len);

               if Result /= Success then
                  Build_Exception_Response (FC, Result, Response_PDU, Response_Len);
                  return;
               end if;

               declare
                  Byte_Count : constant Natural := 2 + Add_Data_Len;
               begin
                  Response_PDU (0) := Byte (FC);
                  Response_PDU (1) := Byte (Byte_Count);
                  Response_PDU (2) := Server_Id;
                  Response_PDU (3) := (if Run_Indicator then 16#FF# else 16#00#);

                  for I in 0 .. Add_Data_Len - 1 loop
                     Response_PDU (4 + I) := Add_Data (I);
                  end loop;

                  Response_Len := 4 + Add_Data_Len;
               end;
            end;

         when FC_Mask_Write_Register =>
            if Request_Len < 7 then
               Build_Exception_Response (FC, Exception_Illegal_Value, Response_PDU, Response_Len);
               return;
            end if;

            declare
               Address  : constant Register_Address :=
                 Register_Address (From_Big_Endian (Request_PDU (1), Request_PDU (2)));
               And_Mask : constant Register_Value :=
                 From_Big_Endian (Request_PDU (3), Request_PDU (4));
               Or_Mask  : constant Register_Value :=
                 From_Big_Endian (Request_PDU (5), Request_PDU (6));
            begin
               Result := Mask_Write_Register (Address, And_Mask, Or_Mask);

               if Result /= Success then
                  Build_Exception_Response (FC, Result, Response_PDU, Response_Len);
                  return;
               end if;

               Response_PDU (0 .. 6) := Request_PDU (0 .. 6);
               Response_Len := 7;
            end;

         when FC_Read_Write_Multiple_Registers =>
            if Request_Len < 10 then
               Build_Exception_Response (FC, Exception_Illegal_Value, Response_PDU, Response_Len);
               return;
            end if;

            declare
               Read_Start     : constant Register_Address :=
                 Register_Address (From_Big_Endian (Request_PDU (1), Request_PDU (2)));
               Read_Qty_Raw   : constant Natural :=
                 Natural (From_Big_Endian (Request_PDU (3), Request_PDU (4)));
               Write_Start    : constant Register_Address :=
                 Register_Address (From_Big_Endian (Request_PDU (5), Request_PDU (6)));
               Write_Qty_Raw  : constant Natural :=
                 Natural (From_Big_Endian (Request_PDU (7), Request_PDU (8)));
               Write_Byte_Cnt : constant Natural := Natural (Request_PDU (9));
            begin
               if Read_Qty_Raw < 1 or else Read_Qty_Raw > 125 then
                  Build_Exception_Response (FC, Exception_Illegal_Value, Response_PDU, Response_Len);
                  return;
               end if;

               if Write_Qty_Raw < 1 or else Write_Qty_Raw > 121 then
                  Build_Exception_Response (FC, Exception_Illegal_Value, Response_PDU, Response_Len);
                  return;
               end if;

               if Write_Byte_Cnt /= Write_Qty_Raw * 2 then
                  Build_Exception_Response (FC, Exception_Illegal_Value, Response_PDU, Response_Len);
                  return;
               end if;

               if Request_Len < 10 + Write_Byte_Cnt then
                  Build_Exception_Response (FC, Exception_Illegal_Value, Response_PDU, Response_Len);
                  return;
               end if;

               declare
                  Read_Quantity  : constant Register_Count := Register_Count (Read_Qty_Raw);
                  Write_Quantity : constant Register_Count := Register_Count (Write_Qty_Raw);
                  Read_Values    : Register_Array (0 .. Natural (Read_Quantity) - 1);
                  Write_Values   : Register_Array (0 .. Natural (Write_Quantity) - 1);
               begin
                  for I in 0 .. Natural (Write_Quantity) - 1 loop
                     Write_Values (I) := From_Big_Endian
                       (Request_PDU (10 + I * 2), Request_PDU (11 + I * 2));
                  end loop;

                  Result := Read_Write_Registers
                    (Read_Start, Read_Quantity, Read_Values,
                     Write_Start, Write_Values);

                  if Result /= Success then
                     Build_Exception_Response (FC, Result, Response_PDU, Response_Len);
                     return;
                  end if;

                  declare
                     Read_Byte_Count : constant Natural := Natural (Read_Quantity) * 2;
                  begin
                     Response_PDU (0) := Byte (FC);
                     Response_PDU (1) := Byte (Read_Byte_Count);

                     for I in 0 .. Natural (Read_Quantity) - 1 loop
                        declare
                           Reg_Bytes : constant Byte_Array := To_Big_Endian (Read_Values (I));
                        begin
                           Response_PDU (2 + I * 2) := Reg_Bytes (0);
                           Response_PDU (3 + I * 2) := Reg_Bytes (1);
                        end;
                     end loop;

                     Response_Len := 2 + Read_Byte_Count;
                  end;
               end;
            end;

         when others =>
            Build_Exception_Response (FC, Exception_Illegal_Function, Response_PDU, Response_Len);
      end case;
   end Process_PDU;

   -------------------------
   -- Process_RTU_Request --
   -------------------------

   procedure Process_RTU_Request
     (Request_Frame   : Byte_Array;
      Request_Length  : Natural;
      Response_Frame  : out Byte_Array;
      Response_Length : out Natural;
      Send_Response   : out Boolean)
   is
      Request_PDU  : Protocol.PDU_Buffer;
      Request_Len  : Natural;
      Response_PDU : Protocol.PDU_Buffer;
      Response_Len : Natural;
      Req_Slave    : Unit_Id;
      Parse_Result : Status;
      Req_ADU      : Protocol.RTU.ADU_Buffer := [others => 0];
   begin
      Response_Frame := [others => 0];
      Response_Length := 0;
      Send_Response := False;

      --  Copy to ADU buffer
      for I in 0 .. Natural'Min (Request_Length, Protocol.RTU.Max_ADU_Size) - 1 loop
         Req_ADU (I) := Request_Frame (Request_Frame'First + I);
      end loop;

      Protocol.RTU.Parse_Frame
        (Req_ADU, Request_Length, Req_Slave, Request_PDU, Request_Len, Parse_Result);

      if Parse_Result /= Success then
         return;
      end if;

      if Req_Slave /= Slave_Unit_Id and then Req_Slave /= Broadcast_Address then
         return;
      end if;

      Process_PDU (Request_PDU, Request_Len, Response_PDU, Response_Len);

      if Req_Slave = Broadcast_Address then
         return;
      end if;

      declare
         Resp_ADU : Protocol.RTU.ADU_Buffer;
         Resp_ADU_Len : Natural;
      begin
         Protocol.RTU.Build_Frame
           (Resp_ADU, Resp_ADU_Len, Slave_Unit_Id, Response_PDU, Response_Len);

         for I in 0 .. Resp_ADU_Len - 1 loop
            Response_Frame (Response_Frame'First + I) := Resp_ADU (I);
         end loop;
         Response_Length := Resp_ADU_Len;
         Send_Response := True;
      end;
   end Process_RTU_Request;

   ---------------------------
   -- Process_ASCII_Request --
   ---------------------------

   procedure Process_ASCII_Request
     (Request_Frame   : Byte_Array;
      Request_Length  : Natural;
      Response_Frame  : out Byte_Array;
      Response_Length : out Natural;
      Send_Response   : out Boolean)
   is
      Request_PDU  : Protocol.PDU_Buffer;
      Request_Len  : Natural;
      Response_PDU : Protocol.PDU_Buffer;
      Response_Len : Natural;
      Req_Slave    : Unit_Id;
      Parse_Result : Status;
      Req_Frame    : Protocol.ASCII.Frame_Buffer := [others => 0];
   begin
      Response_Frame := [others => 0];
      Response_Length := 0;
      Send_Response := False;

      for I in 0 .. Natural'Min (Request_Length, Protocol.ASCII.Max_Frame_Size) - 1 loop
         Req_Frame (I) := Request_Frame (Request_Frame'First + I);
      end loop;

      Protocol.ASCII.Parse_Frame
        (Req_Frame, Request_Length, Req_Slave, Request_PDU, Request_Len, Parse_Result);

      if Parse_Result /= Success then
         return;
      end if;

      if Req_Slave /= Slave_Unit_Id and then Req_Slave /= Broadcast_Address then
         return;
      end if;

      Process_PDU (Request_PDU, Request_Len, Response_PDU, Response_Len);

      if Req_Slave = Broadcast_Address then
         return;
      end if;

      declare
         Resp_Frame : Protocol.ASCII.Frame_Buffer;
         Resp_Frame_Len : Natural;
      begin
         Protocol.ASCII.Build_Frame
           (Resp_Frame, Resp_Frame_Len, Slave_Unit_Id, Response_PDU, Response_Len);

         for I in 0 .. Resp_Frame_Len - 1 loop
            Response_Frame (Response_Frame'First + I) := Resp_Frame (I);
         end loop;
         Response_Length := Resp_Frame_Len;
         Send_Response := True;
      end;
   end Process_ASCII_Request;

   -------------------------
   -- Process_TCP_Request --
   -------------------------

   procedure Process_TCP_Request
     (Request_Frame   : Byte_Array;
      Request_Length  : Natural;
      Response_Frame  : out Byte_Array;
      Response_Length : out Natural;
      Send_Response   : out Boolean)
   is
      Request_PDU  : Protocol.PDU_Buffer;
      Request_Len  : Natural;
      Response_PDU : Protocol.PDU_Buffer;
      Response_Len : Natural;
      Parse_Result : Status;
      Req_ADU      : Protocol.TCP.ADU_Buffer := [others => 0];
      Trans_Id     : Protocol.TCP.Transaction_Id;
      Req_Unit     : Unit_Id;
   begin
      Response_Frame := [others => 0];
      Response_Length := 0;
      Send_Response := False;

      for I in 0 .. Natural'Min (Request_Length, Protocol.TCP.Max_ADU_Size) - 1 loop
         Req_ADU (I) := Request_Frame (Request_Frame'First + I);
      end loop;

      Protocol.TCP.Parse_Frame
        (Req_ADU, Request_Length, Trans_Id, Req_Unit, Request_PDU, Request_Len, Parse_Result);

      if Parse_Result /= Success then
         return;
      end if;

      if Req_Unit /= Slave_Unit_Id and then Req_Unit /= 0 then
         return;
      end if;

      Process_PDU (Request_PDU, Request_Len, Response_PDU, Response_Len);

      declare
         Resp_ADU : Protocol.TCP.ADU_Buffer;
         Resp_ADU_Len : Natural;
      begin
         Protocol.TCP.Build_Frame
           (Resp_ADU, Resp_ADU_Len, Trans_Id, Slave_Unit_Id, Response_PDU, Response_Len);

         for I in 0 .. Resp_ADU_Len - 1 loop
            Response_Frame (Response_Frame'First + I) := Resp_ADU (I);
         end loop;
         Response_Length := Resp_ADU_Len;
         Send_Response := True;
      end;
   end Process_TCP_Request;

end Ada_Modbus.Slave_Generic;
