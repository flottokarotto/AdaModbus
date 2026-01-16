--  TCP_Slave - Modbus TCP Slave (Server) with multi-client support
--  Copyright (c) 2026 Florian Fischer
--  SPDX-License-Identifier: MIT
--
--  A Modbus TCP server that:
--  - Listens on port 502 (or specified port)
--  - Handles multiple concurrent client connections via tasking
--  - Each client is handled by a separate task
--
--  Usage: tcp_slave [port]
--  Default port: 1502 (to avoid needing admin rights)

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with Ada_Modbus; use Ada_Modbus;
with Ada_Modbus.Slave; use Ada_Modbus.Slave;
with Ada_Modbus.Transport.TCP;

procedure TCP_Slave is

   use Ada_Modbus.Transport.TCP;

   --  Shared data protected by a protected object
   protected Shared_Data is
      procedure Read_Holding_Registers
        (Start  : Natural;
         Count  : Natural;
         Values : out Register_Array;
         Result : out Status);

      procedure Write_Holding_Register
        (Addr   : Natural;
         Value  : Register_Value;
         Result : out Status);

      procedure Write_Multiple_Registers
        (Start  : Natural;
         Values : Register_Array;
         Result : out Status);

      procedure Read_Input_Registers
        (Start  : Natural;
         Count  : Natural;
         Values : out Register_Array;
         Result : out Status);

      procedure Read_Coils
        (Start  : Natural;
         Count  : Natural;
         Values : out Coil_Array;
         Result : out Status);

      procedure Write_Coil
        (Addr   : Natural;
         Value  : Boolean;
         Result : out Status);

      procedure Write_Multiple_Coils
        (Start  : Natural;
         Values : Coil_Array;
         Result : out Status);

      procedure Read_Discrete_Inputs
        (Start  : Natural;
         Count  : Natural;
         Values : out Coil_Array;
         Result : out Status);


   private
      Holding_Registers : Register_Array (0 .. 99) := [others => 0];
      Input_Registers   : Register_Array (0 .. 99) := [others => 0];
      Coils             : Coil_Array (0 .. 99) := [others => False];
      Discrete_Inputs   : Coil_Array (0 .. 99) := [others => False];
      Request_Count     : Natural := 0;
      Initialized       : Boolean := False;
   end Shared_Data;

   protected body Shared_Data is

      procedure Initialize_Data is
      begin
         if not Initialized then
            for I in Holding_Registers'Range loop
               Holding_Registers (I) := Register_Value (I * 100);
            end loop;
            for I in Input_Registers'Range loop
               Input_Registers (I) := Register_Value (I * 10 + 1000);
            end loop;
            Coils (0) := True;
            Coils (1) := False;
            Coils (2) := True;
            Discrete_Inputs (0) := True;
            Discrete_Inputs (5) := True;
            Initialized := True;
         end if;
      end Initialize_Data;

      procedure Read_Holding_Registers
        (Start  : Natural;
         Count  : Natural;
         Values : out Register_Array;
         Result : out Status)
      is
      begin
         Initialize_Data;
         Request_Count := Request_Count + 1;

         if Start + Count > Holding_Registers'Length then
            Result := Exception_Illegal_Address;
            return;
         end if;

         for I in 0 .. Count - 1 loop
            Values (Values'First + I) := Holding_Registers (Start + I);
         end loop;
         Result := Success;
      end Read_Holding_Registers;

      procedure Write_Holding_Register
        (Addr   : Natural;
         Value  : Register_Value;
         Result : out Status)
      is
      begin
         Initialize_Data;
         Request_Count := Request_Count + 1;

         if Addr >= Holding_Registers'Length then
            Result := Exception_Illegal_Address;
            return;
         end if;

         Holding_Registers (Addr) := Value;
         Result := Success;
      end Write_Holding_Register;

      procedure Write_Multiple_Registers
        (Start  : Natural;
         Values : Register_Array;
         Result : out Status)
      is
      begin
         Initialize_Data;
         Request_Count := Request_Count + 1;

         if Start + Values'Length > Holding_Registers'Length then
            Result := Exception_Illegal_Address;
            return;
         end if;

         for I in Values'Range loop
            Holding_Registers (Start + I - Values'First) := Values (I);
         end loop;
         Result := Success;
      end Write_Multiple_Registers;

      procedure Read_Input_Registers
        (Start  : Natural;
         Count  : Natural;
         Values : out Register_Array;
         Result : out Status)
      is
      begin
         Initialize_Data;
         Request_Count := Request_Count + 1;

         if Start + Count > Input_Registers'Length then
            Result := Exception_Illegal_Address;
            return;
         end if;

         for I in 0 .. Count - 1 loop
            Values (Values'First + I) := Input_Registers (Start + I);
         end loop;
         Result := Success;
      end Read_Input_Registers;

      procedure Read_Coils
        (Start  : Natural;
         Count  : Natural;
         Values : out Coil_Array;
         Result : out Status)
      is
      begin
         Initialize_Data;
         Request_Count := Request_Count + 1;

         if Start + Count > Coils'Length then
            Result := Exception_Illegal_Address;
            return;
         end if;

         for I in 0 .. Count - 1 loop
            Values (Values'First + I) := Coils (Start + I);
         end loop;
         Result := Success;
      end Read_Coils;

      procedure Write_Coil
        (Addr   : Natural;
         Value  : Boolean;
         Result : out Status)
      is
      begin
         Initialize_Data;
         Request_Count := Request_Count + 1;

         if Addr >= Coils'Length then
            Result := Exception_Illegal_Address;
            return;
         end if;

         Coils (Addr) := Value;
         Result := Success;
      end Write_Coil;

      procedure Write_Multiple_Coils
        (Start  : Natural;
         Values : Coil_Array;
         Result : out Status)
      is
      begin
         Initialize_Data;
         Request_Count := Request_Count + 1;

         if Start + Values'Length > Coils'Length then
            Result := Exception_Illegal_Address;
            return;
         end if;

         for I in Values'Range loop
            Coils (Start + I - Values'First) := Values (I);
         end loop;
         Result := Success;
      end Write_Multiple_Coils;

      procedure Read_Discrete_Inputs
        (Start  : Natural;
         Count  : Natural;
         Values : out Coil_Array;
         Result : out Status)
      is
      begin
         Initialize_Data;
         Request_Count := Request_Count + 1;

         if Start + Count > Discrete_Inputs'Length then
            Result := Exception_Illegal_Address;
            return;
         end if;

         for I in 0 .. Count - 1 loop
            Values (Values'First + I) := Discrete_Inputs (Start + I);
         end loop;
         Result := Success;
      end Read_Discrete_Inputs;

   end Shared_Data;

   --  Client ID counter (protected)
   protected Client_Counter is
      procedure Get_Next_Id (Id : out Positive);
   private
      Next_Id : Positive := 1;
   end Client_Counter;

   protected body Client_Counter is
      procedure Get_Next_Id (Id : out Positive) is
      begin
         Id := Next_Id;
         Next_Id := Next_Id + 1;
      end Get_Next_Id;
   end Client_Counter;

   --  Access type for TCP connections (needed because TCP_Connection is limited)
   type TCP_Connection_Access is access TCP_Connection;
   procedure Free_Connection is new Ada.Unchecked_Deallocation
     (TCP_Connection, TCP_Connection_Access);

   --  Task type to handle a single client connection
   task type Client_Handler is
      entry Start (Conn : TCP_Connection_Access; Id : Positive);
   end Client_Handler;

   type Client_Handler_Access is access Client_Handler;
   procedure Free_Handler is new Ada.Unchecked_Deallocation
     (Client_Handler, Client_Handler_Access);

   --  List to track active handlers for cleanup
   Max_Handlers : constant := 100;
   type Handler_Array is array (1 .. Max_Handlers) of Client_Handler_Access;

   protected Handler_List is
      procedure Add (Handler : Client_Handler_Access);
      procedure Cleanup_Terminated;
   private
      Handlers : Handler_Array := [others => null];
   end Handler_List;

   protected body Handler_List is
      procedure Add (Handler : Client_Handler_Access) is
      begin
         --  Find empty slot
         for I in Handlers'Range loop
            if Handlers (I) = null then
               Handlers (I) := Handler;
               return;
            end if;
         end loop;
         --  No slot available - this handler will leak
         --  (shouldn't happen with 100 slots unless server is very busy)
      end Add;

      procedure Cleanup_Terminated is
         H : Client_Handler_Access;
      begin
         for I in Handlers'Range loop
            H := Handlers (I);
            if H /= null and then H'Terminated then
               Free_Handler (Handlers (I));
               Handlers (I) := null;
            end if;
         end loop;
      end Cleanup_Terminated;
   end Handler_List;

   task body Client_Handler is
      Client_Ptr : TCP_Connection_Access;
      Client_Id  : Positive;

      --  Callbacks that use the shared protected data
      function Read_Holding_Registers_CB
        (Start_Address : Register_Address;
         Quantity      : Register_Count;
         Values        : out Register_Array) return Status
      is
         Result : Status;
      begin
         Shared_Data.Read_Holding_Registers
           (Natural (Start_Address), Natural (Quantity), Values, Result);
         Put_Line ("[Client" & Client_Id'Image & "] FC03 Read Holding Registers:" &
                   " Start=" & Start_Address'Image & ", Qty=" & Quantity'Image &
                   " -> " & Result'Image);
         return Result;
      end Read_Holding_Registers_CB;

      function Read_Input_Registers_CB
        (Start_Address : Register_Address;
         Quantity      : Register_Count;
         Values        : out Register_Array) return Status
      is
         Result : Status;
      begin
         Shared_Data.Read_Input_Registers
           (Natural (Start_Address), Natural (Quantity), Values, Result);
         Put_Line ("[Client" & Client_Id'Image & "] FC04 Read Input Registers:" &
                   " Start=" & Start_Address'Image & ", Qty=" & Quantity'Image &
                   " -> " & Result'Image);
         return Result;
      end Read_Input_Registers_CB;

      function Write_Single_Register_CB
        (Address : Register_Address;
         Value   : Register_Value) return Status
      is
         Result : Status;
      begin
         Shared_Data.Write_Holding_Register (Natural (Address), Value, Result);
         Put_Line ("[Client" & Client_Id'Image & "] FC06 Write Single Register:" &
                   " Addr=" & Address'Image & ", Value=" & Value'Image &
                   " -> " & Result'Image);
         return Result;
      end Write_Single_Register_CB;

      function Write_Multiple_Registers_CB
        (Start_Address : Register_Address;
         Values        : Register_Array) return Status
      is
         Result : Status;
      begin
         Shared_Data.Write_Multiple_Registers (Natural (Start_Address), Values, Result);
         Put_Line ("[Client" & Client_Id'Image & "] FC16 Write Multiple Registers:" &
                   " Start=" & Start_Address'Image & ", Count=" & Values'Length'Image &
                   " -> " & Result'Image);
         return Result;
      end Write_Multiple_Registers_CB;

      function Read_Coils_CB
        (Start_Address : Coil_Address;
         Quantity      : Coil_Count;
         Values        : out Coil_Array) return Status
      is
         Result : Status;
      begin
         Shared_Data.Read_Coils
           (Natural (Start_Address), Natural (Quantity), Values, Result);
         Put_Line ("[Client" & Client_Id'Image & "] FC01 Read Coils:" &
                   " Start=" & Start_Address'Image & ", Qty=" & Quantity'Image &
                   " -> " & Result'Image);
         return Result;
      end Read_Coils_CB;

      function Read_Discrete_Inputs_CB
        (Start_Address : Coil_Address;
         Quantity      : Coil_Count;
         Values        : out Coil_Array) return Status
      is
         Result : Status;
      begin
         Shared_Data.Read_Discrete_Inputs
           (Natural (Start_Address), Natural (Quantity), Values, Result);
         Put_Line ("[Client" & Client_Id'Image & "] FC02 Read Discrete Inputs:" &
                   " Start=" & Start_Address'Image & ", Qty=" & Quantity'Image &
                   " -> " & Result'Image);
         return Result;
      end Read_Discrete_Inputs_CB;

      function Write_Single_Coil_CB
        (Address : Coil_Address;
         Value   : Coil_Value) return Status
      is
         Result : Status;
      begin
         Shared_Data.Write_Coil (Natural (Address), Value, Result);
         Put_Line ("[Client" & Client_Id'Image & "] FC05 Write Single Coil:" &
                   " Addr=" & Address'Image & ", Value=" & Value'Image &
                   " -> " & Result'Image);
         return Result;
      end Write_Single_Coil_CB;

      function Write_Multiple_Coils_CB
        (Start_Address : Coil_Address;
         Values        : Coil_Array) return Status
      is
         Result : Status;
      begin
         Shared_Data.Write_Multiple_Coils (Natural (Start_Address), Values, Result);
         Put_Line ("[Client" & Client_Id'Image & "] FC15 Write Multiple Coils:" &
                   " Start=" & Start_Address'Image & ", Count=" & Values'Length'Image &
                   " -> " & Result'Image);
         return Result;
      end Write_Multiple_Coils_CB;

      Slave_Cfg : constant Slave_Config :=
        (Mode      => TCP,
         Unit_Id   => 1,
         Callbacks =>
           (Read_Holding_Registers   => Read_Holding_Registers_CB'Unrestricted_Access,
            Read_Input_Registers     => Read_Input_Registers_CB'Unrestricted_Access,
            Write_Single_Register    => Write_Single_Register_CB'Unrestricted_Access,
            Write_Multiple_Registers => Write_Multiple_Registers_CB'Unrestricted_Access,
            Read_Coils               => Read_Coils_CB'Unrestricted_Access,
            Read_Discrete_Inputs     => Read_Discrete_Inputs_CB'Unrestricted_Access,
            Write_Single_Coil        => Write_Single_Coil_CB'Unrestricted_Access,
            Write_Multiple_Coils     => Write_Multiple_Coils_CB'Unrestricted_Access,
            Read_Exception_Status    => null,
            Diagnostics              => null,
            Report_Server_Id         => null,
            Mask_Write_Register      => null,
            Read_Write_Registers     => null));

      Request_Buffer  : Byte_Array (0 .. 259) := [others => 0];
      Request_Length  : Natural;
      Response_Buffer : Byte_Array (0 .. 259) := [others => 0];
      Response_Length : Natural;
      Send_Response   : Boolean;
      Result          : Status;

   begin
      accept Start (Conn : TCP_Connection_Access; Id : Positive) do
         Client_Ptr := Conn;
         Client_Id := Id;
      end Start;

      Put_Line ("[Client" & Client_Id'Image & "] Connected");

      --  Handle client requests
      while State (Client_Ptr.all) = Connected loop
         Receive_Frame (Client_Ptr.all, Request_Buffer, Request_Length, 30000, Result);

         if Result = Timeout then
            Put_Line ("[Client" & Client_Id'Image & "] Timeout, closing");
            exit;
         elsif Result /= Success then
            Put_Line ("[Client" & Client_Id'Image & "] Receive error");
            exit;
         end if;

         Process_Request
           (Slave_Cfg,
            Request_Buffer, Request_Length,
            Response_Buffer, Response_Length,
            Send_Response);

         if Send_Response then
            Send_Frame (Client_Ptr.all, Response_Buffer (0 .. Response_Length - 1), Result);
            if Result /= Success then
               Put_Line ("[Client" & Client_Id'Image & "] Send error");
               exit;
            end if;
         end if;
      end loop;

      Disconnect (Client_Ptr.all);
      Put_Line ("[Client" & Client_Id'Image & "] Disconnected");
      Free_Connection (Client_Ptr);

   exception
      when E : others =>
         Put_Line ("[Client" & Client_Id'Image & "] Exception: " &
                   Ada.Exceptions.Exception_Message (E));
         if Client_Ptr /= null then
            Disconnect (Client_Ptr.all);
            Free_Connection (Client_Ptr);
         end if;
   end Client_Handler;

   Server     : TCP_Connection;
   Client_Ptr : TCP_Connection_Access;
   Port       : Natural := 1502;
   Result     : Status;
   Client_Id  : Positive;
   Handler    : Client_Handler_Access;

begin
   Put_Line ("=== Modbus TCP Slave ===");
   New_Line;

   --  Parse command line for port
   if Ada.Command_Line.Argument_Count >= 1 then
      Port := Natural'Value (Ada.Command_Line.Argument (1));
   end if;

   --  Start listening
   Put_Line ("Starting server on port" & Port'Image & "...");
   Listen (Server, Port, Result);

   if Result /= Success then
      Put_Line ("Failed to start server: " & Last_Error (Server));
      return;
   end if;

   Put_Line ("Server listening. Press Ctrl+C to stop.");
   Put_Line ("Connect with: tcp_master localhost" & Port'Image);
   Put_Line ("Multiple clients can connect simultaneously.");
   New_Line;

   --  Main server loop - accept connections and spawn tasks
   loop
      --  Clean up any terminated tasks
      Handler_List.Cleanup_Terminated;

      --  Allocate new connection on heap (task will take ownership and free it)
      Client_Ptr := new TCP_Connection;
      Accept_Connection (Server, Client_Ptr.all, Result);

      if Result /= Success then
         Put_Line ("Accept failed");
         Free_Connection (Client_Ptr);
         exit;
      end if;

      Client_Counter.Get_Next_Id (Client_Id);

      --  Spawn a new task for this client (task takes ownership of Client_Ptr)
      Handler := new Client_Handler;
      Handler_List.Add (Handler);
      Handler.Start (Client_Ptr, Client_Id);
   end loop;

   Close_Server (Server);
   Put_Line ("Server stopped");

exception
   when E : others =>
      Put_Line ("Exception: " & Ada.Exceptions.Exception_Message (E));
      Close_Server (Server);
end TCP_Slave;
