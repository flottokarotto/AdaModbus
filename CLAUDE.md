# AdaModbus

Ada 2022 Modbus-Bibliothek für Embedded und Desktop-Systeme.

## Projektübersicht

| Eigenschaft | Wert |
|-------------|------|
| Sprache | Ada 2022 |
| Lizenz | MIT |
| Build-System | GPRbuild |
| Entwicklungsplattform | Windows |
| SPARK | Protokoll-Kern verifiziert |

## Unterstützte Protokolle

- **Modbus TCP** - Ethernet-basiert
- **Modbus RTU** - Binär über serielle Schnittstelle
- **Modbus ASCII** - ASCII über serielle Schnittstelle

## Funktionalität

- **Client (Master)** - Anfragen an Modbus-Geräte senden
- **Server (Slave)** - Auf Modbus-Anfragen reagieren

## Architektur

### Schichtenmodell

```
┌─────────────────────────────────────────────────┐
│              C Interface (geplant)              │  ← Callback-basiert
├─────────────────────────────────────────────────┤
│           Tasking Wrapper (Full Runtime)        │  ← Optional
├─────────────────────────────────────────────────┤
│         Transport Backends (TCP, Serial)        │  ← Plattformspezifisch
├─────────────────────────────────────────────────┤
│              Modbus Protokoll-Kern              │  ← ZFP-kompatibel
│         (Generische Transport-Abstraktion)      │
└─────────────────────────────────────────────────┘
```

### Kern (ZFP-kompatibel)

Der Protokoll-Kern muss mit ZFP (Zero Footprint) und Light Runtime funktionieren:

- **Keine Tasking** im Kern
- **Keine Exceptions** im Kern (Fehler über Rückgabewerte)
- **Keine dynamische Allokation** im Kern
- **Generische Transport-Abstraktion** für Zero-Overhead

**Wichtig für Embedded:** Die Standard-Stack-Größe der Light-Runtime (2 KB) ist für
Modbus-Anwendungen zu klein! Die Puffer (256 Bytes ADU, 253 Bytes PDU) plus
verschachtelte Funktionsaufrufe benötigen ca. 2-3 KB Stack pro Aufrufkette.
Empfohlen: mindestens 8 KB Stack via `-Wl,--defsym=__stack_size=8192`.

### Erweiterungen (Full Runtime)

- Tasking-basierte asynchrone Wrapper
- Exception-basierte Fehlerbehandlung (optional)

### C-Interface (geplant)

- Callback-basierte Transport-Abstraktion (function pointers)
- Flache API ohne Ada-spezifische Typen

## SPARK-Architektur

Der Protokoll-Kern ist SPARK-kompatibel für formale Verifikation.

### SPARK Schichtenmodell

```
┌─────────────────────────────────────────────────┐
│  C-API, Transport, Master, Slave (Non-SPARK)    │  ← Runtime-Checks aktiv
├─────────────────────────────────────────────────┤
│  Slave_Generic, Slave_Stubs (SPARK)             │  ← Formal Subprograms
├─────────────────────────────────────────────────┤
│  Protocol Core (SPARK)                          │  ← Verifiziert
│    Protocol, Protocol.RTU/ASCII/TCP             │
├─────────────────────────────────────────────────┤
│  Energy (SPARK)                                 │  ← Verifiziert
│    SunSpec, SG_Ready, Grid_Control              │
├─────────────────────────────────────────────────┤
│  Utilities (SPARK)                              │  ← Pure Functions
│    CRC16, LRC, Utilities                        │
├─────────────────────────────────────────────────┤
│  Base Types (SPARK)                             │  ← Pure
│    Ada_Modbus                                   │
└─────────────────────────────────────────────────┘
```

### SPARK-kompatible Packages

Alle mit `SPARK_Mode => On`:

| Package | Funktion | Status |
|---------|----------|--------|
| `Ada_Modbus` | Basis-Typen (Byte, Status, FC) | ✅ Flow + Proof |
| `Ada_Modbus.CRC16` | CRC-16/Modbus Berechnung | ✅ Flow + Proof |
| `Ada_Modbus.LRC` | LRC Berechnung (ASCII) | ✅ Flow + Proof |
| `Ada_Modbus.Utilities` | Big-Endian Konvertierung | ✅ Flow + Proof |
| `Ada_Modbus.Protocol` | PDU Encoding/Decoding | ✅ Flow + Proof |
| `Ada_Modbus.Protocol.RTU` | RTU ADU Framing | ✅ Flow + Proof |
| `Ada_Modbus.Protocol.ASCII` | ASCII Framing | ✅ Flow + Proof |
| `Ada_Modbus.Protocol.TCP` | TCP/MBAP Framing | ✅ Flow + Proof |
| `Ada_Modbus.Slave_Generic` | Generischer Slave | ✅ Flow |
| `Ada_Modbus.Slave_Stubs` | Default-Handler | ✅ Flow + Proof |
| `Ada_Modbus.Energy.SunSpec` | SunSpec Model-Discovery | ✅ Flow + Proof |
| `Ada_Modbus.Energy.SunSpec.Common` | SunSpec Model 1 | ✅ Flow + Proof |
| `Ada_Modbus.Energy.SunSpec.Inverter` | SunSpec Models 101-103, 160 | ✅ Flow + Proof |
| `Ada_Modbus.Energy.SunSpec.Storage` | SunSpec Model 124 | ✅ Flow + Proof |
| `Ada_Modbus.Energy.SunSpec.Meter` | SunSpec Models 201-204 | ✅ Flow + Proof |
| `Ada_Modbus.Energy.SunSpec.Nameplate` | SunSpec Model 120 | ✅ Flow + Proof |
| `Ada_Modbus.Energy.SunSpec.Settings` | SunSpec Model 121 | ✅ Flow + Proof |
| `Ada_Modbus.Energy.SunSpec.Battery` | SunSpec Model 802 | ✅ Flow + Proof |
| `Ada_Modbus.Energy.SunSpec.DER` | SunSpec Models 701, 704 | ✅ Flow + Proof |
| `Ada_Modbus.Energy.SG_Ready` | Wärmepumpen-Steuerung | ✅ Flow + Proof |
| `Ada_Modbus.Energy.Grid_Control` | §14a EnWG Netzsteuerung | ✅ Flow + Proof |
| `Ada_Modbus.Energy.Go_E` | go-e Charger Wallbox | ✅ Flow + Proof |
| `Ada_Modbus.Energy.Delta_Charger` | Delta AC Max Wallbox | ✅ Flow + Proof |
| `Ada_Modbus.Energy.Kostal` | Kostal Inverter Profil | ✅ Flow + Proof |
| `Ada_Modbus.Energy.KSEM` | Kostal Smart Energy Meter | ✅ Flow + Proof |
| `Ada_Modbus.Gateway` | TCP↔RTU Protokollumsetzung | ✅ Flow + Proof |

### Non-SPARK Packages

Diese Packages haben **aktive Runtime-Checks** im Release-Modus:

| Package | Grund |
|---------|-------|
| `Ada_Modbus.Slave` | Access Types (Callbacks) |
| `Ada_Modbus.Master` | Access Types |
| `Ada_Modbus.Master.Async` | Tasking |
| `Ada_Modbus.C_API` | Function Pointers |
| `Ada_Modbus.Transport` | Plattformspezifisch |
| `Ada_Modbus.Transport.TCP` | Socket-API |
| `Ada_Modbus.Transport.Serial` | OS-API |
| `Ada_Modbus.Transport.TLS` | AWS-Abhängigkeit (OpenSSL) |
| `Ada_Modbus.Transport.TLS_Mbed` | mbedTLS-Bindings (Embedded) |

### Generischer Slave (SPARK)

Für SPARK-Kompatibilität verwendet `Slave_Generic` formale Subprogram-Parameter statt Callbacks:

```ada
generic
   Slave_Unit_Id : Unit_Id;

   with function Read_Holding_Registers
     (Start_Address : Register_Address;
      Quantity      : Register_Count;
      Values        : out Register_Array) return Status
     is Slave_Stubs.Null_Read_Holding_Registers;

   --  ... weitere Funktionscodes ...

package Ada_Modbus.Slave_Generic
  with SPARK_Mode => On
is
   procedure Process_RTU_Request
     (Request_Frame   : Byte_Array;
      Request_Length  : Natural;
      Response_Frame  : out Byte_Array;
      Response_Length : out Natural;
      Send_Response   : out Boolean);
end Ada_Modbus.Slave_Generic;
```

### Anwendungsbeispiel (SPARK)

```ada
--  Eigene Handler implementieren
function My_Read_Registers
  (Start_Address : Register_Address;
   Quantity      : Register_Count;
   Values        : out Register_Array) return Status
is
begin
   for I in 0 .. Natural (Quantity) - 1 loop
      Values (I) := My_Storage (Natural (Start_Address) + I);
   end loop;
   return Success;
end My_Read_Registers;

--  Generischen Slave instanziieren
package My_Slave is new Ada_Modbus.Slave_Generic
  (Slave_Unit_Id          => 1,
   Read_Holding_Registers => My_Read_Registers);
```

### gnatprove Verwendung

Für SPARK-Verifikation wird `adamodbus.gpr` verwendet. gnatprove analysiert nur
Packages mit `SPARK_Mode => On` (Non-SPARK Packages werden übersprungen):

```bash
# Als Dependency hinzufügen
alr with gnatprove

# Check-Modus (schnell)
alr exec -- gnatprove -P adamodbus.gpr --mode=check

# Flow-Analyse (Datenfluss)
alr exec -- gnatprove -P adamodbus.gpr --mode=flow

# Proof-Analyse (vollständig)
alr exec -- gnatprove -P adamodbus.gpr --mode=prove --level=1
```

### Build-Modi

```bash
# Kern-Bibliothek (ohne TLS, für Embedded)
alr with adamodbus
alr build

# Mit TLS-Unterstützung (separates Crate)
alr with adamodbus_tls
alr build

# Release-Modus
alr build -- -XADAMODBUS_BUILD_MODE=release
```

### Alire Crates

| Crate | Beschreibung |
|-------|--------------|
| `adamodbus` | Kern-Bibliothek (Protokoll-Kern SPARK-verifiziert) |
| `adamodbus_tls` | TLS-Transport (benötigt AWS/OpenSSL) |
| `adamodbus_tls_mbed` | TLS-Transport für Embedded (benötigt mbedTLS) |

### GPR-Projekte

| Projekt | Beschreibung |
|---------|--------------|
| `adamodbus.gpr` | Kern-Bibliothek (SPARK-verifizierbar) |
| `tls/adamodbus_tls.gpr` | TLS-Transport-Extension (AWS/OpenSSL) |
| `tls_mbed/adamodbus_tls_mbed.gpr` | TLS-Transport für MCU (mbedTLS) |

### Lokale TLS-Entwicklung

Für lokale Entwicklung am TLS-Crate (bevor `adamodbus` im Alire-Index ist):

```bash
cd tls
alr pin adamodbus --use=..
alr build
```

### TLS mbedTLS für Embedded

Für Embedded-Systeme (Cortex-M, etc.) mit mbedTLS statt AWS/OpenSSL:

```bash
cd tls_mbed
alr pin adamodbus --use=..
alr build
```

**Voraussetzungen:**
- mbedTLS Library für Zielplattform kompiliert
- Konfiguration in `tls_mbed/config/mbedtls_config.h`
- Ca. 50-80 KB Flash, 15-20 KB RAM pro TLS-Session

**Client-Beispiel:**
```ada
--  TLS-Client verbinden (Zertifikat-Modus)
Connection : TLS_Connection;
Config : TLS_Config := (
   Mode            => Auth_Certificate,
   CA_Certificate  => CA_Cert'Address,
   CA_Cert_Len     => CA_Cert'Length,
   Verify_Peer     => True,
   others          => <>);

Connect (Connection, "192.168.1.100", 802, Config, 5000, Result);
Send_Frame (Connection, Request, Result);
Receive_Frame (Connection, Response, Resp_Len, 1000, Result);
Disconnect (Connection);
```

**Server-Beispiel:**
```ada
--  TLS-Server starten (Modbus Slave)
Server : TLS_Server;
Client : TLS_Connection;
Config : TLS_Config := (
   Mode            => Auth_Certificate,
   Certificate     => Server_Cert'Address,
   Certificate_Len => Server_Cert'Length,
   Private_Key     => Server_Key'Address,
   Private_Key_Len => Server_Key'Length,
   Verify_Peer     => False,  --  Client-Zertifikat optional
   others          => <>);

Listen (Server, 802, Config, Result);
loop
   Accept_Connection (Server, Client, 30000, Result);
   if Result = Success then
      --  Handle client requests...
      Disconnect (Client);
   end if;
end loop;
Close_Server (Server);
```

**PSK-Beispiel (ohne Zertifikate):**
```ada
--  Pre-Shared Key für ressourcenbeschränkte Geräte
--  Spart Flash/RAM im Vergleich zu X.509-Zertifikaten
PSK_Secret : constant Byte_Array := (16#01#, 16#23#, ...);  --  16-32 Bytes
PSK_ID     : constant Byte_Array := "modbus_client" & (1 => 0);  --  null-terminated

Config : TLS_Config := (
   Mode             => Auth_PSK,
   PSK              => PSK_Secret'Address,
   PSK_Len          => PSK_Secret'Length,
   PSK_Identity     => PSK_ID'Address,
   PSK_Identity_Len => PSK_ID'Length - 1,  --  ohne null-Terminator
   others           => <>);

Connect (Connection, "192.168.1.100", 802, Config, 5000, Result);
```

**Fehlerbehandlung:**
```ada
--  Detaillierte Fehleranalyse nach fehlgeschlagener Operation
Connect (Connection, Host, Port, Config, Timeout, Result);
if Result /= Success then
   --  Fehlerkategorie für Benutzer-Feedback
   case Last_Error (Connection) is
      when Certificate_Expired =>
         Put_Line ("Zertifikat abgelaufen");
      when Certificate_Verify_Failed =>
         Put_Line ("Zertifikatsprüfung fehlgeschlagen");
      when Connect_Failed =>
         Put_Line ("Verbindung fehlgeschlagen");
      when Handshake_Failure =>
         Put_Line ("TLS-Handshake fehlgeschlagen");
      when others =>
         Put_Line ("Fehler: " & TLS_Error_Category'Image (Last_Error (Connection)));
   end case;

   --  Für Debugging: Raw mbedTLS Error Code
   declare
      Code : constant Integer := Last_Error_Code (Connection);
   begin
      --  Negative Hex-Werte, z.B. -0x7780 = Alert received
      Put_Line ("mbedTLS Error: " & Integer'Image (Code));
   end;
end if;
```

**TLS_Error_Category Werte:**
- `No_Error` - Kein Fehler
- `Socket_Error`, `Connect_Failed`, `DNS_Error` - Netzwerk
- `Certificate_Parse_Error`, `Certificate_Verify_Failed`, `Certificate_Expired` - Zertifikate
- `Key_Parse_Error`, `Key_Invalid_Format`, `Key_Password_Required` - Private Keys
- `Handshake_Timeout`, `Handshake_Failure`, `Alert_Received` - TLS Handshake
- `PSK_Identity_Unknown`, `PSK_Key_Error` - PSK
- `Memory_Error`, `RNG_Error`, `Internal_Error` - Runtime

**QEMU-Test:**
```bash
cd examples/embedded/cortex_m4_tls
alr build -- -XMBEDTLS_MODE=test
./run_ci_test.sh
```

| Modus | Flags | Beschreibung |
|-------|-------|--------------|
| debug | `-g -O0` | Debug-Info, keine Optimierung |
| release | `-O2 -gnatn` | Optimiert, Inlining, Runtime-Checks aktiv |
| release (SPARK-Core) | `-O2 -gnatn -gnatp` | Zusätzlich: keine Runtime-Checks |

**Hinweis**: `-gnatp` wird nur für SPARK-verifizierte Dateien in `src/core/` verwendet.
Non-SPARK Code (Transport, C-API, Master, Slave) behält Runtime-Checks im Release-Modus.

### Verifikationsstatus

**100% der SPARK-Checks bewiesen** (gnatprove -P adamodbus.gpr --level=1):

| Kategorie | Anzahl | Prozent |
|-----------|--------|---------|
| Flow-Analyse | 354 | 30% |
| Provers (CVC5/Z3) | 816 | 69% |
| Justified | 7 | 1% |
| Unbewiesen | 0 | 0% |
| **Gesamt** | **1177** | **100%** |

- **Flow-Analyse**: ✅ Bestanden - Keine uninitialisierten Variablen
- **Proof-Analyse**: ✅ 100% bewiesen mit Preconditions und Loop-Invarianten
- **Runtime-Checks**: Für SPARK-Core mit `-gnatp` deaktiviert (Release-Modus)

### Preconditions

Alle Protocol-Funktionen haben SPARK-Preconditions für sichere Bounds:

```ada
--  Encode: Array-Längen-Grenzen
procedure Encode_Write_Multiple_Registers_Request (...)
  with Pre => Values'Length >= 1 and then Values'Length <= 123;

procedure Encode_Write_Multiple_Coils_Request (...)
  with Pre => Values'Length >= 1 and then Values'Length <= 1968;

--  Decode: PDU-Längen + Array-Bounds (für 100% Proof)
procedure Decode_Read_Registers_Response (...)
  with Pre => Length <= Max_PDU_Size
              and then Values'Length <= 125;

procedure Decode_Read_Bits_Response (...)
  with Pre => Length <= Max_PDU_Size
              and then Values'Length <= 2000;

--  Frame-Building: ADU-Größen-Grenzen
procedure Build_Frame (...)  --  RTU
  with Pre => PDU_Length <= Max_PDU_Size
              and then PDU_Length + 3 <= Max_ADU_Size,
       Post => ADU_Length = PDU_Length + 3;
```

### Loop-Invarianten

Schleifen haben Invarianten für Index-Bounds:

```ada
--  Register-Encoding
for I in Values'Range loop
   pragma Loop_Invariant (Idx = 6 + 2 * (I - Values'First));
   pragma Loop_Invariant (Idx + 1 < Max_PDU_Size);
   Buffer (Idx) := Val (0);
   Buffer (Idx + 1) := Val (1);
   Idx := Idx + 2;
end loop;

--  Coil-Packing (Bit-Manipulation)
for I in Values'Range loop
   pragma Loop_Invariant (Bit_Idx <= 7);
   pragma Loop_Invariant (6 + (I - Values'First) / 8 < Max_PDU_Size);
   if Values (I) then
      Byte_Val := Byte_Val or Byte (2 ** Bit_Idx);
   end if;
   ...
end loop;
```

### Unit_Id Validierung

RTU/TCP/ASCII Parse-Funktionen validieren Unit_Id Range (0..247):

```ada
--  In Parse_Frame (RTU/TCP/ASCII)
if ADU (0) > 247 then
   Result := Frame_Error;
   return;
end if;
Slave := Unit_Id (ADU (0));  --  Jetzt sicher
```

## Design-Entscheidungen

### Error-Handling

**Status-Enum Rückgabewert** - jede Funktion gibt expliziten Status zurück.

```ada
type Status is
  (Success,
   Timeout,
   CRC_Error,
   LRC_Error,
   Frame_Error,
   Invalid_Response,
   Invalid_Request,
   Buffer_Too_Small,
   Not_Implemented,
   -- Modbus Exception Codes (vom Server)
   Exception_Illegal_Function,      -- 01
   Exception_Illegal_Address,       -- 02
   Exception_Illegal_Value,         -- 03
   Exception_Slave_Failure,         -- 04
   Exception_Acknowledge,           -- 05
   Exception_Slave_Busy,            -- 06
   Exception_Gateway_Path,          -- 10
   Exception_Gateway_Target);       -- 11
```

### Basis-Datentypen

Eigene Typen als Wrapper über Standard-Typen für Typsicherheit (definiert in `Ada_Modbus`):

```ada
--  In Ada_Modbus (Haupt-Package)

-- Modbus verwendet Big-Endian (Network Byte Order)

-- Byte und Arrays
type Byte is new Interfaces.Unsigned_8;
type Byte_Array is array (Natural range <>) of Byte;

-- Register (16-bit)
type Register_Value is new Interfaces.Unsigned_16;
type Register_Address is new Interfaces.Unsigned_16;
subtype Register_Count is Natural range 1 .. 125;  -- Max per Modbus spec
type Register_Array is array (Natural range <>) of Register_Value;

-- Coils (1-bit)
subtype Coil_Value is Boolean;
type Coil_Address is new Interfaces.Unsigned_16;
subtype Coil_Count is Natural range 1 .. 2000;  -- Max per Modbus spec
type Coil_Array is array (Natural range <>) of Coil_Value
  with Pack;

-- Adressen
type Unit_Id is new Interfaces.Unsigned_8 range 0 .. 247;
Broadcast_Address : constant Unit_Id := 0;

-- Function Codes
FC_Read_Coils               : constant Byte := 16#01#;
FC_Read_Discrete_Inputs     : constant Byte := 16#02#;
FC_Read_Holding_Registers   : constant Byte := 16#03#;
FC_Read_Input_Registers     : constant Byte := 16#04#;
FC_Write_Single_Coil        : constant Byte := 16#05#;
FC_Write_Single_Register    : constant Byte := 16#06#;
FC_Write_Multiple_Coils     : constant Byte := 16#0F#;
FC_Write_Multiple_Registers : constant Byte := 16#10#;
```

### Buffer-Management

**Generisch konfigurierbar** - Buffer-Größe als Generic-Parameter:

```ada
generic
   Max_PDU_Size : Positive := 256;  -- Default: max Modbus PDU
package Ada_Modbus.Protocol.Generic_Buffer is
   subtype PDU_Buffer is Byte_Array (1 .. Max_PDU_Size);
   -- ...
end Ada_Modbus.Protocol.Generic_Buffer;
```

### CRC/LRC Berechnung

**Austauschbar mit Default** - Software-Implementierung eingebaut, aber ersetzbar für Hardware-CRC:

```ada
generic
   with function Calculate_CRC (Data : Byte_Array) return Unsigned_16
     is Default_CRC_16;  -- Default Software-CRC
package Ada_Modbus.Protocol.RTU is
   -- ...
end Ada_Modbus.Protocol.RTU;
```

### Adressierung

**Flexibel** - Default-Adresse in Client-Instanz, Override pro Request möglich:

```ada
type Client_Config is record
   Default_Server : Slave_Address := 1;
   Default_Timeout_Ms : Natural := 1000;
end record;

-- Verwendet Default-Adresse
function Read_Registers (Client : ...) return Status;

-- Override mit expliziter Adresse
function Read_Registers (Client : ...; Server : Slave_Address) return Status;
```

### Timeout-Handling

**Zeit-Callback** mit Windows-Default für Entwicklung:

```ada
-- Callback-Typ für Zeitabfrage
type Get_Tick_Ms_Callback is access function return Unsigned_32;

-- Default für Windows (QueryPerformanceCounter oder GetTickCount)
function Default_Get_Tick_Ms return Unsigned_32;

generic
   Get_Tick_Ms : Get_Tick_Ms_Callback := Default_Get_Tick_Ms'Access;
package Ada_Modbus.Timing is
   function Has_Elapsed (Start_Ms, Timeout_Ms : Unsigned_32) return Boolean;
end Ada_Modbus.Timing;
```

### Byte-Order

**Automatische Konvertierung** - Lib behandelt Big-Endian (Modbus-Standard) transparent:

```ada
-- Interne Hilfsfunktionen
function To_Big_Endian (Value : Unsigned_16) return Byte_Array;
function From_Big_Endian (Data : Byte_Array) return Unsigned_16;
```

### RTU Inter-Frame Timing

**Optional/Konfigurierbar** - 3.5 Character Time Prüfung kann aktiviert werden:

```ada
type RTU_Config is record
   Check_Inter_Frame_Delay : Boolean := False;  -- Optional
   Baud_Rate : Positive := 9600;  -- Für Timing-Berechnung
end record;
```

## Server Callback-Interface

Der Server verwendet ein Callback-basiertes Interface. Für jeden Modbus-Funktionscode kann ein Callback registriert werden, der bei entsprechenden Requests aufgerufen wird.

### Callback-Typen (Konzept)

```ada
-- Callback für Read Holding Registers (FC 03)
type Read_Holding_Registers_Callback is access function
  (Start_Address : Register_Address;
   Quantity      : Register_Count;
   Values        : out Register_Array) return Status;

-- Callback für Write Single Register (FC 06)
type Write_Single_Register_Callback is access function
  (Address : Register_Address;
   Value   : Register_Value) return Status;

-- Callback für Read Coils (FC 01)
type Read_Coils_Callback is access function
  (Start_Address : Coil_Address;
   Quantity      : Coil_Count;
   Values        : out Coil_Array) return Status;

-- usw. für alle Funktionscodes
```

### Server-Konfiguration (Konzept)

```ada
type Server_Callbacks is record
   Read_Coils              : Read_Coils_Callback;
   Read_Discrete_Inputs    : Read_Discrete_Inputs_Callback;
   Read_Holding_Registers  : Read_Holding_Registers_Callback;
   Read_Input_Registers    : Read_Input_Registers_Callback;
   Write_Single_Coil       : Write_Single_Coil_Callback;
   Write_Single_Register   : Write_Single_Register_Callback;
   Write_Multiple_Coils    : Write_Multiple_Coils_Callback;
   Write_Multiple_Registers: Write_Multiple_Registers_Callback;
end record;

-- Nicht registrierte Callbacks (null) -> Exception Code 01 (Illegal Function)
```

### Anwendungsbeispiel (Konzept)

```ada
-- Benutzer implementiert Callback
function My_Read_Registers
  (Start   : Register_Address;
   Count   : Register_Count;
   Values  : out Register_Array) return Status
is
begin
   -- Daten aus eigenem Speicher/Hardware lesen
   for I in 0 .. Count - 1 loop
      Values (I) := My_Data_Store (Start + I);
   end loop;
   return Success;
end My_Read_Registers;

-- Callback beim Server registrieren
Server_Config.Read_Holding_Registers := My_Read_Registers'Access;
```

### Vorteile

- **ZFP-kompatibel**: Access-to-Subprogram funktioniert ohne Runtime
- **C-Interface-freundlich**: Direkte Abbildung auf function pointers
- **Flexibel**: Benutzer entscheidet, woher Daten kommen (RAM, Flash, Hardware, ...)
- **Lazy**: Nur implementierte Funktionscodes werden unterstützt

## Client Callback-Interface

Der Client unterstützt sowohl synchrone als auch asynchrone (Callback-basierte) Operationen.

### Synchrone API (Blocking)

```ada
-- Einfache synchrone Funktion - blockiert bis Response oder Timeout
function Read_Holding_Registers
  (Client        : in out Modbus_Client;
   Server_Address: Slave_Address;
   Start_Address : Register_Address;
   Quantity      : Register_Count;
   Values        : out Register_Array;
   Timeout_Ms    : Natural := 1000) return Status;
```

### Asynchrone API (Callback-basiert)

Für nicht-blockierende Operationen, besonders nützlich bei:
- Polling mehrerer Geräte
- Event-Loop-Integration
- Tasking-basierte Anwendungen

```ada
-- Response-Callback Typen
type Response_Status is (Success, Timeout, Error, Exception_Response);

type Read_Registers_Response_Callback is access procedure
  (Status        : Response_Status;
   Server_Address: Slave_Address;
   Values        : Register_Array;
   Exception_Code: Modbus_Exception_Code);

type Write_Response_Callback is access procedure
  (Status        : Response_Status;
   Server_Address: Slave_Address;
   Exception_Code: Modbus_Exception_Code);

-- Generischer Completion-Callback für alle Operationen
type Completion_Callback is access procedure
  (Request_Id    : Request_Handle;
   Status        : Response_Status;
   Exception_Code: Modbus_Exception_Code);
```

### Asynchrone Request-Funktionen

```ada
-- Startet Request, kehrt sofort zurück
function Read_Holding_Registers_Async
  (Client        : in out Modbus_Client;
   Server_Address: Slave_Address;
   Start_Address : Register_Address;
   Quantity      : Register_Count;
   On_Response   : Read_Registers_Response_Callback;
   Timeout_Ms    : Natural := 1000) return Request_Handle;

-- Muss regelmäßig aufgerufen werden um Responses zu verarbeiten
procedure Process_Pending
  (Client : in out Modbus_Client);
```

### Anwendungsbeispiel Async

```ada
-- Callback-Handler
procedure Handle_Response
  (Status  : Response_Status;
   Address : Slave_Address;
   Values  : Register_Array;
   Exc     : Modbus_Exception_Code)
is
begin
   if Status = Success then
      -- Verarbeite empfangene Daten
      Process_Data (Values);
   else
      -- Fehlerbehandlung
      Log_Error (Address, Status, Exc);
   end if;
end Handle_Response;

-- Request starten
Handle := Read_Holding_Registers_Async
  (Client, Server => 1, Start => 0, Quantity => 10,
   On_Response => Handle_Response'Access);

-- In Main-Loop
loop
   Client.Process_Pending;  -- Verarbeitet eingehende Responses
   -- ... andere Aufgaben ...
end loop;
```

### Broadcast-Unterstützung

```ada
-- Broadcast (Adresse 0) - keine Response erwartet
procedure Write_Single_Register_Broadcast
  (Client  : in out Modbus_Client;
   Address : Register_Address;
   Value   : Register_Value;
   On_Sent : Completion_Callback := null);  -- Optional: Callback wenn gesendet
```

### Client-Modi

| Modus | Beschreibung | Use Case |
|-------|--------------|----------|
| **Synchron** | Blockiert bis Response/Timeout | Einfache Anwendungen, Scripts |
| **Async + Polling** | Non-blocking mit `Process_Pending` | Single-Thread, Event-Loop |
| **Async + Tasking** | Separater Task für Responses | Full Runtime, Concurrent |

## Package-Struktur

```
Ada_Modbus                         -- Haupt-Package, gemeinsame Typen
├── Ada_Modbus.Protocol            -- PDU Encoding/Decoding
│   ├── Ada_Modbus.Protocol.RTU
│   ├── Ada_Modbus.Protocol.ASCII
│   └── Ada_Modbus.Protocol.TCP
├── Ada_Modbus.Master              -- Master-Funktionalität (generisch)
│   └── Ada_Modbus.Master.Async    -- Non-blocking Async API
├── Ada_Modbus.Slave               -- Slave-Funktionalität (Callbacks)
├── Ada_Modbus.Transport           -- Transport-Abstraktion
│   ├── Ada_Modbus.Transport.Serial   -- Serielle Schnittstelle
│   ├── Ada_Modbus.Transport.TCP      -- TCP/IP Sockets
│   └── Ada_Modbus.Transport.TLS      -- TLS/SSL (AWS, Port 802)
├── Ada_Modbus.Energy              -- Energy Management
│   ├── Ada_Modbus.Energy.SG_Ready    -- Wärmepumpen (SG-Ready)
│   ├── Ada_Modbus.Energy.Grid_Control -- §14a EnWG Netzsteuerung
│   ├── Ada_Modbus.Energy.Go_E        -- go-e Charger Wallbox
│   ├── Ada_Modbus.Energy.Delta_Charger -- Delta AC Max Wallbox
│   ├── Ada_Modbus.Energy.Kostal      -- Kostal Inverter (SunSpec)
│   ├── Ada_Modbus.Energy.KSEM        -- Kostal Smart Energy Meter (SunSpec)
│   └── Ada_Modbus.Energy.SunSpec     -- SunSpec Alliance Profile
│       ├── Ada_Modbus.Energy.SunSpec.Common   -- Model 1: Geräteinformation
│       ├── Ada_Modbus.Energy.SunSpec.Inverter -- Models 101-103, 160: Wechselrichter + MPPT
│       ├── Ada_Modbus.Energy.SunSpec.Storage  -- Model 124: Batteriespeicher
│       ├── Ada_Modbus.Energy.SunSpec.Meter    -- Models 201-204: Energiezähler
│       ├── Ada_Modbus.Energy.SunSpec.Nameplate -- Model 120: Nennleistung
│       ├── Ada_Modbus.Energy.SunSpec.Settings  -- Model 121: Konfiguration
│       ├── Ada_Modbus.Energy.SunSpec.Battery   -- Model 802: Batterie erweitert
│       └── Ada_Modbus.Energy.SunSpec.DER      -- Models 701, 704: DER Messungen/Steuerung
├── Ada_Modbus.Gateway              -- TCP↔RTU Protokollumsetzung
└── Ada_Modbus.C_API               -- C-Bindings
```

## Energy Management

Energy-Management-Packages für Smart-Grid-Integration (SPARK-verifiziert).

### SunSpec (Offizieller Standard)

SunSpec Alliance Modbus-Profile für PV-Anlagen und Speicher.

```ada
with Ada_Modbus.Energy.SunSpec; use Ada_Modbus.Energy.SunSpec;

--  SunSpec-Gerät erkennen
Encode_Check_SunSpec_Request (Base_Address => 40000, Buffer, Length);
--  Response: "SunS" Identifier prüfen

--  Model-Header lesen (ID + Länge)
Encode_Read_Model_Header_Request (Base_Address, Offset => 2, Buffer, Length);
Decode_Model_Header_Response (Buffer, Length, Header, Result);
--  Header.ID = 1 (Common), Header.Length = 66

--  Scale Factor anwenden
Scaled_Value := Apply_Scale (Raw_Value, Scale_Factor);
```

**SunSpec Models:**

| Model | Package | Beschreibung |
|-------|---------|--------------|
| 1 | `SunSpec.Common` | Geräteinformation (Hersteller, Modell, Version) |
| 101-103 | `SunSpec.Inverter` | Wechselrichter (AC/DC, Leistung, Energie) |
| 120 | `SunSpec.Nameplate` | Nennleistung (DER-Typ, Kapazitäten) |
| 121 | `SunSpec.Settings` | Konfiguration (Betriebsgrenzen) |
| 122 | `SunSpec.Settings` | Messungen |
| 123 | `SunSpec.Settings` | Sofortsteuerung |
| 124 | `SunSpec.Storage` | Batteriespeicher (SOC, Kapazität, Steuerung) |
| 160 | `SunSpec.Inverter` | MPPT (per-String DC-Messungen) |
| 201-204 | `SunSpec.Meter` | Energiezähler (1P, Split, 3P-Wye, 3P-Delta) |
| 701 | `SunSpec.DER` | DER AC-Messungen |
| 704 | `SunSpec.DER` | DER AC-Steuerung (Leistungsbegrenzung) |
| 802 | `SunSpec.Battery` | Batterie erweitert (Zellspannung, Status) |

### SG-Ready (Branchenlabel)

Wärmepumpen-Steuerung nach BWP-Empfehlung.

```ada
with Ada_Modbus.Energy.SG_Ready;

--  Generisch mit eigener Kommunikation
package My_SG_Ready is new Ada_Modbus.Energy.SG_Ready
  (Write_Coil => My_Write_Coil);

--  Betriebszustand setzen
My_SG_Ready.Set_Operating_State (State => Enhanced_Operation);
```

**Betriebszustände:**
- `Blocked` - EVU-Sperre (harter Abschaltbefehl)
- `Normal` - Normalbetrieb
- `Enhanced` - Verstärkter Betrieb (PV-Überschuss)
- `Forced` - Erzwungener Betrieb (maximale Leistung)

### §14a EnWG Grid Control

Netzdienliche Steuerung gemäß deutschem Energiewirtschaftsgesetz.

```ada
with Ada_Modbus.Energy.Grid_Control;

package My_Grid is new Ada_Modbus.Energy.Grid_Control
  (Write_Register => My_Write_Register);

--  Leistungsbegrenzung setzen (0-100%)
My_Grid.Set_Power_Limit (Limit_Percent => 60);

--  Netzzustand abfragen
State := My_Grid.Get_Grid_State;
```

### go-e Charger (Wallbox)

Modbus TCP Interface für go-e Charger Wallboxen (Firmware 040+).

```ada
with Ada_Modbus.Energy.Go_E; use Ada_Modbus.Energy.Go_E;

--  Ladestrom setzen (6-32A)
Encode_Set_Ampere_Request (Ampere => 16, Buffer, Length);

--  Ladevorgang starten/stoppen
Encode_Set_Force_State_Request (Force_On, Buffer, Length);   -- Start
Encode_Set_Force_State_Request (Force_Off, Buffer, Length);  -- Stop
Encode_Set_Force_State_Request (Neutral, Buffer, Length);    -- Auto

--  Status auslesen
Encode_Read_Car_State_Request (Buffer, Length);
Decode_Car_State_Response (Buffer, Length, State, Result);
--  State: Idle, Charging, Waiting_For_Car, Charge_Complete
```

**Dashboard-Beispiel:**
```bash
# Interaktives Terminal-Dashboard
bin/go_e_dashboard 192.168.1.50

# Steuerung:
#   +/-  Stromstärke ändern (6-32A)
#   s    Laden starten
#   p    Laden pausieren
#   n    Neutral (Auto-Modus)
#   q    Beenden
```

**Hinweis:** Modbus TCP muss in der go-e App aktiviert werden!

## Verzeichnisstruktur

```
AdaModbus/
├── src/
│   ├── core/           -- Protokoll + Master/Slave (gemischt SPARK/Non-SPARK)
│   ├── transport/      -- Transport-Backends (Non-SPARK)
│   ├── energy/         -- Energy Management (SPARK-verifiziert)
│   └── c_api/          -- C-Interface (Non-SPARK)
├── tls/                -- Separates Alire-Crate: adamodbus_tls
│   ├── alire.toml
│   ├── adamodbus_tls.gpr
│   └── src/            -- TLS-Transport (AWS, Non-SPARK)
├── examples/
├── tests/
├── adamodbus.gpr       -- Kern-Bibliothek (SPARK-verifizierbar)
├── alire.toml          -- Alire-Crate: adamodbus
├── CLAUDE.md
└── LICENSE
```

## Zielplattformen

- **Primär**: Embedded-Systeme mit ZFP/Light Runtime
- **Sekundär**: Desktop (Windows, Linux) mit Full Runtime
- **Transport-Wrapper**: BSD Sockets, Windows Sockets, HAL-basiert

## Coding-Konventionen

### Prinzipien

- **KISS** - Keep It Simple, Stupid: Einfachste Lösung bevorzugen
- **DRY** - Don't Repeat Yourself: Keine Code-Duplikation
- **Pragmatisch** - Funktionalität vor Perfektion

### Stil

- Ada 2022 Features bevorzugen
- Keine impliziten Abhängigkeiten zur Standard-Runtime im Kern
- Generics für Transport-Abstraktion
- Klare Trennung zwischen ZFP-kompatiblem Kern und Full Runtime Erweiterungen
- Code-Kommentare in Englisch
- Keine überflüssigen Abstraktionen
- Flache Hierarchien wo möglich

## Testing

### Framework

**AUnit** für Unit-Tests (via Alire: `alr with aunit`).

### Verzeichnisstruktur Tests

```
tests/
├── unit/                    -- AUnit Tests
│   ├── test_crc.ads/adb         -- CRC-16 Tests
│   ├── test_lrc.ads/adb         -- LRC Tests
│   ├── test_utilities.ads/adb   -- Byte-Konvertierung
│   ├── test_pdu.ads/adb         -- PDU Encoding/Decoding
│   ├── test_rtu.ads/adb         -- RTU Framing
│   ├── test_ascii.ads/adb       -- ASCII Framing
│   ├── test_tcp.ads/adb         -- TCP/MBAP Framing
│   ├── test_slave.ads/adb       -- Slave Callbacks
│   ├── test_master.ads/adb      -- Master Requests
│   ├── test_async.ads/adb       -- Async API
│   ├── test_sunspec.ads/adb     -- SunSpec Energy
│   └── test_runner.adb          -- Haupt-Test-Runner
├── integration/             -- Integration Tests (pymodbus)
│   ├── integration_tests.adb    -- Alle FC gegen Simulator
│   ├── modbus_simulator.py      -- Python Modbus Server
│   └── README.md
└── aunit_tests.gpr              -- GPR für Tests
```

**236 Unit-Tests** für alle Funktionscodes und Protokollschichten.

### Testprogramme (Windows)

Zwei ausführbare Programme für manuelle Tests und Demonstration:

```
examples/
├── master_demo/
│   ├── master_demo.adb      -- Modbus Master/Client Testprogramm
│   └── master_demo.gpr
└── slave_demo/
    ├── slave_demo.adb       -- Modbus Slave/Server Testprogramm
    └── slave_demo.gpr
```

#### Master Demo (Client)

```ada
-- Funktionen:
-- - Verbindung zu Slave (TCP oder Serial)
-- - Interaktive Kommandozeile oder Skript-Modus
-- - Alle Funktionscodes testen
-- - Response-Anzeige

-- Beispiel-Aufruf:
-- master_demo --tcp 127.0.0.1:502
-- master_demo --serial COM3 --baud 9600
```

#### Slave Demo (Server)

```ada
-- Funktionen:
-- - Simulierter Modbus Slave
-- - Konfigurierbare Register/Coil-Bereiche
-- - Logging aller Requests
-- - Einfache Daten-Simulation

-- Beispiel-Aufruf:
-- slave_demo --tcp 502 --unit-id 1
-- slave_demo --serial COM3 --baud 9600
```

### Test-Strategie

1. **Unit-Tests (AUnit)**: Protokoll-Kern isoliert testen
   - PDU Encoding/Decoding
   - CRC/LRC Berechnung
   - Frame-Parsing

2. **Integration-Tests**: Master ↔ Slave Demo
   - Loopback über TCP localhost
   - Alle Funktionscodes durchspielen

3. **Manuell**: Mit echter Modbus-Hardware oder Simulatoren

## Unterstützte Modbus Funktionscodes

Alle Funktionscodes sind für Master und Slave implementiert:

| Code | Funktion | Status |
|------|----------|--------|
| 01 | Read Coils | ✅ |
| 02 | Read Discrete Inputs | ✅ |
| 03 | Read Holding Registers | ✅ |
| 04 | Read Input Registers | ✅ |
| 05 | Write Single Coil | ✅ |
| 06 | Write Single Register | ✅ |
| 07 | Read Exception Status | ✅ |
| 08 | Diagnostics | ✅ |
| 15 | Write Multiple Coils | ✅ |
| 16 | Write Multiple Registers | ✅ |
| 17 | Report Server ID | ✅ |
| 22 | Mask Write Register | ✅ |
| 23 | Read/Write Multiple Registers | ✅ |
