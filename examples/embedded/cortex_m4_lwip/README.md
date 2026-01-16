# Embedded Modbus Beispiele

Vollständige Modbus-Beispiele für ARM Cortex-M Mikrocontroller mit LwIP TCP/IP-Stack.

## Schnellstart mit QEMU (ohne Netzwerk)

```bash
# 1. ARM Cross-Compiler installieren (falls nicht vorhanden)
alr toolchain --select gnat_arm_elf

# 2. Beispiel bauen (Loopback-Test)
cd examples/embedded/cortex_m4_lwip
gprbuild -P embedded_example.gpr

# 3. Mit QEMU ausführen
qemu-system-arm -M lm3s6965evb -nographic -semihosting \
  -kernel bin/main_loopback.elf
```

**Erwartete Ausgabe:**
```
=== Modbus Loopback Test ===

Test 1: Read Holding Registers (FC03)... PASS
Test 2: Write Single Register (FC06)... PASS
Test 3: Read Input Registers (FC04)... PASS
Test 4: Write Multiple Registers (FC16)... PASS
Test 5: Read Coils (FC01)... PASS
Test 6: Write Single Coil (FC05)... PASS
Test 7: Illegal Address Exception... PASS

=== Test Summary ===
Passed: 7
Failed: 0
All tests PASSED!
```

QEMU beenden: `Ctrl+A`, dann `X`

## Build mit LwIP (Netzwerk)

LwIP ist als Git-Submodul eingebunden:

```bash
# 1. Submodul initialisieren
git submodule update --init --recursive

# 2. Mit LwIP bauen
gprbuild -P embedded_example.gpr -XLWIP=enabled

# 3. Alle Programme werden gebaut:
#    - bin/main_loopback.elf  (Loopback-Test)
#    - bin/main_slave.elf     (TCP Slave/Server)
#    - bin/main_master.elf    (TCP Master/Client)
```

## Beispiel-Programme

| Programm | Beschreibung | Netzwerk |
|----------|--------------|----------|
| `main_loopback.adb` | Loopback-Test ohne Netzwerk | Nein |
| `main_slave.adb` | TCP Slave/Server mit LwIP | Ja |
| `main_master.adb` | TCP Master/Client mit LwIP | Ja |

## Targets

### lm3s6965evb (Standard)

QEMU-emuliertes Board mit Cortex-M3. Ideal zum Testen.

```bash
gprbuild -P embedded_example.gpr -XTARGET=lm3s6965evb
```

### stm32f4

STM32F4-Discovery Board mit Cortex-M4F.

```bash
gprbuild -P embedded_example.gpr -XTARGET=stm32f4
```

Flash mit OpenOCD:
```bash
openocd -f board/stm32f4discovery.cfg \
  -c "program bin/main_loopback.elf verify reset exit"
```

## Dateien

```
cortex_m4_lwip/
├── embedded_example.gpr     # GPRbuild-Projekt
├── main_loopback.adb        # Loopback-Test (QEMU-kompatibel)
├── main_slave.adb           # Modbus TCP Slave
├── main_master.adb          # Modbus TCP Master
├── modbus_tcp_slave.ads/adb # Slave-Modul
├── modbus_tcp_master.ads/adb# Master-Modul
├── lwip_bindings.ads/adb    # Ada-Bindings für LwIP
├── semihosting.ads/adb      # QEMU-Textausgabe
├── last_chance_handler.*    # Exception-Handler für light runtime
├── lwip/                    # LwIP Submodul (git submodule)
└── lwip_port/               # LwIP-Portierung für Cortex-M
    ├── sys_arch.c           # System-Architektur (NO_SYS)
    └── include/
        ├── lwipopts.h       # LwIP-Optionen
        └── arch/cc.h        # Compiler-Konfiguration
```

## LwIP-Submodul

LwIP wird als Git-Submodul verwaltet:

```bash
# Submodul initialisieren
git submodule update --init

# Submodul aktualisieren
git submodule update --remote

# Bestimmte Version auschecken
cd examples/embedded/cortex_m4_lwip/lwip
git checkout STABLE-2_2_0_RELEASE
```

## LwIP-Integration

### Initialisierung im Code

```ada
with LwIP_Bindings; use LwIP_Bindings;
with Modbus_TCP_Slave;

procedure Main is
   My_Netif : aliased Netif;
   IP       : aliased IP4_Addr := Make_IP4_Addr (192, 168, 1, 10);
   Mask     : aliased IP4_Addr := Make_IP4_Addr (255, 255, 255, 0);
   Gateway  : aliased IP4_Addr := Make_IP4_Addr (192, 168, 1, 1);
   Result   : Status;
begin
   --  1. LwIP initialisieren
   LwIP_Init;

   --  2. Netzwerk-Interface konfigurieren (board-spezifisch)
   --     Der Init-Callback hängt vom Ethernet-Treiber ab
   Netif_Add (My_Netif'Access, IP'Access, Mask'Access, Gateway'Access,
              null, ETH_Init'Access, Ethernet_Input'Access);
   Netif_Set_Default (My_Netif'Access);
   Netif_Set_Up (My_Netif'Access);

   --  3. Modbus Slave starten
   Modbus_TCP_Slave.Modbus_Init (502, Result);

   --  4. Main-Loop
   loop
      --  LwIP Timeouts prüfen
      Sys_Check_Timeouts;

      --  Ethernet-Pakete verarbeiten (board-spezifisch)
      ETH_Poll;

      --  Modbus verarbeiten
      Modbus_TCP_Slave.Modbus_Poll;
   end loop;
end Main;
```

### LwIP-Konfiguration (lwipopts.h)

Wichtige Optionen:

| Option | Wert | Beschreibung |
|--------|------|--------------|
| `NO_SYS` | 1 | Kein OS (Bare-Metal) |
| `LWIP_SOCKET` | 1 | BSD Socket API aktivieren |
| `MEM_SIZE` | 8192 | Heap-Größe (8 KB) |
| `MEMP_NUM_TCP_PCB` | 4 | Max. TCP-Verbindungen |

## Speicherverbrauch

Typische Werte für `main_loopback`:

| Bereich | Größe | Anmerkung |
|---------|-------|-----------|
| Flash   | ~11 KB | Code + konstante Daten |
| RAM     | ~9 KB | Davon 8 KB Stack |

Mit LwIP (`main_slave`):

| Bereich | Größe | Anmerkung |
|---------|-------|-----------|
| Flash   | ~45 KB | Inkl. LwIP |
| RAM     | ~20 KB | Davon 8 KB Stack |

### Stack-Größe

**Wichtig:** Die Standard-Stack-Größe der light Runtime ist nur 2 KB. Für Modbus-Anwendungen
mit den Standard-Puffergrößen (256 Bytes Request/Response, 253 Bytes PDU) ist das zu wenig!

Das Projekt setzt die Stack-Größe auf 8 KB via Linker-Option:
```gpr
package Linker is
   for Default_Switches ("Ada") use
     ("-Wl,--defsym=__stack_size=8192");  --  8 KB Stack
end Linker;
```

Stack-Verbrauch pro Funktionsaufruf (geschätzt):
| Funktion | Stack-Verbrauch |
|----------|-----------------|
| `Master.Read_Holding_Registers` | ~600 Bytes |
| `Slave.Process_Request` | ~800 Bytes |
| RTU-Frame-Verarbeitung | ~512 Bytes |

Bei verschachtelten Aufrufen (Master → Slave → RTU) kann der Stack-Verbrauch
2 KB überschreiten, was bei der Standard-Stack-Größe zu einem HardFault führt.

## Anpassungen

### Unit-ID ändern

In `modbus_tcp_slave.adb`:
```ada
Slave_Config_Val : constant Slave_Config :=
  (Mode    => TCP,
   Unit_Id => 1,   --  <-- Hier ändern
   ...);
```

### Register-Bereiche ändern

In `modbus_tcp_slave.adb`:
```ada
Holding_Registers : Register_Array (0 .. 99);  --  100 Register
Input_Registers   : Register_Array (0 .. 15);  --  16 Register
Coil_Status       : Coil_Array (0 .. 31);      --  32 Coils
Discrete_Inputs   : Coil_Array (0 .. 31);      --  32 Inputs
```

### Master-Zieladresse ändern

In `main_master.adb`:
```ada
SLAVE_IP_A : constant Unsigned_8 := 192;
SLAVE_IP_B : constant Unsigned_8 := 168;
SLAVE_IP_C : constant Unsigned_8 := 1;
SLAVE_IP_D : constant Unsigned_8 := 100;
SLAVE_PORT : constant Unsigned_16 := 502;
```

## Troubleshooting

### HardFault / Stack Overflow

**Symptom:** QEMU zeigt `Lockup: can't escalate 3 to HardFault` oder das Programm
crasht ohne Ausgabe.

**Ursache:** Stack Overflow - die 2 KB Standard-Stack-Größe ist zu klein.

**Lösung:** Stack-Größe erhöhen (siehe Abschnitt "Stack-Größe"):
```gpr
package Linker is
   for Default_Switches ("Ada") use
     ("-Wl,--defsym=__stack_size=8192");  --  8 KB
end Linker;
```

**Diagnose:** Im QEMU-Crash-Dump prüfen:
- `R13` (SP) sollte im SRAM-Bereich liegen (0x20000000+)
- Wenn R13 < 0x20000000, ist der Stack übergelaufen

### "light-cortex-m3" runtime not found

```bash
# Runtime installieren
alr toolchain --select gnat_arm_elf
```

### QEMU startet nicht

QEMU für ARM installieren:
- Windows: `winget install qemu`
- Linux: `apt install qemu-system-arm`
- macOS: `brew install qemu`

### Semihosting funktioniert nicht

Semihosting-Flag prüfen:
```bash
qemu-system-arm -M lm3s6965evb -nographic -semihosting -kernel ...
                                          ^^^^^^^^^^^^
```

### LwIP-Submodul fehlt

```bash
git submodule update --init --recursive
```

### LwIP Compile-Fehler

Include-Pfade prüfen:
```bash
gprbuild -P embedded_example.gpr -XLWIP=enabled -v
```

## QEMU mit Netzwerk

Für Tests mit echtem TCP/IP-Netzwerk:

```bash
# Linux: TAP-Interface erstellen (benötigt root)
sudo ip tuntap add dev tap0 mode tap user $USER
sudo ip link set tap0 up
sudo ip addr add 192.168.1.1/24 dev tap0

# QEMU mit Netzwerk starten
qemu-system-arm -M netduino2 -nographic -semihosting \
  -netdev tap,id=net0,ifname=tap0,script=no,downscript=no \
  -device virtio-net-device,netdev=net0 \
  -kernel bin/main_slave.elf

# Modbus-Client verbinden (anderes Terminal)
modbus-cli read 192.168.1.10 0 10
```
