# NUCLEO-H753ZI PV Surplus Charger

PV-Überschussladung für Delta AC Max Basic Wallbox mit KSEM Energiezähler.

## Hardware

### Bestellliste

| Komponente | Beschreibung | Bezugsquelle |
|------------|--------------|--------------|
| NUCLEO-H753ZI | STM32H753ZI Cortex-M7 Board | Digikey, Mouser, Conrad |
| Waveshare RS485 CAN Shield | SP3485 RS485 + MCP2515 CAN | Conrad, Amazon, eBay |
| SSD1306 OLED 0.96" | 128x64 I2C Display | Conrad, Amazon, eBay |

### Verkabelung

```
NUCLEO-H753ZI
├── Ethernet (RJ45 onboard) ──────> Router/Switch ──> KSEM
├── USB (ST-Link) ────────────────> PC (Debug + Power)
│
├── Arduino Header
│   └── Waveshare RS485 CAN Shield (aufgesteckt)
│       └── RS485 Terminal ───────> Delta Wallbox
│           A ──> RS485 A
│           B ──> RS485 B
│           GND ──> GND (optional)
│
├── CN7 Header (I2C)
│   ├── PB8 (SCL) ────────────────> OLED SCL
│   ├── PB9 (SDA) ────────────────> OLED SDA
│   ├── 3V3 ──────────────────────> OLED VCC
│   └── GND ──────────────────────> OLED GND
│
└── Onboard
    ├── B1 (PC13) ────────────────> User Button
    ├── LD1 (PB0, Grün) ──────────> System Heartbeat
    ├── LD2 (PB7, Blau) ──────────> Laden aktiv
    └── LD3 (PB14, Rot) ──────────> Fehler
```

## Software Setup

### 1. Toolchain installieren

```bash
# GNAT ARM ELF Compiler
# Download: https://www.adacore.com/download
# Oder via Alire:
alr toolchain --select gnat_arm_elf

# Prüfen:
arm-none-eabi-gcc --version
```

### 2. LwIP einrichten

```bash
cd examples/embedded/nucleo_h753_pv_charger

# Windows:
setup_lwip.bat

# Linux/macOS:
./setup_lwip.sh
```

### 3. Projekt bauen

```bash
gprbuild -P nucleo_h753_pv_charger.gpr
```

### 4. Firmware flashen

**Methode 1: Drag & Drop (einfachste)**
1. NUCLEO per USB anschließen
2. Laufwerk "NOD_H753ZI" erscheint
3. `bin/nucleo_h753_pv_charger.bin` auf Laufwerk kopieren
4. LED blinkt → Fertig

**Methode 2: Kommandozeile**
```bash
# Windows:
flash.bat

# Linux/macOS:
./flash.sh
```

**Methode 3: STM32CubeProgrammer**
```bash
STM32_Programmer_CLI -c port=SWD -w bin/nucleo_h753_pv_charger.bin 0x08000000 -rst
```

## Konfiguration

Alle Einstellungen in `src/config.ads`:

```ada
--  NUCLEO IP-Adresse
Local_IP_A/B/C/D      := 192.168.1.100;

--  KSEM IP-Adresse
KSEM_IP_A/B/C/D       := 192.168.1.50;
KSEM_Port             := 502;

--  Delta Wallbox (Modbus RTU)
Delta_Device_Id       := 1;
Delta_Baud_Rate       := 115200;

--  Lade-Parameter
Min_Charging_Power_W  := 1380;   -- ~6A Minimum
Max_Charging_Power_W  := 11000;  -- 11kW Maximum
Power_Hysteresis_W    := 200;    -- Hysterese
Update_Interval_Ms    := 2000;   -- 2s Regelintervall
```

## Bedienung

### Display

```
┌─────────────────────┐
│ PV CHARGER CHARGING │  ← Status
│ Grid: +1234W        │  ← Netzleistung (+ = Export)
│ Charge: 4321W  80%  │  ← Ladeleistung + SOC
│ Session: 12.3 kWh   │  ← Geladene Energie
└─────────────────────┘
```

### LEDs

| LED | Farbe | Bedeutung |
|-----|-------|-----------|
| LD1 | Grün (blinkend) | System läuft (Heartbeat) |
| LD2 | Blau | Ladevorgang aktiv |
| LD3 | Rot | Kommunikationsfehler |

### Taster B1

| Aktion | Funktion |
|--------|----------|
| Kurzer Druck | Anzeigemodus wechseln (Status → Statistik → Debug) |
| Langer Druck (3s) | Laden manuell pausieren/fortsetzen |

## Projektstruktur

```
nucleo_h753_pv_charger/
├── alire.toml                    # Alire Manifest
├── nucleo_h753_pv_charger.gpr    # GPR Build-Datei
├── stm32h753.ld                  # Linker Script
├── setup_lwip.bat / .sh          # LwIP Setup
├── flash.bat / .sh               # Flash Skripte
├── README.md
│
├── src/                          # Ada Quellcode
│   ├── main.adb                  # Hauptprogramm
│   ├── config.ads                # Konfiguration
│   ├── system_init.ads/adb       # System-Init
│   ├── stm32h7_regs.ads          # Register-Definitionen
│   ├── stm32h7_hal.ads/adb       # Low-Level HAL
│   ├── hal_stubs.ads/adb         # HAL-Abstraktionsschicht
│   ├── tcp_client.ads/adb        # TCP Client (LwIP)
│   ├── lwip_bindings.ads/adb     # LwIP Ada Bindings
│   ├── ksem_client.ads/adb       # KSEM Modbus TCP
│   ├── delta_client.ads/adb      # Delta Modbus RTU
│   ├── pv_controller.ads/adb     # PV-Überschuss-Logik
│   ├── oled_display.ads/adb      # SSD1306 Treiber
│   ├── time_exports.ads/adb      # Zeit-Export für C
│   └── startup.S                 # ARM Startup Code
│
├── lwip_port/                    # LwIP Portierung
│   ├── lwipopts.h                # LwIP Konfiguration
│   ├── ethernetif.c/h            # Ethernet Treiber
│   ├── sys_arch.c                # Systemfunktionen
│   ├── ada_lwip_wrapper.c        # C-Wrapper für Ada
│   └── arch/cc.h                 # Compiler-Definitionen
│
└── lwip/                         # LwIP (via setup_lwip)
    └── src/...
```

## Funktionsweise

### PV-Überschusslogik

```
     KSEM (Modbus TCP)              Delta (Modbus RTU)
           │                              │
           ▼                              ▼
    ┌──────────────┐              ┌──────────────┐
    │ Netzleistung │              │ Ladeleistung │
    │  +1500W      │              │   3000W      │
    └──────┬───────┘              └──────┬───────┘
           │                              │
           └──────────┬───────────────────┘
                      ▼
              ┌───────────────┐
              │ PV Controller │
              │               │
              │ Verfügbar:    │
              │ 1500 + 3000   │
              │ = 4500W       │
              └───────┬───────┘
                      │
                      ▼
              ┌───────────────┐
              │ Neues Limit:  │
              │ 4500W → Delta │
              └───────────────┘
```

### Zustände

| Zustand | Beschreibung |
|---------|--------------|
| Idle | Kein Auto verbunden |
| Starting | Ladevorgang startet |
| Charging | Aktiv am Laden |
| Suspending | Wird pausiert (kein Überschuss) |
| Suspended | Pausiert, wartet auf Überschuss |
| Error | Kommunikationsfehler |

## Debugging

```bash
# Windows:
debug.bat

# Linux/macOS (zwei Terminals):
# Terminal 1:
openocd -f board/st_nucleo_h7.cfg

# Terminal 2:
arm-none-eabi-gdb bin/nucleo_h753_pv_charger.elf
(gdb) target remote :3333
(gdb) monitor reset halt
(gdb) load
(gdb) break main
(gdb) continue
```

## Memory Layout

| Region | Adresse | Größe | Verwendung |
|--------|---------|-------|------------|
| Flash | 0x08000000 | 2 MB | Programm + Konstanten |
| DTCM | 0x20000000 | 128 KB | Stack, Heap, BSS |
| SRAM1 | 0x24000000 | 256 KB | ETH DMA Buffers |
| SRAM2 | 0x30000000 | 128 KB | LwIP Memory Pool |

## Resourcen-Verbrauch (geschätzt)

| Resource | Mit LwIP | Ohne LwIP |
|----------|----------|-----------|
| Flash | ~80 KB | ~40 KB |
| RAM | ~50 KB | ~10 KB |
| Stack | 16 KB | 16 KB |

## Lizenz

MIT License - siehe [LICENSE](../../../LICENSE)

## Quellen

- [STM32H753 Reference Manual (RM0433)](https://www.st.com/resource/en/reference_manual/rm0433-stm32h742-stm32h743753-and-stm32h750-value-line-advanced-armbased-32bit-mcus-stmicroelectronics.pdf)
- [LAN8742A Datasheet](https://www.microchip.com/en-us/product/LAN8742A)
- [LwIP Documentation](https://www.nongnu.org/lwip/2_1_x/index.html)
- [Delta AC Max Modbus (evcc)](https://github.com/evcc-io/evcc)
