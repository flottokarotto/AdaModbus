#!/usr/bin/env python3
"""
Validate SunSpec register offsets against official model definitions.

Usage: python validate_sunspec.py
"""

import json
from pathlib import Path

MODELS_DIR = Path(__file__).parent.parent / "sunspec_models" / "json"

def parse_model(model_id: int) -> dict[str, int]:
    """Parse a SunSpec model JSON and return field name -> offset mapping."""
    path = MODELS_DIR / f"model_{model_id}.json"
    if not path.exists():
        raise FileNotFoundError(f"Model {model_id} not found: {path}")

    with open(path) as f:
        data = json.load(f)

    offsets = {}
    current_offset = 0  # Starts after model header (ID + L)

    for point in data["group"]["points"]:
        name = point["name"]
        size = point.get("size", 1)
        offsets[name] = current_offset
        current_offset += size

    return offsets

def validate_model_103():
    """Validate Model 103 (Three Phase Inverter) offsets used in kostal_reader.adb"""
    print("=== Model 103 (Three Phase Inverter) ===")
    offsets = parse_model(103)

    # Expected offsets from kostal_reader.adb (relative to model start + 2)
    # AC_Regs reads offset 2-17 (16 registers)
    # That means: A at offset 2, which is offset 0 in our mapping + 2

    expected = {
        # AC measurements (regs 2-17, so +2 from model start)
        "A": 2,           # AC Current (AC_Regs[0])
        "A_SF": 6,        # Current SF (AC_Regs[4])
        "PhVphA": 10,     # Phase Voltage AN (AC_Regs[8])
        "V_SF": 13,       # Voltage SF (AC_Regs[11])
        "W": 14,          # AC Power (AC_Regs[12])
        "W_SF": 15,       # Power SF (AC_Regs[13])
        "Hz": 16,         # Frequency (AC_Regs[14])
        "Hz_SF": 17,      # Frequency SF (AC_Regs[15])

        # Energy (regs 24-26)
        "WH": 24,         # Energy (32-bit) (Energy_Regs[0..1])
        "WH_SF": 26,      # Energy SF (Energy_Regs[2])

        # DC measurements (regs 27-32)
        "DCA": 27,        # DC Current (DC_Regs[0])
        "DCA_SF": 28,     # DC Current SF (DC_Regs[1])
        "DCV": 29,        # DC Voltage (DC_Regs[2])
        "DCV_SF": 30,     # DC Voltage SF (DC_Regs[3])
        "DCW": 31,        # DC Power (DC_Regs[4])
        "DCW_SF": 32,     # DC Power SF (DC_Regs[5])

        # Temperature (regs 33-37)
        "TmpCab": 33,     # Cabinet Temp (Temp_Regs[0])
        "TmpSnk": 34,     # Heat Sink Temp (Temp_Regs[1])
        "TmpTrns": 35,    # Transformer Temp (Temp_Regs[2])
        "TmpOt": 36,      # Other Temp (Temp_Regs[3])
        "Tmp_SF": 37,     # Temp SF (Temp_Regs[4])

        # State (regs 38-39)
        "St": 38,         # Operating State (State_Regs[0])
        "StVnd": 39,      # Vendor State (State_Regs[1])
    }

    errors = 0
    for name, expected_offset in expected.items():
        # JSON offsets start at 0 (ID), we need to add 0 since ID is at offset 0
        actual = offsets.get(name)
        if actual is None:
            print(f"  ERROR: Field '{name}' not found in model")
            errors += 1
        elif actual != expected_offset:
            print(f"  ERROR: {name}: expected offset {expected_offset}, got {actual}")
            errors += 1
        else:
            print(f"  OK: {name} at offset {actual}")

    return errors

def validate_model_160():
    """Validate Model 160 (MPPT) offsets used in kostal_reader.adb"""
    print("\n=== Model 160 (Multiple MPPT Inverter Extension) ===")
    offsets = parse_model(160)

    # From kostal_reader.adb:
    # Header_Regs reads offset 2-9 (8 registers)
    # Offset 2: DCA_SF, Offset 3: DCV_SF, etc.
    # Offset 8: N (number of modules)
    # Module data starts at offset 10

    expected = {
        "DCA_SF": 2,      # Header_Regs[0]
        "DCV_SF": 3,      # Header_Regs[1]
        "DCW_SF": 4,      # Header_Regs[2]
        "DCWH_SF": 5,     # Header_Regs[3]
        "N": 8,           # Header_Regs[6] - Number of modules
    }

    errors = 0
    for name, expected_offset in expected.items():
        actual = offsets.get(name)
        if actual is None:
            print(f"  ERROR: Field '{name}' not found in model")
            errors += 1
        elif actual != expected_offset:
            print(f"  ERROR: {name}: expected offset {expected_offset}, got {actual}")
            errors += 1
        else:
            print(f"  OK: {name} at offset {actual}")

    # Print all offsets for reference
    print("\n  All Model 160 offsets:")
    for name, offset in sorted(offsets.items(), key=lambda x: x[1]):
        print(f"    {offset:3d}: {name}")

    return errors

def validate_model_1():
    """Validate Model 1 (Common) offsets used in kostal_reader.adb"""
    print("\n=== Model 1 (Common) ===")
    offsets = parse_model(1)

    expected = {
        "Mn": 2,          # Manufacturer (16 regs)
        "Md": 18,         # Model (16 regs)
        "Vr": 42,         # Version (8 regs)
        "SN": 50,         # Serial (16 regs)
    }

    errors = 0
    for name, expected_offset in expected.items():
        actual = offsets.get(name)
        if actual is None:
            print(f"  ERROR: Field '{name}' not found in model")
            errors += 1
        elif actual != expected_offset:
            print(f"  ERROR: {name}: expected offset {expected_offset}, got {actual}")
            errors += 1
        else:
            print(f"  OK: {name} at offset {actual}")

    return errors

def main():
    print("SunSpec Model Validation")
    print("=" * 50)
    print(f"Models directory: {MODELS_DIR}")
    print()

    total_errors = 0
    total_errors += validate_model_1()
    total_errors += validate_model_103()
    total_errors += validate_model_160()

    print()
    print("=" * 50)
    if total_errors == 0:
        print("All validations PASSED")
    else:
        print(f"FAILED: {total_errors} error(s)")

    return total_errors

if __name__ == "__main__":
    exit(main())
