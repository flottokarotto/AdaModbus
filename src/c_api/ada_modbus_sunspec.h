/**
 * @file ada_modbus_sunspec.h
 * @brief SunSpec High-Level API for AdaModbus
 * @copyright 2026 Florian Fischer
 * @license MIT
 *
 * High-level SunSpec functions that decode register data automatically.
 * Requires linking with libadamodbus.
 */

#ifndef ADA_MODBUS_SUNSPEC_H
#define ADA_MODBUS_SUNSPEC_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Include base types */
#include "ada_modbus.h"

/*============================================================================
 * SunSpec Model IDs
 *============================================================================*/

#define SUNSPEC_MODEL_COMMON        1
#define SUNSPEC_MODEL_INVERTER_1P   101
#define SUNSPEC_MODEL_INVERTER_SP   102
#define SUNSPEC_MODEL_INVERTER_3P   103
#define SUNSPEC_MODEL_STORAGE       124
#define SUNSPEC_MODEL_MPPT          160
#define SUNSPEC_MODEL_METER_1P      201
#define SUNSPEC_MODEL_METER_SP      202
#define SUNSPEC_MODEL_METER_3P_WYE  203
#define SUNSPEC_MODEL_METER_3P_DELTA 204
#define SUNSPEC_MODEL_DER_AC        701
#define SUNSPEC_MODEL_DER_CTRL      704
#define SUNSPEC_MODEL_BATTERY       802
#define SUNSPEC_MODEL_END           0xFFFF

/* Default SunSpec base address */
#define SUNSPEC_BASE_ADDRESS        40000

/*============================================================================
 * Operating States
 *============================================================================*/

#define SUNSPEC_STATE_OFF           1
#define SUNSPEC_STATE_SLEEPING      2
#define SUNSPEC_STATE_STARTING      3
#define SUNSPEC_STATE_RUNNING       4
#define SUNSPEC_STATE_THROTTLED     5
#define SUNSPEC_STATE_SHUTTING_DOWN 6
#define SUNSPEC_STATE_FAULT         7
#define SUNSPEC_STATE_STANDBY       8

/*============================================================================
 * Data Structures
 *============================================================================*/

/**
 * @brief Device information from Model 1 (Common)
 */
typedef struct {
    char manufacturer[32];
    char model[32];
    char serial[32];
    char version[16];
} sunspec_common_t;

/**
 * @brief Inverter data from Models 101-103
 */
typedef struct {
    float ac_power_w;       /**< AC output power (W) */
    float ac_voltage_v;     /**< AC voltage (V) */
    float ac_current_a;     /**< AC current (A) */
    float ac_frequency_hz;  /**< Grid frequency (Hz) */
    float ac_energy_wh;     /**< Total energy produced (Wh) */
    float dc_power_w;       /**< DC input power (W) */
    float dc_voltage_v;     /**< DC voltage (V) */
    float dc_current_a;     /**< DC current (A) */
    float cabinet_temp_c;   /**< Cabinet temperature (°C) */
    int   operating_state;  /**< SUNSPEC_STATE_* */
} sunspec_inverter_t;

/**
 * @brief Energy meter data from Models 201-204
 */
typedef struct {
    float total_power_w;    /**< Total active power (W), negative = export */
    float total_voltage_v;  /**< Average voltage L-N (V) */
    float total_current_a;  /**< Total current (A) */
    float frequency_hz;     /**< Grid frequency (Hz) */
    float power_factor;     /**< Power factor */
    float total_va;         /**< Total apparent power (VA) */
    float total_var;        /**< Total reactive power (var) */
    float export_wh;        /**< Total exported energy (Wh) */
    float import_wh;        /**< Total imported energy (Wh) */
    /* Per-phase values (3-phase meters) */
    float l1_power_w;
    float l2_power_w;
    float l3_power_w;
    float l1_voltage_v;
    float l2_voltage_v;
    float l3_voltage_v;
    float l1_current_a;
    float l2_current_a;
    float l3_current_a;
} sunspec_meter_t;

/**
 * @brief Battery/storage data from Models 124, 802
 */
typedef struct {
    float soc_percent;      /**< State of charge (%) */
    float soh_percent;      /**< State of health (%) */
    float voltage_v;        /**< Battery voltage (V) */
    float current_a;        /**< Battery current (A), positive = charging */
    float power_w;          /**< Battery power (W) */
    float capacity_wh;      /**< Total capacity (Wh) */
    float max_charge_w;     /**< Max charge power (W) */
    float max_discharge_w;  /**< Max discharge power (W) */
    int   cycle_count;      /**< Number of charge cycles */
    int   state;            /**< Battery state */
    float cell_v_max;       /**< Max cell voltage (V) */
    float cell_v_min;       /**< Min cell voltage (V) */
} sunspec_battery_t;

/**
 * @brief Model header for iteration
 */
typedef struct {
    uint16_t model_id;
    uint16_t length;
    uint16_t address;
} sunspec_model_header_t;

/*============================================================================
 * Discovery Functions
 *============================================================================*/

/**
 * @brief Check if device supports SunSpec
 * @param master_handle Handle from modbus_master_create()
 * @param slave Modbus slave address
 * @param base_address SunSpec base (typically 40000)
 * @return MODBUS_SUCCESS if "SunS" marker found
 */
int sunspec_check(void* master_handle, uint8_t slave, uint16_t base_address);

/**
 * @brief Read model header at address
 * @param master_handle Handle from modbus_master_create()
 * @param slave Modbus slave address
 * @param address Register address of model header
 * @param[out] header Model information
 * @return MODBUS_SUCCESS on success
 */
int sunspec_read_model_header(void* master_handle, uint8_t slave,
                              uint16_t address, sunspec_model_header_t* header);

/**
 * @brief Find a specific model in the device
 * @param master_handle Handle from modbus_master_create()
 * @param slave Modbus slave address
 * @param start_address Address to start searching (base + 2)
 * @param model_id Model to find (e.g., SUNSPEC_MODEL_INVERTER_3P)
 * @param[out] header Found model information
 * @return MODBUS_SUCCESS if found, MODBUS_NOT_IMPLEMENTED if not found
 */
int sunspec_find_model(void* master_handle, uint8_t slave,
                       uint16_t start_address, uint16_t model_id,
                       sunspec_model_header_t* header);

/*============================================================================
 * High-Level Read Functions
 *============================================================================*/

/**
 * @brief Read device information (Model 1)
 * @param master_handle Handle from modbus_master_create()
 * @param slave Modbus slave address
 * @param model_address Address of Model 1 header
 * @param[out] data Device information
 * @return MODBUS_SUCCESS on success
 */
int sunspec_read_common(void* master_handle, uint8_t slave,
                        uint16_t model_address, sunspec_common_t* data);

/**
 * @brief Read inverter data (Models 101-103)
 * @param master_handle Handle from modbus_master_create()
 * @param slave Modbus slave address
 * @param model_address Address of inverter model header
 * @param[out] data Inverter measurements
 * @return MODBUS_SUCCESS on success
 */
int sunspec_read_inverter(void* master_handle, uint8_t slave,
                          uint16_t model_address, sunspec_inverter_t* data);

/**
 * @brief Read meter data (Models 201-204)
 * @param master_handle Handle from modbus_master_create()
 * @param slave Modbus slave address
 * @param model_address Address of meter model header
 * @param[out] data Meter measurements
 * @return MODBUS_SUCCESS on success
 */
int sunspec_read_meter(void* master_handle, uint8_t slave,
                       uint16_t model_address, sunspec_meter_t* data);

/**
 * @brief Read battery data (Models 124, 802)
 * @param master_handle Handle from modbus_master_create()
 * @param slave Modbus slave address
 * @param model_address Address of battery model header
 * @param[out] data Battery measurements
 * @return MODBUS_SUCCESS on success
 */
int sunspec_read_battery(void* master_handle, uint8_t slave,
                         uint16_t model_address, sunspec_battery_t* data);

#ifdef __cplusplus
}
#endif

#endif /* ADA_MODBUS_SUNSPEC_H */
