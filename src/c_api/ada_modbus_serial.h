/**
 * @file ada_modbus_serial.h
 * @brief Serial/RTU API for AdaModbus
 * @copyright 2026 Florian Fischer
 * @license MIT
 *
 * Serial port and Modbus RTU master functions.
 * Requires linking with libadamodbus.
 */

#ifndef ADA_MODBUS_SERIAL_H
#define ADA_MODBUS_SERIAL_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Include base types */
#include "ada_modbus.h"

/*============================================================================
 * Parity Settings
 *============================================================================*/

#define MODBUS_PARITY_NONE  0
#define MODBUS_PARITY_EVEN  1
#define MODBUS_PARITY_ODD   2

/*============================================================================
 * Stop Bits
 *============================================================================*/

#define MODBUS_STOP_1       1
#define MODBUS_STOP_2       2

/*============================================================================
 * Connection States
 *============================================================================*/

#define MODBUS_SERIAL_DISCONNECTED  0
#define MODBUS_SERIAL_CONNECTED     1
#define MODBUS_SERIAL_ERROR         2

/*============================================================================
 * Serial Port Functions
 *============================================================================*/

/**
 * @brief Create a new serial connection handle
 * @return Handle for serial operations, or NULL on failure
 */
void* modbus_serial_create(void);

/**
 * @brief Destroy a serial connection handle
 * @param handle Handle from modbus_serial_create()
 */
void modbus_serial_destroy(void* handle);

/**
 * @brief Open serial port
 * @param handle Handle from modbus_serial_create()
 * @param port Port name: "COM1" (Windows) or "/dev/ttyUSB0" (Linux)
 * @param baud Baud rate: 9600, 19200, 38400, 57600, 115200
 * @param data_bits Data bits: 7 or 8
 * @param parity MODBUS_PARITY_NONE, MODBUS_PARITY_EVEN, or MODBUS_PARITY_ODD
 * @param stop_bits MODBUS_STOP_1 or MODBUS_STOP_2
 * @return MODBUS_SUCCESS on success, error code otherwise
 */
int modbus_serial_open(void* handle, const char* port, int baud,
                       int data_bits, int parity, int stop_bits);

/**
 * @brief Close serial port
 * @param handle Handle from modbus_serial_create()
 */
void modbus_serial_close(void* handle);

/**
 * @brief Get connection state
 * @param handle Handle from modbus_serial_create()
 * @return MODBUS_SERIAL_DISCONNECTED, MODBUS_SERIAL_CONNECTED, or MODBUS_SERIAL_ERROR
 */
int modbus_serial_state(void* handle);

/**
 * @brief Get last error message
 * @param handle Handle from modbus_serial_create()
 * @return Error message string (do not free)
 */
const char* modbus_serial_last_error(void* handle);

/*============================================================================
 * RTU Master Functions
 *============================================================================*/

/**
 * @brief Create RTU master context
 * @param serial_handle Handle from modbus_serial_create()
 * @param unit_id Default slave unit ID (1-247)
 * @param timeout_ms Default timeout in milliseconds
 * @return RTU master handle, or NULL on failure
 */
void* modbus_rtu_master_create(void* serial_handle, uint8_t unit_id, int timeout_ms);

/**
 * @brief Destroy RTU master context
 * @param handle Handle from modbus_rtu_master_create()
 */
void modbus_rtu_master_destroy(void* handle);

/**
 * @brief Read holding registers (FC 03)
 * @param handle RTU master handle
 * @param slave Slave address (1-247)
 * @param start_address Starting register address
 * @param quantity Number of registers to read (1-125)
 * @param[out] values Array to receive register values (caller allocates)
 * @param timeout_ms Request timeout in milliseconds
 * @return MODBUS_SUCCESS on success, error code otherwise
 */
int modbus_rtu_read_holding_registers(void* handle, uint8_t slave,
                                      uint16_t start_address, uint16_t quantity,
                                      uint16_t* values, int timeout_ms);

/**
 * @brief Read input registers (FC 04)
 * @param handle RTU master handle
 * @param slave Slave address (1-247)
 * @param start_address Starting register address
 * @param quantity Number of registers to read (1-125)
 * @param[out] values Array to receive register values (caller allocates)
 * @param timeout_ms Request timeout in milliseconds
 * @return MODBUS_SUCCESS on success, error code otherwise
 */
int modbus_rtu_read_input_registers(void* handle, uint8_t slave,
                                    uint16_t start_address, uint16_t quantity,
                                    uint16_t* values, int timeout_ms);

/**
 * @brief Write single register (FC 06)
 * @param handle RTU master handle
 * @param slave Slave address (1-247)
 * @param address Register address
 * @param value Value to write
 * @param timeout_ms Request timeout in milliseconds
 * @return MODBUS_SUCCESS on success, error code otherwise
 */
int modbus_rtu_write_single_register(void* handle, uint8_t slave,
                                     uint16_t address, uint16_t value,
                                     int timeout_ms);

/**
 * @brief Write multiple registers (FC 16)
 * @param handle RTU master handle
 * @param slave Slave address (1-247)
 * @param start_address Starting register address
 * @param quantity Number of registers to write (1-123)
 * @param values Array of values to write
 * @param timeout_ms Request timeout in milliseconds
 * @return MODBUS_SUCCESS on success, error code otherwise
 */
int modbus_rtu_write_multiple_registers(void* handle, uint8_t slave,
                                        uint16_t start_address, uint16_t quantity,
                                        const uint16_t* values, int timeout_ms);

/**
 * @brief Read coils (FC 01)
 * @param handle RTU master handle
 * @param slave Slave address (1-247)
 * @param start_address Starting coil address
 * @param quantity Number of coils to read (1-2000)
 * @param[out] values Packed coil values (1 bit per coil, LSB first)
 * @param timeout_ms Request timeout in milliseconds
 * @return MODBUS_SUCCESS on success, error code otherwise
 */
int modbus_rtu_read_coils(void* handle, uint8_t slave,
                          uint16_t start_address, uint16_t quantity,
                          uint8_t* values, int timeout_ms);

/**
 * @brief Write single coil (FC 05)
 * @param handle RTU master handle
 * @param slave Slave address (1-247)
 * @param address Coil address
 * @param value 0 = OFF, non-zero = ON
 * @param timeout_ms Request timeout in milliseconds
 * @return MODBUS_SUCCESS on success, error code otherwise
 */
int modbus_rtu_write_single_coil(void* handle, uint8_t slave,
                                 uint16_t address, int value,
                                 int timeout_ms);

#ifdef __cplusplus
}
#endif

#endif /* ADA_MODBUS_SERIAL_H */
