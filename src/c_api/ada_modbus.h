/**
 * @file ada_modbus.h
 * @brief C API for AdaModbus - Ada 2022 Modbus Library
 * @license MIT
 *
 * This header provides C bindings for the AdaModbus library.
 * It supports Modbus RTU, ASCII, and TCP protocols.
 *
 * Usage:
 * 1. Include this header
 * 2. Link against the Ada library (libadamodbus.a or .dll)
 * 3. Call modbus_* functions
 *
 * Example (TCP Master):
 * @code
 *   modbus_tcp_handle_t conn = modbus_tcp_create();
 *   if (modbus_tcp_connect(conn, "localhost", 502, 5000) == MODBUS_SUCCESS) {
 *       modbus_master_handle_t master = modbus_master_create(conn, MODBUS_MODE_TCP, 1, 1000);
 *       uint16_t values[10];
 *       if (modbus_read_holding_registers(master, 1, 0, 10, values, 1000) == MODBUS_SUCCESS) {
 *           // Process values
 *       }
 *       modbus_master_destroy(master);
 *   }
 *   modbus_tcp_destroy(conn);
 * @endcode
 */

#ifndef ADA_MODBUS_H
#define ADA_MODBUS_H

#include <stdint.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/*============================================================================
 * Status Codes
 *============================================================================*/

/** @brief Modbus operation status codes */
typedef enum {
    MODBUS_SUCCESS                     = 0,   /**< Operation successful */
    MODBUS_TIMEOUT                     = 1,   /**< Operation timed out */
    MODBUS_CRC_ERROR                   = 2,   /**< CRC verification failed (RTU) */
    MODBUS_LRC_ERROR                   = 3,   /**< LRC verification failed (ASCII) */
    MODBUS_FRAME_ERROR                 = 4,   /**< Invalid frame format */
    MODBUS_INVALID_RESPONSE            = 5,   /**< Response doesn't match request */
    MODBUS_INVALID_REQUEST             = 6,   /**< Invalid request parameters */
    MODBUS_BUFFER_TOO_SMALL            = 7,   /**< Provided buffer is too small */
    MODBUS_NOT_IMPLEMENTED             = 8,   /**< Function not implemented */
    /* Modbus Exception Codes (returned by slave) */
    MODBUS_EXCEPTION_ILLEGAL_FUNCTION  = 9,   /**< Exception 01: Illegal function */
    MODBUS_EXCEPTION_ILLEGAL_ADDRESS   = 10,  /**< Exception 02: Illegal data address */
    MODBUS_EXCEPTION_ILLEGAL_VALUE     = 11,  /**< Exception 03: Illegal data value */
    MODBUS_EXCEPTION_SLAVE_FAILURE     = 12,  /**< Exception 04: Slave device failure */
    MODBUS_EXCEPTION_ACKNOWLEDGE       = 13,  /**< Exception 05: Acknowledge */
    MODBUS_EXCEPTION_SLAVE_BUSY        = 14,  /**< Exception 06: Slave device busy */
    MODBUS_EXCEPTION_GATEWAY_PATH      = 15,  /**< Exception 10: Gateway path unavailable */
    MODBUS_EXCEPTION_GATEWAY_TARGET    = 16   /**< Exception 11: Gateway target failed */
} modbus_status_t;

/*============================================================================
 * Protocol Modes
 *============================================================================*/

/** @brief Modbus protocol modes */
typedef enum {
    MODBUS_MODE_RTU   = 0,  /**< RTU mode (binary over serial) */
    MODBUS_MODE_ASCII = 1,  /**< ASCII mode (text over serial) */
    MODBUS_MODE_TCP   = 2   /**< TCP mode (over Ethernet) */
} modbus_protocol_mode_t;

/*============================================================================
 * Connection States
 *============================================================================*/

/** @brief TCP connection states */
typedef enum {
    MODBUS_STATE_DISCONNECTED = 0,  /**< Not connected */
    MODBUS_STATE_CONNECTED    = 1,  /**< Connected to server/client */
    MODBUS_STATE_LISTENING    = 2,  /**< Listening for connections */
    MODBUS_STATE_ERROR        = 3   /**< Error state */
} modbus_connection_state_t;

/*============================================================================
 * Handle Types (Opaque)
 *============================================================================*/

/** @brief Opaque handle for TCP connections */
typedef void* modbus_tcp_handle_t;

/** @brief Opaque handle for master (client) context */
typedef void* modbus_master_handle_t;

/** @brief Opaque handle for slave (server) context */
typedef void* modbus_slave_handle_t;

/*============================================================================
 * TCP Connection API
 *============================================================================*/

/**
 * @brief Create a new TCP connection handle
 * @return New TCP handle, or NULL on allocation failure
 */
modbus_tcp_handle_t modbus_tcp_create(void);

/**
 * @brief Destroy a TCP connection handle and free resources
 * @param handle TCP handle to destroy (may be NULL)
 */
void modbus_tcp_destroy(modbus_tcp_handle_t handle);

/**
 * @brief Connect to a Modbus TCP server
 * @param handle     TCP handle
 * @param host       Hostname or IP address (e.g., "localhost", "192.168.1.1")
 * @param port       TCP port (standard Modbus port is 502)
 * @param timeout_ms Connection timeout in milliseconds
 * @return MODBUS_SUCCESS on success, error code otherwise
 */
modbus_status_t modbus_tcp_connect(modbus_tcp_handle_t handle,
                                   const char* host,
                                   int port,
                                   int timeout_ms);

/**
 * @brief Disconnect from server
 * @param handle TCP handle
 */
void modbus_tcp_disconnect(modbus_tcp_handle_t handle);

/**
 * @brief Start TCP server listening
 * @param handle TCP handle
 * @param port   TCP port to listen on
 * @return MODBUS_SUCCESS on success, error code otherwise
 */
modbus_status_t modbus_tcp_listen(modbus_tcp_handle_t handle, int port);

/**
 * @brief Accept incoming connection (blocking)
 * @param server_handle Server TCP handle (must be listening)
 * @param client_handle Pointer to receive new client handle
 * @return MODBUS_SUCCESS on success, error code otherwise
 */
modbus_status_t modbus_tcp_accept(modbus_tcp_handle_t server_handle,
                                  modbus_tcp_handle_t* client_handle);

/**
 * @brief Close server socket
 * @param handle Server TCP handle
 */
void modbus_tcp_close_server(modbus_tcp_handle_t handle);

/**
 * @brief Get current connection state
 * @param handle TCP handle
 * @return Connection state
 */
modbus_connection_state_t modbus_tcp_state(modbus_tcp_handle_t handle);

/**
 * @brief Get last error message
 * @param handle TCP handle
 * @return Error message string (must be freed by caller)
 */
const char* modbus_tcp_last_error(modbus_tcp_handle_t handle);

/**
 * @brief Receive a complete Modbus TCP frame
 * @param handle      TCP handle
 * @param buffer      Buffer to receive frame
 * @param buffer_size Size of buffer
 * @param timeout_ms  Receive timeout in milliseconds
 * @param length      Pointer to receive actual frame length
 * @return MODBUS_SUCCESS on success, error code otherwise
 */
modbus_status_t modbus_tcp_receive_frame(modbus_tcp_handle_t handle,
                                         uint8_t* buffer,
                                         int buffer_size,
                                         int timeout_ms,
                                         int* length);

/**
 * @brief Send a complete Modbus TCP frame
 * @param handle TCP handle
 * @param frame  Frame data to send
 * @param length Frame length in bytes
 * @return MODBUS_SUCCESS on success, error code otherwise
 */
modbus_status_t modbus_tcp_send_frame(modbus_tcp_handle_t handle,
                                      const uint8_t* frame,
                                      int length);

/*============================================================================
 * Master (Client) API
 *============================================================================*/

/**
 * @brief Create a new master (client) context
 * @param tcp_handle TCP connection handle
 * @param mode       Protocol mode (use MODBUS_MODE_TCP for TCP)
 * @param unit_id    Default slave/unit ID (1-247)
 * @param timeout_ms Default operation timeout in milliseconds
 * @return New master handle, or NULL on failure
 */
modbus_master_handle_t modbus_master_create(modbus_tcp_handle_t tcp_handle,
                                            modbus_protocol_mode_t mode,
                                            uint8_t unit_id,
                                            int timeout_ms);

/**
 * @brief Destroy a master context
 * @param handle Master handle to destroy
 */
void modbus_master_destroy(modbus_master_handle_t handle);

/**
 * @brief Read Coils (Function Code 01)
 * @param handle        Master handle
 * @param slave         Slave address (1-247)
 * @param start_address Starting address (0-based)
 * @param quantity      Number of coils to read (1-2000)
 * @param values        Buffer for coil values (bit-packed, LSB first)
 * @param timeout_ms    Operation timeout (0 = use default)
 * @return MODBUS_SUCCESS on success, error code otherwise
 */
modbus_status_t modbus_read_coils(modbus_master_handle_t handle,
                                  uint8_t slave,
                                  uint16_t start_address,
                                  uint16_t quantity,
                                  uint8_t* values,
                                  int timeout_ms);

/**
 * @brief Read Discrete Inputs (Function Code 02)
 * @param handle        Master handle
 * @param slave         Slave address (1-247)
 * @param start_address Starting address (0-based)
 * @param quantity      Number of inputs to read (1-2000)
 * @param values        Buffer for input values (bit-packed, LSB first)
 * @param timeout_ms    Operation timeout (0 = use default)
 * @return MODBUS_SUCCESS on success, error code otherwise
 */
modbus_status_t modbus_read_discrete_inputs(modbus_master_handle_t handle,
                                            uint8_t slave,
                                            uint16_t start_address,
                                            uint16_t quantity,
                                            uint8_t* values,
                                            int timeout_ms);

/**
 * @brief Read Holding Registers (Function Code 03)
 * @param handle        Master handle
 * @param slave         Slave address (1-247)
 * @param start_address Starting address (0-based)
 * @param quantity      Number of registers to read (1-125)
 * @param values        Buffer for register values (16-bit each)
 * @param timeout_ms    Operation timeout (0 = use default)
 * @return MODBUS_SUCCESS on success, error code otherwise
 */
modbus_status_t modbus_read_holding_registers(modbus_master_handle_t handle,
                                              uint8_t slave,
                                              uint16_t start_address,
                                              uint16_t quantity,
                                              uint16_t* values,
                                              int timeout_ms);

/**
 * @brief Read Input Registers (Function Code 04)
 * @param handle        Master handle
 * @param slave         Slave address (1-247)
 * @param start_address Starting address (0-based)
 * @param quantity      Number of registers to read (1-125)
 * @param values        Buffer for register values (16-bit each)
 * @param timeout_ms    Operation timeout (0 = use default)
 * @return MODBUS_SUCCESS on success, error code otherwise
 */
modbus_status_t modbus_read_input_registers(modbus_master_handle_t handle,
                                            uint8_t slave,
                                            uint16_t start_address,
                                            uint16_t quantity,
                                            uint16_t* values,
                                            int timeout_ms);

/**
 * @brief Write Single Coil (Function Code 05)
 * @param handle     Master handle
 * @param slave      Slave address (1-247)
 * @param address    Coil address (0-based)
 * @param value      Coil value (0 = OFF, non-zero = ON)
 * @param timeout_ms Operation timeout (0 = use default)
 * @return MODBUS_SUCCESS on success, error code otherwise
 */
modbus_status_t modbus_write_single_coil(modbus_master_handle_t handle,
                                         uint8_t slave,
                                         uint16_t address,
                                         int value,
                                         int timeout_ms);

/**
 * @brief Write Single Register (Function Code 06)
 * @param handle     Master handle
 * @param slave      Slave address (1-247)
 * @param address    Register address (0-based)
 * @param value      Register value (16-bit)
 * @param timeout_ms Operation timeout (0 = use default)
 * @return MODBUS_SUCCESS on success, error code otherwise
 */
modbus_status_t modbus_write_single_register(modbus_master_handle_t handle,
                                             uint8_t slave,
                                             uint16_t address,
                                             uint16_t value,
                                             int timeout_ms);

/**
 * @brief Write Multiple Coils (Function Code 15)
 * @param handle        Master handle
 * @param slave         Slave address (1-247)
 * @param start_address Starting address (0-based)
 * @param quantity      Number of coils to write (1-1968)
 * @param values        Coil values (bit-packed, LSB first)
 * @param timeout_ms    Operation timeout (0 = use default)
 * @return MODBUS_SUCCESS on success, error code otherwise
 */
modbus_status_t modbus_write_multiple_coils(modbus_master_handle_t handle,
                                            uint8_t slave,
                                            uint16_t start_address,
                                            uint16_t quantity,
                                            const uint8_t* values,
                                            int timeout_ms);

/**
 * @brief Write Multiple Registers (Function Code 16)
 * @param handle        Master handle
 * @param slave         Slave address (1-247)
 * @param start_address Starting address (0-based)
 * @param quantity      Number of registers to write (1-123)
 * @param values        Register values (16-bit each)
 * @param timeout_ms    Operation timeout (0 = use default)
 * @return MODBUS_SUCCESS on success, error code otherwise
 */
modbus_status_t modbus_write_multiple_registers(modbus_master_handle_t handle,
                                                uint8_t slave,
                                                uint16_t start_address,
                                                uint16_t quantity,
                                                const uint16_t* values,
                                                int timeout_ms);

/*============================================================================
 * Slave (Server) Callback Types
 *============================================================================*/

/**
 * @brief Callback for Read Coils (FC 01)
 * @param user_data     User-provided context pointer
 * @param start_address Starting coil address
 * @param quantity      Number of coils requested
 * @param values        Buffer to fill with coil values (bit-packed)
 * @return MODBUS_SUCCESS or exception code
 */
typedef modbus_status_t (*modbus_read_coils_cb)(
    void* user_data,
    uint16_t start_address,
    uint16_t quantity,
    uint8_t* values);

/**
 * @brief Callback for Read Discrete Inputs (FC 02)
 * @param user_data     User-provided context pointer
 * @param start_address Starting input address
 * @param quantity      Number of inputs requested
 * @param values        Buffer to fill with input values (bit-packed)
 * @return MODBUS_SUCCESS or exception code
 */
typedef modbus_status_t (*modbus_read_discrete_inputs_cb)(
    void* user_data,
    uint16_t start_address,
    uint16_t quantity,
    uint8_t* values);

/**
 * @brief Callback for Read Holding Registers (FC 03)
 * @param user_data     User-provided context pointer
 * @param start_address Starting register address
 * @param quantity      Number of registers requested
 * @param values        Buffer to fill with register values
 * @return MODBUS_SUCCESS or exception code
 */
typedef modbus_status_t (*modbus_read_holding_registers_cb)(
    void* user_data,
    uint16_t start_address,
    uint16_t quantity,
    uint16_t* values);

/**
 * @brief Callback for Read Input Registers (FC 04)
 * @param user_data     User-provided context pointer
 * @param start_address Starting register address
 * @param quantity      Number of registers requested
 * @param values        Buffer to fill with register values
 * @return MODBUS_SUCCESS or exception code
 */
typedef modbus_status_t (*modbus_read_input_registers_cb)(
    void* user_data,
    uint16_t start_address,
    uint16_t quantity,
    uint16_t* values);

/**
 * @brief Callback for Write Single Coil (FC 05)
 * @param user_data User-provided context pointer
 * @param address   Coil address
 * @param value     Coil value (0 = OFF, 1 = ON)
 * @return MODBUS_SUCCESS or exception code
 */
typedef modbus_status_t (*modbus_write_single_coil_cb)(
    void* user_data,
    uint16_t address,
    int value);

/**
 * @brief Callback for Write Single Register (FC 06)
 * @param user_data User-provided context pointer
 * @param address   Register address
 * @param value     Register value
 * @return MODBUS_SUCCESS or exception code
 */
typedef modbus_status_t (*modbus_write_single_register_cb)(
    void* user_data,
    uint16_t address,
    uint16_t value);

/**
 * @brief Callback for Write Multiple Coils (FC 15)
 * @param user_data     User-provided context pointer
 * @param start_address Starting coil address
 * @param quantity      Number of coils
 * @param values        Coil values (bit-packed)
 * @return MODBUS_SUCCESS or exception code
 */
typedef modbus_status_t (*modbus_write_multiple_coils_cb)(
    void* user_data,
    uint16_t start_address,
    uint16_t quantity,
    const uint8_t* values);

/**
 * @brief Callback for Write Multiple Registers (FC 16)
 * @param user_data     User-provided context pointer
 * @param start_address Starting register address
 * @param quantity      Number of registers
 * @param values        Register values
 * @return MODBUS_SUCCESS or exception code
 */
typedef modbus_status_t (*modbus_write_multiple_registers_cb)(
    void* user_data,
    uint16_t start_address,
    uint16_t quantity,
    const uint16_t* values);

/**
 * @brief Slave callback structure
 *
 * Set callbacks to NULL for unsupported function codes.
 * The library will return MODBUS_EXCEPTION_ILLEGAL_FUNCTION for those.
 */
typedef struct {
    void* user_data;                                 /**< User context passed to callbacks */
    modbus_read_coils_cb read_coils;                 /**< FC 01 handler */
    modbus_read_discrete_inputs_cb read_discrete_inputs;  /**< FC 02 handler */
    modbus_read_holding_registers_cb read_holding_registers;  /**< FC 03 handler */
    modbus_read_input_registers_cb read_input_registers;  /**< FC 04 handler */
    modbus_write_single_coil_cb write_single_coil;   /**< FC 05 handler */
    modbus_write_single_register_cb write_single_register;  /**< FC 06 handler */
    modbus_write_multiple_coils_cb write_multiple_coils;  /**< FC 15 handler */
    modbus_write_multiple_registers_cb write_multiple_registers;  /**< FC 16 handler */
} modbus_slave_callbacks_t;

/*============================================================================
 * Slave (Server) API
 *============================================================================*/

/**
 * @brief Create a new slave (server) context
 * @param mode      Protocol mode
 * @param unit_id   Unit ID for this slave (1-247)
 * @param callbacks Pointer to callback structure (must remain valid)
 * @return New slave handle, or NULL on failure
 */
modbus_slave_handle_t modbus_slave_create(modbus_protocol_mode_t mode,
                                          uint8_t unit_id,
                                          modbus_slave_callbacks_t* callbacks);

/**
 * @brief Destroy a slave context
 * @param handle Slave handle to destroy
 */
void modbus_slave_destroy(modbus_slave_handle_t handle);

/**
 * @brief Process a received request and generate response
 * @param handle          Slave handle
 * @param request         Request frame data
 * @param request_length  Request length in bytes
 * @param response        Buffer for response frame
 * @param response_size   Size of response buffer
 * @return Response length (>0), 0 for broadcast (no response), -1 on error
 */
int modbus_slave_process(modbus_slave_handle_t handle,
                         const uint8_t* request,
                         int request_length,
                         uint8_t* response,
                         int response_size);

/*============================================================================
 * Utility Functions
 *============================================================================*/

/**
 * @brief Convert status code to human-readable string
 * @param status Status code
 * @return Status string (must be freed by caller)
 */
const char* modbus_status_string(modbus_status_t status);

/**
 * @brief Get library version string
 * @return Version string (e.g., "0.1.0-dev")
 */
const char* modbus_version(void);

/*============================================================================
 * Bit Manipulation Helpers
 *============================================================================*/

/**
 * @brief Get a single coil value from bit-packed array
 * @param values Bit-packed array
 * @param index  Coil index (0-based)
 * @return 1 if ON, 0 if OFF
 */
static inline int modbus_get_coil(const uint8_t* values, int index) {
    return (values[index / 8] >> (index % 8)) & 1;
}

/**
 * @brief Set a single coil value in bit-packed array
 * @param values Bit-packed array
 * @param index  Coil index (0-based)
 * @param value  0 = OFF, non-zero = ON
 */
static inline void modbus_set_coil(uint8_t* values, int index, int value) {
    if (value) {
        values[index / 8] |= (1 << (index % 8));
    } else {
        values[index / 8] &= ~(1 << (index % 8));
    }
}

/**
 * @brief Calculate required byte count for coils
 * @param quantity Number of coils
 * @return Number of bytes needed
 */
static inline int modbus_coil_bytes(int quantity) {
    return (quantity + 7) / 8;
}

#ifdef __cplusplus
}
#endif

#endif /* ADA_MODBUS_H */
