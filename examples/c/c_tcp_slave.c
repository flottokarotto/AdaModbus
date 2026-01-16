/**
 * @file c_tcp_slave.c
 * @brief Modbus TCP Slave (Server) Demo - C API
 * @license MIT
 * Copyright (c) 2026 Florian Fischer
 *
 * A simple Modbus TCP server that:
 * - Listens on specified port (default 1502)
 * - Handles client connections sequentially
 * - Simulates holding registers, input registers, coils, and discrete inputs
 *
 * Usage: c_tcp_slave [port]
 * Default port: 1502 (to avoid needing admin rights)
 *
 * Note: This is a single-threaded example. For multi-client support,
 * consider using threads or async I/O.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include "../c_api/ada_modbus.h"

/* Default settings */
#define DEFAULT_PORT 1502
#define RECEIVE_TIMEOUT 30000  /* 30 seconds */

/* Simulated data storage */
#define NUM_HOLDING_REGISTERS 100
#define NUM_INPUT_REGISTERS 100
#define NUM_COILS 100
#define NUM_DISCRETE_INPUTS 100

static uint16_t holding_registers[NUM_HOLDING_REGISTERS];
static uint16_t input_registers[NUM_INPUT_REGISTERS];
static uint8_t coils[NUM_COILS / 8 + 1];
static uint8_t discrete_inputs[NUM_DISCRETE_INPUTS / 8 + 1];

/* Global server handle for signal handler */
static modbus_tcp_handle_t g_server = NULL;
static volatile int g_running = 1;

/**
 * @brief Signal handler for graceful shutdown
 */
static void signal_handler(int sig) {
    (void)sig;
    g_running = 0;
    printf("\nShutting down...\n");
}

/**
 * @brief Initialize simulated data
 */
static void init_data(void) {
    /* Initialize holding registers with pattern (i * 100) */
    for (int i = 0; i < NUM_HOLDING_REGISTERS; i++) {
        holding_registers[i] = (uint16_t)(i * 100);
    }

    /* Initialize input registers with pattern (i * 10 + 1000) */
    for (int i = 0; i < NUM_INPUT_REGISTERS; i++) {
        input_registers[i] = (uint16_t)(i * 10 + 1000);
    }

    /* Initialize some coils */
    memset(coils, 0, sizeof(coils));
    modbus_set_coil(coils, 0, 1);  /* Coil 0 ON */
    modbus_set_coil(coils, 2, 1);  /* Coil 2 ON */

    /* Initialize some discrete inputs */
    memset(discrete_inputs, 0, sizeof(discrete_inputs));
    modbus_set_coil(discrete_inputs, 0, 1);  /* Input 0 ON */
    modbus_set_coil(discrete_inputs, 5, 1);  /* Input 5 ON */
}

/*============================================================================
 * Slave Callbacks
 *============================================================================*/

/**
 * @brief FC 01: Read Coils
 */
static modbus_status_t cb_read_coils(void* user_data,
                                     uint16_t start_address,
                                     uint16_t quantity,
                                     uint8_t* values) {
    (void)user_data;

    printf("[FC01] Read Coils: start=%u, qty=%u\n", start_address, quantity);

    if (start_address + quantity > NUM_COILS) {
        return MODBUS_EXCEPTION_ILLEGAL_ADDRESS;
    }

    /* Copy coil values */
    memset(values, 0, modbus_coil_bytes(quantity));
    for (uint16_t i = 0; i < quantity; i++) {
        if (modbus_get_coil(coils, start_address + i)) {
            modbus_set_coil(values, i, 1);
        }
    }

    return MODBUS_SUCCESS;
}

/**
 * @brief FC 02: Read Discrete Inputs
 */
static modbus_status_t cb_read_discrete_inputs(void* user_data,
                                               uint16_t start_address,
                                               uint16_t quantity,
                                               uint8_t* values) {
    (void)user_data;

    printf("[FC02] Read Discrete Inputs: start=%u, qty=%u\n", start_address, quantity);

    if (start_address + quantity > NUM_DISCRETE_INPUTS) {
        return MODBUS_EXCEPTION_ILLEGAL_ADDRESS;
    }

    /* Copy discrete input values */
    memset(values, 0, modbus_coil_bytes(quantity));
    for (uint16_t i = 0; i < quantity; i++) {
        if (modbus_get_coil(discrete_inputs, start_address + i)) {
            modbus_set_coil(values, i, 1);
        }
    }

    return MODBUS_SUCCESS;
}

/**
 * @brief FC 03: Read Holding Registers
 */
static modbus_status_t cb_read_holding_registers(void* user_data,
                                                 uint16_t start_address,
                                                 uint16_t quantity,
                                                 uint16_t* values) {
    (void)user_data;

    printf("[FC03] Read Holding Registers: start=%u, qty=%u\n", start_address, quantity);

    if (start_address + quantity > NUM_HOLDING_REGISTERS) {
        return MODBUS_EXCEPTION_ILLEGAL_ADDRESS;
    }

    for (uint16_t i = 0; i < quantity; i++) {
        values[i] = holding_registers[start_address + i];
    }

    return MODBUS_SUCCESS;
}

/**
 * @brief FC 04: Read Input Registers
 */
static modbus_status_t cb_read_input_registers(void* user_data,
                                               uint16_t start_address,
                                               uint16_t quantity,
                                               uint16_t* values) {
    (void)user_data;

    printf("[FC04] Read Input Registers: start=%u, qty=%u\n", start_address, quantity);

    if (start_address + quantity > NUM_INPUT_REGISTERS) {
        return MODBUS_EXCEPTION_ILLEGAL_ADDRESS;
    }

    for (uint16_t i = 0; i < quantity; i++) {
        values[i] = input_registers[start_address + i];
    }

    return MODBUS_SUCCESS;
}

/**
 * @brief FC 05: Write Single Coil
 */
static modbus_status_t cb_write_single_coil(void* user_data,
                                            uint16_t address,
                                            int value) {
    (void)user_data;

    printf("[FC05] Write Single Coil: addr=%u, value=%s\n",
           address, value ? "ON" : "OFF");

    if (address >= NUM_COILS) {
        return MODBUS_EXCEPTION_ILLEGAL_ADDRESS;
    }

    modbus_set_coil(coils, address, value);
    return MODBUS_SUCCESS;
}

/**
 * @brief FC 06: Write Single Register
 */
static modbus_status_t cb_write_single_register(void* user_data,
                                                uint16_t address,
                                                uint16_t value) {
    (void)user_data;

    printf("[FC06] Write Single Register: addr=%u, value=%u\n", address, value);

    if (address >= NUM_HOLDING_REGISTERS) {
        return MODBUS_EXCEPTION_ILLEGAL_ADDRESS;
    }

    holding_registers[address] = value;
    return MODBUS_SUCCESS;
}

/**
 * @brief FC 15: Write Multiple Coils
 */
static modbus_status_t cb_write_multiple_coils(void* user_data,
                                               uint16_t start_address,
                                               uint16_t quantity,
                                               const uint8_t* values) {
    (void)user_data;

    printf("[FC15] Write Multiple Coils: start=%u, qty=%u\n", start_address, quantity);

    if (start_address + quantity > NUM_COILS) {
        return MODBUS_EXCEPTION_ILLEGAL_ADDRESS;
    }

    for (uint16_t i = 0; i < quantity; i++) {
        modbus_set_coil(coils, start_address + i, modbus_get_coil(values, i));
    }

    return MODBUS_SUCCESS;
}

/**
 * @brief FC 16: Write Multiple Registers
 */
static modbus_status_t cb_write_multiple_registers(void* user_data,
                                                   uint16_t start_address,
                                                   uint16_t quantity,
                                                   const uint16_t* values) {
    (void)user_data;

    printf("[FC16] Write Multiple Registers: start=%u, qty=%u\n", start_address, quantity);

    if (start_address + quantity > NUM_HOLDING_REGISTERS) {
        return MODBUS_EXCEPTION_ILLEGAL_ADDRESS;
    }

    for (uint16_t i = 0; i < quantity; i++) {
        holding_registers[start_address + i] = values[i];
    }

    return MODBUS_SUCCESS;
}

/**
 * @brief Handle a single client connection
 */
static void handle_client(modbus_tcp_handle_t client, modbus_slave_handle_t slave) {
    uint8_t request[260];
    uint8_t response[260];
    int request_length;
    int response_length;
    modbus_status_t status;

    printf("[Client] Connected\n");

    while (g_running && modbus_tcp_state(client) == MODBUS_STATE_CONNECTED) {
        /* Receive request */
        status = modbus_tcp_receive_frame(client, request, sizeof(request),
                                          RECEIVE_TIMEOUT, &request_length);

        if (status == MODBUS_TIMEOUT) {
            printf("[Client] Timeout, closing connection\n");
            break;
        }

        if (status != MODBUS_SUCCESS) {
            printf("[Client] Receive error, closing connection\n");
            break;
        }

        /* Process request */
        response_length = modbus_slave_process(slave, request, request_length,
                                               response, sizeof(response));

        if (response_length < 0) {
            printf("[Client] Process error\n");
            break;
        }

        /* Send response (if not broadcast) */
        if (response_length > 0) {
            status = modbus_tcp_send_frame(client, response, response_length);
            if (status != MODBUS_SUCCESS) {
                printf("[Client] Send error\n");
                break;
            }
        }
    }

    printf("[Client] Disconnected\n");
}

/**
 * @brief Main entry point
 */
int main(int argc, char* argv[]) {
    int port = DEFAULT_PORT;
    modbus_status_t status;
    modbus_slave_handle_t slave;
    modbus_slave_callbacks_t callbacks;

    printf("=== Modbus TCP Slave Demo (C API) ===\n");
    printf("Library version: %s\n\n", modbus_version());

    /* Parse command line */
    if (argc >= 2) {
        port = atoi(argv[1]);
    }

    /* Set up signal handler */
    signal(SIGINT, signal_handler);
#ifdef SIGTERM
    signal(SIGTERM, signal_handler);
#endif

    /* Initialize data */
    init_data();

    /* Set up callbacks */
    memset(&callbacks, 0, sizeof(callbacks));
    callbacks.user_data = NULL;
    callbacks.read_coils = cb_read_coils;
    callbacks.read_discrete_inputs = cb_read_discrete_inputs;
    callbacks.read_holding_registers = cb_read_holding_registers;
    callbacks.read_input_registers = cb_read_input_registers;
    callbacks.write_single_coil = cb_write_single_coil;
    callbacks.write_single_register = cb_write_single_register;
    callbacks.write_multiple_coils = cb_write_multiple_coils;
    callbacks.write_multiple_registers = cb_write_multiple_registers;

    /* Create slave context */
    slave = modbus_slave_create(MODBUS_MODE_TCP, 1, &callbacks);
    if (!slave) {
        printf("Failed to create slave context\n");
        return 1;
    }

    /* Create TCP server */
    g_server = modbus_tcp_create();
    if (!g_server) {
        printf("Failed to create TCP server\n");
        modbus_slave_destroy(slave);
        return 1;
    }

    /* Start listening */
    printf("Starting server on port %d...\n", port);
    status = modbus_tcp_listen(g_server, port);
    if (status != MODBUS_SUCCESS) {
        const char* err = modbus_tcp_last_error(g_server);
        printf("Failed to start server: %s\n", err);
        free((void*)err);
        modbus_tcp_destroy(g_server);
        modbus_slave_destroy(slave);
        return 1;
    }

    printf("Server listening. Press Ctrl+C to stop.\n");
    printf("Connect with: c_tcp_master localhost %d\n", port);
    printf("Note: Single-threaded - handles one client at a time.\n\n");

    /* Main server loop */
    while (g_running) {
        modbus_tcp_handle_t client = NULL;

        /* Accept connection */
        status = modbus_tcp_accept(g_server, &client);
        if (status != MODBUS_SUCCESS) {
            if (g_running) {
                printf("Accept failed\n");
            }
            break;
        }

        /* Handle client */
        handle_client(client, slave);

        /* Clean up client */
        modbus_tcp_disconnect(client);
        modbus_tcp_destroy(client);
    }

    /* Cleanup */
    modbus_tcp_close_server(g_server);
    modbus_tcp_destroy(g_server);
    modbus_slave_destroy(slave);

    printf("Server stopped.\n");
    return 0;
}
