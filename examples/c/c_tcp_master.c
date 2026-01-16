/**
 * @file c_tcp_master.c
 * @brief Modbus TCP Master (Client) Demo - C API
 * @license MIT
 * Copyright (c) 2026 Florian Fischer
 *
 * A simple Modbus TCP client that:
 * - Connects to a Modbus TCP server
 * - Provides interactive commands
 * - Demonstrates Master API usage
 *
 * Usage: c_tcp_master [host] [port]
 * Default: localhost 1502
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "../c_api/ada_modbus.h"

/* Default connection settings */
#define DEFAULT_HOST "localhost"
#define DEFAULT_PORT 1502
#define DEFAULT_TIMEOUT 3000

/* Global handles */
static modbus_tcp_handle_t g_conn = NULL;
static modbus_master_handle_t g_master = NULL;

/**
 * @brief Print command help
 */
static void print_help(void) {
    printf("\nCommands:\n");
    printf("  rhr <start> <count>   - Read Holding Registers (FC03)\n");
    printf("  rir <start> <count>   - Read Input Registers (FC04)\n");
    printf("  wsr <addr> <value>    - Write Single Register (FC06)\n");
    printf("  wmr <start> <v1> ...  - Write Multiple Registers (FC16)\n");
    printf("  rc <start> <count>    - Read Coils (FC01)\n");
    printf("  rdi <start> <count>   - Read Discrete Inputs (FC02)\n");
    printf("  wsc <addr> <0|1>      - Write Single Coil (FC05)\n");
    printf("  wmc <start> <b1> ...  - Write Multiple Coils (FC15)\n");
    printf("  help                  - Show this help\n");
    printf("  quit                  - Exit program\n\n");
}

/**
 * @brief Print status message
 */
static void print_status(modbus_status_t status) {
    const char* msg = modbus_status_string(status);
    printf("  Status: %s\n", msg);
    free((void*)msg);
}

/**
 * @brief Read Holding Registers command
 */
static void do_read_holding_registers(int start, int count) {
    uint16_t values[125];
    modbus_status_t status;

    if (count < 1 || count > 125) {
        printf("Error: count must be 1-125\n");
        return;
    }

    printf("Reading %d holding registers from %d\n", count, start);
    status = modbus_read_holding_registers(g_master, 1, (uint16_t)start,
                                           (uint16_t)count, values, DEFAULT_TIMEOUT);
    print_status(status);

    if (status == MODBUS_SUCCESS) {
        for (int i = 0; i < count; i++) {
            printf("  [%d] = %u\n", start + i, values[i]);
        }
    }
}

/**
 * @brief Read Input Registers command
 */
static void do_read_input_registers(int start, int count) {
    uint16_t values[125];
    modbus_status_t status;

    if (count < 1 || count > 125) {
        printf("Error: count must be 1-125\n");
        return;
    }

    printf("Reading %d input registers from %d\n", count, start);
    status = modbus_read_input_registers(g_master, 1, (uint16_t)start,
                                         (uint16_t)count, values, DEFAULT_TIMEOUT);
    print_status(status);

    if (status == MODBUS_SUCCESS) {
        for (int i = 0; i < count; i++) {
            printf("  [%d] = %u\n", start + i, values[i]);
        }
    }
}

/**
 * @brief Write Single Register command
 */
static void do_write_single_register(int addr, int value) {
    modbus_status_t status;

    printf("Writing %d to register %d\n", value, addr);
    status = modbus_write_single_register(g_master, 1, (uint16_t)addr,
                                          (uint16_t)value, DEFAULT_TIMEOUT);
    print_status(status);
}

/**
 * @brief Write Multiple Registers command
 */
static void do_write_multiple_registers(int start, int count, uint16_t* values) {
    modbus_status_t status;

    printf("Writing %d registers starting at %d\n", count, start);
    status = modbus_write_multiple_registers(g_master, 1, (uint16_t)start,
                                             (uint16_t)count, values, DEFAULT_TIMEOUT);
    print_status(status);
}

/**
 * @brief Read Coils command
 */
static void do_read_coils(int start, int count) {
    uint8_t values[250];  /* Max 2000 coils = 250 bytes */
    modbus_status_t status;

    if (count < 1 || count > 2000) {
        printf("Error: count must be 1-2000\n");
        return;
    }

    printf("Reading %d coils from %d\n", count, start);
    memset(values, 0, sizeof(values));
    status = modbus_read_coils(g_master, 1, (uint16_t)start,
                               (uint16_t)count, values, DEFAULT_TIMEOUT);
    print_status(status);

    if (status == MODBUS_SUCCESS) {
        for (int i = 0; i < count; i++) {
            printf("  [%d] = %s\n", start + i,
                   modbus_get_coil(values, i) ? "ON" : "OFF");
        }
    }
}

/**
 * @brief Read Discrete Inputs command
 */
static void do_read_discrete_inputs(int start, int count) {
    uint8_t values[250];
    modbus_status_t status;

    if (count < 1 || count > 2000) {
        printf("Error: count must be 1-2000\n");
        return;
    }

    printf("Reading %d discrete inputs from %d\n", count, start);
    memset(values, 0, sizeof(values));
    status = modbus_read_discrete_inputs(g_master, 1, (uint16_t)start,
                                         (uint16_t)count, values, DEFAULT_TIMEOUT);
    print_status(status);

    if (status == MODBUS_SUCCESS) {
        for (int i = 0; i < count; i++) {
            printf("  [%d] = %s\n", start + i,
                   modbus_get_coil(values, i) ? "ON" : "OFF");
        }
    }
}

/**
 * @brief Write Single Coil command
 */
static void do_write_single_coil(int addr, int value) {
    modbus_status_t status;

    printf("Writing %s to coil %d\n", value ? "ON" : "OFF", addr);
    status = modbus_write_single_coil(g_master, 1, (uint16_t)addr,
                                      value, DEFAULT_TIMEOUT);
    print_status(status);
}

/**
 * @brief Write Multiple Coils command
 */
static void do_write_multiple_coils(int start, int count, uint8_t* values) {
    modbus_status_t status;

    printf("Writing %d coils starting at %d\n", count, start);
    status = modbus_write_multiple_coils(g_master, 1, (uint16_t)start,
                                         (uint16_t)count, values, DEFAULT_TIMEOUT);
    print_status(status);
}

/**
 * @brief Parse and execute command
 */
static int process_command(const char* line) {
    char cmd[16];
    int n;

    /* Skip whitespace */
    while (*line && isspace(*line)) line++;
    if (!*line) return 1;

    /* Parse command */
    if (sscanf(line, "%15s%n", cmd, &n) != 1) return 1;
    line += n;

    /* Check command */
    if (strcmp(cmd, "quit") == 0 || strcmp(cmd, "exit") == 0) {
        return 0;
    }

    if (strcmp(cmd, "help") == 0) {
        print_help();
        return 1;
    }

    if (strcmp(cmd, "rhr") == 0) {
        int start, count;
        if (sscanf(line, "%d %d", &start, &count) == 2) {
            do_read_holding_registers(start, count);
        } else {
            printf("Usage: rhr <start> <count>\n");
        }
        return 1;
    }

    if (strcmp(cmd, "rir") == 0) {
        int start, count;
        if (sscanf(line, "%d %d", &start, &count) == 2) {
            do_read_input_registers(start, count);
        } else {
            printf("Usage: rir <start> <count>\n");
        }
        return 1;
    }

    if (strcmp(cmd, "wsr") == 0) {
        int addr, value;
        if (sscanf(line, "%d %d", &addr, &value) == 2) {
            do_write_single_register(addr, value);
        } else {
            printf("Usage: wsr <addr> <value>\n");
        }
        return 1;
    }

    if (strcmp(cmd, "wmr") == 0) {
        int start;
        uint16_t values[125];
        int count = 0;

        if (sscanf(line, "%d%n", &start, &n) == 1) {
            line += n;
            while (count < 125) {
                int val;
                if (sscanf(line, "%d%n", &val, &n) == 1) {
                    values[count++] = (uint16_t)val;
                    line += n;
                } else {
                    break;
                }
            }
            if (count > 0) {
                do_write_multiple_registers(start, count, values);
            } else {
                printf("Usage: wmr <start> <value1> [value2] ...\n");
            }
        } else {
            printf("Usage: wmr <start> <value1> [value2] ...\n");
        }
        return 1;
    }

    if (strcmp(cmd, "rc") == 0) {
        int start, count;
        if (sscanf(line, "%d %d", &start, &count) == 2) {
            do_read_coils(start, count);
        } else {
            printf("Usage: rc <start> <count>\n");
        }
        return 1;
    }

    if (strcmp(cmd, "rdi") == 0) {
        int start, count;
        if (sscanf(line, "%d %d", &start, &count) == 2) {
            do_read_discrete_inputs(start, count);
        } else {
            printf("Usage: rdi <start> <count>\n");
        }
        return 1;
    }

    if (strcmp(cmd, "wsc") == 0) {
        int addr, value;
        if (sscanf(line, "%d %d", &addr, &value) == 2) {
            do_write_single_coil(addr, value);
        } else {
            printf("Usage: wsc <addr> <0|1>\n");
        }
        return 1;
    }

    if (strcmp(cmd, "wmc") == 0) {
        int start;
        uint8_t values[250] = {0};
        int count = 0;

        if (sscanf(line, "%d%n", &start, &n) == 1) {
            line += n;
            while (count < 2000) {
                int val;
                if (sscanf(line, "%d%n", &val, &n) == 1) {
                    modbus_set_coil(values, count, val);
                    count++;
                    line += n;
                } else {
                    break;
                }
            }
            if (count > 0) {
                do_write_multiple_coils(start, count, values);
            } else {
                printf("Usage: wmc <start> <bit1> [bit2] ...\n");
            }
        } else {
            printf("Usage: wmc <start> <bit1> [bit2] ...\n");
        }
        return 1;
    }

    printf("Unknown command. Type 'help' for commands.\n");
    return 1;
}

/**
 * @brief Main entry point
 */
int main(int argc, char* argv[]) {
    const char* host = DEFAULT_HOST;
    int port = DEFAULT_PORT;
    modbus_status_t status;
    char line[256];

    printf("=== Modbus TCP Master Demo (C API) ===\n");
    printf("Library version: %s\n\n", modbus_version());

    /* Parse command line */
    if (argc >= 2) host = argv[1];
    if (argc >= 3) port = atoi(argv[2]);

    /* Create TCP connection */
    g_conn = modbus_tcp_create();
    if (!g_conn) {
        printf("Failed to create TCP connection\n");
        return 1;
    }

    /* Connect to server */
    printf("Connecting to %s:%d...\n", host, port);
    status = modbus_tcp_connect(g_conn, host, port, 5000);

    if (status != MODBUS_SUCCESS) {
        const char* err = modbus_tcp_last_error(g_conn);
        printf("Connection failed: %s\n", err);
        free((void*)err);
        modbus_tcp_destroy(g_conn);
        return 1;
    }

    printf("Connected!\n");

    /* Create master context */
    g_master = modbus_master_create(g_conn, MODBUS_MODE_TCP, 1, DEFAULT_TIMEOUT);
    if (!g_master) {
        printf("Failed to create master context\n");
        modbus_tcp_destroy(g_conn);
        return 1;
    }

    print_help();

    /* Interactive command loop */
    while (1) {
        printf("modbus> ");
        fflush(stdout);

        if (!fgets(line, sizeof(line), stdin)) {
            break;
        }

        /* Remove newline */
        size_t len = strlen(line);
        if (len > 0 && line[len - 1] == '\n') {
            line[len - 1] = '\0';
        }

        if (!process_command(line)) {
            break;
        }
    }

    /* Cleanup */
    modbus_master_destroy(g_master);
    modbus_tcp_disconnect(g_conn);
    modbus_tcp_destroy(g_conn);

    printf("Disconnected. Goodbye!\n");
    return 0;
}
