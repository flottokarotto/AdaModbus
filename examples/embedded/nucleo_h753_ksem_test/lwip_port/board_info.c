/**
 * board_info.c - Board name from C preprocessor define
 *
 * Returns a pointer to the board name string, determined at compile time
 * by the -DSTM32H753xx / -DSTM32H743xx switch in the GPR file.
 */

#if defined(STM32H753xx)
static const char board_name[] = "NUCLEO-H753ZI";
#elif defined(STM32H743xx)
static const char board_name[] = "NUCLEO-H743ZI2";
#else
static const char board_name[] = "UNKNOWN";
#endif

const char *get_board_name(void)
{
    return board_name;
}

int get_board_name_length(void)
{
    return (int)(sizeof(board_name) - 1);  /* exclude null terminator */
}
