/*
 * OpenDaVINCI - STM32F4 Discovery Board Software/Hardware Interface.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef USER_SHELL_COMMANDS_H
#define USER_SHELL_COMMANDS_H

#include "DiscoveryBoard.h"

/**
 * This method prints information about the internal mem usage.
 *
 * @chp Stream where to print the data to.
 * @argc Number of arguments for this command.
 * @argv List of arguments.
 */
void commandMem(BaseSequentialStream *chp, int argc, char *argv[]);

/**
 * This method prints information about the currently running threads.
 *
 * @chp Stream where to print the data to.
 * @argc Number of arguments for this command.
 * @argv List of arguments.
 */
void commandThreads(BaseSequentialStream *chp, int argc, char *argv[]);

#endif // USER_SHELL_COMMANDS_H

