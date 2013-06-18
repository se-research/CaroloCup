/*
 * OpenDaVINCI - STM32F4 Discovery Board Software/Hardware Interface.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef USE_SHELL_H
#define USE_SHELL_H

#include "DiscoveryBoard.h"

/**
 * @return 1 if the user wants to use the shell, 0 otherwise.
 */
int hasShell(void);

/**
 * This method is used to determine whether to start the
 * UI-shell or to transfer the data according to the protocol.
 *
 * @return 1 if the user wants to use the shell, 0 otherwise.
 */
int useShell(void);

#endif // USE_SHELL_H

