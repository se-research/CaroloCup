/*
 * OpenDaVINCI - STM32F4 Discovery Board Software/Hardware Interface.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef USE_SHELL_H
#define USE_SHELL_H

#include "DiscoveryBoard.h"

/**
 * This method is invoked as the first call in newly created
 * threads to suspend the further execution unless the
 * initialization period with waiting for the user pressing
 * the button has passed.
 */
void waitForCompletingInitialization(void);

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

