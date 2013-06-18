/*
 * OpenDaVINCI - STM32F4 Discovery Board Software/Hardware Interface.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "DiscoveryBoard.h"

static int HAS_SHELL = 0;

int hasShell(void) {
    return HAS_SHELL;
}

int useShell(void) {
    enableLEDs();
    turnOffLEDs();

    const int USER_BUTTON_TIMEOUT = 5;
    HAS_SHELL = isUserButtonPressed(USER_BUTTON_TIMEOUT);

    // Visualize the final status.
    turnOffLEDs();
    if (HAS_SHELL)
        changeStateLED(RED, ON);
    else
        changeStateLED(GREEN, ON);

    return HAS_SHELL;
}

