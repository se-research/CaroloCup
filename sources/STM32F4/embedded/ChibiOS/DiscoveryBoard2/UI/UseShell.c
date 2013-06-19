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
        changeStateLED(BLUE, ON);

    return HAS_SHELL;
}

void waitForCompletingInitialization(void) {
    Thread *tp = chMsgWait();
    msg_t msg = chMsgGet(tp);
    chMsgRelease(tp, msg);
}

