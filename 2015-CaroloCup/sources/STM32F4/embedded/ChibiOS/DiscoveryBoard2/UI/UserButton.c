/*
 * OpenDaVINCI - STM32F4 Discovery Board Software/Hardware Interface.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "DiscoveryBoard.h"

int isUserButtonPressed(unsigned int timeOut) {
    const int FIVE_HUNDRED_MILLISECONDS = 500;

    // User button is connected to PA0.
    palSetPadMode(GPIOA, 0, PAL_MODE_INPUT);

    int userButtonPressed = 0;
    while (timeOut > 0) {
        timeOut--;

        if (!userButtonPressed) {
            userButtonPressed = palReadPad(GPIOA, 0);
        }

        // Indicate the checking process by blinking the LEDs
        if (userButtonPressed) {
            changeStateLED(RED, ON);
            chThdSleepMilliseconds(FIVE_HUNDRED_MILLISECONDS);
            changeStateLED(RED, OFF);
            chThdSleepMilliseconds(FIVE_HUNDRED_MILLISECONDS);
        }
        else {
            changeStateLED(BLUE, ON);
            chThdSleepMilliseconds(FIVE_HUNDRED_MILLISECONDS);
            changeStateLED(BLUE, OFF);
            chThdSleepMilliseconds(FIVE_HUNDRED_MILLISECONDS);
        }
    }

    return userButtonPressed;
}

