/*
 * OpenDaVINCI - STM32F4 Discovery Board Software/Hardware Interface.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "DiscoveryBoard.h"

void enableLEDs(void) {
    palSetPadMode(GPIOD, GPIOD_LED3, PAL_MODE_OUTPUT_PUSHPULL); // Orange
    palSetPadMode(GPIOD, GPIOD_LED4, PAL_MODE_OUTPUT_PUSHPULL); // Green
    palSetPadMode(GPIOD, GPIOD_LED5, PAL_MODE_OUTPUT_PUSHPULL); // Red
    palSetPadMode(GPIOD, GPIOD_LED6, PAL_MODE_OUTPUT_PUSHPULL); // Blue
}

void changeStateLED(enum LED led, enum LED_STATE state) {
    switch (led) {
        case ORANGE:
            if (state == ON)
                palSetPad(GPIOD, GPIOD_LED3);
            else
                palClearPad(GPIOD, GPIOD_LED3);
        break;
        case GREEN:
            if (state == ON)
                palSetPad(GPIOD, GPIOD_LED4);
            else
                palClearPad(GPIOD, GPIOD_LED4);
        break;
        case RED:
            if (state == ON)
                palSetPad(GPIOD, GPIOD_LED5);
            else
                palClearPad(GPIOD, GPIOD_LED5);
        break;
        case BLUE:
            if (state == ON)
                palSetPad(GPIOD, GPIOD_LED6);
            else
                palClearPad(GPIOD, GPIOD_LED6);
        break;
    }
}

void turnOffLEDs(void) {
    changeStateLED(ORANGE, OFF);
    changeStateLED(GREEN, OFF);
    changeStateLED(RED, OFF);
    changeStateLED(BLUE, OFF);
}

