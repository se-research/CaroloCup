/*
 * OpenDaVINCI - STM32F4 Discovery Board Software/Hardware Interface.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef LEDS_H
#define LEDS_H

enum LED { ORANGE = 1, 
           GREEN,
           RED,
           BLUE };

enum LED_STATE { OFF = 0,
                 ON };

/**
 * This method enables the STM32F4 Discovery Board LEDs.
 */
void enableLEDs(void);

/**
 * This method changes the status of the specified LED.
 *
 * @led LED that should be changes.
 * @state State of the LED.
 */
void changeStateLED(enum LED led, enum LED_STATE state);

/**
 * This method turns off the STM32F4 Discovery Board LEDs.
 */
void turnOffLEDs(void);

#endif // USER_SHELL_H

