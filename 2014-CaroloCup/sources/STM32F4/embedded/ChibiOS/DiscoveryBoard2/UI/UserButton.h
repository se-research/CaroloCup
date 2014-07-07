/*
 * OpenDaVINCI - STM32F4 Discovery Board Software/Hardware Interface.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef USER_BUTTON_H
#define USER_BUTTON_H

/**
 * This method waits timeOut seconds and checks
 * periodically if the user has pressed the button.
 *
 * @timeOut Time out for checking the user button.
 * @return 0 if the user has not pushed the button, 1 otherwise.
 */
int isUserButtonPressed(unsigned int timeOut);

#endif // USER_BUTTON_H

