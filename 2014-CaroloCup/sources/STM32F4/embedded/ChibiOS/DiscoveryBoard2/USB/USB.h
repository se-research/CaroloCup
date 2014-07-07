/*
 * OpenDaVINCI - STM32F4 Discovery Board Software/Hardware Interface.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef USB_H
#define USB_H

extern SerialUSBDriver SDU1;

void initializeUSB(void);

int isUSBActive(void);

#endif // USB_H
