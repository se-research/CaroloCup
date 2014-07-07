/*
 * OpenDaVINCI - STM32F4 Discovery Board Software/Hardware Interface.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef RCRECEIVER_H
#define RCRECEIVER_H

#include "DiscoveryBoard.h"

/**
 * This method initializes the RC-receiver ICU sensor.
 */
void initializeRCReceiver(void);

/**
 * This method transfers the RC-receiver ICU
 * data to a data structure the will be handled
 * as part of a protocol.
 *
 * @param data Array to store the read data.
 */
void getRCReceiverData(int8_t data[3]);

/**
 * This method enables interactive access to the data
 * read from the RC-receiver ICU sensor and allows
 * the user to print the last read data.
 *
 * @chp Stream where to print the data to.
 * @argc Number of arguments for this command.
 * @argv List of arguments.
 */
void commandPrintRCReceiver(BaseSequentialStream *chp, int argc, char *argv[]);

#endif // RCRECEIVER_H

