/*
 * OpenDaVINCI - STM32F4 Discovery Board Software/Hardware Interface.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef WHEELENCODER_H
#define WHEELENCODER_H

#include "DiscoveryBoard.h"

/**
 * This method initializes the wheel encoder ICU sensor.
 */
void initializeWheelEncoder(void);

/**
 * This method transfers the wheel encoder ICU
 * data to a data structure the will be handled
 * as part of a protocol.
 *
 * @param data Array to store the read data.
 */
void getWheelEncoderData(int8_t data[4]);

/**
 * This method enables interactive access to the data
 * read from the wheel encoder ICU sensor and allows
 * the user to print the last read data.
 *
 * @chp Stream where to print the data to.
 * @argc Number of arguments for this command.
 * @argv List of arguments.
 */
void commandPrintWheelEncoder(BaseSequentialStream *chp, int argc, char *argv[]);

#endif // WHEELENCODER_H

