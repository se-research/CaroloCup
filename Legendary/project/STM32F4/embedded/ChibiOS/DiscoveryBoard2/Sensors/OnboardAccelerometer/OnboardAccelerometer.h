/*
 * OpenDaVINCI - STM32F4 Discovery Board Software/Hardware Interface.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef ONBOARD_ACCELEROMETER_H
#define ONBOARD_ACCELEROMETER_H

#include "DiscoveryBoard.h"

/**
 * This method initializes the onboard accelerometer.
 */
void initializeOnboardAccelerometer(void);

/**
 * @return The associated thread for handling the onboard accelerometer.
 */
Thread* getThreadOnboardAccelerometer(void);

/**
 * This method transfers the onboard accelerometer
 * data to a data structure the will be handled
 * as part of a protocol.
 *
 * @param data Array to store the read data.
 */
void getOnboardAccelerometerData(int8_t data[2]);

/**
 * This method enables interactive access to the data
 * read from the onboard accelerometer and allows the
 * user to print the last read data.
 *
 * @chp Stream where to print the data to.
 * @argc Number of arguments for this command.
 * @argv List of arguments.
 */
void commandPrintAccelerometer(BaseSequentialStream *chp, int argc, char *argv[]);

#endif // ONBOARD_ACCELEROMETER_H

