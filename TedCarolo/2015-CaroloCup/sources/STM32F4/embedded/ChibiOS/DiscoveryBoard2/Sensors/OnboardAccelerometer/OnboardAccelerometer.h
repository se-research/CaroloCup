/*
 * OpenDaVINCI - STM32F4 Discovery Board Software/Hardware Interface.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef ONBOARD_ACCELEROMETER_H
#define ONBOARD_ACCELEROMETER_H

#include "DiscoveryBoard.h"

typedef struct OnboardAccelerometerDataT OnboardAccelerometerDataT;

struct OnboardAccelerometerDataT {
    int8_t x;
    int8_t y;
    int8_t z;
};

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
 * @param Pointer to a data structure to return data.
 */
void getOnboardAccelerometerData(OnboardAccelerometerDataT *data);

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

