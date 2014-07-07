/*
 * OpenDaVINCI - STM32F4 Discovery Board Software/Hardware Interface.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef INFRARED_H
#define INFRARED_H

#include "DiscoveryBoard.h"

/**
 * This method initializes the infrared sensors.
 */
void initializeInfrared(void);

/**
 * @return The associated thread for handling the infrared sensors.
 */
Thread* getThreadInfrared(void);

/**
 * This method transfers the distances measured
 * by the infrared sensors to a data structure that
 * will be handled as part of a protocol.
 *
 * @param data Array to store the read data.
 */
void getInfraredData(int data[3]);

/**
 * This method enables interactive access to the data
 * read from the infrarred sensor and allows the user
 * to print the last read data.
 *
 * @chp Stream where to print the data to.
 * @argc Number of arguments for this command.
 * @argv List of arguments.
 */
void commandPrintInfraredDistances(BaseSequentialStream *chp, int argc, char *argv[]);

#endif // INFRARED_H

