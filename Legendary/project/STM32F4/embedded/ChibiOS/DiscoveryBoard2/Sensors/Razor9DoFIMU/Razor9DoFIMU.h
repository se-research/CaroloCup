/*
 * OpenDaVINCI - STM32F4 Discovery Board Software/Hardware Interface.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef RAZOR9DOFIMU_H
#define RAZOR9DOFIMU_H

#include "DiscoveryBoard.h"

/**
 * This method initializes the Razor 9DoF IMU sensor.
 */
void initializeRazor9DoFIMU(void);

/**
 * @return The associated thread for handling the Razor 9DoF IMU.
 */
Thread* getThreadRazor9DoFIMU(void);

/**
 * This method transfers the 9DoF data measured
 * by the Razor 9DoF IMU sensor to a data structure that
 * will be handled as part of a protocol.
 *
 * @param data Array to store the read data.
 */
void getRazor9DoFIMUData(int data[3]);

/**
 * This method enables interactive access to the data
 * read from the Razor 9DoF IMU sensor and allows the user
 * to print the last read data.
 *
 * @chp Stream where to print the data to.
 * @argc Number of arguments for this command.
 * @argv List of arguments.
 */
void commandPrintRazor9DoFIMU(BaseSequentialStream *chp, int argc, char *argv[]);

#endif // RAZOR9DOFIMU_H

