/*
 * OpenDaVINCI - STM32F4 Discovery Board Software/Hardware Interface.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef TEMPERATURE_H
#define TEMPERATURE_H

#include "DiscoveryBoard.h"

/**
 * This method initializes the onboard temperature sensor.
 */
void initializeTemperature(void);

/**
 * This method transfers the onboard temperature
 * data to a data structure the will be handled
 * as part of a protocol.
 *
 * @param data Array to store the read data.
 */
void getTemperatureData(int8_t data[1]);

/**
 * This method enables interactive access to the data
 * read from the onboard temperature sensor and allows
 * the user to print the last read data.
 *
 * @chp Stream where to print the data to.
 * @argc Number of arguments for this command.
 * @argv List of arguments.
 */
void commandPrintTemperature(BaseSequentialStream *chp, int argc, char *argv[]);

#endif // TEMPERATURE_H

