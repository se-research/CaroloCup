/*
 * OpenDaVINCI - STM32F4 Discovery Board Software/Hardware Interface.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef TEMPERATURE_H
#define TEMPERATURE_H

#include "DiscoveryBoard.h"

typedef struct TemperatureDataT TemperatureDataT;

struct TemperatureDataT {
    int8_t T;
};

/**
 * This method initializes the onboard temperature sensor.
 */
void initializeTemperature(void);

/**
 * @return The associated thread for handling the temperature sensor.
 */
Thread* getThreadTemperature(void);

/**
 * This method transfers the onboard temperature
 * data to a data structure the will be handled
 * as part of a protocol.
 *
 * @param Pointer to a data structure to return data.
 */
void getTemperatureData(TemperatureDataT *data);

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

