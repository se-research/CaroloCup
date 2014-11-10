/*
 * OpenDaVINCI - STM32F4 Discovery Board Software/Hardware Interface.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef ULTRASONIC_H
#define ULTRASONIC_H

#include "DiscoveryBoard.h"

typedef struct UltrasonicDataT UltrasonicDataT;

struct UltrasonicDataT {
    int address;
    int distance;

    UltrasonicDataT *next;
};

/**
 * This method initializes the ultrasonic sensors.
 */
void initializeUltrasonic(void);

/**
 * @return The associated thread for handling the ultrasonic sensors.
 */
Thread* getThreadUltrasonic(void);

/**
 * This method transfers the distances measured
 * by the ultrasonic sensors to a data structure that
 * will be handled as part of a protocol.
 *
 * @param Pointer to a data structure to return data.
 */
void getUltrasonicData(UltrasonicDataT *data);

/**
 * This method enables interactive access to the data
 * read from the ultrasonic sensor and allows the user
 * to print the last read data.
 *
 * @chp Stream where to print the data to.
 * @argc Number of arguments for this command.
 * @argv List of arguments.
 */
void commandPrintUltrasonicDistances(BaseSequentialStream *chp, int argc, char *argv[]);

/**
 * This method enables interactive access to change
 * the I2C bus address of the ultrasonic sensor.
 *
 * @chp Stream where to print the data to.
 * @argc Number of arguments for this command.
 * @argv List of arguments.
 */
void commandSetUltrasonicAddress(BaseSequentialStream *chp, int argc, char *argv[]);

/**
 * This method enables interactive access to trigger
 * one measurement of all ultrasonic sensors.
 *
 * @chp Stream where to print the data to.
 * @argc Number of arguments for this command.
 * @argv List of arguments.
 */
void commandTriggerSingleUltrasonicMeasurement(BaseSequentialStream *chp, int argc, char *argv[]);

/**
 * This method enables interactive access to start
 * continuous measurements from all ultrasonic sensors.
 *
 * @chp Stream where to print the data to.
 * @argc Number of arguments for this command.
 * @argv List of arguments.
 */
void commandStartContinuousUltrasonicMeasurements(BaseSequentialStream *chp, int argc, char *argv[]);

/**
 * This method enables interactive access to stop
 * continuous measurements from all ultrasonic sensors.
 *
 * @chp Stream where to print the data to.
 * @argc Number of arguments for this command.
 * @argv List of arguments.
 */
void commandStopContinuousUltrasonicMeasurements(BaseSequentialStream *chp, int argc, char *argv[]);

#endif // ULTRASONIC_H

