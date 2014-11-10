/*
 * OpenDaVINCI - STM32F4 Discovery Board Software/Hardware Interface.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef WHEELENCODER_H
#define WHEELENCODER_H

#include "DiscoveryBoard.h"

typedef struct WheelEncoderDataT WheelEncoderDataT;

struct WheelEncoderDataT {
    int drivenDistanceLeftWheel;
    int drivenDistanceRightWheel;
    int speedLeftWheel;
    int speedRightWheel;
};

/**
 * This method initializes the wheel encoder ICU sensor.
 */
void initializeWheelEncoder(void);

/**
 * @return The associated thread for handling computing the vehicle speed.
 */
Thread* getThreadSpeed(void);

/**
 * This method transfers the wheel encoder ICU
 * data to a data structure the will be handled
 * as part of a protocol.
 *
 * @param Pointer to a data structure to return data.
 */
void getWheelEncoderData(WheelEncoderDataT *data);

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

/**
 * This method resets the travelled paths.
 */
void commandResetWheelEncoder(BaseSequentialStream *chp, int argc, char *argv[]);

#endif // WHEELENCODER_H

