/*
 * OpenDaVINCI - STM32F4 Discovery Board Software/Hardware Interface.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef STEERING_ACCELERATION_H
#define STEERING_ACCELERATION_H

#include "DiscoveryBoard.h"

/**
 * This method initializes the actors.
 */
void initializeSteeringAcceleration(void);

/**
 * @return The associated thread for handling the acceleration motor.
 */
Thread* getThreadAcceleration(void);

/**
 * @return The associated thread for handling the steering servo.
 */
Thread* getThreadSteering(void);

/**
 * This method sets the desired steering and
 * speed values.
 *
 * @param steering Desired steering wheel angle.
 * @param speed Desired speed.
 */
void setMotorData(int steering, int speed);

/**
 * This method enables interactive access to the control
 * the steering servo and acceleration motor and allows
 * the user to print the last read data.
 *
 * @chp Stream where to print the data to.
 * @argc Number of arguments for this command.
 * @argv List of arguments.
 */
void commandControlSteeringAccelerationMotors(BaseSequentialStream *chp, int argc, char *argv[]);

#endif // STEERING_ACCELERATION_H

