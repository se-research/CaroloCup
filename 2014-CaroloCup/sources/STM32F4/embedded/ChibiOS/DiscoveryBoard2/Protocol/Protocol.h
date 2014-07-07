/*
 * OpenDaVINCI - STM32F4 Discovery Board Software/Hardware Interface.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef PROTOCOL_H
#define PROTOCOL_H

#include <stdint.h>

uint32_t yaw(int razorData[12]);
uint32_t magnetometer(int razorData[12]);
uint32_t gyroscope(int razorData[12]);
uint32_t accelerometer_razor(int razorData[12]);
uint32_t accelerometer_discovery(int8_t discoveryData[2]);
uint8_t ultrasonic(int value);
uint32_t infrared(int ir1, int ir2, int ir3);
uint32_t currentPos(float imuInfo[7]);
uint32_t traveled_path(float imuInfo[7]);
uint32_t current_velocity(float imuInfo[7]);
uint32_t current_orientation(float imuInfo[7]);
void translate(int request, int ir_data[3], int16_t us_data[3],int razorData[12], float imuInfo[7], int8_t discoveryAccelData[2], uint8_t data[4]);

#endif // PROTOCOL_H

