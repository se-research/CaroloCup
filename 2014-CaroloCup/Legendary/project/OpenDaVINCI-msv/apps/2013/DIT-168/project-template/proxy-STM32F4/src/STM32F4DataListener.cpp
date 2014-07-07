/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "STM32F4DataListener.h"

namespace msv {

    InfraredSensorMeasurement::InfraredSensorMeasurement() :
        address(0),
        value(0) {}

    InfraredSensorMeasurement::~InfraredSensorMeasurement() {}

    UltrasonicSensorMeasurement::UltrasonicSensorMeasurement() :
        address(0),
        value(0) {}

    UltrasonicSensorMeasurement::~UltrasonicSensorMeasurement() {}

    RazorMeasurement::RazorMeasurement() :
        yaw(0),
        pitch(0),
        roll(0),
        mag_X(0),
        mag_Y(0),
        mag_Z(0),
        acc_X(0),
        acc_Y(0),
        acc_Z(0),
        gyro_X(0),
        gyro_Y(0),
        gyro_Z(0) {}

    RazorMeasurement::~RazorMeasurement() {}

    STM32F4AccelerometerMeasurement::STM32F4AccelerometerMeasurement() :
        acc_X(0),
        acc_Y(0),
        acc_Z(0) {}

    STM32F4AccelerometerMeasurement::~STM32F4AccelerometerMeasurement() {}

    STM32F4DataListener::~STM32F4DataListener() {}

}

