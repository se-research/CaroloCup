/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef STM32F4DATALISTENER_H_
#define STM32F4DATALISTENER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/data/environment/VehicleData.h"

#include <string>
#include <vector>

namespace msv {

    using namespace std;

    class InfraredSensorMeasurement {
        public:
            uint32_t address;
            int32_t value;

            InfraredSensorMeasurement();
            virtual ~InfraredSensorMeasurement();
    };

    class UltrasonicSensorMeasurement {
        public:
            uint32_t address;
            int32_t value;

            UltrasonicSensorMeasurement();
            virtual ~UltrasonicSensorMeasurement();
    };

    class RazorMeasurement {
        public:
            double yaw;
            double pitch;
            double roll;

            double mag_X;
            double mag_Y;
            double mag_Z;

            double acc_X; // Razor board's accelerometer
            double acc_Y;
            double acc_Z;

            double gyro_X;
            double gyro_Y;
            double gyro_Z;

            RazorMeasurement();
            virtual ~RazorMeasurement();
    };

    class STM32F4AccelerometerMeasurement {
        public:
            double acc_X; // Discovery board's internal accelerometer
            double acc_Y;
            double acc_Z;

            STM32F4AccelerometerMeasurement();
            virtual ~STM32F4AccelerometerMeasurement();
    };

    class STM32F4DataListener {
        public:
            virtual ~STM32F4DataListener();

            /**
             * This method is called when data for the infrared sensors is received.
             *
             * @param measurement List of values from infrared sensors.
             */
            virtual void nextMeasurement(const vector<InfraredSensorMeasurement> &measurement) = 0;

            /**
             * This method is called when data for the ultra sonics is received.
             *
             * @param measurement List of values from ultra sonic sensors.
             */
            virtual void nextMeasurement(const vector<UltrasonicSensorMeasurement> &measurement) = 0;

            /**
             * This method is called when data from the Razor is received.
             *
             * @param measurement Update from Razor IMU.
             */
            virtual void nextMeasurement(const RazorMeasurement &measurement) = 0;

            /**
             * This method is called when data from the STM32F4 accelerometer is received.
             *
             * @param measurement Update from STM32F4 accelerometer.
             */
            virtual void nextMeasurement(const STM32F4AccelerometerMeasurement &measurement) = 0;

            /**
             * This method is called when data from the STM32F4 IMU algorithms is received.
             *
             * @param vehicleData Update from STM32F4 IMU algorithms.
             */
            virtual void nextMeasurement(const core::data::environment::VehicleData &vehicleData) = 0;
    };

}

#endif /* STM32F4DATALISTENER_H_ */

