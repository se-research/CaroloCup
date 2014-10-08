/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef SENSORBOARDDATA_H_
#define SENSORBOARDDATA_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/data/SerializableData.h"

namespace msv {

	using namespace std;

	class SensorBoardData : public core::data::SerializableData {
		public:
			SensorBoardData();

			virtual ~SensorBoardData();

			/**
			 * Copy constructor.
			 *
			 * @param obj Reference to an object of this class.
			 */
			SensorBoardData(const SensorBoardData &obj);

			/**
			 * Assignment operator.
			 *
			 * @param obj Reference to an object of this class.
			 * @return Reference to this instance.
			 */
			SensorBoardData& operator=(const SensorBoardData &obj);

			/**
			 * This method returns the number of sensors.
			 *
			 * @return Number of sensors.
			 */
			uint32_t getNumberOfSensors() const;

            /**
             * This method returns the distance for the given sensor (>0),
             * -1 if the sensor did not measure anything, and -2 if the
             * specified sensor is not available.
             *
             * @param sensorID ID of the sensor.
             * @return positive distance w.r.t. the sensor, -1 if nothing
             *         was measured, or -2 if sensorID is not available.
             */
            double getDistance(const uint32_t &sensorID) const;

            /**
             * This method updates the sensor-distance-map.
             *
             * @param sensorID ID of the sensor to be updated.
             * @param distance Measured distance.
             */
            void update(const uint32_t &sensorID, const double &distance);


			virtual ostream& operator<<(ostream &out) const;
			virtual istream& operator>>(istream &in);

			virtual const string toString() const;

		private:
            map<uint32_t,double> m_sensorDistanceMap;
	};

} // msv

#endif /*SENSORBOARDDATA_H_*/
