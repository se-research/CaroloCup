/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef SENSORDETECTIONDATA_H_
#define SENSORDETECTIONDATA_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/data/SerializableData.h"

namespace carolocup {

	using namespace std;

	class SensorDetectionData : public core::data::SerializableData {
		public:
			SensorDetectionData();

			virtual ~SensorDetectionData();

			/**
			 * Copy constructor.
			 *
			 * @param obj Reference to an object of this class.
			 */
			SensorDetectionData(const SensorDetectionData &obj);

			/**
			 * Assignment operator.
			 *
			 * @param obj Reference to an object of this class.
			 * @return Reference to this instance.
			 */
			SensorDetectionData& operator=(const SensorDetectionData &obj);

			/**
			 * This method returns the numerical value.
			 *
			 * @return Numerical value.
			 */
			double getNumericalValue() const;

			/**
			 * This method sets the numerical value.
			 *
			 * @param nv Numerical value.
			 */
			void setNumericalValue(const double &nv);

			virtual ostream& operator<<(ostream &out) const;
			virtual istream& operator>>(istream &in);

			virtual const string toString() const;

		private:
			double m_numericalValue;
	};

}

#endif
