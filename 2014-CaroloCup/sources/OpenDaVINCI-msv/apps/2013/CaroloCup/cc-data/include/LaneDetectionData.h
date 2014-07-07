/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef LANEDETECTIONDATA_H_
#define LANEDETECTIONDATA_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/data/SerializableData.h"

namespace carolocup {

	using namespace std;

	class LaneDetectionData : public core::data::SerializableData {
		public:
			LaneDetectionData();

			virtual ~LaneDetectionData();

			/**
			 * Copy constructor.
			 *
			 * @param obj Reference to an object of this class.
			 */
			LaneDetectionData(const LaneDetectionData &obj);

			/**
			 * Assignment operator.
			 *
			 * @param obj Reference to an object of this class.
			 * @return Reference to this instance.
			 */
			LaneDetectionData& operator=(const LaneDetectionData &obj);

			/**
			 * This method returns the numerical value.
			 *
			 * @return Numerical value.
			 */
			uint32_t getNumericalValue() const;

			/**
			 * This method sets the numerical value.
			 *
			 * @param nv Numerical value.
			 */
			void setNumericalValue(const uint32_t &nv);

			/**
			 * This method returns the distance.
			 *
			 * @return Distance.
			 */
			double getDistance() const;

			/**
			 * This method sets the distance.
			 *
			 * @param d distance.
			 */
			void setDistance(const double &d);

			virtual ostream& operator<<(ostream &out) const;
			virtual istream& operator>>(istream &in);

			virtual const string toString() const;

		private:
			uint32_t m_numericalValue;
			double m_distance;
	};

}

#endif
