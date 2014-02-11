/*
 * CaroloCup.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef SENSORDATA_H_
#define SENSORDATA_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/data/SerializableData.h"


namespace carolocup {

	using namespace std;

	class SensorData : public core::data::SerializableData {
		public:
			SensorData();
			SensorData(const SensorData &obj);
			virtual ~SensorData();

			/**
			 * Copy constructor.
			 *
			 * @param obj Reference to an object of this class.
			 */
			SensorData& operator=(const SensorData &obj);

            int32_t getInfraredDistance( int id) const;
	    void setInfraredDistance( int id,  int32_t newInfrared) ;
	    int32_t getUltrasonicDistance( int id) const;
	    void setUltrasonicDistance( int id,  int32_t newUltrasonic);
	    int32_t getMovement() const;
	    void setMovement(int newMovement);

			/**
			 * Assignment operator.
			 *
			 * @param obj Reference to an object of this class.
			 * @return Reference to this instance.
			 */


			virtual ostream& operator<<(ostream &out) const;
			virtual istream& operator>>(istream &in);

			virtual const string toString() const;

		private:
      int32_t firstInfraredDistance;
      int32_t secondInfraredDistance;
      int32_t thirdInfraredDistance;
      int32_t fourthInfraredDistance;

      int32_t firstUltrasonicDistance;
      int32_t secondUltrasonicDistance;
      int32_t movement;
		
		

	};

} // carolocup

#endif /*LIDARDATA_H_*/
