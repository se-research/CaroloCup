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

            int getInfraredDistance( int id);
	    void setInfraredDistance( int id,  int newInfrared);
	    int getUltrasonicDistance( int id);
	    void setUltrasonicDistance( int id,  int newUltrasonic);

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
      int firstInfraredDistance;
      int secondInfraredDistance;
      int thirdInfraredDistance;
      int fourthInfraredDistance;

      int firstUltrasonicDistance;
      int secondUltrasonicDistance;
		
		

	};

} // carolocup

#endif /*LIDARDATA_H_*/
