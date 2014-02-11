/*
 * CaroloCup.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef LIDARDATA_H_
#define LIDARDATA_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/data/SerializableData.h"

typedef struct {

  unsigned int readingIndex;
  unsigned int firstDegree;
  unsigned int firstDistance;
  unsigned int secondDegree;
  unsigned int secondDistance;
  unsigned int thirdDegree;
  unsigned int thirdDistance;
  unsigned int fourthDegree;
  unsigned int fourthDistance;
} lidarReading;

namespace carolocup {

	using namespace std;

	class LidarData : public core::data::SerializableData {
		public:
			LidarData();
			LidarData(const LidarData &obj);
			virtual ~LidarData();

			/**
			 * Copy constructor.
			 *
			 * @param obj Reference to an object of this class.
			 */
			LidarData& operator=(const LidarData &obj);


	     void setIndex(unsigned int newIndex);
	     void setFirstDeg(unsigned int newFirstDeg);
	     void setFirstDist(unsigned int newFirstDist);
	     void setSecondDeg(unsigned int newSecondDeg);
	     void setSecondDist(unsigned int newSecondDist);
   	     void setThirdDeg(unsigned int newThirdDeg);
	     void setThirdDist(unsigned int newThirdDist);
   	     void setFourthDeg(unsigned int newFourthDeg);
	     void setFourthDist(unsigned int newFourthDist);

             lidarReading getDistance();

	     unsigned int index;
  	     unsigned int firstDeg;
  	     unsigned int firstDist;
  	     unsigned int secondDeg;
  	     unsigned int secondDist;
  	     unsigned int thirdDeg;
 	     unsigned int thirdDist;
  	     unsigned int fourthDeg;
 	     unsigned int fourthDist;
	     int32_t data[360];

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

	};

} // carolocup

#endif /*LIDARDATA_H_*/
