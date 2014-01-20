/*
 * CaroloCup.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */


#include "core/base/Hash.h"
#include "core/base/Deserializer.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"

#include "LidarData.h"

namespace carolocup {

	using namespace std;
	using namespace core::base;
   
        LidarData::LidarData(){}

	LidarData::LidarData(const LidarData &obj) :
	  SerializableData(),
           index(0),
  	   firstDeg(0),
  	   firstDist(0),
  	   secondDeg(0),
  	   secondDist(0),
  	   thirdDeg(0),
 	   thirdDist(0),
  	   fourthDeg(0),
 	   fourthDist(0){}


        LidarData::~LidarData() {}
	LidarData& LidarData::operator=(const LidarData &obj) {

		return (*this);
	}

   lidarReading LidarData::getDistance(){

   lidarReading latestReading;

   latestReading.readingIndex = index;
   latestReading.firstDegree = firstDeg;
   latestReading.firstDistance = firstDist;
   latestReading.secondDegree = secondDeg;
   latestReading.secondDistance = secondDist;
   latestReading.thirdDegree = thirdDeg;
   latestReading.thirdDistance = thirdDist;
   latestReading.fourthDegree = fourthDeg;
   latestReading.fourthDistance = fourthDist;

        return latestReading;
  }

	const string LidarData::toString() const {
				 
		return NULL;
	}

	ostream& LidarData::operator<<(ostream &out) const {

		return out;
	}

	istream& LidarData::operator>>(istream &in) {

		return in;
	}

   void LidarData::setIndex(unsigned int newIndex){
       index = newIndex;
  }
   void LidarData::setFirstDeg(unsigned int newFirstDeg){
	firstDeg = newFirstDeg;
  }
   void LidarData::setFirstDist(unsigned int newFirstDist){
  	firstDist = newFirstDist;
  }
   void LidarData::setSecondDeg(unsigned int newSecondDeg){
	secondDeg = newSecondDeg;
  }
   void LidarData::setSecondDist(unsigned int newSecondDist){
	secondDist = newSecondDist;
  }
   void LidarData::setThirDeg(unsigned int newThirdDeg){
	thirdDeg = newThirdDeg;
  }
   void LidarData::setThirdDist(unsigned int newThirdDist){
	thirdDist = newThirdDist;
  }
   void LidarData::setFourthDeg(unsigned int newFourthDeg){
	fourthDeg = newFourthDeg;
  }
   void LidarData::setFourthDist(unsigned int newFourthDist){
 	fourthDist = newFourthDist;
  }

} // carolocup

