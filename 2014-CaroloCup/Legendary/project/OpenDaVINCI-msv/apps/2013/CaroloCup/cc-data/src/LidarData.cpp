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
 	       fourthDist(0),
		data({0}){}


        LidarData::~LidarData() {}
	LidarData& LidarData::operator=(const LidarData &obj) {
		memcpy(data, obj.data, 360*sizeof(int32_t));
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
		stringstream s;
		s << "LidarData: " << data[0] << ", " << data[1];
		return s.str();
	}

	ostream& LidarData::operator<<(ostream &out) const {
		SerializationFactory sf;
		Serializer &s = sf.getSerializer(out);
		stringstream stream;
		/*s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('d', 'e', 'g', '1') >::RESULT,
				firstDeg);
		s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL5('d', 'i', 's', 't', '1') >::RESULT,
				firstDist);
		s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('d', 'e', 'g', '2') >::RESULT,
				secondDeg);
		s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL5('d', 'i', 's', 't', '2') >::RESULT,
				secondDist);
		s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('d', 'e', 'g', '3') >::RESULT,
				thirdDeg);
		s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL5('d', 'i', 's', 't', '3') >::RESULT,
				thirdDist);
		s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('d', 'e', 'g', '4') >::RESULT,
				fourthDeg);
		s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL5('d', 'i', 's', 't', '4') >::RESULT,
				fourthDist);*/

		stringstream rawData;

		for(int i =0; i < 360; i++) {
			rawData << data[i] << endl;
		}
		string lidarStr = rawData.str();
		s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL5('a', 'r', 'r', 'l', 'd') >::RESULT,
				lidarStr);
		return out;
	}

	istream& LidarData::operator>>(istream &in) {
		SerializationFactory sf;
		Deserializer &d = sf.getDeserializer(in);
		string lidarD;
		/*d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('d', 'e', 'g', '1') >::RESULT,
				firstDeg);
		d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL5('d', 'i', 's', 't', '1') >::RESULT,
				firstDist);
		d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('d', 'e', 'g', '2') >::RESULT,
				secondDeg);
		d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL5('d', 'i', 's', 't', '2') >::RESULT,
				secondDist);
		d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('d', 'e', 'g', '3') >::RESULT,
				thirdDeg);
		d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL5('d', 'i', 's', 't', '3') >::RESULT,
				thirdDist);
		d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('d', 'e', 'g', '4') >::RESULT,
				fourthDeg);
		d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL5('d', 'i', 's', 't', '4') >::RESULT,
				fourthDist);*/
		d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL5('a', 'r', 'r', 'l', 'd') >::RESULT,
				lidarD);

                stringstream rawData;
                rawData.str(lidarD);

		for(int i = 0; i < 360; i ++) {
			rawData >> data[i];
		}

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
   void LidarData::setThirdDeg(unsigned int newThirdDeg){
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

