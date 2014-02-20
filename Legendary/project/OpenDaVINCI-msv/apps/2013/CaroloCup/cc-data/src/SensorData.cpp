/*
 * CaroloCup.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */


#include "core/base/Hash.h"
#include "core/base/Deserializer.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"

#include "SensorData.h"

namespace carolocup {

	using namespace std;
	using namespace core::base;
   
        SensorData::SensorData(){}

	SensorData::SensorData(const SensorData &obj) :
	SerializableData(),          
	firstInfraredDistance(0),
  	secondInfraredDistance(0),
 	thirdInfraredDistance(0),
 	fourthInfraredDistance(0),
        firstUltrasonicDistance(0),
        secondUltrasonicDistance(0),
	movement(0){}


      SensorData::~SensorData() {}
	SensorData& SensorData::operator=(const SensorData &obj) {
		firstInfraredDistance = obj.getInfraredDistance(1);
  		secondInfraredDistance = obj.getInfraredDistance(2);
	 	thirdInfraredDistance = obj.getInfraredDistance(3);
 		fourthInfraredDistance = obj.getInfraredDistance(4);
	        firstUltrasonicDistance = obj.getUltrasonicDistance(1);
        	secondUltrasonicDistance = obj.getUltrasonicDistance(2);
		movement = obj.getMovement();
		return (*this);
	}

	const string SensorData::toString() const {
		stringstream s;
		s << "SensorData: " << getInfraredDistance(1) << ", " 
				    << getInfraredDistance(2) << ", " 
				    << getInfraredDistance(3) << ", " 
				    << getInfraredDistance(4) << ", " 
				    << getUltrasonicDistance(1) << ", " 
				    << getUltrasonicDistance(2) << ", " 
				    << getMovement();
		return s.str();
	}

	ostream& SensorData::operator<<(ostream &out) const {
		SerializationFactory sf;
		Serializer &s = sf.getSerializer(out);
		s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL3('i', 'r', '1') >::RESULT,
				firstInfraredDistance);
		s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL3('i', 'r', '2') >::RESULT,
				secondInfraredDistance);
		s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL3('i', 'r', '3') >::RESULT,
				thirdInfraredDistance);
		s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL3('i', 'r', '4') >::RESULT,
				fourthInfraredDistance);
		s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL3('u', 's', '1') >::RESULT,
				firstUltrasonicDistance);
		s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL3('u', 's', '2') >::RESULT,
				secondUltrasonicDistance);
		s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('h', 'a', 'l', 'l') >::RESULT,
				movement);

		return out;
	}

	istream& SensorData::operator>>(istream &in) {
		SerializationFactory sf;
		Deserializer &d = sf.getDeserializer(in);

		d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL3('i', 'r', '1') >::RESULT,
				firstInfraredDistance);
		d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL3('i', 'r', '2') >::RESULT,
				secondInfraredDistance);
		d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL3('i', 'r', '3') >::RESULT,
				thirdInfraredDistance);
		d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL3('i', 'r', '4') >::RESULT,
				fourthInfraredDistance);
		d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL3('u', 's', '1') >::RESULT,
				firstUltrasonicDistance);
		d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL3('u', 's', '2') >::RESULT,
				secondUltrasonicDistance);
		d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('h', 'a', 'l', 'l') >::RESULT,
				movement);

		return in;
	}

   int32_t SensorData::getMovement() const {
	return movement;
   }

   int32_t SensorData::getInfraredDistance( int id) const{
    
      int32_t returnInfraredDistance = 0;
          if(id == 1){

           returnInfraredDistance = firstInfraredDistance;
          }
	  else if(id == 2){
	   returnInfraredDistance = secondInfraredDistance;
          }
	  else if(id == 3){
	   returnInfraredDistance = thirdInfraredDistance;
          }
	  else if(id == 4){
	   returnInfraredDistance = fourthInfraredDistance;
          }

        return returnInfraredDistance;
  }

   void SensorData::setInfraredDistance( int id,  int32_t newInfrared){
    
          if(id == 1){

           firstInfraredDistance = newInfrared;
          }
	  else if(id == 2){
	   secondInfraredDistance = newInfrared;
          }
	  else if(id == 3){
	   thirdInfraredDistance = newInfrared;
          }
	  else if(id == 4){
	   fourthInfraredDistance = newInfrared;
          }

  }
   int32_t SensorData::getUltrasonicDistance( int id) const {
      int32_t returnUltrasonicDistance = 0;
	 if(id == 1){

           returnUltrasonicDistance = firstUltrasonicDistance;
          }
	  else if(id == 2){
	   returnUltrasonicDistance = secondUltrasonicDistance;
          }
              
             return returnUltrasonicDistance;
  }

   void SensorData::setUltrasonicDistance( int id,  int32_t newUltrasonic){

	 if(id == 1){

            firstUltrasonicDistance = newUltrasonic;
          }
	  else if(id == 2){
	    secondUltrasonicDistance = newUltrasonic;
          }
  }

  void SensorData::setMovement(int32_t newMovement) {
	movement = newMovement;
	}


} // carolocup

