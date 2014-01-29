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
           secondUltrasonicDistance(0){}


        SensorData::~SensorData() {}
	SensorData& SensorData::operator=(const SensorData &obj) {

		return (*this);
	}

	const string SensorData::toString() const {
				 
		return NULL;
	}

	ostream& SensorData::operator<<(ostream &out) const {

		return out;
	}

	istream& SensorData::operator>>(istream &in) {

		return in;
	}

   int SensorData::getInfraredDistance( int id){
    
      int returnInfraredDistance = 0;
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

   void SensorData::setInfraredDistance( int id,  int newInfrared){
    
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
   int SensorData::getUltrasonicDistance( int id){
      int returnUltrasonicDistance = 0;
	 if(id == 1){

           returnUltrasonicDistance = firstUltrasonicDistance;
          }
	  else if(id == 2){
	   returnUltrasonicDistance = secondUltrasonicDistance;
          }
              
             return returnUltrasonicDistance;
  }

   void SensorData::setUltrasonicDistance( int id,  int newUltrasonic){

	 if(id == 1){

            firstUltrasonicDistance = newUltrasonic;
          }
	  else if(id == 2){
	    secondUltrasonicDistance = newUltrasonic;
          }
  }


} // carolocup

