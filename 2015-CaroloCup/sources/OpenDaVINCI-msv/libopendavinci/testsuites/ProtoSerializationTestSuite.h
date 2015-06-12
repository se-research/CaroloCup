/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef CORE_PROTOSERIALIZATIONTESTSUITE_H_
#define CORE_PROTOSERIALIZATIONTESTSUITE_H_

#include "cxxtest/TestSuite.h"

#include <cstdlib>

#include <iostream>
#include <sstream>
#include <string>
#include <boost/concept_check.hpp>

#include "core/base/Hash.h"
#include "core/base/Serializable.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"
#include "core/base/Deserializer.h"
#include "core/base/PROTODeserializer.h"
#include "core/base/PROTOSerializer.h"
#include "core/base/Thread.h"
#include "core/data/Container.h"
#include "core/data/TimeStamp.h"
#include "core/data/control/VehicleControl.h"


using namespace std;
using namespace core::base;
using namespace core::data;
using namespace core::data::control;


class SerializationTestNestedData : public core::base::Serializable {
    public:
        SerializationTestNestedData() :
                m_int(0),
                m_double(0),
                m_string(""){}

       uint32_t m_int;
       double m_double;
       string m_string;

        ostream& operator<<(ostream &out) const {
            SerializationFactory sf;

            Serializer &s = sf.getSerializer(out);

            s.write(1,m_int);
            s.write(2,m_double);
            s.write(3,m_string);

            return out;
        }

        istream& operator>>(istream &in) {
            SerializationFactory sf;

            Deserializer &d = sf.getDeserializer(in);

            d.read(2, m_int);
            d.read(3,m_double);
            d.read(4,m_string);

            return in;
        }
};

class SerializationTestSampleData : public core::base::Serializable {
    public:
        SerializationTestSampleData() :
                m_bool(false),
                m_int(0),
                m_string(""),
                m_double(),
                m_float(),
                m_nestedData() 
                {}
    
        bool m_bool;
        int32_t m_int;
        string m_string;
        double  m_double;
        float m_float;
        SerializationTestNestedData m_nestedData;

        ostream& operator<<(ostream &out) const {
            SerializationFactory sf;

            Serializer &s = sf.getSerializer(out);
            
            s.write(1,m_float);
            s.write(2,m_double);
            s.write(3, m_bool); 
            s.write(4, m_nestedData);      
            s.write(5, m_int);
            s.write(6, m_string);
              
            return out;
        }

        istream& operator>>(istream &in) {
            SerializationFactory sf;

            Deserializer &d = sf.getDeserializer(in);
            
            d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL6('m', '_', 'b', '3', 'o', 'l') >::RESULT,m_float);
            d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL6('m', '_', 'b', '2', 'o', 'l') >::RESULT,m_double);
            d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL6('m', '_', 'b', 'o', 'o', 'l') >::RESULT,
                   m_bool);
            d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL5('m', '_', 'n', 'n', 't') >::RESULT,m_nestedData);           
            d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL5('m', '_', 'i', 'n', 't') >::RESULT,
                   m_int);
            d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL5('m', '_', 's', 't', 'r') >::RESULT,
                   m_string);
            
            return in;
        }
};


class SerializationTest : public CxxTest::TestSuite {
    public:
        void testSerializationNested() {

           SerializationTestSampleData sd;
           sd.m_bool = true;
           sd.m_int = 42;
           sd.m_nestedData.m_double = 1234.32;
           sd.m_nestedData.m_int = 1234;
           sd.m_string = "This is an example.";
           sd.m_float = 123.32;
           sd.m_double = 1999.354;
           sd.m_nestedData.m_string = "Nested one.";
           stringstream inout;
           inout << sd;
           inout.flush();

           // Read from the previously created data sink.
           SerializationTestSampleData sd2;
           inout >> sd2;

           TS_ASSERT(sd2.m_bool == sd.m_bool);
           TS_ASSERT(sd2.m_int == sd.m_int);
           TS_ASSERT(sd2.m_string ==  sd.m_string);
           TS_ASSERT_DELTA(sd2.m_float, sd.m_float, 1e-5);
           TS_ASSERT_DELTA(sd2.m_double, sd.m_double, 1e-5);
           TS_ASSERT_DELTA(sd2.m_nestedData.m_double,sd.m_nestedData.m_double, 1e-5);
           TS_ASSERT(sd2.m_nestedData.m_int == sd.m_nestedData.m_int);
           TS_ASSERT(sd2.m_nestedData.m_string == sd.m_nestedData.m_string);
           

           
        }
        
          void testSerializationContainer() {

            // Create some data.
        
              
            VehicleControl vc;
            vc.setSpeed(2.0);
            vc.setAcceleration(1.6);
            vc.setSteeringWheelAngle(32);
            vc.setBrakeLights(true);
            vc.setLeftFlashingLights(false);
            vc.setRightFlashingLights(true);
             cout << vc.toString() <<endl;
              
            Container c(Container::VEHICLECONTROL,vc);
          

            stringstream inout;

            SerializationFactory sf;
            PROTOSerializer &protos = sf.getPROTOSerializer(inout);
            protos.write(c);
            inout.flush();
            
            Container c2;
            PROTODeserializer &protod = sf.getPROTODeserializer(inout);
            protod.read(inout, c2);

            VehicleControl vc2 = c2.getData<VehicleControl>();
             cout << vc2.toString()<<endl;

            TS_ASSERT(vc.toString() == vc2.toString());


        }
        
          void testSerializationPayload() {

            
            VehicleControl vc;
            vc.setSpeed(2.0);
            vc.setAcceleration(1.6);
            vc.setSteeringWheelAngle(32);
            vc.setBrakeLights(true);
            vc.setLeftFlashingLights(false);
            vc.setRightFlashingLights(true);
            
//             cout << vc.toString() <<endl;
              
            
            stringstream inout2;
            inout2 << vc;
            
            VehicleControl vc3 ;
            inout2 >> vc3;
//             cout << vc3.toString()<<endl;

             TS_ASSERT(vc.toString() == vc3.toString());


       }
       
       
        
        void testProtoSerialisation() {

          string hex = "2B0dd7a3f6421123dbf97e6a3d9f401801202a2a1750726f746f62756620526177204461746120546573742e";
          int len = hex.length();
          string rawData;
            
          for(int i=0; i< len; i+=2) {
                string byte = hex.substr(i,2);
                char chr = (char) (int)strtol(byte.c_str(), NULL, 16);
                rawData.push_back(chr);
           }
          stringstream rawDataStream;
          rawDataStream << rawData;
             
          SerializationTestSampleData sd;
           
          rawDataStream >> sd;
           
           
          TS_ASSERT(sd.m_bool == true);
          TS_ASSERT(sd.m_int == 42);
          TS_ASSERT(sd.m_string == "Protobuf Raw Data Test.");
          TS_ASSERT_DELTA(sd.m_float, 123.32, 1e-5);
          TS_ASSERT_DELTA(sd.m_double, 1999.354, 1e-5);
                   
         

        
        }
        
  
        
       
};

#endif /*CORE_PROTOSERIALIZATIONTESTSUITE_H_*/
