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
#include "core/base/ROSDeserializer.h"
#include "core/base/ROSSerializer.h"
#include "core/base/Thread.h"
#include "core/data/Container.h"
#include "core/data/TimeStamp.h"
#include "core/data/control/VehicleControl.h"

// #include "core/platform.h"

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

            ROSSerializer &s = sf.getROSSerializer(out);

            s.write(1,m_int);
            s.write(2,m_double);
            s.write(3,m_string);

            return out;
        }

        istream& operator>>(istream &in) {
            SerializationFactory sf;

            ROSDeserializer &d = sf.getROSDeserializer(in);

            d.read(2, m_int);
            d.read(3,m_double);
            d.read(4,m_string);

            return in;
        }
};

class ROSSerializationData : public core::base::Serializable{
public:
    string m_string;
    
    ROSSerializationData():
    m_string(){}
    
     ostream& operator<<(ostream &out) const {
            SerializationFactory sf;

            ROSSerializer &s = sf.getROSSerializer(out);
            s.write(3,m_string);

            return out;
        }
    istream& operator>>(istream &in) {
            SerializationFactory sf;

            ROSDeserializer &d = sf.getROSDeserializer(in);
            d.read(1,m_string);

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

            ROSSerializer &s = sf.getROSSerializer(out);
            
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

            ROSDeserializer &d = sf.getROSDeserializer(in);
            
             d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL6('m', '_', 'b', '3', 'o', 'l') >::RESULT,m_float);
             d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL6('m', '_', 'b', '2', 'o', 'l') >::RESULT,m_double);
             d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL6('m', '_', 'b', 'o', 'o', 'l') >::RESULT,
                   m_bool);
             d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL8('m', '_', 'n', 'e', 's', 't', 'e', 'd') >::RESULT,
                    m_nestedData);                       
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

            SerializationTestSampleData sd2;
            inout >> sd2;

            TS_ASSERT(sd2.m_bool == sd.m_bool);
            TS_ASSERT(sd2.m_int == 42);
            TS_ASSERT(sd2.m_string == "This is an example.");
            TS_ASSERT_DELTA(sd2.m_float, 123.32, 1e-5);
            TS_ASSERT_DELTA(sd2.m_double, 1999.354, 1e-5);
            TS_ASSERT_DELTA(sd2.m_nestedData.m_double,1234.32, 1e-5);
            TS_ASSERT(sd2.m_nestedData.m_int == 1234);
            TS_ASSERT(sd2.m_nestedData.m_string == "Nested one.");
        }
        
          void testSerializationContainer() {
              
            VehicleControl vc;
            vc.setSpeed(2.0);
            vc.setAcceleration(1.6);
            vc.setSteeringWheelAngle(32);
            vc.setBrakeLights(true);
            vc.setLeftFlashingLights(false);
            vc.setRightFlashingLights(true);
              
            Container c(Container::VEHICLECONTROL,vc);
          
            stringstream inout;

            SerializationFactory sf;
            ROSSerializer &ros = sf.getROSSerializer(inout);
            ros.write(c);
            inout.flush();
            
            Container c2;
            ROSDeserializer &rosd = sf.getROSDeserializer(inout);
            rosd.read(inout, c2);

            VehicleControl vc2 = c2.getData<VehicleControl>();

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
            
            stringstream inout2;
            inout2 << vc;
            
            VehicleControl vc3 ;
            inout2 >> vc3;

             TS_ASSERT(vc.toString() == vc3.toString());

           }
              
        
        void testROSDeserialisationRawData() {
            
          ROSSerializationData data;
          // Raw data structure : message length , string length, string with value: Hello, World!
          
          string hex = "110000000D00000048656c6c6f2c20576f726c6421";
          int len = hex.length();
          string rawData;
            
          for(int i=0; i< len; i+=2) {
               string byte = hex.substr(i,2);
               char chr = (char) (int)strtol(byte.c_str(), NULL, 16);
               rawData.push_back(chr);
          }
          data.m_string = "";
          stringstream rawDataStream ;
          rawDataStream << rawData;
          
          rawDataStream >> data;
          
          TS_ASSERT(data.m_string == "Hello, World!");
        }
        
        
        void testROSSerialisationRawData() {
            
          ROSSerializationData data;
          // Raw data structure : message length , string length, string with value: Hello, World!
          ROSSerializationData data1;
          string hexString = "110000000D00000048656c6c6f2c20576f726c6421";
          int len = hexString.length();
          string rawData;
            
          for(int i=0; i< len; i+=2) {
               string byte = hexString.substr(i,2);
               char chr = (char) (int)strtol(byte.c_str(), NULL, 16);
               rawData.push_back(chr);
          }
         stringstream rawDataStream ;
         rawDataStream << rawData;
          
         data.m_string = "Hello, World!";
          
          stringstream ss;
          ss << data;
   
          TS_ASSERT(rawDataStream.str()== ss.str());
         
        }
    
};

#endif /*CORE_PROTOSERIALIZATIONTESTSUITE_H_*/
