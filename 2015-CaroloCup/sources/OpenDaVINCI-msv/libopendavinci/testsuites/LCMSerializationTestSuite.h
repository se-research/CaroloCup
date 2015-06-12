/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef CORE_LCMSERIALIZATIONTESTSUITE_H_
#define CORE_LCMSERIALIZATIONTESTSUITE_H_

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
#include "core/base/LCMDeserializer.h"
#include "core/base/LCMSerializer.h"
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
                m_double(0) {}

        double m_double;

        ostream& operator<<(ostream &out) const {
            SerializationFactory sf;

            Serializer &s = sf.getSerializer(out);
            
            s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL8('m', '_', 'd', 'o', 'u', 'b', 'l', 'e') >::RESULT,
                    m_double);

            return out;
        }

        istream& operator>>(istream &in) {
            SerializationFactory sf;

            Deserializer &d = sf.getDeserializer(in);
            
            d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL8('m', '_', 'd', 'o', 'u', 'b', 'l', 'e') >::RESULT,
                   m_double);

            return in;
        }
};

class SerializationTestSampleData : public core::base::Serializable {
    public:
        SerializationTestSampleData() :
                m_bool(false),
                m_int(0),
                m_string(""),
                m_nestedData() {}

        bool m_bool;
        int32_t m_int;
        string m_string;
        SerializationTestNestedData m_nestedData;

        ostream& operator<<(ostream &out) const {
            SerializationFactory sf;

            Serializer &s = sf.getSerializer(out);
            
            

            s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL6('m', '_', 'b', 'o', 'o', 'l') >::RESULT,
                    m_bool);
            
            s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL8('m', '_', 'n', 'e', 's', 't', 'e', 'd') >::RESULT,
                    m_nestedData);

            s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL5('m', '_', 'i', 'n', 't') >::RESULT,
                    m_int);

            s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL5('m', '_', 's', 't', 'r') >::RESULT,
                    m_string);
            
            

            return out;
        }

        istream& operator>>(istream &in) {
            SerializationFactory sf;
            
            Deserializer &d = sf.getDeserializer(in);
            
            
            
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
         //   cout << "Testing nested data" << endl;
            SerializationTestSampleData sd;
            sd.m_bool = true;
            sd.m_int = 42;
            sd.m_nestedData.m_double = -42.42;
            sd.m_string = "This is an example.";
            
            stringstream inout;
            inout << sd;
            inout.flush();

            // Read from the previously created data sink.
            SerializationTestSampleData sd2;
            inout >> sd2;
            
          //  cout << "Is: " << sd2.m_nestedData.m_double << " Should: " << -42.42 << endl;
            //cout << "Is: " << sd2.m_bool << " Should: " << true << endl;
            //cout << "Is: " << sd2.m_string << " Should: " << "This is an example." << endl;
            //cout << "Is: " << sd2.m_int << " Should: " << 42 << endl;
            
            TS_ASSERT(sd2.m_bool);
            TS_ASSERT(sd2.m_int == 42);
            TS_ASSERT(sd2.m_string == "This is an example.");
            TS_ASSERT_DELTA(sd2.m_nestedData.m_double, -42.42, 1e-5);
            
        //    cout << "end Testing nested data" << endl; 

           
        }
        
          void testSerializationContainer() {
           // cout << "Test serializing container" << endl;
            // Create some data.
        
              
            VehicleControl vc;
            vc.setSpeed(2.0);
            vc.setAcceleration(1.6);
            vc.setSteeringWheelAngle(32);
            vc.setBrakeLights(true);
            vc.setLeftFlashingLights(false);
            vc.setRightFlashingLights(true);
        //    cout << vc.toString() <<endl;
              
            Container c(Container::VEHICLECONTROL,vc);
          

            stringstream inout;

            SerializationFactory sf;
            LCMSerializer &lcms = sf.getLCMSerializer(inout);
            lcms.write(c);
            inout.flush();
            
            Container c2;
            LCMDeserializer &lcmd = sf.getLCMDeserializer(inout);
            lcmd.read(inout, c2);

            VehicleControl vc2 = c2.getData<VehicleControl>();
           // cout << vc2.toString()<<endl;

            TS_ASSERT(vc.toString() == vc2.toString());

            //cout << "end Test of container" << endl;
        }
        
          void testSerializationPayload() {
          //  cout << "Test payload" << endl;
            
            VehicleControl vc;
            vc.setSpeed(2.0);
            vc.setAcceleration(1.6);
            vc.setSteeringWheelAngle(32);
            vc.setBrakeLights(true);
            vc.setLeftFlashingLights(false);
            vc.setRightFlashingLights(true);
            
            //cout << vc.toString() <<endl;
              
            
            stringstream inout2;
            inout2 << vc;
            
            VehicleControl vc3 ;
            inout2 >> vc3;
            //cout << vc3.toString()<<endl;

             TS_ASSERT(vc.toString() == vc3.toString());

            //cout << "end Test payload" << endl;
       }
        
        void testLcmSerialisation() {
            //cout << " LCM serialisation test " << endl;
            string hex = "4c433032000000003431009c880a5168462f7440000000000000003ff999999999999a4040000000000000010001";
            int len = hex.length();
            string rawData;
            
            for(int i=0; i< len; i+=2) {
                string byte = hex.substr(i,2);
                char chr = (char) (int)strtol(byte.c_str(), NULL, 16);
                rawData.push_back(chr);
            }
            
            stringstream rawDataStream;
            rawDataStream << rawData;
            
            VehicleControl vc;
            vc.setSpeed(2.0);
            vc.setAcceleration(1.6);
            vc.setSteeringWheelAngle(32);
            vc.setBrakeLights(true);
            vc.setLeftFlashingLights(false);
            vc.setRightFlashingLights(true);
            
            stringstream inout;
            
            Container c(Container::VEHICLECONTROL,vc);
            
            SerializationFactory sf;
            LCMSerializer &lcms = sf.getLCMSerializer(inout);
            
            lcms.write(c);
            
            TS_ASSERT(rawDataStream.str() == inout.str());
            
            //cout << " End of the lcm serialisation test " << endl;
            
        }
        
        void testLcmDeserialisation() {
            
            //cout << " lcm Deserialisation test " << endl;
            
            string hex = "4c433032000000003431009c880a5168462f7440000000000000003ff999999999999a4040000000000000010001";
            int len = hex.length();
            string rawData;
            
            for(int i=0; i< len; i+=2) {
                string byte = hex.substr(i,2);
                char chr = (char) (int)strtol(byte.c_str(), NULL, 16);
                rawData.push_back(chr);
            }
            
            stringstream rawDataStream;
            rawDataStream << rawData;

            VehicleControl vc;
            vc.setSpeed(2.0);
            vc.setAcceleration(1.6);
            vc.setSteeringWheelAngle(32);
            vc.setBrakeLights(true);
            vc.setLeftFlashingLights(false);
            vc.setRightFlashingLights(true);
            
            //cout << vc.toString() << endl;
            
            SerializationFactory sf;
            LCMDeserializer &lcmd = sf.getLCMDeserializer(rawDataStream);
            Container c;
            lcmd.read(rawDataStream,c);
            
            VehicleControl vc2 = c.getData<VehicleControl>();
            //cout << vc2.toString()<<endl;

            TS_ASSERT(vc.toString() == vc2.toString());
            
           // cout << " End of lcm Deserialisation test" <<endl;
        }
        
};

#endif /*CORE_LCMSERIALIZATIONTESTSUITE_H_*/

