/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef CORE_FALSESERIALIZATIONTESTSUITE_H_
#define CORE_FALSESERIALIZATIONTESTSUITE_H_

#include "cxxtest/TestSuite.h"

#include <iostream>
#include <sstream>
#include <string>

#include "core/base/Hash.h"
#include "core/base/Serializable.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"
#include "core/base/Deserializer.h"
#include "core/data/TimeStamp.h"

using namespace std;
using namespace core::base;
using namespace core::data;

class FalseSerializationTestSuiteNestedData : public core::base::Serializable {
    public:
        FalseSerializationTestSuiteNestedData() :
                m_string("BlaBla") {}

        string m_string;

        ostream& operator<<(ostream &out) const {
            SerializationFactory sf;

            Serializer &s = sf.getSerializer(out);

            s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('t', 'e', 's', 't') >::RESULT,
                    m_string);

            return out;
        }

        istream& operator>>(istream &in) {
            SerializationFactory sf;

            Deserializer &d = sf.getDeserializer(in);

            d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('t', 'e', 's', 't') >::RESULT,
                   m_string);

            return in;
        }
};

class FalseSerializationTestSuiteSampleData : public core::base::Serializable {
    public:
        FalseSerializationTestSuiteSampleData() :
                m_string("Bla"), m_nestedData() {}

        string m_string;
        FalseSerializationTestSuiteNestedData m_nestedData;

        ostream& operator<<(ostream &out) const {
            SerializationFactory sf;

            Serializer &s = sf.getSerializer(out);

            s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL6('n', 'e', 's', 't', 'e', 'd') >::RESULT,
                    m_nestedData);

            s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('t', 'e', 's', 't') >::RESULT,
                    m_string);

            return out;
        }

        istream& operator>>(istream &in) {
            SerializationFactory sf;

            Deserializer &d = sf.getDeserializer(in);

            d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('t', 'e', 's', 't') >::RESULT,
                   m_string);

            d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL6('n', 'e', 's', 't', 'e', 'd') >::RESULT,
                   m_nestedData);

            return in;
        }
};

class FalseSerializationTest : public CxxTest::TestSuite {
    public:
        void testSerializationDeserialization() {
            // Create some data.
            FalseSerializationTestSuiteSampleData sd;
            sd.m_string = "RealData";
            sd.m_nestedData.m_string = "NestedData";

            // Create a data sink.
            stringstream inout;
            inout << sd;
            inout.flush();

            // Read from the previously created data sink.
            FalseSerializationTestSuiteSampleData sd2;
            inout >> sd2;

            TS_ASSERT(sd2.m_string == "RealData");
            TS_ASSERT(sd2.m_nestedData.m_string == "NestedData");
        }

        void testTimeStamp35() {
            // This test case yielded an error if the payload contains a '#' sign in the last few bytes.
            stringstream sstr;
            TimeStamp ts1(0, 35);
            sstr << ts1;

            TimeStamp ts2;
            sstr >> ts2;
            TS_ASSERT(ts1.toMicroseconds() == ts2.toMicroseconds());
            TS_ASSERT(ts1.toString() == ts2.toString());
        }
};

#endif /*CORE_FALSESERIALIZATIONTESTSUITE_H_*/
