/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef CORE_SERIALIZATIONTESTSUITE_H_
#define CORE_SERIALIZATIONTESTSUITE_H_

#include "cxxtest/TestSuite.h"

#include <cstdlib>

#include <iostream>
#include <sstream>
#include <string>

#include "core/version.h"
#include "core/base/Hash.h"
#include "core/base/Serializable.h"
#include "core/base/SerializationFactory.h"
#include "core/base/Serializer.h"
#include "core/base/Deserializer.h"
#include "core/base/NetstringsDeserializer.h"
#include "core/base/NetstringsSerializer.h"
#include "core/base/QueryableNetstringsDeserializer.h"
#include "core/base/QueryableNetstringsSerializer.h"
#include "core/base/Thread.h"
#include "core/data/Container.h"
#include "core/data/TimeStamp.h"

using namespace std;
using namespace core::base;
using namespace core::data;

class SerializationTestNestedData : public core::base::Serializable {
    public:
        SerializationTestNestedData() :
                m_double(0) {}

        double m_double;

        ostream& operator<<(ostream &out) const {
            SerializationFactory sf;

            Serializer &s = sf.getSerializer(out);

            s.write(CRC32 < HESPERIA_CORE_STRINGLITERAL8('m', '_', 'd', 'o', 'u', 'b', 'l', 'e') >::RESULT,
                    m_double);

            return out;
        }

        istream& operator>>(istream &in) {
            SerializationFactory sf;

            Deserializer &d = sf.getDeserializer(in);

            d.read(CRC32 < HESPERIA_CORE_STRINGLITERAL8('m', '_', 'd', 'o', 'u', 'b', 'l', 'e') >::RESULT,
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

            s.write(CRC32 < HESPERIA_CORE_STRINGLITERAL6('m', '_', 'b', 'o', 'o', 'l') >::RESULT,
                    m_bool);

            s.write(CRC32 < HESPERIA_CORE_STRINGLITERAL8('m', '_', 'n', 'e', 's', 't', 'e', 'd') >::RESULT,
                    m_nestedData);

            s.write(CRC32 < HESPERIA_CORE_STRINGLITERAL5('m', '_', 'i', 'n', 't') >::RESULT,
                    m_int);

            s.write(CRC32 < HESPERIA_CORE_STRINGLITERAL5('m', '_', 's', 't', 'r') >::RESULT,
                    m_string);

            return out;
        }

        istream& operator>>(istream &in) {
            SerializationFactory sf;

            Deserializer &d = sf.getDeserializer(in);

            d.read(CRC32 < HESPERIA_CORE_STRINGLITERAL8('m', '_', 'n', 'e', 's', 't', 'e', 'd') >::RESULT,
                   m_nestedData);

            d.read(CRC32 < HESPERIA_CORE_STRINGLITERAL5('m', '_', 's', 't', 'r') >::RESULT,
                   m_string);

            d.read(CRC32 < HESPERIA_CORE_STRINGLITERAL5('m', '_', 'i', 'n', 't') >::RESULT,
                   m_int);

            d.read(CRC32 < HESPERIA_CORE_STRINGLITERAL6('m', '_', 'b', 'o', 'o', 'l') >::RESULT,
                   m_bool);

            return in;
        }
};

class PlainNetstringsOut : public NetstringsSerializer {
    public:
        PlainNetstringsOut(ostream &out) :
                NetstringsSerializer(out) {}

};

class PlainNetstringsIn : public NetstringsDeserializer {
    public:
        PlainNetstringsIn(istream &in) :
                NetstringsDeserializer(in) {}

};

class SerializationTest : public CxxTest::TestSuite {
    public:
        void testSerializationDeserialization() {
            // Create some data.
            SerializationTestSampleData sd;
            sd.m_bool = true;
            sd.m_int = 42;
            sd.m_nestedData.m_double = -42.42;
            sd.m_string = "This is an example.";

            // Create a data sink.
            stringstream inout("testoutput");
            inout << sd;
            inout.flush();

            // Read from the previously created data sink.
            SerializationTestSampleData sd2;
            inout >> sd2;

            TS_ASSERT(sd2.m_bool);
            TS_ASSERT(sd2.m_int == 42);
            TS_ASSERT(sd2.m_string == "This is an example.");
            TS_ASSERT_DELTA(sd2.m_nestedData.m_double, -42.42, 1e-5);
        }

        void testPlainNetstrings() {
//            long start = clock();
//            long end = clock();
//            double duration = ((end-start) * 1000)/CLOCKS_PER_SEC;
//            clog << size << " entries of type TIMESTAMP each containing "
//                    << sizeof(c[0]) << " bytes stored in " << duration
//                    << "ms." << endl;
//
            stringstream inout;
            TimeStamp ts1;

            // Force destruction of out to write data.
            {
                PlainNetstringsOut out(inout);
                out.write(0, ts1);
            }

            TimeStamp ts2;
            {
                PlainNetstringsIn in(inout);
                in.read(0, ts2);
            }

            TS_ASSERT(ts1.toString() == ts2.toString());
        }

        void testPlainNetstringsComparedToQueryableNetstrings() {
            const uint32_t size = 20;
            // Generate data.
            Container c[size];
            for (uint32_t i = 0; i < size; i++) {
                TimeStamp ts;
                c[i] = Container(Container::TIMESTAMP, ts);
                Thread::usleep(10);
            }

            stringstream inoutPlainNetstrings;

#ifndef WIN32
            long start = clock();
#endif
            // Force destruction of out to write data.
            {
                PlainNetstringsOut out(inoutPlainNetstrings);
                for (uint32_t i = 0; i < size; i++) {
                    out.write(0, c[i]);
                }
            }
#ifndef WIN32
            long end = clock();
            double duration = ((end - start) * 1000) / CLOCKS_PER_SEC;
            clog << endl << size << " entries of type TIMESTAMP each containing "
                 << sizeof(c[0]) << " bytes serialized using plain netstrings took " << duration
                 << "ms." << endl;
#endif

            stringstream inoutQueryableNetstrings;

#ifndef WIN32
            start = clock();
#endif
            // Force destruction of out to write data.
            {
                for (uint32_t i = 0; i < size; i++) {
                    inoutQueryableNetstrings << c[i];
                }
            }
#ifndef WIN32
            end = clock();
            duration = ((end - start) * 1000) / CLOCKS_PER_SEC;
            clog << size << " entries of type TIMESTAMP each containing "
                 << sizeof(c[0]) << " bytes serialized using queryable netstrings took " << duration
                 << "ms." << endl;
#endif

            // Reading:
#ifndef WIN32
            start = clock();
#endif
            {
                PlainNetstringsIn in(inoutPlainNetstrings);
                for (uint32_t i = 0; i < size; i++) {
                    Container ct;
                    in.read(0, ct);
                    TimeStamp ts1 = ct.getData<TimeStamp>();
                    TimeStamp ts2 = c[i].getData<TimeStamp>();
                    TS_ASSERT(ts1.toString() == ts2.toString());
                }
            }
#ifndef WIN32
            end = clock();
            duration = ((end - start) * 1000) / CLOCKS_PER_SEC;
            clog << size << " entries of type TIMESTAMP each containing "
                 << sizeof(c[0]) << " bytes deserialized and compared using plain netstrings took " << duration
                 << "ms." << endl;
#endif

#ifndef WIN32
            start = clock();
#endif
            {
                for (uint32_t i = 0; i < size; i++) {
                    Container ct;
                    inoutQueryableNetstrings >> ct;
                    TimeStamp ts1 = ct.getData<TimeStamp>();
                    TimeStamp ts2 = c[i].getData<TimeStamp>();
                    TS_ASSERT(ts1.toString() == ts2.toString());
                }
            }
#ifndef WIN32
            end = clock();
            duration = ((end - start) * 1000) / CLOCKS_PER_SEC;
            clog << size << " entries of type TIMESTAMP each containing "
                 << sizeof(c[0]) << " bytes deserialized and compared using queryable netstrings took " << duration
                 << "ms." << endl;
#endif
        }

        void testArraySerialisation()
        {
            stringstream stream;
            uint32_t array[10];
            for (uint32_t i=0; i<10; ++i)
            {
                array[i] = i;
            }

            SerializationFactory sf;

            {
                Serializer& s = sf.getSerializer(stream);
                s.write(1, &array, 10);
            }


            Deserializer& d = sf.getDeserializer(stream);
            uint32_t deserializedArray[10];
            d.read(1, &deserializedArray, 10);

            for (uint32_t i=0; i<10; ++i)
            {
                TS_WARN(deserializedArray[i] == i);
                cout << "Is: " << deserializedArray[i] << ", Should: " << i << endl;
            }

        }
};

#endif /*CORE_SERIALIZATIONTESTSUITE_H_*/
