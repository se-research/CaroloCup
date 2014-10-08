/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef CORE_STRINGDATATESTSUITE_H_
#define CORE_STRINGDATATESTSUITE_H_

#include "cxxtest/TestSuite.h"

#include <cstdlib>

#include <iostream>
#include <sstream>
#include <string>

#include "core/data/Container.h"
#include "core/data/StringData.h"

using namespace std;
using namespace core::base;
using namespace core::data;

class StringDataTest : public CxxTest::TestSuite {
    public:
        void testSerializationDeserialization() {
            // Create some data.
            StringData sd;
            string hw = "Hello World!";
            sd.setData(hw);

            // Create a data sink.
            stringstream inout("testoutput");
            inout << sd;
            inout.flush();

            // Read from the previously created data sink.
            StringData sd2;
            inout >> sd2;

            TS_ASSERT(sd2.getData() == "Hello World!");
        }

        void testSerializationDeserializationContainer() {
            // Create some data.
            StringData sd;
            string hw = "Hello World!";
            sd.setData(hw);

            Container c(Container::STRINGDATA, sd);

            // Create a data sink.
            stringstream inout("testoutput");
            inout << c;
            inout.flush();

            // Read from the previously created data sink.
            Container c2;
            inout >> c2;

            StringData sd2 = c2.getData<StringData>();

            TS_ASSERT(sd2.getData() == "Hello World!");
        }
};

#endif /*CORE_STRINGDATATESTSUITE_H_*/
