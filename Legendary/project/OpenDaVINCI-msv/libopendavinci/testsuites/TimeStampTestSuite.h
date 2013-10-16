/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef CORE_TIMESTAMPTESTSUITE_H_
#define CORE_TIMESTAMPTESTSUITE_H_

#include "cxxtest/TestSuite.h"

#include <iostream>
#include <sstream>
#include <string>

#include "core/data/TimeStamp.h"

using namespace std;
using namespace core::data;

class TimeStampTest : public CxxTest::TestSuite {
    public:
        void testTimeStamp28042009() {
            TimeStamp ts(1240926174, 1234);

            TS_ASSERT(ts.getDay() == 28);
            TS_ASSERT(ts.getMonth() == 4);
            TS_ASSERT(ts.getYear() == 2009);
            TS_ASSERT(ts.getHour() == 13);
            TS_ASSERT(ts.getMinute() == 42);
            TS_ASSERT(ts.getSecond() == 54);

            TimeStamp ts2("28042009134254");

            TS_ASSERT(ts2.getDay() == 28);
            TS_ASSERT(ts2.getMonth() == 4);
            TS_ASSERT(ts2.getYear() == 2009);
            TS_ASSERT(ts2.getHour() == 13);
            TS_ASSERT(ts2.getMinute() == 42);
            TS_ASSERT(ts2.getSecond() == 54);
        }
};

#endif /*CORE_TIMESTAMPTESTSUITE_H_*/
