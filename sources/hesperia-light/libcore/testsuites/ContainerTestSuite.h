/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef CORE_CONTAINERTESTSUITE_H_
#define CORE_CONTAINERTESTSUITE_H_

#include "cxxtest/TestSuite.h"

#include <sstream>

#include "core/data/Container.h"
#include "core/data/TimeStamp.h"

using namespace std;
using namespace core::data;

class ContainerTest : public CxxTest::TestSuite {
    public:
        void testContainerData() {
            TimeStamp ts;
            Container c(Container::TIMESTAMP, ts);

            stringstream s;
            s << c;
            s.flush();

            Container c2;
            s >> c2;
            TimeStamp ts2;
            ts2 = c2.getData<TimeStamp>();

            TS_ASSERT(ts.toString() == ts2.toString());
        }
};

#endif /*CORE_CONTAINERTESTSUITE_H_*/
