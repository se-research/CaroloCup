/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_EGOSTATETESTSUITE_H_
#define HESPERIA_EGOSTATETESTSUITE_H_

#include "cxxtest/TestSuite.h"

#include <sstream>

#include "core/data/environment/Point3.h"
#include "hesperia/data/environment/EgoState.h"

using namespace std;
using namespace hesperia::data;
using namespace core::data::environment;
using namespace hesperia::data::environment;

class EgoStateTest : public CxxTest::TestSuite {
    public:
        void testEgoState() {
            Point3 position(1, 2, 3);
            Point3 rotation(4, 5, 6);
            Point3 velocity(7, 8, 9);
            Point3 acceleration(10, 11, 12);

            EgoState es(position, rotation, velocity, acceleration);

            stringstream s;
            s << es;
            s.flush();

            EgoState es2;
            s >> es2;

            TS_ASSERT(es.toString() == es2.toString());
        }
};

#endif /*HESPERIA_EGOSTATETESTSUITE_H_*/
