/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_POSITIONTESTSUITE_H_
#define HESPERIA_POSITIONTESTSUITE_H_

#include "cxxtest/TestSuite.h"

#include <sstream>

#include "hesperia/data/environment/Point3.h"
#include "hesperia/data/environment/Position.h"

using namespace std;
using namespace hesperia::data;
using namespace hesperia::data::environment;

class PositionTest : public CxxTest::TestSuite {
    public:
        void testPosition() {
            Point3 position(1, 2, 3);
            Point3 rotation(4, 5, 6);

            Position p;
            p.setPosition(position);
            p.setRotation(rotation);

            stringstream s;
            s << p;
            s.flush();

            Position p2;
            s >> p2;

            TS_ASSERT(p.toString() == p2.toString());
        }
};

#endif /*HESPERIA_POSITIONTESTSUITE_H_*/
