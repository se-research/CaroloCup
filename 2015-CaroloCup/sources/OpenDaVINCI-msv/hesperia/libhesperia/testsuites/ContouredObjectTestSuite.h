/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CONTOUREDOBJECTTESTSUITE_H_
#define HESPERIA_CONTOUREDOBJECTTESTSUITE_H_

#include "cxxtest/TestSuite.h"

#include <sstream>
#include <vector>

#include "core/data/environment/Point3.h"
#include "hesperia/data/sensor/ContouredObject.h"

using namespace std;
using namespace hesperia::data;
using namespace core::data::environment;
using namespace hesperia::data::sensor;

class ContouredObjectTest : public CxxTest::TestSuite {
    public:
        void testPointShapedObject() {
            Point3 position(1, 2, 3);
            Point3 rotation(4, 5, 6);
            Point3 velocity(7, 8, 9);
            Point3 acceleration(10, 11, 12);

            Point3 c1(13, 14, 15);
            Point3 c2(16, 17, 18);
            vector<Point3> contour;
            contour.push_back(c1);
            contour.push_back(c2);

            ContouredObject co(position, rotation, velocity, acceleration);
            co.setContour(contour);

            stringstream s;
            s << co;
            s.flush();

            ContouredObject co2;
            s >> co2;

            vector<Point3> contour2 = co2.getContour();

            TS_ASSERT(co.toString() == co2.toString());
            TS_ASSERT(contour.at(0).toString() == c1.toString());
            TS_ASSERT(contour.at(1).toString() == c2.toString());
        }
};

#endif /*HESPERIA_CONTOUREDOBJECTTESTSUITE_H_*/
