/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_POINTSHAPEDOBJECTTESTSUITE_H_
#define HESPERIA_POINTSHAPEDOBJECTTESTSUITE_H_

#include "cxxtest/TestSuite.h"

#include <sstream>

#include "core/data/environment/Point3.h"
#include "hesperia/data/environment/PointShapedObject.h"

using namespace std;
using namespace hesperia::data;
using namespace core::data::environment;
using namespace hesperia::data::environment;

class PointShapedObjectTest : public CxxTest::TestSuite {
    public:
        void testPointShapedObject() {
            Point3 position(1, 2, 3);
            Point3 rotation(4, 5, 6);
            Point3 velocity(7, 8, 9);
            Point3 acceleration(10, 11, 12);

            PointShapedObject pso;
            pso.setPosition(position);
            pso.setRotation(rotation);
            pso.setVelocity(velocity);
            pso.setAcceleration(acceleration);

            stringstream s;
            s << pso;
            s.flush();

            PointShapedObject pso2;
            s >> pso2;

            TS_ASSERT(pso.toString() == pso2.toString());
        }
};

#endif /*HESPERIA_POINTSHAPEDOBJECTTESTSUITE_H_*/
