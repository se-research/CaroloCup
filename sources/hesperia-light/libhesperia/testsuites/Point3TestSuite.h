/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_POINT3TESTSUITE_H_
#define HESPERIA_POINT3TESTSUITE_H_

#include "cxxtest/TestSuite.h"

#include <cmath>
#include <string>
#include <sstream>

#include "hesperia/data/environment/Point3.h"

using namespace std;
using namespace hesperia::data;
using namespace hesperia::data::environment;

class Point3Test : public CxxTest::TestSuite {
    public:
        void testPoint3FromString() {
            string s("(1.1; 2.2; 3.3)");
            Point3 c(s);

            TS_ASSERT(c.toString() == s);
        }

        void testGetterSetter() {
            Point3 cc;
            TS_ASSERT_DELTA(cc.getX(), 0, 1e-5);
            TS_ASSERT_DELTA(cc.getY(), 0, 1e-5);
            TS_ASSERT_DELTA(cc.getZ(), 0, 1e-5);

            cc.setX(42);
            cc.setY(-42);
            cc.setZ(342);

            TS_ASSERT_DELTA(cc.getX(), 42, 1e-5);
            TS_ASSERT_DELTA(cc.getY(), -42, 1e-5);
            TS_ASSERT_DELTA(cc.getZ(), 342, 1e-5);
        }

        void testSerialization() {
            Point3 cc;
            TS_ASSERT_DELTA(cc.getX(), 0, 1e-5);
            TS_ASSERT_DELTA(cc.getY(), 0, 1e-5);
            TS_ASSERT_DELTA(cc.getZ(), 0, 1e-5);

            cc.setX(42);
            cc.setY(-42);
            cc.setZ(342);

            TS_ASSERT_DELTA(cc.getX(), 42, 1e-5);
            TS_ASSERT_DELTA(cc.getY(), -42, 1e-5);
            TS_ASSERT_DELTA(cc.getZ(), 342, 1e-5);

            stringstream s;
            s << cc;

            Point3 cc2;
            TS_ASSERT_DELTA(cc2.getX(), 0, 1e-5);
            TS_ASSERT_DELTA(cc2.getY(), 0, 1e-5);
            TS_ASSERT_DELTA(cc2.getZ(), 0, 1e-5);

            s >> cc2;

            TS_ASSERT_DELTA(cc2.getX(), 42, 1e-5);
            TS_ASSERT_DELTA(cc2.getY(), -42, 1e-5);
            TS_ASSERT_DELTA(cc2.getZ(), 342, 1e-5);
        }

        void testPreciseSerialization() {
            Point3 cc;
            TS_ASSERT_DELTA(cc.getX(), 0, 1e-5);
            TS_ASSERT_DELTA(cc.getY(), 0, 1e-5);
            TS_ASSERT_DELTA(cc.getZ(), 0, 1e-5);

            cc.setX(1.23456789);
            cc.setY(3.45678901);
            cc.setZ(4.56789012);

            TS_ASSERT_DELTA(cc.getX(), 1.23456789, 1e-8);
            TS_ASSERT_DELTA(cc.getY(), 3.45678901, 1e-8);
            TS_ASSERT_DELTA(cc.getZ(), 4.56789012, 1e-8);

            stringstream s;
            s << cc;

            Point3 cc2;
            TS_ASSERT_DELTA(cc2.getX(), 0, 1e-5);
            TS_ASSERT_DELTA(cc2.getY(), 0, 1e-5);
            TS_ASSERT_DELTA(cc2.getZ(), 0, 1e-5);

            s >> cc2;

            TS_ASSERT_DELTA(cc2.getX(), 1.23456789, 1e-8);
            TS_ASSERT_DELTA(cc2.getY(), 3.45678901, 1e-8);
            TS_ASSERT_DELTA(cc2.getZ(), 4.56789012, 1e-8);
        }

        void testAdditionWithoutAssignment() {
            Point3 cc1(1, 2, 3);
            Point3 cc2(4, 5, 6);

            TS_ASSERT_DELTA(cc1.getX(), 1, 1e-5);
            TS_ASSERT_DELTA(cc1.getY(), 2, 1e-5);
            TS_ASSERT_DELTA(cc1.getZ(), 3, 1e-5);

            TS_ASSERT_DELTA(cc2.getX(), 4, 1e-5);
            TS_ASSERT_DELTA(cc2.getY(), 5, 1e-5);
            TS_ASSERT_DELTA(cc2.getZ(), 6, 1e-5);

            Point3 cc3 = cc1 + cc2;

            TS_ASSERT_DELTA(cc3.getX(), 5, 1e-5);
            TS_ASSERT_DELTA(cc3.getY(), 7, 1e-5);
            TS_ASSERT_DELTA(cc3.getZ(), 9, 1e-5);
        }

        void testAdditionWithAssignment() {
            Point3 cc1(1, 2, 3);
            Point3 cc2(4, 5, 6);

            TS_ASSERT_DELTA(cc1.getX(), 1, 1e-5);
            TS_ASSERT_DELTA(cc1.getY(), 2, 1e-5);
            TS_ASSERT_DELTA(cc1.getZ(), 3, 1e-5);

            TS_ASSERT_DELTA(cc2.getX(), 4, 1e-5);
            TS_ASSERT_DELTA(cc2.getY(), 5, 1e-5);
            TS_ASSERT_DELTA(cc2.getZ(), 6, 1e-5);

            cc1 += cc2;

            TS_ASSERT_DELTA(cc1.getX(), 5, 1e-5);
            TS_ASSERT_DELTA(cc1.getY(), 7, 1e-5);
            TS_ASSERT_DELTA(cc1.getZ(), 9, 1e-5);
        }

        void testSubtractionWithoutAssignment() {
            Point3 cc1(1, 2, 3);
            Point3 cc2(6, 5, 4);

            TS_ASSERT_DELTA(cc1.getX(), 1, 1e-5);
            TS_ASSERT_DELTA(cc1.getY(), 2, 1e-5);
            TS_ASSERT_DELTA(cc1.getZ(), 3, 1e-5);

            TS_ASSERT_DELTA(cc2.getX(), 6, 1e-5);
            TS_ASSERT_DELTA(cc2.getY(), 5, 1e-5);
            TS_ASSERT_DELTA(cc2.getZ(), 4, 1e-5);

            Point3 cc3 = cc1 - cc2;

            TS_ASSERT_DELTA(cc3.getX(), -5, 1e-5);
            TS_ASSERT_DELTA(cc3.getY(), -3, 1e-5);
            TS_ASSERT_DELTA(cc3.getZ(), -1, 1e-5);
        }

        void testSubtractionWithAssignment() {
            Point3 cc1(1, 2, 3);
            Point3 cc2(6, 5, 4);

            TS_ASSERT_DELTA(cc1.getX(), 1, 1e-5);
            TS_ASSERT_DELTA(cc1.getY(), 2, 1e-5);
            TS_ASSERT_DELTA(cc1.getZ(), 3, 1e-5);

            TS_ASSERT_DELTA(cc2.getX(), 6, 1e-5);
            TS_ASSERT_DELTA(cc2.getY(), 5, 1e-5);
            TS_ASSERT_DELTA(cc2.getZ(), 4, 1e-5);

            cc1 -= cc2;

            TS_ASSERT_DELTA(cc1.getX(), -5, 1e-5);
            TS_ASSERT_DELTA(cc1.getY(), -3, 1e-5);
            TS_ASSERT_DELTA(cc1.getZ(), -1, 1e-5);
        }

        void testScalingWithoutAssignment() {
            Point3 cc1(1, 2, 3);
            const double a = 0.5;

            TS_ASSERT_DELTA(cc1.getX(), 1, 1e-5);
            TS_ASSERT_DELTA(cc1.getY(), 2, 1e-5);
            TS_ASSERT_DELTA(cc1.getZ(), 3, 1e-5);

            Point3 cc2 = cc1 * a;

            TS_ASSERT_DELTA(cc2.getX(), 0.5, 1e-5);
            TS_ASSERT_DELTA(cc2.getY(), 1, 1e-5);
            TS_ASSERT_DELTA(cc2.getZ(), 1.5, 1e-5);
        }

        void testScalingWithAssignment() {
            Point3 cc1(1, 2, 3);
            const double a = 0.5;

            TS_ASSERT_DELTA(cc1.getX(), 1, 1e-5);
            TS_ASSERT_DELTA(cc1.getY(), 2, 1e-5);
            TS_ASSERT_DELTA(cc1.getZ(), 3, 1e-5);

            cc1 *= a;

            TS_ASSERT_DELTA(cc1.getX(), 0.5, 1e-5);
            TS_ASSERT_DELTA(cc1.getY(), 1, 1e-5);
            TS_ASSERT_DELTA(cc1.getZ(), 1.5, 1e-5);
        }

        void testScalarProduct() {
            Point3 cc1(1, 2, 3);
            Point3 cc2(6, 5, 4);

            TS_ASSERT_DELTA(cc1 * cc2, 28, 1e-5);
        }

        void testLength() {
            const double e14 = sqrt(14.0f);
            const double e5 = sqrt(5.0f);

            Point3 cc1(1, 2, 3);

            TS_ASSERT_DELTA(cc1.length(), e14, 1e-5);
            TS_ASSERT_DELTA(cc1.lengthXY(), e5, 1e-5);
        }

        void testNormalize() {
            const double e14 = sqrt(14.0f);
            const double e5 = sqrt(5.0f);

            Point3 cc1(1, 2, 3);
            Point3 cc2(1, 2, 3);

            TS_ASSERT_DELTA(cc1.getX(), 1, 1e-5);
            TS_ASSERT_DELTA(cc1.getY(), 2, 1e-5);
            TS_ASSERT_DELTA(cc1.getZ(), 3, 1e-5);

            TS_ASSERT_DELTA(cc2.getX(), 1, 1e-5);
            TS_ASSERT_DELTA(cc2.getY(), 2, 1e-5);
            TS_ASSERT_DELTA(cc2.getZ(), 3, 1e-5);

            cc1.normalize();
            cc2.normalizeXY();

            TS_ASSERT_DELTA(cc1.getX(), 1.0 / e14, 1e-5);
            TS_ASSERT_DELTA(cc1.getY(), 2.0 / e14, 1e-5);
            TS_ASSERT_DELTA(cc1.getZ(), 3.0 / e14, 1e-5);

            TS_ASSERT_DELTA(cc2.getX(), 1.0 / e5, 1e-5);
            TS_ASSERT_DELTA(cc2.getY(), 2.0 / e5, 1e-5);
            TS_ASSERT_DELTA(cc2.getZ(), 0, 1e-5);
        }

        void testEqual() {
            Point3 cc1(1, 2, 3);
            Point3 cc2(1, 2, 3);
            Point3 cc3(1, 2, 4);

            TS_ASSERT(cc1 == cc2);
            TS_ASSERT(cc1 != cc3);
        }

        void testCrossProduct() {
            Point3 cc1(1, 2, 3);
            Point3 cc2(4, 5, 6);
            Point3 cc3 = cc1.cross(cc2);

            TS_ASSERT_DELTA(cc3.getX(), -3, 1e-5);
            TS_ASSERT_DELTA(cc3.getY(), 6, 1e-5);
            TS_ASSERT_DELTA(cc3.getZ(), -3, 1e-5);
        }

        void testDistance() {
            Point3 cc1(1, 2, 3);
            Point3 cc2(4, 5, 6);

            TS_ASSERT_DELTA(cc1.getSquaredDistanceTo(cc2), 27, 1e-5);
            TS_ASSERT_DELTA(cc1.getSquaredXYDistanceTo(cc2), 18, 1e-5);
            TS_ASSERT_DELTA(cc1.getDistanceTo(cc2), sqrt(27.0f), 1e-5);
            TS_ASSERT_DELTA(cc1.getXYDistanceTo(cc2), sqrt(18.0f), 1e-5);
        }

        void testRotationX() {
            Point3 cc1(1, 2, 3);
            Point3 cc1Assignment = cc1.rotateX(0.5);
            Point3 cc2(1, 2, 3);
            Point3 cc2Assignment = cc2.rotateX(-0.5);

            TS_ASSERT(cc1 == cc1Assignment);
            TS_ASSERT(cc2 == cc2Assignment);

            TS_ASSERT_DELTA(cc1.getX(), 1, 1e-3);
            TS_ASSERT_DELTA(cc1.getY(), 0.3168, 1e-3);
            TS_ASSERT_DELTA(cc1.getZ(), 3.5915, 1e-3);

            TS_ASSERT_DELTA(cc2.getX(), 1, 1e-3);
            TS_ASSERT_DELTA(cc2.getY(), 3.1934, 1e-3);
            TS_ASSERT_DELTA(cc2.getZ(), 1.6738, 1e-3);
        }

        void testRotationY() {
            Point3 cc1(1, 2, 3);
            Point3 cc1Assignment = cc1.rotateY(0.5);
            Point3 cc2(1, 2, 3);
            Point3 cc2Assignment = cc2.rotateY(-0.5);

            TS_ASSERT(cc1 == cc1Assignment);
            TS_ASSERT(cc2 == cc2Assignment);

            TS_ASSERT_DELTA(cc1.getX(), -0.5606, 1e-3);
            TS_ASSERT_DELTA(cc1.getY(), 2, 1e-3);
            TS_ASSERT_DELTA(cc1.getZ(), 3.1121, 1e-3);

            TS_ASSERT_DELTA(cc2.getX(), 2.3158, 1e-3);
            TS_ASSERT_DELTA(cc2.getY(), 2, 1e-3);
            TS_ASSERT_DELTA(cc2.getZ(), 2.1533, 1e-3);
        }

        void testRotationZ() {
            Point3 cc1(1, 2, 3);
            Point3 cc1Assignment = cc1.rotateZ(0.5);
            Point3 cc2(1, 2, 3);
            Point3 cc2Assignment = cc2.rotateZ(-0.5);

            TS_ASSERT(cc1 == cc1Assignment);
            TS_ASSERT(cc2 == cc2Assignment);

            TS_ASSERT_DELTA(cc1.getX(), -0.0812, 1e-3);
            TS_ASSERT_DELTA(cc1.getY(), 2.2345, 1e-3);
            TS_ASSERT_DELTA(cc1.getZ(), 3, 1e-3);

            TS_ASSERT_DELTA(cc2.getX(), 1.8364, 1e-3);
            TS_ASSERT_DELTA(cc2.getY(), 1.2757, 1e-3);
            TS_ASSERT_DELTA(cc2.getZ(), 3, 1e-3);
        }
};

#endif /*HESPERIA_POINT3TESTSUITE_H_*/
