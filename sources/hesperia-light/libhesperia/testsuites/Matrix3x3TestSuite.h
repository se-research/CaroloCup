/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_MATRIX3X3TESTSUITE_H_
#define HESPERIA_MATRIX3X3TESTSUITE_H_

#include "cxxtest/TestSuite.h"

#include <cmath>
#include <string>
#include <sstream>

#include "hesperia/data/environment/Matrix3x3.h"

using namespace std;
using namespace hesperia::data;
using namespace hesperia::data::environment;

class Matrix3x3Test : public CxxTest::TestSuite {
    public:
        void testMatrix3x3FromString() {
            string s("(1.1; 2.2; 3.3; 4.4; 5.5; 6.6; 7.7; 8.8; 9.9)");
            Matrix3x3 m(s);

            TS_ASSERT(m.toString() == s);
        }

        void testGetterSetter() {
            Matrix3x3 m;
            TS_ASSERT_DELTA(m.getXX(), 0, 1e-5);
            TS_ASSERT_DELTA(m.getXY(), 0, 1e-5);
            TS_ASSERT_DELTA(m.getXZ(), 0, 1e-5);

            TS_ASSERT_DELTA(m.getYX(), 0, 1e-5);
            TS_ASSERT_DELTA(m.getYY(), 0, 1e-5);
            TS_ASSERT_DELTA(m.getYZ(), 0, 1e-5);

            TS_ASSERT_DELTA(m.getZX(), 0, 1e-5);
            TS_ASSERT_DELTA(m.getZY(), 0, 1e-5);
            TS_ASSERT_DELTA(m.getZZ(), 0, 1e-5);

            m.setXX(1.1);
            m.setXY(2.2);
            m.setXZ(3.3);

            m.setYX(4.4);
            m.setYY(5.5);
            m.setYZ(6.6);

            m.setZX(7.7);
            m.setZY(8.8);
            m.setZZ(9.9);

            TS_ASSERT_DELTA(m.getXX(), 1.1, 1e-5);
            TS_ASSERT_DELTA(m.getXY(), 2.2, 1e-5);
            TS_ASSERT_DELTA(m.getXZ(), 3.3, 1e-5);

            TS_ASSERT_DELTA(m.getYX(), 4.4, 1e-5);
            TS_ASSERT_DELTA(m.getYY(), 5.5, 1e-5);
            TS_ASSERT_DELTA(m.getYZ(), 6.6, 1e-5);

            TS_ASSERT_DELTA(m.getZX(), 7.7, 1e-5);
            TS_ASSERT_DELTA(m.getZY(), 8.8, 1e-5);
            TS_ASSERT_DELTA(m.getZZ(), 9.9, 1e-5);
        }

        void testSerialization() {
            Matrix3x3 m;
            TS_ASSERT_DELTA(m.getXX(), 0, 1e-5);
            TS_ASSERT_DELTA(m.getXY(), 0, 1e-5);
            TS_ASSERT_DELTA(m.getXZ(), 0, 1e-5);

            TS_ASSERT_DELTA(m.getYX(), 0, 1e-5);
            TS_ASSERT_DELTA(m.getYY(), 0, 1e-5);
            TS_ASSERT_DELTA(m.getYZ(), 0, 1e-5);

            TS_ASSERT_DELTA(m.getZX(), 0, 1e-5);
            TS_ASSERT_DELTA(m.getZY(), 0, 1e-5);
            TS_ASSERT_DELTA(m.getZZ(), 0, 1e-5);

            m.setXX(1.123456789);
            m.setXY(2.2);
            m.setXZ(3.3);

            m.setYX(4.4);
            m.setYY(5.5);
            m.setYZ(6.6);

            m.setZX(7.7);
            m.setZY(8.8);
            m.setZZ(9.9);

            TS_ASSERT_DELTA(m.getXX(), 1.123456789, 1e-9);
            TS_ASSERT_DELTA(m.getXY(), 2.2, 1e-5);
            TS_ASSERT_DELTA(m.getXZ(), 3.3, 1e-5);

            TS_ASSERT_DELTA(m.getYX(), 4.4, 1e-5);
            TS_ASSERT_DELTA(m.getYY(), 5.5, 1e-5);
            TS_ASSERT_DELTA(m.getYZ(), 6.6, 1e-5);

            TS_ASSERT_DELTA(m.getZX(), 7.7, 1e-5);
            TS_ASSERT_DELTA(m.getZY(), 8.8, 1e-5);
            TS_ASSERT_DELTA(m.getZZ(), 9.9, 1e-5);

            stringstream s;
            s << m;

            Matrix3x3 m2;

            TS_ASSERT_DELTA(m2.getXX(), 0, 1e-5);
            TS_ASSERT_DELTA(m2.getXY(), 0, 1e-5);
            TS_ASSERT_DELTA(m2.getXZ(), 0, 1e-5);

            TS_ASSERT_DELTA(m2.getYX(), 0, 1e-5);
            TS_ASSERT_DELTA(m2.getYY(), 0, 1e-5);
            TS_ASSERT_DELTA(m2.getYZ(), 0, 1e-5);

            TS_ASSERT_DELTA(m2.getZX(), 0, 1e-5);
            TS_ASSERT_DELTA(m2.getZY(), 0, 1e-5);
            TS_ASSERT_DELTA(m2.getZZ(), 0, 1e-5);

            s >> m2;

            TS_ASSERT_DELTA(m2.getXX(), 1.123456789, 1e-9);
            TS_ASSERT_DELTA(m2.getXY(), 2.2, 1e-5);
            TS_ASSERT_DELTA(m2.getXZ(), 3.3, 1e-5);

            TS_ASSERT_DELTA(m2.getYX(), 4.4, 1e-5);
            TS_ASSERT_DELTA(m2.getYY(), 5.5, 1e-5);
            TS_ASSERT_DELTA(m2.getYZ(), 6.6, 1e-5);

            TS_ASSERT_DELTA(m2.getZX(), 7.7, 1e-5);
            TS_ASSERT_DELTA(m2.getZY(), 8.8, 1e-5);
            TS_ASSERT_DELTA(m2.getZZ(), 9.9, 1e-5);

            TS_ASSERT(m == m2);
        }

        void testAdditionWithoutAssignment() {
            Matrix3x3 m1(1, 2, 3, 4, 5, 6, 7, 8, 9);
            Matrix3x3 m2(10, 20, 30, 40, 50, 60, 70, 80, 90);

            TS_ASSERT_DELTA(m1.getXX(), 1, 1e-5);
            TS_ASSERT_DELTA(m1.getXY(), 2, 1e-5);
            TS_ASSERT_DELTA(m1.getXZ(), 3, 1e-5);

            TS_ASSERT_DELTA(m1.getYX(), 4, 1e-5);
            TS_ASSERT_DELTA(m1.getYY(), 5, 1e-5);
            TS_ASSERT_DELTA(m1.getYZ(), 6, 1e-5);

            TS_ASSERT_DELTA(m1.getZX(), 7, 1e-5);
            TS_ASSERT_DELTA(m1.getZY(), 8, 1e-5);
            TS_ASSERT_DELTA(m1.getZZ(), 9, 1e-5);

            TS_ASSERT_DELTA(m2.getXX(), 10, 1e-5);
            TS_ASSERT_DELTA(m2.getXY(), 20, 1e-5);
            TS_ASSERT_DELTA(m2.getXZ(), 30, 1e-5);

            TS_ASSERT_DELTA(m2.getYX(), 40, 1e-5);
            TS_ASSERT_DELTA(m2.getYY(), 50, 1e-5);
            TS_ASSERT_DELTA(m2.getYZ(), 60, 1e-5);

            TS_ASSERT_DELTA(m2.getZX(), 70, 1e-5);
            TS_ASSERT_DELTA(m2.getZY(), 80, 1e-5);
            TS_ASSERT_DELTA(m2.getZZ(), 90, 1e-5);

            Matrix3x3 m3 = m1 + m2;

            TS_ASSERT_DELTA(m3.getXX(), 11, 1e-5);
            TS_ASSERT_DELTA(m3.getXY(), 22, 1e-5);
            TS_ASSERT_DELTA(m3.getXZ(), 33, 1e-5);

            TS_ASSERT_DELTA(m3.getYX(), 44, 1e-5);
            TS_ASSERT_DELTA(m3.getYY(), 55, 1e-5);
            TS_ASSERT_DELTA(m3.getYZ(), 66, 1e-5);

            TS_ASSERT_DELTA(m3.getZX(), 77, 1e-5);
            TS_ASSERT_DELTA(m3.getZY(), 88, 1e-5);
            TS_ASSERT_DELTA(m3.getZZ(), 99, 1e-5);
        }

        void testAdditionWithAssignment() {
            Matrix3x3 m1(1, 2, 3, 4, 5, 6, 7, 8, 9);
            Matrix3x3 m2(10, 20, 30, 40, 50, 60, 70, 80, 90);

            TS_ASSERT_DELTA(m1.getXX(), 1, 1e-5);
            TS_ASSERT_DELTA(m1.getXY(), 2, 1e-5);
            TS_ASSERT_DELTA(m1.getXZ(), 3, 1e-5);

            TS_ASSERT_DELTA(m1.getYX(), 4, 1e-5);
            TS_ASSERT_DELTA(m1.getYY(), 5, 1e-5);
            TS_ASSERT_DELTA(m1.getYZ(), 6, 1e-5);

            TS_ASSERT_DELTA(m1.getZX(), 7, 1e-5);
            TS_ASSERT_DELTA(m1.getZY(), 8, 1e-5);
            TS_ASSERT_DELTA(m1.getZZ(), 9, 1e-5);

            TS_ASSERT_DELTA(m2.getXX(), 10, 1e-5);
            TS_ASSERT_DELTA(m2.getXY(), 20, 1e-5);
            TS_ASSERT_DELTA(m2.getXZ(), 30, 1e-5);

            TS_ASSERT_DELTA(m2.getYX(), 40, 1e-5);
            TS_ASSERT_DELTA(m2.getYY(), 50, 1e-5);
            TS_ASSERT_DELTA(m2.getYZ(), 60, 1e-5);

            TS_ASSERT_DELTA(m2.getZX(), 70, 1e-5);
            TS_ASSERT_DELTA(m2.getZY(), 80, 1e-5);
            TS_ASSERT_DELTA(m2.getZZ(), 90, 1e-5);

            m1 += m2;

            TS_ASSERT_DELTA(m1.getXX(), 11, 1e-5);
            TS_ASSERT_DELTA(m1.getXY(), 22, 1e-5);
            TS_ASSERT_DELTA(m1.getXZ(), 33, 1e-5);

            TS_ASSERT_DELTA(m1.getYX(), 44, 1e-5);
            TS_ASSERT_DELTA(m1.getYY(), 55, 1e-5);
            TS_ASSERT_DELTA(m1.getYZ(), 66, 1e-5);

            TS_ASSERT_DELTA(m1.getZX(), 77, 1e-5);
            TS_ASSERT_DELTA(m1.getZY(), 88, 1e-5);
            TS_ASSERT_DELTA(m1.getZZ(), 99, 1e-5);
        }

        void testSubtractionWithoutAssignment() {
            Matrix3x3 m1(1, 2, 3, 4, 5, 6, 7, 8, 9);
            Matrix3x3 m2(10, 20, 30, 40, 50, 60, 70, 80, 90);

            TS_ASSERT_DELTA(m1.getXX(), 1, 1e-5);
            TS_ASSERT_DELTA(m1.getXY(), 2, 1e-5);
            TS_ASSERT_DELTA(m1.getXZ(), 3, 1e-5);

            TS_ASSERT_DELTA(m1.getYX(), 4, 1e-5);
            TS_ASSERT_DELTA(m1.getYY(), 5, 1e-5);
            TS_ASSERT_DELTA(m1.getYZ(), 6, 1e-5);

            TS_ASSERT_DELTA(m1.getZX(), 7, 1e-5);
            TS_ASSERT_DELTA(m1.getZY(), 8, 1e-5);
            TS_ASSERT_DELTA(m1.getZZ(), 9, 1e-5);

            TS_ASSERT_DELTA(m2.getXX(), 10, 1e-5);
            TS_ASSERT_DELTA(m2.getXY(), 20, 1e-5);
            TS_ASSERT_DELTA(m2.getXZ(), 30, 1e-5);

            TS_ASSERT_DELTA(m2.getYX(), 40, 1e-5);
            TS_ASSERT_DELTA(m2.getYY(), 50, 1e-5);
            TS_ASSERT_DELTA(m2.getYZ(), 60, 1e-5);

            TS_ASSERT_DELTA(m2.getZX(), 70, 1e-5);
            TS_ASSERT_DELTA(m2.getZY(), 80, 1e-5);
            TS_ASSERT_DELTA(m2.getZZ(), 90, 1e-5);

            Matrix3x3 m3 = m1 - m2;

            TS_ASSERT_DELTA(m3.getXX(), -9, 1e-5);
            TS_ASSERT_DELTA(m3.getXY(), -18, 1e-5);
            TS_ASSERT_DELTA(m3.getXZ(), -27, 1e-5);

            TS_ASSERT_DELTA(m3.getYX(), -36, 1e-5);
            TS_ASSERT_DELTA(m3.getYY(), -45, 1e-5);
            TS_ASSERT_DELTA(m3.getYZ(), -54, 1e-5);

            TS_ASSERT_DELTA(m3.getZX(), -63, 1e-5);
            TS_ASSERT_DELTA(m3.getZY(), -72, 1e-5);
            TS_ASSERT_DELTA(m3.getZZ(), -81, 1e-5);
        }

        void testSubtractionWithAssignment() {
            Matrix3x3 m1(1, 2, 3, 4, 5, 6, 7, 8, 9);
            Matrix3x3 m2(10, 20, 30, 40, 50, 60, 70, 80, 90);

            TS_ASSERT_DELTA(m1.getXX(), 1, 1e-5);
            TS_ASSERT_DELTA(m1.getXY(), 2, 1e-5);
            TS_ASSERT_DELTA(m1.getXZ(), 3, 1e-5);

            TS_ASSERT_DELTA(m1.getYX(), 4, 1e-5);
            TS_ASSERT_DELTA(m1.getYY(), 5, 1e-5);
            TS_ASSERT_DELTA(m1.getYZ(), 6, 1e-5);

            TS_ASSERT_DELTA(m1.getZX(), 7, 1e-5);
            TS_ASSERT_DELTA(m1.getZY(), 8, 1e-5);
            TS_ASSERT_DELTA(m1.getZZ(), 9, 1e-5);

            TS_ASSERT_DELTA(m2.getXX(), 10, 1e-5);
            TS_ASSERT_DELTA(m2.getXY(), 20, 1e-5);
            TS_ASSERT_DELTA(m2.getXZ(), 30, 1e-5);

            TS_ASSERT_DELTA(m2.getYX(), 40, 1e-5);
            TS_ASSERT_DELTA(m2.getYY(), 50, 1e-5);
            TS_ASSERT_DELTA(m2.getYZ(), 60, 1e-5);

            TS_ASSERT_DELTA(m2.getZX(), 70, 1e-5);
            TS_ASSERT_DELTA(m2.getZY(), 80, 1e-5);
            TS_ASSERT_DELTA(m2.getZZ(), 90, 1e-5);

            m1 -= m2;

            TS_ASSERT_DELTA(m1.getXX(), -9, 1e-5);
            TS_ASSERT_DELTA(m1.getXY(), -18, 1e-5);
            TS_ASSERT_DELTA(m1.getXZ(), -27, 1e-5);

            TS_ASSERT_DELTA(m1.getYX(), -36, 1e-5);
            TS_ASSERT_DELTA(m1.getYY(), -45, 1e-5);
            TS_ASSERT_DELTA(m1.getYZ(), -54, 1e-5);

            TS_ASSERT_DELTA(m1.getZX(), -63, 1e-5);
            TS_ASSERT_DELTA(m1.getZY(), -72, 1e-5);
            TS_ASSERT_DELTA(m1.getZZ(), -81, 1e-5);
        }

        void testScalingWithoutAssignment() {
            const double a = 2;

            Matrix3x3 m1(1, 2, 3, 4, 5, 6, 7, 8, 9);

            TS_ASSERT_DELTA(m1.getXX(), 1, 1e-5);
            TS_ASSERT_DELTA(m1.getXY(), 2, 1e-5);
            TS_ASSERT_DELTA(m1.getXZ(), 3, 1e-5);

            TS_ASSERT_DELTA(m1.getYX(), 4, 1e-5);
            TS_ASSERT_DELTA(m1.getYY(), 5, 1e-5);
            TS_ASSERT_DELTA(m1.getYZ(), 6, 1e-5);

            TS_ASSERT_DELTA(m1.getZX(), 7, 1e-5);
            TS_ASSERT_DELTA(m1.getZY(), 8, 1e-5);
            TS_ASSERT_DELTA(m1.getZZ(), 9, 1e-5);

            Matrix3x3 m3 = m1 * a;

            TS_ASSERT_DELTA(m3.getXX(), 2, 1e-5);
            TS_ASSERT_DELTA(m3.getXY(), 4, 1e-5);
            TS_ASSERT_DELTA(m3.getXZ(), 6, 1e-5);

            TS_ASSERT_DELTA(m3.getYX(), 8, 1e-5);
            TS_ASSERT_DELTA(m3.getYY(), 10, 1e-5);
            TS_ASSERT_DELTA(m3.getYZ(), 12, 1e-5);

            TS_ASSERT_DELTA(m3.getZX(), 14, 1e-5);
            TS_ASSERT_DELTA(m3.getZY(), 16, 1e-5);
            TS_ASSERT_DELTA(m3.getZZ(), 18, 1e-5);
        }

        void testScalingWithAssignment() {
            const double a = 2;

            Matrix3x3 m1(1, 2, 3, 4, 5, 6, 7, 8, 9);

            TS_ASSERT_DELTA(m1.getXX(), 1, 1e-5);
            TS_ASSERT_DELTA(m1.getXY(), 2, 1e-5);
            TS_ASSERT_DELTA(m1.getXZ(), 3, 1e-5);

            TS_ASSERT_DELTA(m1.getYX(), 4, 1e-5);
            TS_ASSERT_DELTA(m1.getYY(), 5, 1e-5);
            TS_ASSERT_DELTA(m1.getYZ(), 6, 1e-5);

            TS_ASSERT_DELTA(m1.getZX(), 7, 1e-5);
            TS_ASSERT_DELTA(m1.getZY(), 8, 1e-5);
            TS_ASSERT_DELTA(m1.getZZ(), 9, 1e-5);

            m1 *= a;

            TS_ASSERT_DELTA(m1.getXX(), 2, 1e-5);
            TS_ASSERT_DELTA(m1.getXY(), 4, 1e-5);
            TS_ASSERT_DELTA(m1.getXZ(), 6, 1e-5);

            TS_ASSERT_DELTA(m1.getYX(), 8, 1e-5);
            TS_ASSERT_DELTA(m1.getYY(), 10, 1e-5);
            TS_ASSERT_DELTA(m1.getYZ(), 12, 1e-5);

            TS_ASSERT_DELTA(m1.getZX(), 14, 1e-5);
            TS_ASSERT_DELTA(m1.getZY(), 16, 1e-5);
            TS_ASSERT_DELTA(m1.getZZ(), 18, 1e-5);
        }

        void testMultiplyWithoutAssignment() {
            Matrix3x3 m1(1, 2, 3, 4, 5, 6, 7, 8, 9);
            Matrix3x3 m2(10, 20, 30, 40, 50, 60, 70, 80, 90);

            TS_ASSERT_DELTA(m1.getXX(), 1, 1e-5);
            TS_ASSERT_DELTA(m1.getXY(), 2, 1e-5);
            TS_ASSERT_DELTA(m1.getXZ(), 3, 1e-5);

            TS_ASSERT_DELTA(m1.getYX(), 4, 1e-5);
            TS_ASSERT_DELTA(m1.getYY(), 5, 1e-5);
            TS_ASSERT_DELTA(m1.getYZ(), 6, 1e-5);

            TS_ASSERT_DELTA(m1.getZX(), 7, 1e-5);
            TS_ASSERT_DELTA(m1.getZY(), 8, 1e-5);
            TS_ASSERT_DELTA(m1.getZZ(), 9, 1e-5);

            TS_ASSERT_DELTA(m2.getXX(), 10, 1e-5);
            TS_ASSERT_DELTA(m2.getXY(), 20, 1e-5);
            TS_ASSERT_DELTA(m2.getXZ(), 30, 1e-5);

            TS_ASSERT_DELTA(m2.getYX(), 40, 1e-5);
            TS_ASSERT_DELTA(m2.getYY(), 50, 1e-5);
            TS_ASSERT_DELTA(m2.getYZ(), 60, 1e-5);

            TS_ASSERT_DELTA(m2.getZX(), 70, 1e-5);
            TS_ASSERT_DELTA(m2.getZY(), 80, 1e-5);
            TS_ASSERT_DELTA(m2.getZZ(), 90, 1e-5);

            Matrix3x3 m3 = m1 * m2;

            TS_ASSERT_DELTA(m3.getXX(), 300, 1e-5);
            TS_ASSERT_DELTA(m3.getXY(), 360, 1e-5);
            TS_ASSERT_DELTA(m3.getXZ(), 420, 1e-5);

            TS_ASSERT_DELTA(m3.getYX(), 660, 1e-5);
            TS_ASSERT_DELTA(m3.getYY(), 810, 1e-5);
            TS_ASSERT_DELTA(m3.getYZ(), 960, 1e-5);

            TS_ASSERT_DELTA(m3.getZX(), 1020, 1e-5);
            TS_ASSERT_DELTA(m3.getZY(), 1260, 1e-5);
            TS_ASSERT_DELTA(m3.getZZ(), 1500, 1e-5);
        }

        void testMultiplyWithAssignment() {
            Matrix3x3 m1(1, 2, 3, 4, 5, 6, 7, 8, 9);
            Matrix3x3 m2(10, 20, 30, 40, 50, 60, 70, 80, 90);

            TS_ASSERT_DELTA(m1.getXX(), 1, 1e-5);
            TS_ASSERT_DELTA(m1.getXY(), 2, 1e-5);
            TS_ASSERT_DELTA(m1.getXZ(), 3, 1e-5);

            TS_ASSERT_DELTA(m1.getYX(), 4, 1e-5);
            TS_ASSERT_DELTA(m1.getYY(), 5, 1e-5);
            TS_ASSERT_DELTA(m1.getYZ(), 6, 1e-5);

            TS_ASSERT_DELTA(m1.getZX(), 7, 1e-5);
            TS_ASSERT_DELTA(m1.getZY(), 8, 1e-5);
            TS_ASSERT_DELTA(m1.getZZ(), 9, 1e-5);

            TS_ASSERT_DELTA(m2.getXX(), 10, 1e-5);
            TS_ASSERT_DELTA(m2.getXY(), 20, 1e-5);
            TS_ASSERT_DELTA(m2.getXZ(), 30, 1e-5);

            TS_ASSERT_DELTA(m2.getYX(), 40, 1e-5);
            TS_ASSERT_DELTA(m2.getYY(), 50, 1e-5);
            TS_ASSERT_DELTA(m2.getYZ(), 60, 1e-5);

            TS_ASSERT_DELTA(m2.getZX(), 70, 1e-5);
            TS_ASSERT_DELTA(m2.getZY(), 80, 1e-5);
            TS_ASSERT_DELTA(m2.getZZ(), 90, 1e-5);

            m1 *= m2;

            TS_ASSERT_DELTA(m1.getXX(), 300, 1e-5);
            TS_ASSERT_DELTA(m1.getXY(), 360, 1e-5);
            TS_ASSERT_DELTA(m1.getXZ(), 420, 1e-5);

            TS_ASSERT_DELTA(m1.getYX(), 660, 1e-5);
            TS_ASSERT_DELTA(m1.getYY(), 810, 1e-5);
            TS_ASSERT_DELTA(m1.getYZ(), 960, 1e-5);

            TS_ASSERT_DELTA(m1.getZX(), 1020, 1e-5);
            TS_ASSERT_DELTA(m1.getZY(), 1260, 1e-5);
            TS_ASSERT_DELTA(m1.getZZ(), 1500, 1e-5);
        }

        void testEqual() {
            Matrix3x3 m1(1, 2, 3, 4, 5, 6, 7, 8, 9);
            Matrix3x3 m2(1, 2, 3, 4, 5, 6, 7, 8, 9);
            Matrix3x3 m3(1, 4, 3, 4, 5, 6, 7, 8, 9);

            TS_ASSERT(m1 == m2);
            TS_ASSERT(m1 != m3);
        }

        void testTranspose() {
            Matrix3x3 m1(1, 2, 3, 4, 5, 6, 7, 8, 9);

            TS_ASSERT_DELTA(m1.getXX(), 1, 1e-5);
            TS_ASSERT_DELTA(m1.getXY(), 2, 1e-5);
            TS_ASSERT_DELTA(m1.getXZ(), 3, 1e-5);

            TS_ASSERT_DELTA(m1.getYX(), 4, 1e-5);
            TS_ASSERT_DELTA(m1.getYY(), 5, 1e-5);
            TS_ASSERT_DELTA(m1.getYZ(), 6, 1e-5);

            TS_ASSERT_DELTA(m1.getZX(), 7, 1e-5);
            TS_ASSERT_DELTA(m1.getZY(), 8, 1e-5);
            TS_ASSERT_DELTA(m1.getZZ(), 9, 1e-5);

            m1.transpose();

            TS_ASSERT_DELTA(m1.getXX(), 1, 1e-5);
            TS_ASSERT_DELTA(m1.getXY(), 4, 1e-5);
            TS_ASSERT_DELTA(m1.getXZ(), 7, 1e-5);

            TS_ASSERT_DELTA(m1.getYX(), 2, 1e-5);
            TS_ASSERT_DELTA(m1.getYY(), 5, 1e-5);
            TS_ASSERT_DELTA(m1.getYZ(), 8, 1e-5);

            TS_ASSERT_DELTA(m1.getZX(), 3, 1e-5);
            TS_ASSERT_DELTA(m1.getZY(), 6, 1e-5);
            TS_ASSERT_DELTA(m1.getZZ(), 9, 1e-5);
        }
};

#endif /*HESPERIA_MATRIX3X3TESTSUITE_H_*/
