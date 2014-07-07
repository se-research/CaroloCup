/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef SENSORBOARDDATATESTSUITE_H_
#define SENSORBOARDDATATESTSUITE_H_

#include "cxxtest/TestSuite.h"

// Include local header files.
#include "../include/SensorBoardData.h"

using namespace std;
using namespace msv;

class MSVDataTest : public CxxTest::TestSuite {
    public:
        /**
         * This method will be called before each testXYZ-method.
         */
        void setUp() {}

        /**
         * This method will be called after each testXYZ-method.
         */
        void tearDown() {}

        void testDataSpecification() {
            const double EPSILON = 0.00001;

            SensorBoardData sbd1;
            sbd1.update(1, 0.5); 
            sbd1.update(2, 0.3);
            sbd1.update(3, -1);

            TS_ASSERT(sbd1.getNumberOfSensors() == 3);
            TS_ASSERT_DELTA(sbd1.getDistance(1), 0.5, EPSILON);
            TS_ASSERT_DELTA(sbd1.getDistance(2), 0.3, EPSILON);
            TS_ASSERT_DELTA(sbd1.getDistance(3), -1, EPSILON);
            TS_ASSERT_DELTA(sbd1.getDistance(4), -2, EPSILON);
        }

        void testCopyConstructor() {
            const double EPSILON = 0.00001;

            SensorBoardData sbd1;
            sbd1.update(1, 0.5); 
            sbd1.update(2, 0.3);
            sbd1.update(3, -1);

            SensorBoardData sbd2(sbd1);

            TS_ASSERT(sbd2.getNumberOfSensors() == 3);
            TS_ASSERT_DELTA(sbd2.getDistance(1), 0.5, EPSILON);
            TS_ASSERT_DELTA(sbd2.getDistance(2), 0.3, EPSILON);
            TS_ASSERT_DELTA(sbd2.getDistance(3), -1, EPSILON);
            TS_ASSERT_DELTA(sbd2.getDistance(4), -2, EPSILON);
        }

        void testAssignmentOperator() {
            const double EPSILON = 0.00001;

            SensorBoardData sbd1;
            sbd1.update(1, 0.5); 
            sbd1.update(2, 0.3);
            sbd1.update(3, -1);

            SensorBoardData sbd2;
            sbd2 = sbd1;

            TS_ASSERT(sbd2.getNumberOfSensors() == 3);
            TS_ASSERT_DELTA(sbd2.getDistance(1), 0.5, EPSILON);
            TS_ASSERT_DELTA(sbd2.getDistance(2), 0.3, EPSILON);
            TS_ASSERT_DELTA(sbd2.getDistance(3), -1, EPSILON);
            TS_ASSERT_DELTA(sbd2.getDistance(4), -2, EPSILON);
        }

        void testSerialization() {
            const double EPSILON = 0.00001;

            SensorBoardData sbd1;
            sbd1.update(1, 0.5); 
            sbd1.update(2, 0.3);
            sbd1.update(3, -1);

            stringstream sstr;
            sstr << sbd1;

            SensorBoardData sbd2;
            
            sstr >> sbd2;

            TS_ASSERT(sbd2.getNumberOfSensors() == 3);
            TS_ASSERT_DELTA(sbd2.getDistance(1), 0.5, EPSILON);
            TS_ASSERT_DELTA(sbd2.getDistance(2), 0.3, EPSILON);
            TS_ASSERT_DELTA(sbd2.getDistance(3), -1, EPSILON);
            TS_ASSERT_DELTA(sbd2.getDistance(4), -2, EPSILON);
        }
};

#endif /*SENSORBOARDDATATESTSUITE_H_*/
