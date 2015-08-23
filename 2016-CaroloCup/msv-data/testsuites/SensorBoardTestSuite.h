/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef SENSORBOARDDATATESTSUITE_H_
#define SENSORBOARDDATATESTSUITE_H_

#include "cxxtest/TestSuite.h"

// Include local header files.
#include "GeneratedHeaders_AutomotiveData.h"

using namespace std;
using namespace automotive::miniature;

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
            sbd1.putTo_MapOfDistances(1, 0.5); 
            sbd1.putTo_MapOfDistances(2, 0.3);
            sbd1.putTo_MapOfDistances(3, -1);

            TS_ASSERT(sbd1.getSize_MapOfDistances() == 3);
            TS_ASSERT_DELTA(sbd1.getValueForKey_MapOfDistances(1), 0.5, EPSILON);
            TS_ASSERT_DELTA(sbd1.getValueForKey_MapOfDistances(2), 0.3, EPSILON);
            TS_ASSERT_DELTA(sbd1.getValueForKey_MapOfDistances(3), -1, EPSILON);
            //TS_ASSERT_DELTA(sbd1.getValueForKey_MapOfDistances(4), -2, EPSILON); not implemented anymore
        }

        void testCopyConstructor() {
            const double EPSILON = 0.00001;

            SensorBoardData sbd1;
            sbd1.putTo_MapOfDistances(1, 0.5); 
            sbd1.putTo_MapOfDistances(2, 0.3);
            sbd1.putTo_MapOfDistances(3, -1);

            SensorBoardData sbd2(sbd1);

            TS_ASSERT(sbd2.getSize_MapOfDistances() == 3);
            TS_ASSERT_DELTA(sbd2.getValueForKey_MapOfDistances(1), 0.5, EPSILON);
            TS_ASSERT_DELTA(sbd2.getValueForKey_MapOfDistances(2), 0.3, EPSILON);
            TS_ASSERT_DELTA(sbd2.getValueForKey_MapOfDistances(3), -1, EPSILON);
            //TS_ASSERT_DELTA(sbd2.getValueForKey_MapOfDistances(4), -2, EPSILON); not implemented anymore
        }

        void testAssignmentOperator() {
            const double EPSILON = 0.00001;

            SensorBoardData sbd1;
            sbd1.putTo_MapOfDistances(1, 0.5); 
            sbd1.putTo_MapOfDistances(2, 0.3);
            sbd1.putTo_MapOfDistances(3, -1);

            SensorBoardData sbd2;
            sbd2 = sbd1;

            TS_ASSERT(sbd2.getSize_MapOfDistances() == 3);
            TS_ASSERT_DELTA(sbd2.getValueForKey_MapOfDistances(1), 0.5, EPSILON);
            TS_ASSERT_DELTA(sbd2.getValueForKey_MapOfDistances(2), 0.3, EPSILON);
            TS_ASSERT_DELTA(sbd2.getValueForKey_MapOfDistances(3), -1, EPSILON);
            //TS_ASSERT_DELTA(sbd2.getValueForKey_MapOfDistances(4), -2, EPSILON); not implemented anymore
        }

        void testSerialization() {
            const double EPSILON = 0.00001;

            SensorBoardData sbd1;
            sbd1.putTo_MapOfDistances(1, 0.5); 
            sbd1.putTo_MapOfDistances(2, 0.3);
            sbd1.putTo_MapOfDistances(3, -1);

            stringstream sstr;
            sstr << sbd1;

            SensorBoardData sbd2;
            
            sstr >> sbd2;

            TS_ASSERT(sbd2.getSize_MapOfDistances() == 3);
            TS_ASSERT_DELTA(sbd2.getValueForKey_MapOfDistances(1), 0.5, EPSILON);
            TS_ASSERT_DELTA(sbd2.getValueForKey_MapOfDistances(2), 0.3, EPSILON);
            TS_ASSERT_DELTA(sbd2.getValueForKey_MapOfDistances(3), -1, EPSILON);
            //TS_ASSERT_DELTA(sbd2.getValueForKey_MapOfDistances(4), -2, EPSILON); not implemented anymore
        }
};

#endif /*SENSORBOARDDATATESTSUITE_H_*/
