/*
 * CaroloCup.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef TESTSENSORBOARDSENDERTESTSUITE_H_
#define TESTSENSORBOARDSENDERTESTSUITE_H_

#include "cxxtest/TestSuite.h"

// Include local header files.
#include "../include/TestSensorBoardSender.h"

using namespace std;
using namespace core::data;
using namespace carolocup;

/**
 * This class derives from SensorBoard to allow access to protected methods.
 */
class TestSensorBoardSenderTestling : public TestSensorBoardSender {
    private:
        TestSensorBoardSenderTestling();
    
    public:
        TestSensorBoardSenderTestling(const int32_t &argc, char **argv) :
            TestSensorBoardSender(argc, argv) {}

        // Here, you need to add all methods which are protected in TestSensorBoardSender and which are needed for the test cases.

        /**
         * This method calls the inherited but protected method randomDistance from TestSensorBoardSender.
         *
         *
         * @param minDistance minimum distance.
         * @param maxDistance maximum distance.
         * @return result from inherited method.
         */
        double callrandomDistance(const double &minDistance, const double &maxDistance) const {
            return randomDistance(minDistance, maxDistance); 
        }
};

/**
 * The actual testsuite starts here.
 */
class TestSensorBoardSenderTest : public CxxTest::TestSuite {
    private:
        TestSensorBoardSenderTestling *tsbst;

    public:
        /**
         * This method will be called before each testXYZ-method.
         */
        void setUp() {
            // Prepare the data that would be available from commandline.
            string argv0("sensorboard");
            string argv1("--cid=100");
            int32_t argc = 2;
            char **argv;
            argv = new char*[2];
            argv[0] = const_cast<char*>(argv0.c_str());
            argv[1] = const_cast<char*>(argv1.c_str());

            // Create an instance of sensorboard through SensorBoardTestling which will be deleted in tearDown().
            tsbst = new TestSensorBoardSenderTestling(argc, argv);
        }

        /**
         * This method will be called after each testXYZ-method.
         */
        void tearDown() {
            delete tsbst;
            tsbst = NULL;
        }

        ////////////////////////////////////////////////////////////////////////////////////
        // Below this line the actual testcases are defined.
        ////////////////////////////////////////////////////////////////////////////////////

        void testTestSensorBoardSenderSuccessfullyCreated() {
            TS_ASSERT(tsbst != NULL);
        }

        void testRandomFunction() {
            const double MIN_DISTANCE = 1;
            const double MAX_DISTANCE = 2;

            for(uint32_t i = 0; i < 100; i++) {
                double randomDistance = tsbst->callrandomDistance(MIN_DISTANCE, MAX_DISTANCE);

                TS_ASSERT_LESS_THAN_EQUALS(randomDistance, MAX_DISTANCE);
                // CxxTest dos not provide a GREATER_THAN_EQUALS-assert method, thus, simply use -1 to invert the test.
                TS_ASSERT_LESS_THAN_EQUALS(-randomDistance, MIN_DISTANCE);
            }
        }

        ////////////////////////////////////////////////////////////////////////////////////
        // Below this line the necessary constructor for initializing the pointer variables,
        // and the forbidden copy constructor and assignment operator are declared.
        //
        // These functions are normally not changed.
        ////////////////////////////////////////////////////////////////////////////////////

    public:
        /**
         * This constructor is only necessary to initialize the pointer variable.
         */
        TestSensorBoardSenderTest() : tsbst(NULL) {}

    private:
        /**
         * "Forbidden" copy constructor. Goal: The compiler should warn
         * already at compile time for unwanted bugs caused by any misuse
         * of the copy constructor.
         *
         * @param obj Reference to an object of this class.
         */
        TestSensorBoardSenderTest(const TestSensorBoardSenderTest &/*obj*/);

        /**
         * "Forbidden" assignment operator. Goal: The compiler should warn
         * already at compile time for unwanted bugs caused by any misuse
         * of the assignment operator.
         *
         * @param obj Reference to an object of this class.
         * @return Reference to this instance.
         */
        TestSensorBoardSenderTest& operator=(const TestSensorBoardSenderTest &/*obj*/);

};

#endif /*TESTSENSORBOARDSENDERTESTSUITE_H_*/

