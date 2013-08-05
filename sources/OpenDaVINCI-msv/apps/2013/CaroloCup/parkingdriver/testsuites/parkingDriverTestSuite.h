/*
 * CaroloCup.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef PARKINGDRIVERTESTSUITE_H_
#define PARKINGDRIVERTESTSUITE_H_

#include "cxxtest/TestSuite.h"

// Include local header files.
#include "../include/parkingDriver.h"

using namespace std;
using namespace core::data;
using namespace carolocup;

/**
 * This class derives from SensorBoard to allow access to protected methods.
 */
class parkingDriverTestling : public parkingDriver {
    private:
        parkingDriverTestling();
    
    public:
        parkingDriverTestling(const int32_t &argc, char **argv) :
            parkingDriver(argc, argv) {}

        // Here, you need to add all methods which are protected in parkingDriver and which are needed for the test cases.
};

/**
 * The actual testsuite starts here.
 */
class parkingDriverTest : public CxxTest::TestSuite {
    private:
        parkingDriverTestling *dt;

    public:
        /**
         * This method will be called before each testXYZ-method.
         */
        void setUp() {
            // Prepare the data that would be available from commandline.
            string argv0("driver");
            string argv1("--cid=100");
            int32_t argc = 2;
            char **argv;
            argv = new char*[2];
            argv[0] = const_cast<char*>(argv0.c_str());
            argv[1] = const_cast<char*>(argv1.c_str());

            // Create an instance of sensorboard through SensorBoardTestling which will be deleted in tearDown().
            dt = new parkingDriverTestling(argc, argv);
        }

        /**
         * This method will be called after each testXYZ-method.
         */
        void tearDown() {
            delete dt;
            dt = NULL;
        }

        ////////////////////////////////////////////////////////////////////////////////////
        // Below this line the actual testcases are defined.
        ////////////////////////////////////////////////////////////////////////////////////

        void testparkingDriverSuccessfullyCreated() {
            TS_ASSERT(dt != NULL);
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
        parkingDriverTest() : dt(NULL) {}

    private:
        /**
         * "Forbidden" copy constructor. Goal: The compiler should warn
         * already at compile time for unwanted bugs caused by any misuse
         * of the copy constructor.
         *
         * @param obj Reference to an object of this class.
         */
        parkingDriverTest(const parkingDriverTest &/*obj*/);

        /**
         * "Forbidden" assignment operator. Goal: The compiler should warn
         * already at compile time for unwanted bugs caused by any misuse
         * of the assignment operator.
         *
         * @param obj Reference to an object of this class.
         * @return Reference to this instance.
         */
        parkingDriverTest& operator=(const parkingDriverTest &/*obj*/);

};

#endif /*DRIVERTESTSUITE_H_*/

