/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef COCKPITTESTSUITE_H_
#define COCKPITTESTSUITE_H_

#include "cxxtest/TestSuite.h"

// Include local header files.
#include "../include/Cockpit.h"

using namespace std;
using namespace core::data;
using namespace cockpit;

/**
 * This class derives from SensorBoard to allow access to protected methods.
 */
class CockpitTestling : public Cockpit {
    private:
        CockpitTestling();
    
    public:
        CockpitTestling(int32_t &argc, char **argv) :
            Cockpit(argc, argv) {}

        // Here, you need to add all methods which are protected in Cockpit and which are needed for the test cases.
};

/**
 * The actual testsuite starts here.
 */
class CockpitTest : public CxxTest::TestSuite {
    private:
        CockpitTestling *tsbst;

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
            tsbst = new CockpitTestling(argc, argv);
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

        void testCockpitSuccessfullyCreated() {
            TS_ASSERT(tsbst != NULL);
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
        CockpitTest() : tsbst(NULL) {}

    private:
        /**
         * "Forbidden" copy constructor. Goal: The compiler should warn
         * already at compile time for unwanted bugs caused by any misuse
         * of the copy constructor.
         *
         * @param obj Reference to an object of this class.
         */
        CockpitTest(const CockpitTest &/*obj*/);

        /**
         * "Forbidden" assignment operator. Goal: The compiler should warn
         * already at compile time for unwanted bugs caused by any misuse
         * of the assignment operator.
         *
         * @param obj Reference to an object of this class.
         * @return Reference to this instance.
         */
        CockpitTest& operator=(const CockpitTest &/*obj*/);

};

#endif /*COCKPITTESTSUITE_H_*/

