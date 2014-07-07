/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef RECINTEGRITYTESTSUITE_H_
#define RECINTEGRITYTESTSUITE_H_

#include "cxxtest/TestSuite.h"

// Include local header files.
#include "../include/RecIntegrity.h"

using namespace std;
using namespace core::data;
using namespace recintegrity;

/**
 * This class derives from RecIntegrity to allow access to protected methods.
 */
class RecIntegrityTestling : public RecIntegrity {
    private:
        RecIntegrityTestling();
    
    public:
        RecIntegrityTestling(const int32_t &argc, char **argv) :
            RecIntegrity(argc, argv) {}

        // Here, you need to add all methods which are protected in RecIntegrity and which are needed for the test cases.
};

/**
 * The actual testsuite starts here.
 */
class RecIntegrityTest : public CxxTest::TestSuite {
    private:
        RecIntegrityTestling *dt;

    public:
        /**
         * This method will be called before each testXYZ-method.
         */
        void setUp() {
            // Prepare the data that would be available from commandline.
            string argv0("recintegrity");
            string argv1("--cid=100");
            int32_t argc = 2;
            char **argv;
            argv = new char*[2];
            argv[0] = const_cast<char*>(argv0.c_str());
            argv[1] = const_cast<char*>(argv1.c_str());

            // Create an instance of sensorboard through SensorBoardTestling which will be deleted in tearDown().
            dt = new RecIntegrityTestling(argc, argv);
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

        void testRecIntegritySuccessfullyCreated() {
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
        RecIntegrityTest() : dt(NULL) {}

    private:
        /**
         * "Forbidden" copy constructor. Goal: The compiler should warn
         * already at compile time for unwanted bugs caused by any misuse
         * of the copy constructor.
         *
         * @param obj Reference to an object of this class.
         */
        RecIntegrityTest(const RecIntegrityTest &/*obj*/);

        /**
         * "Forbidden" assignment operator. Goal: The compiler should warn
         * already at compile time for unwanted bugs caused by any misuse
         * of the assignment operator.
         *
         * @param obj Reference to an object of this class.
         * @return Reference to this instance.
         */
        RecIntegrityTest& operator=(const RecIntegrityTest &/*obj*/);

};

#endif /*RECINTEGRITYTESTSUITE_H_*/

