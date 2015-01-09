/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef RECINTEGRITYTESTSUITE_H_
#define RECINTEGRITYTESTSUITE_H_

#include "cxxtest/TestSuite.h"

// Include local header files.
#include "../include/Split.h"

using namespace std;
using namespace core::data;
using namespace split;

/**
 * This class derives from Split to allow access to protected methods.
 */
class SplitTestling : public Split {
    private:
        SplitTestling();
    
    public:
        SplitTestling(const int32_t &argc, char **argv) :
            Split(argc, argv) {}

        // Here, you need to add all methods which are protected in Split and which are needed for the test cases.
};

/**
 * The actual testsuite starts here.
 */
class SplitTest : public CxxTest::TestSuite {
    private:
        SplitTestling *dt;

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
            dt = new SplitTestling(argc, argv);
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

        void testSplitSuccessfullyCreated() {
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
        SplitTest() : dt(NULL) {}

    private:
        /**
         * "Forbidden" copy constructor. Goal: The compiler should warn
         * already at compile time for unwanted bugs caused by any misuse
         * of the copy constructor.
         *
         * @param obj Reference to an object of this class.
         */
        SplitTest(const SplitTest &/*obj*/);

        /**
         * "Forbidden" assignment operator. Goal: The compiler should warn
         * already at compile time for unwanted bugs caused by any misuse
         * of the assignment operator.
         *
         * @param obj Reference to an object of this class.
         * @return Reference to this instance.
         */
        SplitTest& operator=(const SplitTest &/*obj*/);

};

#endif /*RECINTEGRITYTESTSUITE_H_*/

