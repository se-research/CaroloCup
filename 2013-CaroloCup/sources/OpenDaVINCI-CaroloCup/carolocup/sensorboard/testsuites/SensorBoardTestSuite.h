/*
 * CaroloCup.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef SENSORBOARDTESTSUITE_H_
#define SENSORBOARDTESTSUITE_H_

#include "cxxtest/TestSuite.h"

#include "core/data/Container.h"

#include "SensorBoardData.h"

// Include local header files.
#include "../include/SensorBoard.h"

using namespace std;
using namespace core::data;
using namespace carolocup;

/**
 * This class derives from SensorBoard to allow access to protected methods.
 */
class SensorBoardTestling : public SensorBoard {
    private:
        SensorBoardTestling();
    
    public:
        SensorBoardTestling(const int32_t &argc, char **argv) :
            SensorBoard(argc, argv) {}

        // Here, you need to add all methods which are protected in SensorBoard and which are needed for the test cases.

        /**
         * This method calls the inherited but protected method processContainerExample from SensorBoard.
         *
         * @param c Container to be processed.
         * @return result from inherited method.
         */
        bool callProcessContainerExample(Container &c) {
            return processContainerExample(c);
        }
};

/**
 * The actual testsuite starts here.
 */
class SensorBoardTest : public CxxTest::TestSuite {
    private:
        SensorBoardTestling *sbt;

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
            sbt = new SensorBoardTestling(argc, argv);
        }

        /**
         * This method will be called after each testXYZ-method.
         */
        void tearDown() {
            delete sbt;
            sbt = NULL;
        }

        ////////////////////////////////////////////////////////////////////////////////////
        // Below this line the actual testcases are defined.
        ////////////////////////////////////////////////////////////////////////////////////

        void testSensorBoardSuccessfullyCreated() {
            TS_ASSERT(sbt != NULL);
        }

        void testSensorBoardProcessingMethod() {
            // Create exemplary data.
            SensorBoardData sbd1;
            sbd1.update(1, 0.5); 
            sbd1.update(2, 0.3);
            sbd1.update(3, -1);

    		// Create container with user data Container::USER_DATA_0.
    		Container c(Container::USER_DATA_0, sbd1);

            TS_ASSERT(sbt->callProcessContainerExample(c) == true);
        }

        void testSensorBoardProcessingMethodWrongUserDataTypeID() {
            // Create exemplary data.
            SensorBoardData sbd1;
            sbd1.update(1, 0.5); 
            sbd1.update(2, 0.3);
            sbd1.update(3, -1);

    		// Create container with user data Container::USER_DATA_8.
    		Container c(Container::USER_DATA_8, sbd1);

            TS_ASSERT(sbt->callProcessContainerExample(c) == false);
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
        SensorBoardTest() : sbt(NULL) {}

    private:
        /**
         * "Forbidden" copy constructor. Goal: The compiler should warn
         * already at compile time for unwanted bugs caused by any misuse
         * of the copy constructor.
         *
         * @param obj Reference to an object of this class.
         */
        SensorBoardTest(const SensorBoardTest &/*obj*/);

        /**
         * "Forbidden" assignment operator. Goal: The compiler should warn
         * already at compile time for unwanted bugs caused by any misuse
         * of the assignment operator.
         *
         * @param obj Reference to an object of this class.
         * @return Reference to this instance.
         */
        SensorBoardTest& operator=(const SensorBoardTest &/*obj*/);

};

#endif /*SENSORBOARDTESTSUITE_H_*/
