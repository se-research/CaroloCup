/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef PROXYSTM32F4TESTSUITE_H_
#define PROXYSTM32F4TESTSUITE_H_

#include "cxxtest/TestSuite.h"

// Include local header files.
#include "../include/ProxySTM32F4.h"

using namespace std;
using namespace core::data;
using namespace msv;

/**
 * This class derives from SensorBoard to allow access to protected methods.
 */
class ProxySTM32F4Testling : public ProxySTM32F4 {
    private:
        ProxySTM32F4Testling();
    
    public:
        ProxySTM32F4Testling(const int32_t &argc, char **argv) :
            ProxySTM32F4(argc, argv) {}

        // Here, you need to add all methods which are protected in ProxySTM32F4 and which are needed for the test cases.
};

/**
 * The actual testsuite starts here.
 */
class ProxySTM32F4Test : public CxxTest::TestSuite {
    private:
        ProxySTM32F4Testling *dt;

    public:
        /**
         * This method will be called before each testXYZ-method.
         */
        void setUp() {
            // Prepare the data that would be available from commandline.
            string argv0("proxy-STM32F4");
            string argv1("--cid=100");
            int32_t argc = 2;
            char **argv;
            argv = new char*[2];
            argv[0] = const_cast<char*>(argv0.c_str());
            argv[1] = const_cast<char*>(argv1.c_str());

            // Create an instance of sensorboard through SensorBoardTestling which will be deleted in tearDown().
            dt = new ProxySTM32F4Testling(argc, argv);
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

        void testProxySTM32F4SuccessfullyCreated() {
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
        ProxySTM32F4Test() : dt(NULL) {}

    private:
        /**
         * "Forbidden" copy constructor. Goal: The compiler should warn
         * already at compile time for unwanted bugs caused by any misuse
         * of the copy constructor.
         *
         * @param obj Reference to an object of this class.
         */
        ProxySTM32F4Test(const ProxySTM32F4Test &/*obj*/);

        /**
         * "Forbidden" assignment operator. Goal: The compiler should warn
         * already at compile time for unwanted bugs caused by any misuse
         * of the assignment operator.
         *
         * @param obj Reference to an object of this class.
         * @return Reference to this instance.
         */
        ProxySTM32F4Test& operator=(const ProxySTM32F4Test &/*obj*/);

};

#endif /*PROXYSTM32F4TESTSUITE_H_*/

