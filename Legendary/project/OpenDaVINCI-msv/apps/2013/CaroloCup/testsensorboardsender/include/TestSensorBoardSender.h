/*
 * CaroloCup.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef TESTSENSORBOARDSENDER_H_
#define TESTSENSORBOARDSENDER_H_

#include "core/base/ConferenceClientModule.h"

namespace carolocup {

    using namespace std;

    /**
     * This class demonstrates how to send data as a Container.
     */
    class TestSensorBoardSender : public core::base::ConferenceClientModule {
        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             *
             * @param obj Reference to an object of this class.
             */
            TestSensorBoardSender(const TestSensorBoardSender &/*obj*/);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             *
             * @param obj Reference to an object of this class.
             * @return Reference to this instance.
             */
            TestSensorBoardSender& operator=(const TestSensorBoardSender &/*obj*/);

        public:
            /**
             * Constructor.
             *
             * @param argc Number of command line arguments.
             * @param argv Command line arguments.
             */
            TestSensorBoardSender(const int32_t &argc, char **argv);

            virtual ~TestSensorBoardSender();

            core::base::ModuleState::MODULE_EXITCODE body();

        protected:
            /**
             * This method is used to generate a random number for the distance.
             *
             * @param minDistance minimum distance.
             * @param maxDistance maximum distance.
             * @return random distance.
             */
            double randomDistance(const double &minDistance, const double &maxDistance) const; 

        private:
            virtual void setUp();

            virtual void tearDown();
    };

} // carolocup

#endif /*TESTSENSORBOARDSENDER_H_*/
