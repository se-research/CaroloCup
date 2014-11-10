/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef EXAMPLE2_H_
#define EXAMPLE2_H_

#include "core/base/ConferenceClientModule.h"

namespace examples {

    using namespace std;

    /**
     * This class demonstrates the usage of a centralized configuration file with OpenDaVINCI.
     */
    class Example2 : public core::base::ConferenceClientModule {
        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             *
             * @param obj Reference to an object of this class.
             */
            Example2(const Example2 &/*obj*/);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             *
             * @param obj Reference to an object of this class.
             * @return Reference to this instance.
             */
            Example2& operator=(const Example2 &/*obj*/);

        public:
            /**
             * Constructor.
             *
             * @param argc Number of command line arguments.
             * @param argv Command line arguments.
             */
            Example2(const int32_t &argc, char **argv);

            virtual ~Example2();

            core::base::ModuleState::MODULE_EXITCODE body();

        private:
            virtual void setUp();

            virtual void tearDown();
    };

} // examples

#endif /*EXAMPLE2_H_*/
