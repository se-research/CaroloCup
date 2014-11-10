/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef EXAMPLE6RECEIVER_H_
#define EXAMPLE6RECEIVER_H_

#include "core/base/ConferenceClientModule.h"
#include "core/base/FIFOQueue.h"

namespace examples {

    using namespace std;

    /**
     * This class demonstrates how to receive sent data.
     */
    class Example6Receiver : public core::base::ConferenceClientModule {
        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             *
             * @param obj Reference to an object of this class.
             */
            Example6Receiver(const Example6Receiver &/*obj*/);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             *
             * @param obj Reference to an object of this class.
             * @return Reference to this instance.
             */
            Example6Receiver& operator=(const Example6Receiver &/*obj*/);

        public:
            /**
             * Constructor.
             *
             * @param argc Number of command line arguments.
             * @param argv Command line arguments.
             */
            Example6Receiver(const int32_t &argc, char **argv);

            virtual ~Example6Receiver();

            core::base::ModuleState::MODULE_EXITCODE body();

        private:
            virtual void setUp();

            virtual void tearDown();

            core::base::FIFOQueue m_fifo;
    };

} // examples

#endif /*EXAMPLE6RECEIVER_H_*/
