/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef EXAMPLE5SENDER_H_
#define EXAMPLE5SENDER_H_

#include "core/base/ConferenceClientModule.h"
#include "core/base/FIFOQueue.h"

namespace examples {

    using namespace std;

    /**
     * This class demonstrates how to send data as a Container.
     */
    class Example5Sender : public core::base::ConferenceClientModule, public core::wrapper::StringListener, public core::wrapper::ConnectionListener {
        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             *
             * @param obj Reference to an object of this class.
             */
            Example5Sender(const Example5Sender &/*obj*/);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             *
             * @param obj Reference to an object of this class.
             * @return Reference to this instance.
             */
            Example5Sender& operator=(const Example5Sender &/*obj*/);

        public:
            /**
             * Constructor.
             *
             * @param argc Number of command line arguments.
             * @param argv Command line arguments.
             */
            Example5Sender(const int32_t &argc, char **argv);

            virtual ~Example5Sender();

            core::base::ModuleState::MODULE_EXITCODE body();

            virtual void handleConnectionError();

            virtual void nextString(const string &s);

        private:
            virtual void setUp();

            virtual void tearDown();
    };

} // examples

#endif /*EXAMPLE5SENDER_H_*/
