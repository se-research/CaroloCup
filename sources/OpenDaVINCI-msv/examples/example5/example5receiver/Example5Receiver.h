/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef EXAMPLE5RECEIVER_H_
#define EXAMPLE5RECEIVER_H_

#include "core/base/ConferenceClientModule.h"
#include "core/base/Mutex.h"

namespace examples {

    using namespace std;

    /**
     * This class demonstrates how to receive sent data over a serial port.
     */
    class Example5Receiver : public core::base::ConferenceClientModule, public core::wrapper::StringListener, public core::wrapper::ConnectionListener {
        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             *
             * @param obj Reference to an object of this class.
             */
            Example5Receiver(const Example5Receiver &/*obj*/);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             *
             * @param obj Reference to an object of this class.
             * @return Reference to this instance.
             */
            Example5Receiver& operator=(const Example5Receiver &/*obj*/);

        public:
            /**
             * Constructor.
             *
             * @param argc Number of command line arguments.
             * @param argv Command line arguments.
             */
            Example5Receiver(const int32_t &argc, char **argv);

            virtual ~Example5Receiver();

            core::base::ModuleState::MODULE_EXITCODE body();

            virtual void handleConnectionError();

            virtual void nextString(const string &s);

        private:
            virtual void setUp();

            virtual void tearDown();

            core::base::Mutex m_bufferMutex;
            string m_buffer;
    };

} // examples

#endif /*EXAMPLE5RECEIVER_H_*/
