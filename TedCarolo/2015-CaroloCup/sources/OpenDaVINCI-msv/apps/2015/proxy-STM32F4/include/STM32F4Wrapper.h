/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef STM32F4WRAPPER_H_
#define STM32F4WRAPPER_H_

#include "core/base/ConferenceClientModule.h"
#include "core/base/Mutex.h"
#include "core/wrapper/ConnectionListener.h"
#include "core/wrapper/StringListener.h"
#include "core/wrapper/NetstringsProtocol.h"

#include "STM32F4DataListener.h"

namespace msv {

    using namespace std;

    /**
     * This class wraps the STM32F4 Discovery Board.
     */
    class STM32F4Wrapper : public core::base::ConferenceClientModule, public core::wrapper::ConnectionListener, public core::wrapper::StringListener {
        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             *
             * @param obj Reference to an object of this class.
             */
            STM32F4Wrapper(const STM32F4Wrapper &/*obj*/);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             *
             * @param obj Reference to an object of this class.
             * @return Reference to this instance.
             */
            STM32F4Wrapper& operator=(const STM32F4Wrapper &/*obj*/);

        public:
            /**
             * Constructor.
             *
             * @param argc Number of command line arguments.
             * @param argv Command line arguments.
             */
            STM32F4Wrapper(const int32_t &argc, char **argv);

            virtual ~STM32F4Wrapper();

            core::base::ModuleState::MODULE_EXITCODE body();

            virtual void handleConnectionError();

            virtual void nextString(const string &s);

        private:
            virtual void setUp();

            virtual void tearDown();

        private:
            core::wrapper::NetstringsProtocol m_netstringsProtocol;
    };

} // msv

#endif /*STM32F4WRAPPER_H_*/
