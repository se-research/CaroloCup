/*
 * Mini-Smart-Vehicles.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef STM32F4TESTER2_H_
#define STM32F4TESTER2_H_

#include "core/base/ConferenceClientModule.h"
#include "core/base/Mutex.h"
#include "core/wrapper/ConnectionListener.h"
#include "core/wrapper/AbstractProtocol.h"
#include "core/wrapper/StringSender.h"

#include "STM32F4DataListener.h"

namespace msv {

    using namespace std;

    /**
     * This class wraps the STM32F4 Discovery Board and demonstrates how to communicate with the board.
     */
    class STM32F4Tester2 : public core::base::ConferenceClientModule, public core::wrapper::ConnectionListener, public core::wrapper::AbstractProtocol {
        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             *
             * @param obj Reference to an object of this class.
             */
            STM32F4Tester2(const STM32F4Tester2 &/*obj*/);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             *
             * @param obj Reference to an object of this class.
             * @return Reference to this instance.
             */
            STM32F4Tester2& operator=(const STM32F4Tester2 &/*obj*/);

        public:
            /**
             * Constructor.
             *
             * @param argc Number of command line arguments.
             * @param argv Command line arguments.
             */
            STM32F4Tester2(const int32_t &argc, char **argv);

            virtual ~STM32F4Tester2();

            core::base::ModuleState::MODULE_EXITCODE body();

            virtual void handleConnectionError();

            virtual void receivedPartialString(const string &partialData);

        private:
            virtual void setUp();

            virtual void tearDown();

            void decodeNetstring();

            string encodeNetstring(const string &d);

        private:
            stringstream m_partialData;
            string m_receivedPayload;
    };

} // msv

#endif /*STM32F4TESTER2_H_*/
