/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_UDPSENDER_H_
#define OPENDAVINCI_CORE_WRAPPER_UDPSENDER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

namespace core {
    namespace wrapper {

        using namespace std;

        /**
         * This interface encapsulates the necessary methods
         * for sending data using UDP. This interface
         * is implemented by Boost or plain POSIX system
         * calls.
         *
         * It can be used as follows:
         *
         * @code
         * const string address = "192.168.0.22";
         * const uint32_t port = 12345;
         * UDPSender *s = NULL;
         *
         * try {
         *     s = UDPFactory::getInstance().createUDPSender(address, port);
         * }
         * catch(string &st) {
         *    clog << "Failed: " << st << endl;
         * }
         *
         * if (s != NULL) {
         *     s->send("ABCD");
         * }
         *
         * ...
         *
         * if (s != NULL) {
         *     delete s;
         * }
         *
         * @endcode
         */
        class OPENDAVINCI_API UDPSender {
            public:
                virtual ~UDPSender();

                /**
                 * This method sends data using UDP.
                 *
                 * @param data Data to be sent.
                 */
                virtual void send(const string &data) const = 0;
        };

    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_UDPSENDER_H_*/
