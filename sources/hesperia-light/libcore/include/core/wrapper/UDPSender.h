/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_UDPSENDER_H_
#define HESPERIA_CORE_WRAPPER_UDPSENDER_H_

#include <string>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

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
        class HESPERIA_API UDPSender {
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

#endif /*HESPERIA_CORE_WRAPPER_UDPSENDER_H_*/
