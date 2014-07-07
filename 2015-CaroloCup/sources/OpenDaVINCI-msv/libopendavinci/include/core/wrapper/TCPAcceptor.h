/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_TCPACCEPTOR_H_
#define OPENDAVINCI_CORE_WRAPPER_TCPACCEPTOR_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/Mutex.h"
#include "core/wrapper/TCPAcceptorListener.h"

namespace core {
    namespace wrapper {

        class OPENDAVINCI_API TCPAcceptor {
            public:
                TCPAcceptor() {};
                virtual ~TCPAcceptor() {};

                virtual void setAcceptorListener(TCPAcceptorListener* listener) = 0;

                virtual void start() = 0;
                virtual void stop() = 0;

                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                TCPAcceptor(const TCPAcceptor &);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                TCPAcceptor& operator=(const TCPAcceptor &);
        };
    }
}

#endif /* OPENDAVINCI_CORE_WRAPPER_TCPACCEPTOR_H_ */
