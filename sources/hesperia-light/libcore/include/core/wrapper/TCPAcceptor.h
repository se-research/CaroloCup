/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_TCPACCEPTOR_H_
#define HESPERIA_CORE_WRAPPER_TCPACCEPTOR_H_

#include "core/native.h"
#include "core/wrapper/Mutex.h"
#include "core/wrapper/TCPAcceptorListener.h"

namespace core {
    namespace wrapper {

        class HESPERIA_API TCPAcceptor {
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

#endif /* HESPERIA_CORE_WRAPPER_TCPACCEPTOR_H_ */
