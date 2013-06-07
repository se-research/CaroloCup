/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_TCPACCEPTORLISTENER_H_
#define HESPERIA_CORE_WRAPPER_TCPACCEPTORLISTENER_H_

#include "core/native.h"
#include "core/wrapper/TCPConnection.h"

namespace core {
    namespace wrapper {
        class HESPERIA_API TCPAcceptorListener {
            public:
                virtual ~TCPAcceptorListener() {};
                virtual void onNewConnection(TCPConnection* connection) = 0;
        };
    }
}

#endif /* HESPERIA_CORE_WRAPPER_TCPACCEPTORLISTENER_H_ */
