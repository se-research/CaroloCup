/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef MOCKS__CONNECTIONERRORLISTENERMOCK_H_
#define MOCKS__CONNECTIONERRORLISTENERMOCK_H_

#include "core/mocks/FunctionCallWaiter.h"
#include "core/io/ConnectionErrorListener.h"

namespace mocks {
    using namespace core;

    class ConnectionErrorListenerMock : public core::io::ConnectionErrorListener {
        public:
            ConnectionErrorListenerMock() :
                CALLWAITER_handleConnectionError()
            {}

            virtual void handleConnectionError() {
                CALLWAITER_handleConnectionError.called();
            }

            mocks::FunctionCallWaiter CALLWAITER_handleConnectionError;
    };
}
#endif
