/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef MOCKS__CONNECTIONLISTENERMOCK_H_
#define MOCKS__CONNECTIONLISTENERMOCK_H_

#include "core/mocks/FunctionCallWaiter.h"
#include "core/wrapper/ConnectionListener.h"

namespace mocks {
    class ConnectionListenerMock : public core::wrapper::ConnectionListener {
        public:
            ConnectionListenerMock() :
                CALLWAITER_handleConnectionError()
            {}

            virtual ~ConnectionListenerMock()
            {};

            virtual void handleConnectionError() {
                CALLWAITER_handleConnectionError.called();
            }

            FunctionCallWaiter CALLWAITER_handleConnectionError;
    };
}
#endif
