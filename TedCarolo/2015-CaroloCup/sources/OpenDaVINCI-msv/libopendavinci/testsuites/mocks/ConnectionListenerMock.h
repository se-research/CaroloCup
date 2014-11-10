/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef MOCKS__CONNECTIONLISTENERMOCK_H_
#define MOCKS__CONNECTIONLISTENERMOCK_H_

#include "FunctionCallWaiter.h"
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
