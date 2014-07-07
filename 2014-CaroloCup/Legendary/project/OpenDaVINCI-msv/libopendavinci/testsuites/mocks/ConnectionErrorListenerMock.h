/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef MOCKS__CONNECTIONERRORLISTENERMOCK_H_
#define MOCKS__CONNECTIONERRORLISTENERMOCK_H_

#include "FunctionCallWaiter.h"
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
