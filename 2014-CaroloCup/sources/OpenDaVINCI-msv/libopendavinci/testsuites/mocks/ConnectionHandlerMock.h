/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef CONNECTIONHANDLERMOCK_H_
#define CONNECTIONHANDLERMOCK_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/dmcp/connection/ConnectionHandler.h"
#include "core/dmcp/connection/ModuleConnection.h"
#include "FunctionCallWaiter.h"

namespace mocks {

    class ConnectionHandlerMock : public core::dmcp::connection::ConnectionHandler {
        public:
            ConnectionHandlerMock() :
                connection(NULL),
                WAITER()
            {};

            virtual ~ConnectionHandlerMock() {};

            virtual void onNewModule(core::dmcp::connection::ModuleConnection* mc)
            {
                connection = mc;
                WAITER.called();
            }

            core::dmcp::connection::ModuleConnection* connection;
            FunctionCallWaiter WAITER;
        private:
            ConnectionHandlerMock(const ConnectionHandlerMock &);
            ConnectionHandlerMock& operator=(const ConnectionHandlerMock &);
    };
}
#endif /* CONNECTIONHANDLERMOCK_H_ */
