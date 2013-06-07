/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef CONNECTIONHANDLERMOCK_H_
#define CONNECTIONHANDLERMOCK_H_

#include <map>

#include "core/native.h"
#include "hesperia/dmcp/connection/ConnectionHandler.h"
#include "hesperia/dmcp/connection/ModuleConnection.h"
#include "core/mocks/FunctionCallWaiter.h"

namespace mocks {

    class ConnectionHandlerMock : public hesperia::dmcp::connection::ConnectionHandler {
        public:
            ConnectionHandlerMock() :
                connection(NULL),
                WAITER()
            {};

            virtual ~ConnectionHandlerMock() {};

            virtual void onNewModule(hesperia::dmcp::connection::ModuleConnection* mc)
            {
                connection = mc;
                WAITER.called();
            }

            hesperia::dmcp::connection::ModuleConnection* connection;
            FunctionCallWaiter WAITER;
        private:
            ConnectionHandlerMock(const ConnectionHandlerMock &);
            ConnectionHandlerMock& operator=(const ConnectionHandlerMock &);
    };
}
#endif /* CONNECTIONHANDLERMOCK_H_ */
