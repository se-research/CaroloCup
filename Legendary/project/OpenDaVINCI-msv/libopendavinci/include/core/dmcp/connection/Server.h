/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_DMCP_CONNECTION_SERVER_H_
#define OPENDAVINCI_DMCP_CONNECTION_SERVER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/SharedPointer.h"

#include "core/io/Connection.h"
#include "core/io/ConnectionAcceptor.h"
#include "core/io/ConnectionAcceptorListener.h"

#include "core/dmcp/ServerInformation.h"
#include "core/dmcp/connection/ConnectionHandler.h"

namespace core {
    namespace dmcp {
        namespace connection {

            class OPENDAVINCI_API Server : protected core::io::ConnectionAcceptorListener
            {
                public:
                    Server(const ServerInformation& serverInformation,
                           ModuleConfigurationProvider& configProvider);
                    virtual ~Server();

                    void setConnectionHandler(ConnectionHandler* connectionHandler);

                protected:
                    virtual void onNewConnection(core::io::Connection* connection);

                    core::base::Mutex m_configProviderMutex;
                    ModuleConfigurationProvider& m_configProvider;

                    core::base::Mutex m_connectionHandlerMutex;
                    ConnectionHandler* m_connectionHandler;

                    core::io::ConnectionAcceptor m_acceptor;

                private:
                    Server(const Server &);
                    Server& operator=(const Server &);

            };
        }
    }
} // core::dmcp

#endif  /*OPENDAVINCI_DMCP_CONNECTION_SERVER_H_*/
