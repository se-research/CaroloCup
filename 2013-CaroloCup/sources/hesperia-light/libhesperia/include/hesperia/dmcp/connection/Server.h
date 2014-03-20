/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_DMCP_CONNECTION_SERVER_H_
#define HESPERIA_DMCP_CONNECTION_SERVER_H_

#include <map>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"
#include "core/SharedPointer.h"

#include "core/io/Connection.h"
#include "core/io/ConnectionAcceptor.h"
#include "core/io/ConnectionAcceptorListener.h"

#include "hesperia/dmcp/ServerInformation.h"
#include "hesperia/dmcp/connection/ConnectionHandler.h"

namespace hesperia {
    namespace dmcp {
        namespace connection {

            class HESPERIA_API Server : protected core::io::ConnectionAcceptorListener
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
} // hesperia::dmcp

#endif  /*HESPERIA_DMCP_CONNECTION_SERVER_H_*/
