/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/Lock.h"
#include "core/exceptions/Exceptions.h"

#include "core/dmcp/Config.h"
#include "core/dmcp/connection/ModuleConnection.h"
#include "core/dmcp/connection/Server.h"

namespace core {
    namespace dmcp {
        namespace connection {

            using namespace std;
            using namespace core::base;
            using namespace core::exceptions;

            Server::Server(const ServerInformation& serverInformation, ModuleConfigurationProvider& configProvider) :
                m_configProviderMutex(),
                m_configProvider(configProvider),
                m_connectionHandlerMutex(),
                m_connectionHandler(NULL),
                m_acceptor(serverInformation.getPort())
            {
                m_acceptor.setConnectionAcceptorListener(this);
                m_acceptor.start();
            }

            Server::~Server() {}

            void Server::setConnectionHandler(ConnectionHandler* connectionHandler)
            {
                Lock l(m_connectionHandlerMutex);
                m_connectionHandler = connectionHandler;
            }

            void Server::onNewConnection(core::io::Connection* connection)
            {
                Lock l(m_configProviderMutex);
                ModuleConnection* mc = NULL;

                try {
                    mc = new ModuleConnection(connection, m_configProvider);
                } catch (...) {
                    clog << "(DMCP-ConnectionServer) cannot create ModuleConnection for new request!" << endl;
                }

                if (m_connectionHandler) {
                    m_connectionHandler->onNewModule(mc);
                } else {
                    clog << "(DMCP-ConnectionServer) received a module connection without set ModuleConnectionHandler" << endl;
                    delete mc;
                }
            }
        }
    }
} // core::dmcp
