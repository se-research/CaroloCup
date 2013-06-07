/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include <iostream>

#include "core/base/Lock.h"
#include "core/exceptions/Exceptions.h"

#include "hesperia/dmcp/Config.h"
#include "hesperia/dmcp/connection/ModuleConnection.h"
#include "hesperia/dmcp/connection/Server.h"

namespace hesperia {
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
} // hesperia::dmcp
