/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include <sstream>

#include <boost/bind.hpp>

#include "core/base/KeyValueConfiguration.h"
#include "core/base/Lock.h"
#include "core/base/Thread.h"
#include "core/data/Container.h"
#include "core/exceptions/Exceptions.h"

#include "hesperia/data/Configuration.h"
#include "hesperia/data/dmcp/ModuleExitCodeMessage.h"
#include "hesperia/data/dmcp/ModuleStateMessage.h"
#include "hesperia/data/dmcp/ModuleDescriptor.h"
#include "hesperia/dmcp/connection/Client.h"

namespace hesperia {
    namespace dmcp {
        namespace connection {
            using namespace std;

            using namespace core::base;
            using namespace core::exceptions;
            using namespace core::io;
            using namespace core::data;

            using namespace hesperia::data;
            using namespace hesperia::data::dmcp;
            using namespace hesperia::dmcp;

            Client::Client(const ModuleDescriptor& moduleDescriptor, const ServerInformation& serverInformation) :
                m_moduleDescriptor(moduleDescriptor),
                m_serverInformation(serverInformation),
                m_connection(serverInformation.getIP(), serverInformation.getPort()),
                m_configurationMutex(),
                m_configuration(),
                m_configured(false),
                m_configuredMutex(),
                m_configurationRequestCondition(),
                m_listenerMutex(),
                m_listener(NULL)
            {
                m_connection.setContainerListener(this);
                m_connection.setErrorListener(this);
                m_connection.start();
            }

            Client::~Client()
            {
                m_connection.setContainerListener(NULL);
                m_connection.setErrorListener(NULL);
                m_connection.stop();
            }

            void Client::initialize()
            {
                sendConfigurationRequest();
                waitForConfiguration();
            }

            void Client::sendModuleExitCode(const ModuleState::MODULE_EXITCODE& exitCode)
            {
                Container container(Container::DMCP_MODULEEXITCODEMESSAGE,
                                    ModuleExitCodeMessage(exitCode));
                m_connection.send(container);
            }

            void Client::sendModuleState(const ModuleState::MODULE_STATE& state)
            {
                Container container(Container::DMCP_MODULESTATEMESSAGE,
                                    ModuleStateMessage(state));
                m_connection.send(container);
            }

            void Client::sendStatistics(const RuntimeStatistic& rs)
            {
                Container container(Container::RUNTIMESTATISTIC, rs);
                m_connection.send(container);
            }

            KeyValueConfiguration Client::getConfiguration()
            {
                Lock l(m_configurationMutex);
                return m_configuration;
            }

            void Client::setSupercomponentStateListener(SupercomponentStateListener* listener)
            {
                Lock l(m_listenerMutex);
                m_listener = listener;
            }

            void Client::sendConfigurationRequest()
            {
                clog << "(DMCP-ConnectionClient) sending configuration request..." << m_serverInformation.toString() << endl;

                Container container(Container::DMCP_CONFIGURATION_REQUEST, m_moduleDescriptor);
                m_connection.send(container);
            }

            void Client::nextContainer(Container &c)
            {
                if (c.getDataType() == Container::CONFIGURATION) {
                    Configuration configuration = c.getData<Configuration>();
                    handleConfiguration(configuration);
                }
            }

            void Client::handleConnectionError() {
                Lock l(m_listenerMutex);
                if (m_listener) {
                    m_listener->handleConnectionLost();
                }
            }

            void Client::handleConfiguration(Configuration& configuration) {
                Lock l(m_configurationRequestCondition);
                clog << "(DMCP-Client) Received Configuration" << endl;

                try {
                    KeyValueConfiguration kvc = configuration.getKeyValueConfiguration();
                    clog << configuration.toString() << endl;

                    {
                        Lock ll(m_configurationMutex);
                        m_configuration = kvc;
                    }

                } catch (...) {
                    HESPERIA_CORE_THROW_EXCEPTION(DMCPClientConfigurationException,
                                                  "Received configuration is invalid");
                }

                m_configured = true;
                m_configurationRequestCondition.wakeAll();
            }

            void Client::waitForConfiguration()
            {
                Lock l(m_configurationRequestCondition);
                if ( !isConfigured() ) {
                    m_configurationRequestCondition.waitOnSignalWithTimeout(CONFIGURATION_TIMEOUT);
                }

                if ( !isConfigured() ) {
                    HESPERIA_CORE_THROW_EXCEPTION(DMCPClientConfigurationException,
                                                  "Timeout while waiting for Configuration");
                }
            }

            bool Client::isConfigured()
            {
                Lock l(m_configuredMutex);
                return m_configured;
            }
        }
    }
}
