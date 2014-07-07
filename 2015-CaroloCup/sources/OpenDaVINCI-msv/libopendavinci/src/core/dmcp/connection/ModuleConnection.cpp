/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/Lock.h"
#include "core/data/Container.h"

#include "core/data/Configuration.h"
#include "core/data/dmcp/ModuleStateMessage.h"
#include "core/data/dmcp/ModuleExitCodeMessage.h"

#include "core/dmcp/connection/ModuleConnection.h"

namespace core {
    namespace dmcp {
        namespace connection {

            using namespace std;
            using namespace core::base;
            using namespace core::data;
            using namespace data;
            using namespace data::dmcp;

            ModuleConnection::ModuleConnection(core::io::Connection* connection,
                                               ModuleConfigurationProvider& configProvider) :
                m_connection(connection),
                m_configurationProvider(configProvider),
                m_discriptorCondition(),
                m_descriptor(),
                m_hasDescriptor(),
                m_stateListener(),
                m_stateListenerMutex()
            {
                m_connection->setContainerListener(this);
                m_connection->setErrorListener(this);
                m_connection->start();
            }

            ModuleConnection::~ModuleConnection()
            {
                m_connection->setContainerListener(NULL);
                m_connection->setErrorListener(NULL);
                m_connection->stop();
            }

            void ModuleConnection::setModuleStateListener(ModuleStateListener* listener)
            {
                Lock l(m_stateListenerMutex);
                m_stateListener = listener;
            }

            void ModuleConnection::waitForModuleDescription()
            {
                Lock l(m_discriptorCondition);
                if (!m_hasDescriptor) {
                    m_discriptorCondition.waitOnSignal();
                }
            }

            const ModuleDescriptor ModuleConnection::getModuleDescriptor() const
            {
                return m_descriptor;
            }

            void ModuleConnection::nextContainer(Container &container)
            {
                switch (container.getDataType()) {
                    case Container::DMCP_CONFIGURATION_REQUEST:
                    {
                        m_descriptor = container.getData<ModuleDescriptor>();
                        {
                            Lock l(m_discriptorCondition);
                            m_hasDescriptor = true;
                            m_discriptorCondition.wakeAll();
                        }

                        KeyValueConfiguration config = m_configurationProvider.getConfiguration(m_descriptor);

                        Container c(Container::CONFIGURATION, Configuration(config));
                        m_connection->send(c);
                        break;
                    }

                    case Container::DMCP_MODULESTATEMESSAGE:
                    {
                        ModuleStateMessage msg = container.getData<ModuleStateMessage>();

                        Lock l(m_stateListenerMutex);
                        if (m_stateListener) {
                            m_stateListener->handleChangeState(m_descriptor, msg.getModuleState());
                        }

                        break;
                    }

                    case Container::DMCP_MODULEEXITCODEMESSAGE:
                    {
                        ModuleExitCodeMessage msg = container.getData<ModuleExitCodeMessage>();

                        Lock l(m_stateListenerMutex);
                        if (m_stateListener) {
                            m_stateListener->handleExitCode(m_descriptor, msg.getModuleExitCode());
                        }

                        break;
                    }

                    case Container::RUNTIMESTATISTIC:
                    {
                        RuntimeStatistic rs = container.getData<RuntimeStatistic>();

                        Lock l(m_stateListenerMutex);
                        if (m_stateListener) {
                            m_stateListener->handleRuntimeStatistics(m_descriptor, rs);
                        }

                        break;
                    }

                    default:
                    {
                        Lock l(m_stateListenerMutex);
                        if (m_stateListener) {
                            m_stateListener->handleUnkownContainer(m_descriptor, container);
                        }

                        break;
                    }
                }
            }

            void ModuleConnection::handleConnectionError()
            {
                Lock l(m_stateListenerMutex);

                if (m_stateListener) {
                    m_stateListener->handleConnectionLost(m_descriptor);
                }
            }
        }
    }
} // core::dmcp
