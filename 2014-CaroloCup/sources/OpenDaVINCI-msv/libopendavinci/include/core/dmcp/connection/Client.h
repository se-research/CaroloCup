/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_DMCP_CONNECTION_CLIENT_H_
#define OPENDAVINCI_DMCP_CONNECTION_CLIENT_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/base/Condition.h"
#include "core/base/KeyValueConfiguration.h"
#include "core/base/ModuleState.h"
#include "core/data/Container.h"
#include "core/data/RuntimeStatistic.h"

#include "core/io/Connection.h"
#include "core/io/ConnectionErrorListener.h"

#include "core/data/Configuration.h"
#include "core/data/dmcp/ModuleDescriptor.h"
#include "core/dmcp/Config.h"
#include "core/dmcp/ServerInformation.h"
#include "core/dmcp/SuperComponentStateListener.h"

namespace core {
    namespace dmcp {
        namespace connection {

            using namespace std;

            class OPENDAVINCI_API Client : protected core::io::ContainerListener,
                                        protected core::io::ConnectionErrorListener
            {

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    Client(const Client &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    Client& operator=(const Client &);

                public:

                    Client(const core::data::dmcp::ModuleDescriptor& moduleDescriptor,
                           const ServerInformation& serverInformation);

                    virtual ~Client();

                    void initialize();

                    void sendModuleExitCode(const core::base::ModuleState::MODULE_EXITCODE& me);
                    void sendModuleState(const core::base::ModuleState::MODULE_STATE& me);
                    void sendStatistics(const core::data::RuntimeStatistic& rs);

                    core::base::KeyValueConfiguration getConfiguration();

                    void setSupercomponentStateListener(SupercomponentStateListener* listener);

                protected:
                    void sendConfigurationRequest();
                    virtual void nextContainer(core::data::Container &c);
                    virtual void handleConnectionError();

                    void handleConfiguration(core::data::Configuration& configuration);
                    void waitForConfiguration();
                    bool isConfigured();

                private:
                    core::data::dmcp::ModuleDescriptor m_moduleDescriptor;
                    ServerInformation m_serverInformation;

                    core::io::Connection m_connection;

                    core::base::Mutex m_configurationMutex;
                    core::base::KeyValueConfiguration m_configuration;

                    bool m_configured;
                    core::base::Mutex m_configuredMutex;
                    core::base::Condition m_configurationRequestCondition;

                    core::base::Mutex m_listenerMutex;
                    SupercomponentStateListener* m_listener;
            };
        }
    }
} // core::dmcp

#endif /*OPENDAVINCI_DMCP_CONNECTION_CLIENT_H_*/
