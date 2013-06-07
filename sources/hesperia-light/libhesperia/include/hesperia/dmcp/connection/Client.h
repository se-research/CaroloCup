/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_DMCP_CONNECTION_CLIENT_H_
#define HESPERIA_DMCP_CONNECTION_CLIENT_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"
#include "core/base/Condition.h"
#include "core/base/KeyValueConfiguration.h"
#include "core/base/ModuleState.h"
#include "core/data/Container.h"
#include "core/data/RuntimeStatistic.h"

#include "core/io/Connection.h"
#include "core/io/ConnectionErrorListener.h"

#include "hesperia/data/Configuration.h"
#include "hesperia/data/dmcp/ModuleDescriptor.h"
#include "hesperia/dmcp/Config.h"
#include "hesperia/dmcp/ServerInformation.h"
#include "hesperia/dmcp/SuperComponentStateListener.h"

namespace hesperia {
    namespace dmcp {
        namespace connection {

            using namespace std;

            class HESPERIA_API Client : protected core::io::ContainerListener,
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

                    Client(const hesperia::data::dmcp::ModuleDescriptor& moduleDescriptor,
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

                    void handleConfiguration(hesperia::data::Configuration& configuration);
                    void waitForConfiguration();
                    bool isConfigured();

                private:
                    hesperia::data::dmcp::ModuleDescriptor m_moduleDescriptor;
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
} // hesperia::dmcp

#endif /*HESPERIA_DMCP_CONNECTION_CLIENT_H_*/
