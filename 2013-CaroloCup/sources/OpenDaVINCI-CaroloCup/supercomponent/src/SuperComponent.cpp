/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include <cstdlib>
#include <fstream>

#include "core/base/KeyValueConfiguration.h"
#include "core/base/Lock.h"
#include "core/base/Thread.h"
#include "core/data/Container.h"
#include "core/io/ContainerConferenceFactory.h"

#include "core/data/Configuration.h"
#include "core/dmcp/discoverer/Client.h"
#include "core/dmcp/ServerInformation.h"

#include "SuperComponent.h"
#include "ConnectedModule.h"

namespace supercomponent {

    using namespace core::base;
    using namespace core::data;
    using namespace core::exceptions;
    using namespace core::data::dmcp;
    using namespace core::dmcp;
    using namespace core::dmcp::connection;
    using namespace core::io;

    SuperComponent::SuperComponent(const int &argc, char **argv) :
            MasterModule(argc, argv),
            m_configurationProvider(),
            m_discovererServer(),
            m_connectionServer(NULL),
            m_conference(NULL),
            m_modules(),
            m_shutdownModules(),
            m_moduleStatisticsMutex(),
            m_moduleStatistics() {
        checkForSuperComponent();

        cout << "(supercomponent) parsing configuration file..." << endl;
        core::base::KeyValueConfiguration configuration;
        ifstream configStream("configuration", ios::in);
        if (configStream.good()) {
            configStream >> configuration;
        }
        else {
            OPENDAVINCI_CORE_THROW_EXCEPTION(ConfigurationFileNotFoundException, "Configuration stream invalid.");
        }

        const uint32_t SERVER_PORT = CONNECTIONSERVER_PORT_BASE + getCID();
        // Listen on all interfaces.
        ServerInformation serverInformation("0.0.0.0", SERVER_PORT);

        cout << "(supercomponent) server information: " << serverInformation.toString() << endl;
        cout << "(supercomponent) creating discoverer server..." << endl;
        m_discovererServer = new discoverer::Server(serverInformation,
													getMultiCastGroup(),
                                                    BROADCAST_PORT_SERVER,
                                                    BROADCAST_PORT_CLIENT);
        m_discovererServer->startResponding();

        cout << "(supercomponent) creating connection server..." << endl;
        m_configurationProvider = GlobalConfigurationProvider(configuration);
        m_connectionServer = new connection::Server(serverInformation, m_configurationProvider);
        m_connectionServer->setConnectionHandler(this);

        m_conference = core::SharedPointer<ContainerConference>(ContainerConferenceFactory::getInstance().getContainerConference(getMultiCastGroup()));
        m_conference->setContainerListener(this);
    }

    SuperComponent::~SuperComponent() {
        delete m_connectionServer;
        m_connectionServer = NULL;

        delete m_discovererServer;
        m_discovererServer = NULL;
    }

    void SuperComponent::checkForSuperComponent() {
        discoverer::Client discovererClient(getMultiCastGroup(),
                                            BROADCAST_PORT_SERVER,
                                            BROADCAST_PORT_CLIENT);

         if ( discovererClient.existsServer() ) {
             cout << "(supercomponent) supercomponent already running for " << getMultiCastGroup() << endl;
             exit(-1);
         }
    }

    ModuleState::MODULE_EXITCODE SuperComponent::body() {
        while (getModuleState() == ModuleState::RUNNING)
        {
            {
                // Update statistics.
                Lock l(m_moduleStatisticsMutex);
                Container c(Container::MODULESTATISTICS, m_moduleStatistics);
                m_conference->send(c);
            }

            m_shutdownModules.deleteAllModules();
            Thread::usleep(1000 * 5000);
        }

        m_conference->setContainerListener(NULL);

        return ModuleState::OKAY;
    }

    void SuperComponent::onNewModule(ModuleConnection* mc) {
        mc->waitForModuleDescription();
        cout << "(supercomponent) new connected module " << mc->getModuleDescriptor().toString() << endl;

        ConnectedModule* module = new ConnectedModule(mc, ModuleState::NOT_RUNNING);
        mc->setModuleStateListener(this);

        m_modules.addModule(mc->getModuleDescriptor(), module);
    }

    void SuperComponent::handleChangeState(const ModuleDescriptor& md, const ModuleState::MODULE_STATE &ms) {
        cout << "(supercomponent) module " << md.toString() << " changed state to " << ModuleState::getAsString(ms) << endl;
        m_modules.getModule(md)->setState(ms);
    }

    void SuperComponent::handleExitCode(const ModuleDescriptor& md, const ModuleState::MODULE_EXITCODE &me) {
        cout << "(supercomponent) module " << md.toString() << " shutdown with exitcode " << ModuleState::getAsString(me) << endl;

        ConnectedModule* module = m_modules.getModule(md);
        m_modules.removeModule(md);
        module->setExitCode();
        m_shutdownModules.addModule(md, module);
    }

    void SuperComponent::handleRuntimeStatistics(const ModuleDescriptor& md, const RuntimeStatistic& rs) {
        Lock l(m_moduleStatisticsMutex);
        m_moduleStatistics.setRuntimeStatistic(md, rs);
    }

    void SuperComponent::handleConnectionLost(const ModuleDescriptor& md) {
        // This methods is called when a module terminates not properly.
        if (m_modules.hasModule(md)) {
            cout << "(supercomponent) connection to module " << md.toString() << " lost" << endl;
            ConnectedModule* module = m_modules.getModule(md);
            m_modules.removeModule(md);
            m_shutdownModules.addModule(md, module);
        } else {
            if (m_shutdownModules.hasModule(md)) {
                if (!m_shutdownModules.getModule(md)->hasExitCode()) {
                    cout << "(supercomponent) something went wrong...lost connection to module " << md.toString();
                    cout << ", module is in shutdownModule list but has not set an ExitCode." << endl;
                }
            }
        }
    }

    void SuperComponent::handleUnkownContainer(const ModuleDescriptor& md, const Container& container) {
        cout << "Received unknown container " << container.toString() << "from " << md.toString() << endl;
    }

    void SuperComponent::nextContainer(Container &/*container*/) {}
    
} // supercomponent
