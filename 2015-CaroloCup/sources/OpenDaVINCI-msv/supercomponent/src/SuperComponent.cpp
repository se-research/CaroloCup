/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include <cstdlib>
#include <fstream>

#include "core/base/CommandLineParser.h"
#include "core/base/Lock.h"
#include "core/base/Thread.h"
#include "core/data/Container.h"
#include "core/io/ContainerConferenceFactory.h"

#include "core/data/TimeStamp.h"
#include "core/data/dmcp/PulseMessage.h"
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
            m_configuration(),
            m_configurationProvider(),
            m_discovererServer(),
            m_connectionServer(NULL),
            m_conference(NULL),
            m_modules(),
            m_shutdownModules(),
            m_moduleStatisticsMutex(),
            m_moduleStatistics(),
            m_managedLevel(core::dmcp::ServerInformation::ML_NONE),
            m_lastCycle(),
            m_lastWaitTime(0),
            m_shiftMicroseconds(0) {
        checkForSuperComponent();

        parseAdditionalCommandLineParameters(argc, argv);

        cout << "(supercomponent) Parsing configuration file..." << endl;
        ifstream configStream("configuration", ios::in);
        if (configStream.good()) {
            configStream >> m_configuration;
        }
        else {
            OPENDAVINCI_CORE_THROW_EXCEPTION(ConfigurationFileNotFoundException, "Configuration stream invalid.");
        }

        const uint32_t SERVER_PORT = CONNECTIONSERVER_PORT_BASE + getCID();
        // Listen on all interfaces.
        ServerInformation serverInformation("0.0.0.0", SERVER_PORT, m_managedLevel);

        cout << "(supercomponent) Server information: " << serverInformation.toString() << endl;
        cout << "(supercomponent) Creating discoverer server..." << endl;
        m_discovererServer = new discoverer::Server(serverInformation,
													getMultiCastGroup(),
                                                    BROADCAST_PORT_SERVER,
                                                    BROADCAST_PORT_CLIENT);
        m_discovererServer->startResponding();

        cout << "(supercomponent) Creating connection server..." << endl;
        m_configurationProvider = GlobalConfigurationProvider(m_configuration);
        m_connectionServer = new connection::Server(serverInformation, m_configurationProvider);
        m_connectionServer->setConnectionHandler(this);

        m_conference = core::SharedPointer<ContainerConference>(ContainerConferenceFactory::getInstance().getContainerConference(getMultiCastGroup()));
        m_conference->setContainerListener(this);

        cout << "(supercomponent) Ready - managed level " << m_managedLevel << endl;
    }

    SuperComponent::~SuperComponent() {
        delete m_connectionServer;
        m_connectionServer = NULL;

        delete m_discovererServer;
        m_discovererServer = NULL;
    }

    void SuperComponent::parseAdditionalCommandLineParameters(const int &argc, char **argv) {
        CommandLineParser cmdParser;
        cmdParser.addCommandLineArgument("managed");

        cmdParser.parse(argc, argv);

        CommandLineArgument cmdArgumentMANAGED = cmdParser.getCommandLineArgument("managed");

        // Check the centrally maintained managed level.
        if (cmdArgumentMANAGED.isSet()) {
            string managedLevel = cmdArgumentMANAGED.getValue<string>();
            core::StringToolbox::trim(managedLevel);

            if (core::StringToolbox::equalsIgnoreCase(managedLevel, "none")) {
                m_managedLevel = core::dmcp::ServerInformation::ML_NONE;
            }
            if (core::StringToolbox::equalsIgnoreCase(managedLevel, "pulse")) {
                m_managedLevel = core::dmcp::ServerInformation::ML_PULSE;
            }
            if (core::StringToolbox::equalsIgnoreCase(managedLevel, "pulse_shift")) {
                m_managedLevel = core::dmcp::ServerInformation::ML_PULSE_SHIFT;

                m_shiftMicroseconds = 10 * 1000;
                try {
                    m_shiftMicroseconds = m_configuration.getValue<uint32_t>("supercomponent.pulseshift.shift");
                }
                catch(...) {
                    cerr << "(supercomponent) Value for 'supercomponent.pulseshift.shift' not found in configuration, using " << m_shiftMicroseconds << " as default." << endl;
                }
            }
        }
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
        uint32_t cumulatedTimeSlice = 0;
        const uint32_t ONE_SECOND_IN_MICROSECONDS = 1000 * 1000 * 1;

        m_lastCycle = TimeStamp();
        while (getModuleState() == ModuleState::RUNNING) {
            TimeStamp current;

            {
                // Update statistics.
                Lock l(m_moduleStatisticsMutex);
                Container c(Container::MODULESTATISTICS, m_moduleStatistics);
                m_conference->send(c);
            }

            m_shutdownModules.deleteAllModules();

            if (m_managedLevel == core::dmcp::ServerInformation::ML_NONE) {
                Thread::usleep(1000 * 5000);
            }
            else if ( (m_managedLevel == core::dmcp::ServerInformation::ML_PULSE) ||
                      (m_managedLevel == core::dmcp::ServerInformation::ML_PULSE_SHIFT) ) {
                const float FREQ = getFrequency();
                const long TIME_CONSUMPTION_OF_CURRENT_SLICE = (current.toMicroseconds() - m_lastCycle.toMicroseconds()) - m_lastWaitTime;
                m_lastCycle = current;
                const long NOMINAL_DURATION_OF_ONE_SLICE = static_cast<long>((1.0f/FREQ) * ONE_SECOND_IN_MICROSECONDS);
                const long WAITING_TIME_OF_CURRENT_SLICE = NOMINAL_DURATION_OF_ONE_SLICE - TIME_CONSUMPTION_OF_CURRENT_SLICE;

                if (isVerbose()) {
                    cerr << "(supercomponent) Waiting real slice: " << WAITING_TIME_OF_CURRENT_SLICE << ", nominal waiting slice: " << NOMINAL_DURATION_OF_ONE_SLICE << ", cumulated time slice: " << cumulatedTimeSlice << endl;
                }

                const TimeStamp supercomponent_now;
                PulseMessage pm(supercomponent_now, NOMINAL_DURATION_OF_ONE_SLICE, cumulatedTimeSlice);

                if (m_managedLevel == core::dmcp::ServerInformation::ML_PULSE) {
                    m_modules.pulse(pm);
                }
                else if (m_managedLevel == core::dmcp::ServerInformation::ML_PULSE_SHIFT) {
                    m_modules.pulseShift(pm, m_shiftMicroseconds);
                }

                m_lastWaitTime = WAITING_TIME_OF_CURRENT_SLICE;
                cumulatedTimeSlice = (cumulatedTimeSlice + NOMINAL_DURATION_OF_ONE_SLICE) % ONE_SECOND_IN_MICROSECONDS;

                Thread::usleep(WAITING_TIME_OF_CURRENT_SLICE);
            }
        }

        m_conference->setContainerListener(NULL);

        return ModuleState::OKAY;
    }

    void SuperComponent::onNewModule(ModuleConnection* mc) {
        mc->waitForModuleDescription();
        cout << "(supercomponent) New connected module " << mc->getModuleDescriptor().toString() << endl;

        ConnectedModule* module = new ConnectedModule(mc, ModuleState::NOT_RUNNING);
        mc->setModuleStateListener(this);

        if (m_modules.hasModule(mc->getModuleDescriptor())) {
            cout << "(supercomponent) WARNING!!! MODULE " << mc->getModuleDescriptor().toString() << " AREADY CONNECTED: Consider using --id to differentiate modules!" << endl;
        }
        else {
            m_modules.addModule(mc->getModuleDescriptor(), module);
        }
    }

    void SuperComponent::handleChangeState(const ModuleDescriptor& md, const ModuleState::MODULE_STATE &ms) {
        cout << "(supercomponent) Module " << md.toString() << " changed state to " << ModuleState::getAsString(ms) << endl;
        if (m_modules.hasModule(md)) {
            m_modules.getModule(md)->setState(ms);
        }
    }

    void SuperComponent::handleExitCode(const ModuleDescriptor& md, const ModuleState::MODULE_EXITCODE &me) {
        cout << "(supercomponent) Module " << md.toString() << " shutdown with exitcode " << ModuleState::getAsString(me) << endl;

        if (m_modules.hasModule(md)) {
            ConnectedModule* module = m_modules.getModule(md);
            m_modules.removeModule(md);
            module->setExitCode();
            m_shutdownModules.addModule(md, module);
        }
    }

    void SuperComponent::handleRuntimeStatistics(const ModuleDescriptor& md, const RuntimeStatistic& rs) {
        Lock l(m_moduleStatisticsMutex);
        m_moduleStatistics.setRuntimeStatistic(md, rs);
    }

    void SuperComponent::handleConnectionLost(const ModuleDescriptor& md) {
        // This methods is called when a module terminates not properly.
        if (m_modules.hasModule(md)) {
            cout << "(supercomponent) Connection to module " << md.toString() << " lost" << endl;
            ConnectedModule* module = m_modules.getModule(md);
            m_modules.removeModule(md);
            m_shutdownModules.addModule(md, module);
        }
        else {
            if (m_shutdownModules.hasModule(md)) {
                if (!m_shutdownModules.getModule(md)->hasExitCode()) {
                    cout << "(supercomponent) Something went wrong...lost connection to module " << md.toString();
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
