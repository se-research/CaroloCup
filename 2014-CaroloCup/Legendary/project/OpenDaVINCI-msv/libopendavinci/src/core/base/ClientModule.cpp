/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/Thread.h"
#include "core/data/Container.h"
#include "core/data/RuntimeStatistic.h"
#include "core/data/TimeStamp.h"
#include "core/exceptions/Exceptions.h"

#include "core/base/ClientModule.h"
#include "core/data/dmcp/ModuleDescriptor.h"

#include "core/dmcp/Config.h"
#include "core/dmcp/ServerInformation.h"
#include "core/dmcp/discoverer/Client.h"
#include "core/dmcp/connection/Client.h"

namespace core {
    namespace base {

        using namespace std;

        using namespace core;
        using namespace core::base;
        using namespace core::data;
        using namespace core::exceptions;

        using namespace core::dmcp;
        using namespace core::data::dmcp;

        ClientModule::ClientModule(const int32_t &argc, char **argv, const string &name) throw (InvalidArgumentException) :
                AbstractCIDModule(argc, argv),
                m_name(name),
                m_keyValueConfiguration(),
                m_dmcpClient(),
                m_lastCycle(),
                m_lastWaitTime(0),
                m_cycleCounter(0) {}

        ClientModule::~ClientModule() {}

        void ClientModule::wait() {
            // Update liveliness.
            m_cycleCounter++;
            TimeStamp current;
            const float FREQ = getFrequency();
            const long TIME_CONSUMPTION_OF_CURRENT_SLICE = (current.toMicroseconds() - m_lastCycle.toMicroseconds()) - m_lastWaitTime;
            m_lastCycle = current;
            const long ONE_SECOND_IN_MICROSECONDS = 1000 * 1000 * 1;
            const long NOMINAL_DURATION_OF_ONE_SLICE = static_cast<long>((1.0f/FREQ) * ONE_SECOND_IN_MICROSECONDS);
            const long WAITING_TIME_OF_CURRENT_SLICE = NOMINAL_DURATION_OF_ONE_SLICE - TIME_CONSUMPTION_OF_CURRENT_SLICE;

            // Inform supercomponent about statistical runtime data.
            bool sendStatistics = false;
            if (FREQ < 1) {
                sendStatistics = true;
            }
            else {
				const int32_t CYCLES_PER_SECOND = static_cast<int32_t>(fabs(floor(FREQ + 0.5)));
                if ( (m_cycleCounter % CYCLES_PER_SECOND) == 0 ) {
                    sendStatistics = true;
                    m_cycleCounter = 0;
                }
            }

            if (sendStatistics) {
                RuntimeStatistic rts;
                rts.setSliceConsumption((float)TIME_CONSUMPTION_OF_CURRENT_SLICE/(float)NOMINAL_DURATION_OF_ONE_SLICE);
                m_dmcpClient->sendStatistics(rts);
            }

            if (WAITING_TIME_OF_CURRENT_SLICE > 0) {
                m_lastWaitTime = WAITING_TIME_OF_CURRENT_SLICE;
                Thread::usleep(WAITING_TIME_OF_CURRENT_SLICE);
            }
            else {
                m_lastWaitTime = 0;
            }
        }

        const string ClientModule::getName() const {
            return m_name;
        }

        const KeyValueConfiguration ClientModule::getKeyValueConfiguration() const {
            return m_keyValueConfiguration;
        }

        ModuleState::MODULE_EXITCODE ClientModule::runModule()
        {
            ModuleState::MODULE_EXITCODE retVal = ModuleState::OKAY;

            // Try to discover supercomponent.
            ServerInformation serverInformation;
            try {
                SharedPointer<discoverer::Client> discovererClient =
                    SharedPointer<discoverer::Client>(
                            new discoverer::Client(getMultiCastGroup(),
                                                   BROADCAST_PORT_SERVER,
                                                   BROADCAST_PORT_CLIENT));

                bool supercomponentFound = false;
                uint32_t attempt  = 0;

                while ( !supercomponentFound && ( attempt < CONNECTION_RETRIES)) {
                	clog << "(ClientModule) discovering supercomponent..." << endl;
                	supercomponentFound = discovererClient->existsServer();
                	attempt++;
                }

                if ( !supercomponentFound ) {
                    clog << "(ClientModule) no supercomponent running for " << getMultiCastGroup() << endl;
                    return ModuleState::NO_SUPERCOMPONENT;
                }

                serverInformation = discovererClient->getServerInformation();
                clog << "(ClientModule) supercomponent found at " << serverInformation.toString() << endl;
            }
            catch (...) {
                clog << "(ClientModule) error while discovering supercomponent." << endl;
                return ModuleState::SERIOUS_ERROR;
            }

            clog << "(ClientModule) connecting to supercomponent..." << endl;
            string myVersion = "No version set.";
            ModuleDescriptor md(getName(), getIdentifier(), myVersion);

            try {
                // Try to get configuration from DMCP server.
                m_dmcpClient = SharedPointer<connection::Client>(
                        new connection::Client(md, serverInformation));
                m_dmcpClient->setSupercomponentStateListener(this);
                m_dmcpClient->initialize();

                // Get configuration from DMCP client.
                m_keyValueConfiguration = m_dmcpClient->getConfiguration();
            } catch (ConnectException& e) {
                clog << "(ClientModule) connecting to supercomponent failed: " << e.getMessage() << endl;
                return ModuleState::SERIOUS_ERROR;
            }

            clog << "(ClientModule) connecting to supercomponent...done" << endl;

            try {
                // Setup the module itself.
                setUp();

                setModuleState(ModuleState::RUNNING);
                m_dmcpClient->sendModuleState(ModuleState::RUNNING);

                // Execute the module's body.
                retVal = body();

                setModuleState(ModuleState::NOT_RUNNING);
                m_dmcpClient->sendModuleState(ModuleState::NOT_RUNNING);

                // Clean up.
                tearDown();
            } catch (std::exception &e) {
                // Try to catch any exception derived from std::exception and print32_t out reason.
                clog << e.what() << endl;
                retVal = ModuleState::EXCEPTION_CAUGHT;
            } catch (std::string &str) {
                clog << "string exception caught in ClientModule::run()" << endl;
                clog << str << endl;
                retVal = ModuleState::SERIOUS_ERROR;
            } catch (...) {
                // Try to catch anything else print32_t generic error.
                clog << "Unknown exception caught in ClientModule::run()" << endl;
                retVal = ModuleState::SERIOUS_ERROR;
            }

            if (m_dmcpClient.isValid()) {
                m_dmcpClient->sendModuleExitCode(retVal);
            }

            return retVal;
        }

        void ClientModule::handleConnectionLost() {
            clog << "(ClientModule) connection to supercomponent lost. Shutting down" << endl;
            setModuleState(ModuleState::NOT_RUNNING);
        }
    }
} // core::base
