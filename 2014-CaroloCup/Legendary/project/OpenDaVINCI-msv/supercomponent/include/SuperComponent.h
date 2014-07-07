/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef SUPERCOMPONENT_H_
#define SUPERCOMPONENT_H_

#include <fstream>
#include <map>

#include "core/SharedPointer.h"
#include "core/base/KeyValueConfiguration.h"
#include "core/base/MasterModule.h"
#include "core/base/Mutex.h"
#include "core/exceptions/Exceptions.h"
#include "core/io/ContainerConference.h"
#include "core/io/ContainerListener.h"
#include "core/data/dmcp/ModuleStatistics.h"
#include "core/dmcp/Config.h"
#include "core/dmcp/ModuleConfigurationProvider.h"
#include "core/dmcp/ModuleStateListener.h"
#include "core/dmcp/connection/Server.h"
#include "core/dmcp/connection/ConnectionHandler.h"
#include "core/dmcp/connection/ModuleConnection.h"
#include "core/dmcp/discoverer/Server.h"

#include "GlobalConfigurationProvider.h"
#include "ConnectedModule.h"
#include "ConnectedModules.h"

namespace supercomponent {

    using namespace std;

    class SuperComponent : public core::base::MasterModule,
                           protected core::dmcp::connection::ConnectionHandler,
                           protected core::dmcp::ModuleStateListener,
                           protected core::io::ContainerListener
   {
        private:
            /**
             * "Forbidden" copy constructor. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the copy constructor.
             */
            SuperComponent(const SuperComponent &);

            /**
             * "Forbidden" assignment operator. Goal: The compiler should warn
             * already at compile time for unwanted bugs caused by any misuse
             * of the assignment operator.
             */
            SuperComponent& operator=(const SuperComponent &);

        public:
            /**
             * Constructor.
             *
             * @param argc Number of command line arguments.
             * @param argv Command line arguments.
             */
            SuperComponent(const int &argc, char **argv);

            virtual ~SuperComponent();

        protected:
            void checkForSuperComponent();

            virtual core::base::ModuleState::MODULE_EXITCODE body();

            virtual void onNewModule(core::dmcp::connection::ModuleConnection* mc);

            virtual void nextContainer(core::data::Container &c);

            virtual void handleChangeState(const core::data::dmcp::ModuleDescriptor& md,
                                           const core::base::ModuleState::MODULE_STATE &ms);

            virtual void handleExitCode(const core::data::dmcp::ModuleDescriptor& md,
                                        const core::base::ModuleState::MODULE_EXITCODE &me);

            virtual void handleRuntimeStatistics(const core::data::dmcp::ModuleDescriptor& md,
                                                 const core::data::RuntimeStatistic& rs);

            virtual void handleConnectionLost(const core::data::dmcp::ModuleDescriptor& md);

            virtual void handleUnkownContainer(const core::data::dmcp::ModuleDescriptor& md,
                                               const core::data::Container& container);

            ConnectedModule* moveToShutdownModules(const core::data::dmcp::ModuleDescriptor& md);

            GlobalConfigurationProvider m_configurationProvider;
            core::dmcp::discoverer::Server* m_discovererServer;
            core::dmcp::connection::Server* m_connectionServer;

            core::SharedPointer<core::io::ContainerConference> m_conference;

            ConnectedModules m_modules;
            ConnectedModules m_shutdownModules;

            core::base::Mutex m_moduleStatisticsMutex;
            core::data::dmcp::ModuleStatistics m_moduleStatistics;
    };
}

#endif /*SUPERCOMPONENT_H_*/
