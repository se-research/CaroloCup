/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
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

#include "hesperia/data/dmcp/ModuleStatistics.h"
#include "hesperia/dmcp/Config.h"
#include "hesperia/dmcp/ModuleConfigurationProvider.h"
#include "hesperia/dmcp/ModuleStateListener.h"
#include "hesperia/dmcp/connection/Server.h"
#include "hesperia/dmcp/connection/ConnectionHandler.h"
#include "hesperia/dmcp/connection/ModuleConnection.h"
#include "hesperia/dmcp/discoverer/Server.h"

#include "GlobalConfigurationProvider.h"
#include "ConnectedModule.h"
#include "ConnectedModules.h"

namespace supercomponent {

    using namespace std;

    class SuperComponent : public core::base::MasterModule,
                           protected hesperia::dmcp::connection::ConnectionHandler,
                           protected hesperia::dmcp::ModuleStateListener,
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

            virtual void onNewModule(hesperia::dmcp::connection::ModuleConnection* mc);

            virtual void nextContainer(core::data::Container &c);

            virtual void handleChangeState(const hesperia::data::dmcp::ModuleDescriptor& md,
                                           const core::base::ModuleState::MODULE_STATE &ms);

            virtual void handleExitCode(const hesperia::data::dmcp::ModuleDescriptor& md,
                                        const core::base::ModuleState::MODULE_EXITCODE &me);

            virtual void handleRuntimeStatistics(const hesperia::data::dmcp::ModuleDescriptor& md,
                                                 const core::data::RuntimeStatistic& rs);

            virtual void handleConnectionLost(const hesperia::data::dmcp::ModuleDescriptor& md);

            virtual void handleUnkownContainer(const hesperia::data::dmcp::ModuleDescriptor& md,
                                               const core::data::Container& container);

            ConnectedModule* moveToShutdownModules(const hesperia::data::dmcp::ModuleDescriptor& md);

            GlobalConfigurationProvider m_configurationProvider;
            hesperia::dmcp::discoverer::Server* m_discovererServer;
            hesperia::dmcp::connection::Server* m_connectionServer;

            core::SharedPointer<core::io::ContainerConference> m_conference;

            ConnectedModules m_modules;
            ConnectedModules m_shutdownModules;

            core::base::Mutex m_moduleStatisticsMutex;
            hesperia::data::dmcp::ModuleStatistics m_moduleStatistics;
    };
}

#endif /*SUPERCOMPONENT_H_*/
