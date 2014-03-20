/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef SUPERCOMPONENT_GLOBALCONFIGURATIONPROVIDER_H_
#define SUPERCOMPONENT_GLOBALCONFIGURATIONPROVIDER_H_

#include <fstream>

#include "core/base/KeyValueConfiguration.h"
#include "core/base/Mutex.h"
#include "core/exceptions/Exceptions.h"
#include "core/dmcp/ModuleConfigurationProvider.h"

namespace supercomponent {

    using namespace std;

    class GlobalConfigurationProvider : public core::dmcp::ModuleConfigurationProvider
    {
        public:
            GlobalConfigurationProvider();
            GlobalConfigurationProvider(const core::base::KeyValueConfiguration& configurations);
            GlobalConfigurationProvider(const GlobalConfigurationProvider& configurationProvider);
            GlobalConfigurationProvider& operator=(const GlobalConfigurationProvider& configurationProvider);

            virtual ~GlobalConfigurationProvider();

            virtual core::base::KeyValueConfiguration getConfiguration(const core::data::dmcp::ModuleDescriptor& md);
            virtual core::base::KeyValueConfiguration getGlobalConfiguration() const;

        protected:
            core::base::KeyValueConfiguration m_configuration;
            core::base::Mutex m_configurationMutex;
    };
}

#endif /*SUPERCOMPONENT_GLOBALCONFIGURATIONPROVIDER_H_*/
