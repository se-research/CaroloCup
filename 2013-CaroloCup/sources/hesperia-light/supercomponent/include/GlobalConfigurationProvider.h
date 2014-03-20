/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef SUPERCOMPONENT_GLOBALCONFIGURATIONPROVIDER_H_
#define SUPERCOMPONENT_GLOBALCONFIGURATIONPROVIDER_H_

#include <fstream>

#include "core/base/KeyValueConfiguration.h"
#include "core/base/Mutex.h"
#include "core/exceptions/Exceptions.h"

#include "hesperia/dmcp/ModuleConfigurationProvider.h"

namespace supercomponent {

    using namespace std;

    class GlobalConfigurationProvider : public hesperia::dmcp::ModuleConfigurationProvider
    {
        public:
            GlobalConfigurationProvider();
            GlobalConfigurationProvider(const core::base::KeyValueConfiguration& configurations);
            GlobalConfigurationProvider(const GlobalConfigurationProvider& configurationProvider);
            GlobalConfigurationProvider& operator=(const GlobalConfigurationProvider& configurationProvider);

            virtual ~GlobalConfigurationProvider();

            virtual core::base::KeyValueConfiguration getConfiguration(const hesperia::data::dmcp::ModuleDescriptor& md);
            virtual core::base::KeyValueConfiguration getGlobalConfiguration() const;

        protected:
            core::base::KeyValueConfiguration m_configuration;
            core::base::Mutex m_configurationMutex;
    };
}

#endif /*SUPERCOMPONENT_GLOBALCONFIGURATIONPROVIDER_H_*/
