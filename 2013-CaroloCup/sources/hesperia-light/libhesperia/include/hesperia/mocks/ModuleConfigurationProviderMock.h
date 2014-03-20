/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef MODULECONFIGURATIONLISTENERMOCK_H_
#define MODULECONFIGURATIONLISTENERMOCK_H_

#include <map>

#include "core/native.h"
#include "core/base/KeyValueConfiguration.h"
#include "hesperia/dmcp/ModuleConfigurationProvider.h"
#include "hesperia/data/dmcp/ModuleDescriptor.h"
#include "hesperia/data/dmcp/ModuleDescriptorComparator.h"

namespace mocks {
    class ModuleConfigurationProviderMock : public hesperia::dmcp::ModuleConfigurationProvider {
        public:
            ModuleConfigurationProviderMock() : m_configs() {};
            virtual ~ModuleConfigurationProviderMock() {};

            void addConfig(const hesperia::data::dmcp::ModuleDescriptor& md,
                           const core::base::KeyValueConfiguration& kv)
            {
                m_configs[md] = kv;
            }

            virtual core::base::KeyValueConfiguration getConfiguration(const hesperia::data::dmcp::ModuleDescriptor& md)
            {
                return m_configs[md];
            };

        private:
            std::map<hesperia::data::dmcp::ModuleDescriptor, core::base::KeyValueConfiguration, hesperia::data::dmcp::ModuleDescriptorComparator> m_configs;
    };
}
#endif /* MODULECONFIGURATIONLISTENERMOCK_H_ */
