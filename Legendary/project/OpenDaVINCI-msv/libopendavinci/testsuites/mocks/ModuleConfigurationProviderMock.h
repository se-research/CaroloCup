/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef MODULECONFIGURATIONLISTENERMOCK_H_
#define MODULECONFIGURATIONLISTENERMOCK_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/base/KeyValueConfiguration.h"
#include "core/dmcp/ModuleConfigurationProvider.h"
#include "core/data/dmcp/ModuleDescriptor.h"
#include "core/data/dmcp/ModuleDescriptorComparator.h"

namespace mocks {
    class ModuleConfigurationProviderMock : public core::dmcp::ModuleConfigurationProvider {
        public:
            ModuleConfigurationProviderMock() : m_configs() {};
            virtual ~ModuleConfigurationProviderMock() {};

            void addConfig(const core::data::dmcp::ModuleDescriptor& md,
                           const core::base::KeyValueConfiguration& kv)
            {
                m_configs[md] = kv;
            }

            virtual core::base::KeyValueConfiguration getConfiguration(const core::data::dmcp::ModuleDescriptor& md)
            {
                return m_configs[md];
            };

        private:
            std::map<core::data::dmcp::ModuleDescriptor, core::base::KeyValueConfiguration, core::data::dmcp::ModuleDescriptorComparator> m_configs;
    };
}
#endif /* MODULECONFIGURATIONLISTENERMOCK_H_ */
