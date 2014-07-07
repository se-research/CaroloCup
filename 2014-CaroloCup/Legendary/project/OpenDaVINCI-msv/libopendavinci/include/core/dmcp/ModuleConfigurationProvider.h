/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_DMCP_MODULECONFIGURATIONPROVIDER_H_
#define OPENDAVINCI_DMCP_MODULECONFIGURATIONPROVIDER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/base/KeyValueConfiguration.h"

#include "core/data/dmcp/ModuleDescriptor.h"

namespace core {
    namespace dmcp {

        /**
         * The ModuleConfigurationProvider provides a KeyValueConfiguration
         * for a given ModuleDescriptor.
         */
        class OPENDAVINCI_API ModuleConfigurationProvider {
            public:
                virtual ~ModuleConfigurationProvider() {};

                /**
                 * Returns a configuration for the given ModuleDescriptor. Since multiple thread may
                 * call this method, it has to be implemented thread-safe.
                 */
                virtual core::base::KeyValueConfiguration getConfiguration(const core::data::dmcp::ModuleDescriptor& md) = 0;
        };
    }
} // core::dmcp

#endif /* OPENDAVINCI_DMCP_MODULECONFIGURATIONPROVIDER_H_ */
