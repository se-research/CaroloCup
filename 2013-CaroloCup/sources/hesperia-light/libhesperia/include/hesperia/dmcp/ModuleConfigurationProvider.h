/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_DMCP_MODULECONFIGURATIONPROVIDER_H_
#define HESPERIA_DMCP_MODULECONFIGURATIONPROVIDER_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"
#include "core/base/KeyValueConfiguration.h"

#include "hesperia/data/dmcp/ModuleDescriptor.h"

namespace hesperia {
    namespace dmcp {

        /**
         * The ModuleConfigurationProvider provides a KeyValueConfiguration
         * for a given ModuleDescriptor.
         */
        class HESPERIA_API ModuleConfigurationProvider {
            public:
                virtual ~ModuleConfigurationProvider() {};

                /**
                 * Returns a configuration for the given ModuleDescriptor. Since multiple thread may
                 * call this method, it has to be implemented thread-safe.
                 */
                virtual core::base::KeyValueConfiguration getConfiguration(const hesperia::data::dmcp::ModuleDescriptor& md) = 0;
        };
    }
} // hesperia::dmcp

#endif /* HESPERIA_DMCP_MODULECONFIGURATIONPROVIDER_H_ */
