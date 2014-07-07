/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_STATICINSTANCEFACTORY_H_
#define OPENDAVINCI_CORE_WRAPPER_STATICINSTANCEFACTORY_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

namespace core {
    namespace wrapper {

        // Compile-time factory.
        template <
                  typename product,
                  template <product> class worker,
                  template <typename> class configuration
                 >
        struct OPENDAVINCI_API StaticInstanceFactory
        {
            // Product type created by factory
            typedef product product_type;

            // Configuration for factory.
            typedef typename configuration<product_type>::configuration configuration_value;

            // Factory method to create a concrete product
            typedef worker<configuration_value::value> worker_type;

            // Wrapper to realize run-time factory.
            static worker_type& getInstance() { return instance; }

            protected:
                // Wrapper to realize run-time factory.
                static worker_type instance;
        };

    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_STATICINSTANCEFACTORY_H_*/
