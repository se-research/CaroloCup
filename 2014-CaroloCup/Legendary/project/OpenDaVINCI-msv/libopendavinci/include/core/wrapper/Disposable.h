/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_DISPOSABLE_H_
#define OPENDAVINCI_CORE_WRAPPER_DISPOSABLE_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

namespace core {
    namespace wrapper {

        /**
         * This interface marks an object as disposable.
         */
        class OPENDAVINCI_API Disposable {
            public:
                virtual ~Disposable();
        };

    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_DISPOSABLE_H_*/
