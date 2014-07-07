/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_BASE_PERIODIC_H_
#define OPENDAVINCI_CORE_BASE_PERIODIC_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

namespace core {
    namespace base {

        using namespace std;

        /**
         * This interface encapsulates all methods for realizing a periodic
         * execution.
         */
        class OPENDAVINCI_API Periodic {
            public:
                virtual ~Periodic();

                /**
                 * This method returns the runtime frequency.
                 *
                 * @return Runtime frequency.
                 */
                virtual float getFrequency() const = 0;
        };

    }
} // core::base

#endif /*OPENDAVINCI_CORE_BASE_PERIODIC_H_*/
