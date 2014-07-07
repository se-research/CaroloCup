/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_BASE_BREAKPOINT_H_
#define OPENDAVINCI_CORE_BASE_BREAKPOINT_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

namespace core {
    namespace base {

        /**
         * This interface encapsulates a breakpoint to interrupt the main application
         * whenever getModuleState() is called.
         */
        class OPENDAVINCI_API Breakpoint {
            public:
                virtual ~Breakpoint();

                /**
                 * This method is called, whenever this breakpoint is reached.
                 */
                virtual void reached() = 0;
        };

    }
} // core::base

#endif /*OPENDAVINCI_CORE_BASE_BREAKPOINT_H_*/
