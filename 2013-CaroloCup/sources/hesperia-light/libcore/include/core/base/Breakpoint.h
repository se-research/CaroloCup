/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_BASE_BREAKPOINT_H_
#define HESPERIA_CORE_BASE_BREAKPOINT_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

namespace core {
    namespace base {

        /**
         * This interface encapsulates a breakpoint to interrupt the main application
         * whenever getModuleState() is called.
         */
        class HESPERIA_API Breakpoint {
            public:
                virtual ~Breakpoint();

                /**
                 * This method is called, whenever this breakpoint is reached.
                 */
                virtual void reached() = 0;
        };

    }
} // core::base

#endif /*HESPERIA_CORE_BASE_BREAKPOINT_H_*/
