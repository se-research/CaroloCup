/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_BASE_MODULESTATE_H_
#define OPENDAVINCI_CORE_BASE_MODULESTATE_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

namespace core {
    namespace base {
        class OPENDAVINCI_API ModuleState {
            public:
                enum MODULE_STATE {
                    NOT_RUNNING,
                    RUNNING,
                    UNDEFINED_STATE
                };

                enum MODULE_EXITCODE {
                    OKAY,
                    EXCEPTION_CAUGHT,
                    SERIOUS_ERROR,
                    CONNECTION_LOST,
                    NO_SUPERCOMPONENT,
                    UNDEFINED_EXITCODE
                };

                static const std::string getAsString(const MODULE_STATE& ms);
                static const std::string getAsString(const MODULE_EXITCODE& me);
        };
    }
}
#endif
