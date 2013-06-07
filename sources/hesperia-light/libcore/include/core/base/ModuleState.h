/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_BASE_MODULESTATE_H_
#define HESPERIA_CORE_BASE_MODULESTATE_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include <string>

namespace core {
    namespace base {
        class HESPERIA_API ModuleState {
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
