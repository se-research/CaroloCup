/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/ModuleState.h"

namespace core {
    namespace base {

        const std::string ModuleState::getAsString(const MODULE_STATE& ms) {
            std::string retVal;
            switch (ms) {
                case RUNNING:
                {
                    retVal = "RUNNING";
                    break;
                }
                case NOT_RUNNING:
                {
                    retVal = "NOT_RUNNING";
                    break;
                }
                case UNDEFINED_STATE:
                {
                    retVal = "UNDEFINED_STATE";
                    break;
                }
            }
            return retVal;
        }

        const std::string ModuleState::getAsString(const MODULE_EXITCODE& me) {
            std::string retVal;
            switch (me) {
                case OKAY:
                {
                    retVal = "OKAY";
                    break;
                }
                case EXCEPTION_CAUGHT:
                {
                    retVal = "EXCEPTION_CAUGHT";
                    break;
                }
                case SERIOUS_ERROR:
                {
                    retVal = "SERIOUS_ERROR";
                    break;
                }
                case CONNECTION_LOST:
                {
                    retVal = "CONNECTION_LOST";
                    break;
                }
                case UNDEFINED_EXITCODE:
                {
                    retVal = "UNDEFINED_EXITCODE";
                    break;
                }
                case NO_SUPERCOMPONENT:
                {
                    retVal = "NO_SUPERCOMPONENT";
                    break;
                }
            }
            return retVal;
        }
    }
} // core::base
