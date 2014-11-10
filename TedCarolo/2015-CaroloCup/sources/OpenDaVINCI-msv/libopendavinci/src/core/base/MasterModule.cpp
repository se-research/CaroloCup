/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/MasterModule.h"

namespace core {
    namespace base {

        using namespace std;
        using namespace exceptions;

        MasterModule::MasterModule(const int32_t &argc, char **argv) throw (InvalidArgumentException) :
                AbstractCIDModule(argc, argv) {}

        MasterModule::~MasterModule() {}

        ModuleState::MODULE_EXITCODE MasterModule::runModule() {
            ModuleState::MODULE_EXITCODE retVal = ModuleState::OKAY;

            setModuleState(ModuleState::RUNNING);

            try {
                retVal = body();
            } catch (std::exception &e) {
                // Try to catch any exception derived from std::exception and print32_t out reason.
                clog << e.what() << endl;
                retVal = ModuleState::EXCEPTION_CAUGHT;
            } catch (...) {
                // Try to catch anything else print32_t generic error.
                clog << "Unknown exception caught in MasterModule::run()" << endl;
                retVal = ModuleState::SERIOUS_ERROR;
            }

            return retVal;
        }

    }
} // core::base
