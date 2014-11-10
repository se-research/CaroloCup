/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_POSIX_POSIXCONDITIONFACTORYWORKER_H_
#define OPENDAVINCI_CORE_WRAPPER_POSIX_POSIXCONDITIONFACTORYWORKER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/ConditionFactoryWorker.h"
#include "core/wrapper/SystemLibraryProducts.h"

#include "core/wrapper/POSIX/POSIXCondition.h"

namespace core {
    namespace wrapper {

        template <> class OPENDAVINCI_API ConditionFactoryWorker<SystemLibraryPosix>
        {
            public:
                static Condition* createCondition()
                {
                    return new core::wrapper::POSIX::POSIXCondition();
                }
        };

    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_POSIX_POSIXCONDITIONFACTORYWORKER_H_*/
