/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_WIN32IMPL_WIN32CONDITIONFACTORYWORKER_H_
#define OPENDAVINCI_CORE_WRAPPER_WIN32IMPL_WIN32CONDITIONFACTORYWORKER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/ConditionFactoryWorker.h"
#include "core/wrapper/SystemLibraryProducts.h"

#include "core/wrapper/WIN32/WIN32Condition.h"

namespace core {
    namespace wrapper {

        template <> class OPENDAVINCI_API ConditionFactoryWorker<SystemLibraryWin32>
        {
            public:
                static Condition* createCondition()
                {
                    return new core::wrapper::WIN32Impl::WIN32Condition();
                }
        };

    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_WIN32IMPL_WIN32CONDITIONFACTORYWORKER_H_*/
