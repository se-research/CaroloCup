/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_WIN32IMPL_WIN32MUTEXFACTORYWORKER_H_
#define OPENDAVINCI_CORE_WRAPPER_WIN32IMPL_WIN32MUTEXFACTORYWORKER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/Mutex.h"
#include "core/wrapper/MutexFactoryWorker.h"

#include "core/wrapper/WIN32/WIN32Mutex.h"

namespace core {
    namespace wrapper {

        template <> class OPENDAVINCI_API MutexFactoryWorker<SystemLibraryWin32>
        {
            public:
                static Mutex* createMutex()
                {
                    return new core::wrapper::WIN32Impl::WIN32Mutex();
                }
        };

    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_WIN32IMPL_WIN32MUTEXFACTORYWORKER_H_*/
