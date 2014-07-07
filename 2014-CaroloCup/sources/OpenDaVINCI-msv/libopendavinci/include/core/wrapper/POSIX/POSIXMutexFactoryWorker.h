/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_POSIX_POSIXMUTEXFACTORYWORKER_H_
#define OPENDAVINCI_CORE_WRAPPER_POSIX_POSIXMUTEXFACTORYWORKER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/Mutex.h"
#include "core/wrapper/MutexFactoryWorker.h"

#include "core/wrapper/POSIX/POSIXMutex.h"

namespace core {
    namespace wrapper {

        template <> class OPENDAVINCI_API MutexFactoryWorker<SystemLibraryPosix>
        {
            public:
                static Mutex* createMutex()
                {
                    return new core::wrapper::POSIX::POSIXMutex();
                }
        };

    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_POSIX_POSIXMUTEXFACTORYWORKER_H_*/
