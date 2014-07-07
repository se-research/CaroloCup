/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_POSIX_POSIXSHAREDMEMORYFACTORY_H_
#define OPENDAVINCI_CORE_WRAPPER_POSIX_POSIXSHAREDMEMORYFACTORY_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/SharedMemoryFactoryWorker.h"
#include "core/wrapper/POSIX/POSIXSharedMemory.h"

namespace core {
    namespace wrapper {

        template <> class OPENDAVINCI_API SharedMemoryFactoryWorker<SystemLibraryPosix>
        {
            public:
                static SharedPointer<SharedMemory> createSharedMemory(const string &name, const uint32_t &size)
                {
                    return SharedPointer<SharedMemory>(new POSIX::POSIXSharedMemory(name, size));
                };

                static SharedPointer<SharedMemory> attachToSharedMemory(const string &name)
                {
                    return SharedPointer<SharedMemory>(new POSIX::POSIXSharedMemory(name));
                };
        };

    }
} // core::wrapper::POSIX

#endif /*OPENDAVINCI_CORE_WRAPPER_POSIX_POSIXSHAREDMEMORYFACTORY_H_*/
