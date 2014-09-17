/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_WIN32IMPL_WIN32SHAREDMEMORYFACTORY_H_
#define OPENDAVINCI_CORE_WRAPPER_WIN32IMPL_WIN32SHAREDMEMORYFACTORY_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/SharedMemoryFactoryWorker.h"
#include "core/wrapper/WIN32/WIN32SharedMemory.h"

namespace core {
    namespace wrapper {

        template <> class OPENDAVINCI_API SharedMemoryFactoryWorker<SystemLibraryWin32>
        {
            public:
                static SharedPointer<SharedMemory> createSharedMemory(const string &name, const uint32_t &size)
                {
                    return SharedPointer<SharedMemory>(new WIN32Impl::WIN32SharedMemory(name, size));
                };

                static SharedPointer<SharedMemory> attachToSharedMemory(const string &name)
                {
                    return SharedPointer<SharedMemory>(new WIN32Impl::WIN32SharedMemory(name));
                };
        };

    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_WIN32IMPL_WIN32SHAREDMEMORYFACTORY_H_*/
