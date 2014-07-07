/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_SHAREDMEMORYFACTORYWORKER_H_
#define OPENDAVINCI_CORE_WRAPPER_SHAREDMEMORYFACTORYWORKER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/SharedPointer.h"
#include "core/wrapper/SharedMemory.h"
#include "core/wrapper/SystemLibraryProducts.h"

namespace core {
    namespace wrapper {

        /**
         * This template class provides factory methods to the
         * SharedMemoryFactory. The factory methods' implementations
         * for different products have to be defined in specializations
         * of the SharedMemoryFactoryWorker template class.
         *
         * @See SharedMemoryFactory, SharedMemoryFactoryWorker,
         *      SystemLibraryProducts, BoostSharedMemoryFactoryWorker,
         *      POSIXSharedMemoryFactoryWorker
         *
         */

        template <SystemLibraryProducts product>
        class OPENDAVINCI_API SharedMemoryFactoryWorker
        {
            public:
                /**
                 * This method returns the shared memory.
                 *
                 * @param name Name of the shared memory to create.
                 * @param size Required size for the new shared memory.
                 * @return Shared memory based on the type of instance this factory is.
                 */
                static SharedPointer<SharedMemory> createSharedMemory(const string &name, const uint32_t &size);

                /**
                 * This method returns the shared memory.
                 *
                 * @param name Name of the shared memory to attach.
                 * @return Shared memory based on the type of instance this factory is.
                 */
                static SharedPointer<SharedMemory> attachToSharedMemory(const string &name);
        };

    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_SHAREDMEMORYFACTORYWORKER_H_*/
