/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_SHAREDMEMORYFACTORY_H_
#define OPENDAVINCI_CORE_WRAPPER_SHAREDMEMORYFACTORY_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/SharedPointer.h"
#include "core/wrapper/SharedMemory.h"

namespace core {
    namespace wrapper {


        /**
         * Abstract factory for creating shared memory between independent
         * processes using different implementations (i.e. Boost or POSIX).
         */
        struct OPENDAVINCI_API SharedMemoryFactory
        {
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

#endif /*OPENDAVINCI_CORE_WRAPPER_SHAREDMEMORYFACTORY_H_*/
