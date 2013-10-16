/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_SHAREDMEMORY_H_
#define OPENDAVINCI_CORE_WRAPPER_SHAREDMEMORY_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

namespace core {
    namespace wrapper {

        using namespace std;

        /**
         * This interface encapsulates all methods necessary to
         * realize a condition.
         *
         * @See SharedMemoryFactory
         */
        class SharedMemory {
            public:
                virtual ~SharedMemory();

                /**
                 * This method returns true if the shared memory is valid.
                 *
                 * @return true if the shared memory is valid.
                 */
                virtual bool isValid() const = 0;

                /**
                 * This method returns the name for the shared memory.
                 *
                 * @return name for the shared memory.
                 */
                virtual const string getName() const = 0;

                /**
                 * This method tries to lock the shared memory.
                 */
                virtual void lock() = 0;

                /**
                 * This method unlocks the shared memory.
                 */
                virtual void unlock() = 0;

                /**
                 * This method returns a pointer to the beginning of the
                 * shared memory.
                 *
                 * @return Pointer to the beginning of the shared memory.
                 */
                virtual void* getSharedMemory() const = 0;

                /**
                 * This method returns the size of the shared memory.
                 *
                 * @return Size of the shared memory.
                 */
                virtual uint32_t getSize() const = 0;
        };

    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_SHAREDMEMORY_H_*/
