/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_POSIX_POSIXSHAREDMEMORY_H_
#define OPENDAVINCI_CORE_WRAPPER_POSIX_POSIXSHAREDMEMORY_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/SharedMemory.h"
#include "core/wrapper/SharedMemoryFactoryWorker.h"

#include "core/wrapper/POSIX/POSIXMutex.h"

namespace core {
    namespace wrapper {
        namespace POSIX {

            /**
             * This class implements a shared memory using POSIX.
             *
             * @See SharedMemory.
             */
            class POSIXSharedMemory : public SharedMemory {
                private:
                    friend class SharedMemoryFactoryWorker<SystemLibraryPosix>;

                    /**
                     * Constructor.
                     *
                     * @param name Name of the shared memory.
                     * @param size Create a new shared memory with the given size.
                     */
                    POSIXSharedMemory(const string &name, const uint32_t &size);

                    /**
                     * Constructor.
                     *
                     * @param name Attach to an already existing shared memory.
                     */
                    POSIXSharedMemory(const string &name);

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    POSIXSharedMemory(const POSIXSharedMemory &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    POSIXSharedMemory& operator=(const POSIXSharedMemory &);

                public:
                    virtual ~POSIXSharedMemory();

                    virtual bool isValid() const;

                    virtual const string getName() const;

                    virtual void lock();

                    virtual void unlock();

                    virtual void* getSharedMemory() const;

                    virtual uint32_t getSize() const;

                private:
                    string m_name;
                    bool m_releaseSharedMemory;
                    int32_t m_shmID;
                    sem_t* m_mutexSharedMemory;
                    void *m_sharedMemory;
                    uint32_t m_size;

                    /**
                     * This method computes a CRC32 hash for the given string.
                     *
                     * @param s String for which the CRC32 hash value should be computed.
                     * @retval CRC32 hash value.
                     */
                    uint32_t getCRC32(const string &s) const;
            };

        }
    }
} // core::wrapper::POSIX

#endif /*OPENDAVINCI_CORE_WRAPPER_POSIX_POSIXSHAREDMEMORY_H_*/
