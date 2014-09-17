/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_WIN32IMPL_WIN32SHAREDMEMORY_H_
#define OPENDAVINCI_CORE_WRAPPER_WIN32IMPL_WIN32SHAREDMEMORY_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/SharedMemory.h"
#include "core/wrapper/SharedMemoryFactoryWorker.h"

#include "core/wrapper/WIN32/WIN32Mutex.h"

namespace core {
    namespace wrapper {
        namespace WIN32Impl {

            /**
             * This class implements a shared memory using WIN32.
             *
             * @See SharedMemory.
             */
            class WIN32SharedMemory : public SharedMemory {
                private:
                    friend class SharedMemoryFactoryWorker<SystemLibraryWin32>;

                    /**
                     * Constructor.
                     *
                     * @param name Name of the shared memory.
                     * @param size Create a new shared memory with the given size.
                     */
                    WIN32SharedMemory(const string &name, const uint32_t &size);

                    /**
                     * Constructor.
                     *
                     * @param name Attach to an already existing shared memory.
                     */
                    WIN32SharedMemory(const string &name);

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    WIN32SharedMemory(const WIN32SharedMemory &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    WIN32SharedMemory& operator=(const WIN32SharedMemory &);

                public:
                    virtual ~WIN32SharedMemory();

                    virtual bool isValid() const;

                    virtual const string getName() const;

                    virtual void lock();

                    virtual void unlock();

                    virtual void* getSharedMemory() const;

                    virtual uint32_t getSize() const;

                private:
                    string m_name;
                    string m_internalName;
                    HANDLE m_shmID;
					HANDLE m_mutexSharedMemory;
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
} // core::wrapper::WIN32Impl

#endif /*OPENDAVINCI_CORE_WRAPPER_WIN32IMPL_WIN32SHAREDMEMORY_H_*/
