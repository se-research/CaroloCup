/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTSHAREDMEMORY_H_
#define OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTSHAREDMEMORY_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include <boost/interprocess/mapped_region.hpp>
#include <boost/interprocess/shared_memory_object.hpp>
#include <boost/interprocess/sync/interprocess_semaphore.hpp>

#include "core/SharedPointer.h"
#include "core/wrapper/SharedMemory.h"
#include "core/wrapper/Boost/BoostMutex.h"
#include "core/wrapper/SharedMemoryFactoryWorker.h"

namespace core {
    namespace wrapper {
        namespace Boost {

            /**
             * This class implements a shared memory using Boost.
             *
             * @See SharedMemory.
             */
            class BoostSharedMemory : public SharedMemory {
                private:
                    friend class SharedMemoryFactoryWorker<SystemLibraryBoost>;

                    /**
                     * Constructor.
                     *
                     * @param name Name of the shared memory.
                     * @param size Create a new shared memory with the given size.
                     */
                    BoostSharedMemory(const string &name, const uint32_t &size);

                    /**
                     * Constructor.
                     *
                     * @param name Attach to an already existing shared memory.
                     */
                    BoostSharedMemory(const string &name);

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    BoostSharedMemory(const BoostSharedMemory &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    BoostSharedMemory& operator=(const BoostSharedMemory &);

                public:
                    virtual ~BoostSharedMemory();

                    virtual bool isValid() const;

                    virtual const string getName() const;

                    virtual void lock();

                    virtual void unlock();

                    virtual void* getSharedMemory() const;

                    virtual uint32_t getSize() const;

                private:
                    string m_name;
                    uint32_t m_size;
                    bool m_valid;
                    bool m_removeSharedMemory;

                    SharedPointer<boost::interprocess::shared_memory_object> m_sharedMemory;
                    SharedPointer<boost::interprocess::mapped_region> m_mappedRegionOfSharedMemory;

                    // Do NOT use SharedPointer here since m_sharedMemoryMutex is created inside the shared memory.
                    boost::interprocess::interprocess_semaphore *m_sharedMemoryMutex;
            };

        }
    }
} // core::wrapper::Boost

#endif /*OPENDAVINCI_CORE_WRAPPER_BOOST_BOOSTSHAREDMEMORY_H_*/
