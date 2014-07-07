/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_POSIX_POSIXMUTEX_H_
#define OPENDAVINCI_CORE_WRAPPER_POSIX_POSIXMUTEX_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/Mutex.h"
#include "core/wrapper/MutexFactoryWorker.h"

namespace core {
    namespace wrapper {
        namespace POSIX {

            class POSIXCondition;

            /**
             * This class implements a mutex for protecting parts
             * using pthread.
             *
             * @See Mutex
             */
            class POSIXMutex : public Mutex {
                private:
                    friend class MutexFactoryWorker<SystemLibraryPosix>;
                    friend class POSIXCondition;
                protected:
                    POSIXMutex();

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    POSIXMutex(const POSIXMutex &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    POSIXMutex& operator=(const POSIXMutex &);

                public:
                    virtual ~POSIXMutex();

                    virtual void lock();

                    virtual bool tryLock();

                    virtual void unlock();

                    pthread_mutex_t& getNativeMutex();

                private:
                    pthread_mutex_t m_mutex;
            };

        }
    }
} // core::wrapper::POSIX

#endif /*OPENDAVINCI_CORE_WRAPPER_POSIX_POSIXMUTEX_H_*/
