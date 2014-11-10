/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_WIN32IMPL_WIN32MUTEX_H_
#define OPENDAVINCI_CORE_WRAPPER_WIN32IMPL_WIN32MUTEX_H_

// Using c++11 standard.
#include <mutex>

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/Mutex.h"
#include "core/wrapper/MutexFactoryWorker.h"

namespace core {
    namespace wrapper {
        namespace WIN32Impl {

            class WIN32Condition;

            /**
             * This class implements a mutex for protecting parts
             * using pthread.
             *
             * @See Mutex
             */
            class WIN32Mutex : public Mutex {
                private:
                    friend class MutexFactoryWorker<SystemLibraryWin32>;
                    friend class WIN32Condition;

                protected:
                    WIN32Mutex();

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    WIN32Mutex(const WIN32Mutex &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    WIN32Mutex& operator=(const WIN32Mutex &);

                public:
                    virtual ~WIN32Mutex();

                    virtual void lock();

                    virtual bool tryLock();

                    virtual void unlock();

                    std::mutex& getNativeMutex();

                private:
                    std::mutex m_mutex;
            };

        }
    }
} // core::wrapper::WIN32Impl

#endif /*OPENDAVINCI_CORE_WRAPPER_WIN32IMPL_WIN32MUTEX_H_*/
