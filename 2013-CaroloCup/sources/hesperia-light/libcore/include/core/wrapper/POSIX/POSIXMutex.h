/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_POSIX_POSIXMUTEX_H_
#define HESPERIA_CORE_WRAPPER_POSIX_POSIXMUTEX_H_

#include <pthread.h>

#include "core/wrapper/Mutex.h"

namespace core {
    namespace wrapper {
        namespace POSIX {

            // Forward declaration to prevent circular dependencies.
            class POSIXMutexFactory;
            class POSIXCondition;

            /**
             * This class implements a mutex for protecting parts
             * using pthread.
             *
             * @See Mutex
             */
            class POSIXMutex : public Mutex {
                private:
                    friend class POSIXMutexFactory;
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

#endif /*HESPERIA_CORE_WRAPPER_POSIX_POSIXMUTEX_H_*/
