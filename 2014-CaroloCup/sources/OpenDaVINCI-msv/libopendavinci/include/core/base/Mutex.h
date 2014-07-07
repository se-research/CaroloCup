/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_BASE_MUTEX_H_
#define OPENDAVINCI_CORE_BASE_MUTEX_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/SharedPointer.h"
#include "core/wrapper/Mutex.h"

namespace core {
    namespace base {

        // Forward declaration to prevent circular dependencies.
        class Lock;

        /**
         * This class implements a mutex for protecting parts
         * of the code for simultaneous access using the concept
         * of scoped locks:
         *
         * @code
         * // Declaration elsewhere.
         * Mutex m;
         *
         * ...
         * {
         *     Lock l(m);
         *     // Critical part.
         * }
         * ...
         * @endcode
         */
        class OPENDAVINCI_API Mutex {
            private:
                friend class Lock;

            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                Mutex(const Mutex&);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                Mutex& operator=(const Mutex &);

            public:
                Mutex();

                virtual ~Mutex();

            protected:
                /**
                 * This method locks this mutex.
                 */
                virtual void lock();

                /**
                 * This method tries to lock this mutex.
                 *
                 * @return true, if the mutex could be locked.
                 */
                virtual bool tryLock();

                /**
                 * This method unlocks this mutex.
                 */
                virtual void unlock();

            private:
                SharedPointer<wrapper::Mutex> m_mutex;
        };

    }
} // core::base

#endif /*OPENDAVINCI_CORE_BASE_MUTEX_H_*/
