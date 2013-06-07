/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_BASE_MUTEX_H_
#define HESPERIA_CORE_BASE_MUTEX_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

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
        class HESPERIA_API Mutex {
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

#endif /*HESPERIA_CORE_BASE_MUTEX_H_*/
