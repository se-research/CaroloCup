/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_BASE_LOCK_H_
#define HESPERIA_CORE_BASE_LOCK_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/base/Mutex.h"

namespace core {
    namespace base {

        /**
         * This class implements an automatic locking mechanism for
         * mutexes. If the control leaves the scope wherein this lock
         * was defined, the mutex is automatically released.
         *
         * @code
         * void foo() {
         *     Lock l(m_mutex);
         *     ...
         * }
         * @endcode
         */
        class HESPERIA_API Lock {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 *
                 * @param obj Reference to an object of this class.
                 */
                Lock(const Lock&);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 *
                 * @param obj Reference to an object of this class.
                 * @return Reference to this instance.
                 */
                Lock& operator=(const Lock&);

            public:
                /**
                 * Constructor.
                 *
                 * @param mutex Mutex to be automagically locked.
                 */
                Lock(Mutex &mutex);

                virtual ~Lock();

            private:
                Mutex &m_mutex;
        };

    }
} // core::base

#endif /*HESPERIA_CORE_BASE_LOCK_H_*/
