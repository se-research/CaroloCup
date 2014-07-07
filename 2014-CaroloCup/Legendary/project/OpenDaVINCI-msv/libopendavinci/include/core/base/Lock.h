/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_BASE_LOCK_H_
#define OPENDAVINCI_CORE_BASE_LOCK_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

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
        class OPENDAVINCI_API Lock {
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

#endif /*OPENDAVINCI_CORE_BASE_LOCK_H_*/
