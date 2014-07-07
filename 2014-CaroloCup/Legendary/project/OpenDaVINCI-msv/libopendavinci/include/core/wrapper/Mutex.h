/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_MUTEX_H_
#define OPENDAVINCI_CORE_WRAPPER_MUTEX_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

namespace core {
    namespace wrapper {

        /**
         * This interface encapsulates all methods necessary to
         * realize a mutex.
         *
         * @See MutexFactory
         */
        class Mutex {
            public:
                virtual ~Mutex();

                /**
                 * This method locks a wrapped mutex.
                 */
                virtual void lock() = 0;

                /**
                 * This method tries to lock a wrapped mutex.
                 *
                 * @return true, if the mutex could be locked.
                 */
                virtual bool tryLock() = 0;

                /**
                 * This method unlocks a wrapped mutex.
                 */
                virtual void unlock() = 0;
        };

    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_MUTEX_H_*/
