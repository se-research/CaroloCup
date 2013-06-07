/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_CONDITION_H_
#define OPENDAVINCI_CORE_WRAPPER_CONDITION_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

namespace core {
    namespace wrapper {

        /**
         * This interface encapsulates all methods necessary to
         * realize a condition.
         *
         * @See ConditionFactory
         */
        class Condition {
            public:
                virtual ~Condition();

                /**
                 * This method suspends the execution of the
                 * current thread.
                 */
                virtual void waitOnSignal() = 0;

                /**
                 * This method suspends the execution of the
                 * current thread with a timeout.
                 */
                virtual bool waitOnSignalWithTimeout(const unsigned long ms) = 0;

                /**
                 * This method awakes only one sleeping thread.
                 */
                virtual void wakeOne() = 0;

                /**
                 * This method awakes all threads sleeping on
                 * this condition.
                 */
                virtual void wakeAll() = 0;

                /**
                 * This method locks the condition's mutex.
                 */
                virtual void lock() = 0;

                /**
                 * This method tries to lock the condition's mutex.
                 *
                 * @return true, if the mutex could be locked.
                 */
                virtual bool tryLock() = 0;

                /**
                 * This method unlocks the condition's mutex.
                 */
                virtual void unlock() = 0;
        };

    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_CONDITION_H_*/
