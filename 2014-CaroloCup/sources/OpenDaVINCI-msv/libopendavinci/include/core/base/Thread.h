/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_BASE_THREAD_H_
#define OPENDAVINCI_CORE_BASE_THREAD_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

namespace core {
    namespace base {

        /**
         * This class provides only a convenient Thread::usleep() - interface.
         */
        class OPENDAVINCI_API Thread {
            public:
                /**
                 * This methods sleeps for the specified amount of time.
                 *
                 * @param microseconds Time to sleep.
                 */
                static void usleep(const long &microseconds);
        };

    }
} // core::base

#endif /*OPENDAVINCI_CORE_BASE_THREAD_H_*/
