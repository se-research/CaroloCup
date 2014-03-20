/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_BASE_THREAD_H_
#define HESPERIA_CORE_BASE_THREAD_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

namespace core {
    namespace base {

        /**
         * This class provides only a convenient Thread::usleep() - interface.
         */
        class HESPERIA_API Thread {
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

#endif /*HESPERIA_CORE_BASE_THREAD_H_*/
