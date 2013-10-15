/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_MUTEXFACTORY_H_
#define OPENDAVINCI_CORE_WRAPPER_MUTEXFACTORY_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/Mutex.h"

namespace core {
    namespace wrapper {

        /**
         * Factory for creating wrapped mutexes (i.e.
         * mutexes based on Boost, pthread, ...).
         *
            * It can be used as follows:
            *
            * @code
            * Mutex *m = MutexFactory::getInstance().createMutex();
            *
            * ...
            *
            * if (m != NULL) {
            *     m->lock();
            * }
            *
            * ...
            * // Do some things in a critical section.
            * ...
            *
            * if (m != NULL) {
            *     m->unlock();
            * }
            *
            * if (m != NULL) {
            *     delete m;
            * }
            *
            * @endcode
         */
        struct OPENDAVINCI_API MutexFactory
        {
                /**
                 * This method creates the mutex.
                 *
                 * @return mutex based on the type of instance this factory is.
                 */
                static Mutex* createMutex();

        };

    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_MUTEXFACTORY_H_*/
