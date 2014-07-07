/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_CONCURRENCYFACTORY_H_
#define OPENDAVINCI_CORE_WRAPPER_CONCURRENCYFACTORY_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/Runnable.h"
#include "core/wrapper/Thread.h"

namespace core {
    namespace wrapper {

        /**
         * Abstract factory for creating wrapped threads based
         * on Boost, pthread.
         *
         * @See Thread, ConcurrencyFactoryWorker
         */
        struct OPENDAVINCI_API ConcurrencyFactory
        {
            /**
             * This method creates a new thread for a given Runnable object
             *
             * @param runnable The Runnable that should be threadified.
             * @return Thread based on the type of instance this factory is.
             */
            static Thread* createThread(Runnable &runnable);

            /**
             * This method causes the calling to sleep for the specified
             * amount of time.
             *
             * @param microseconds Time to sleep in ms.
             */
            static void usleep(const long &microseconds);
        };

    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_CONCURRENCYFACTORY_H_*/
