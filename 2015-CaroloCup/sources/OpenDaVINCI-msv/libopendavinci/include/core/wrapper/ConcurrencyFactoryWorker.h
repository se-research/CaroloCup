/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_CONCURRENCYFACTORYWORKER_H_
#define OPENDAVINCI_CORE_WRAPPER_CONCURRENCYFACTORYWORKER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/Runnable.h"
#include "core/wrapper/Thread.h"
#include "core/wrapper/SystemLibraryProducts.h"

namespace core {
    namespace wrapper {

        /**
         * This template class provides factory methods to the
         * ConcurrencyFactory. The factory methods' implementations
         * for different products have to be defined in specializations
         * of the ConcurrencyFactoryWorker template class.
         *
         * @See ConcurrencyFactory, ConcurrencyFactoryWorker, SystemLibraryProducts,
         *      BoostConcurrencyFactoryWorker, POSIXConcurrencyFactoryWorker
         */
        template <SystemLibraryProducts product>
        class OPENDAVINCI_API ConcurrencyFactoryWorker
        {
            public:
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

#endif /*OPENDAVINCI_CORE_WRAPPER_CONCURRENCYFACTORYWORKER_H_*/
