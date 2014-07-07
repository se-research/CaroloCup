/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_MUTEXFACTORYWORKER_H_
#define OPENDAVINCI_CORE_WRAPPER_MUTEXFACTORYWORKER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/SystemLibraryProducts.h"

namespace core {
    namespace wrapper {

        /**
         * This template class provides factory methods to the
         * MutexFactory. The factory methods' implementations
         * for different products have to be defined in specializations
         * of the MutexFactoryWorker template class.
         *
         * @See MutexFactory, MutexFactoryWorker,
         *      SystemLibraryProducts,
         *      BerkeleyDBKeyValueDatabaseFactory, SimpleDBKeyValueDatabaseFactory
         */

        template <SystemLibraryProducts product>
        class OPENDAVINCI_API MutexFactoryWorker
        {
           public: 
                /**
                 * This method creates the mutex.
                 *
                 * @return mutex based on the type of instance this factory is.
                 */
                static Mutex* createMutex();
        };

    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_MUTEX_H_*/
