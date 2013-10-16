/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_KEYVALUEDATABASEFACTORYWORKER
#define OPENDAVINCI_CORE_WRAPPER_KEYVALUEDATABASEFACTORYWORKER

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/SharedPointer.h"
#include "core/wrapper/KeyValueDatabase.h"
#include "core/wrapper/KeyValueDatabaseLibraryProducts.h"

namespace core {
    namespace wrapper {

        /**
         * This template class provides factory methods to the
         * KeyValueDatabaseFactory. The factory methods' implementations
         * for different products have to be defined in specializations
         * of the KeyValueDatabaseFactoryWorker template class.
         *
         * @See KeyValueDatabaseFactory, KeyValueDatabaseFactoryWorker,
         *      KeyValueDatabaseFactoryLibraryProducts,
         *      BerkeleyDBKeyValueDatabaseFactory, SimpleDBKeyValueDatabaseFactory
         */

        template <KeyValueDatabaseLibraryProducts product>
        class OPENDAVINCI_API KeyValueDatabaseFactoryWorker
        {
            public:
                /**
                 * This method returns a wrapped key/value database.
                 *
                 * @param fileName Name of the underlying file for the database.
                 *                 If left blank (i.e. ""), this method tries to
                 *                 create either an in-memory-only database or
                 *                 uses a default file.
                 * @return  key/value database based on the type of instance this factory is.
                 */
                static SharedPointer<KeyValueDatabase> createKeyValueDatabase(const string &fileName);
        };

    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_KEYVALUEDATABASEFACTORYWORKER*/
