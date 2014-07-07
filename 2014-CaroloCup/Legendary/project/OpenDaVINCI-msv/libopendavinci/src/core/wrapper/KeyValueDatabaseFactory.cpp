/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */
#include "core/SharedPointer.h"
#include "core/wrapper/KeyValueDatabaseFactory.h"
#include "core/wrapper/KeyValueDatabaseFactoryWorker.h"

#include "core/wrapper/Libraries.h"
#include "core/wrapper/ConfigurationTraits.h"
#include "core/wrapper/KeyValueDatabaseLibraryProducts.h"

#include "core/wrapper/BerkeleyDB/BerkeleyDBKeyValueDatabaseFactoryWorker.h"
#include "core/wrapper/SimpleDB/SimpleDBFactoryWorker.h"

namespace core {
    namespace wrapper {

        SharedPointer<KeyValueDatabase> KeyValueDatabaseFactory::createKeyValueDatabase(const string &fileName)
        {
            typedef ConfigurationTraits<KeyValueDatabaseLibraryProducts>::configuration configuration;
            return KeyValueDatabaseFactoryWorker<configuration::value>::createKeyValueDatabase(fileName);
        }
    }
} // core::wrapper
