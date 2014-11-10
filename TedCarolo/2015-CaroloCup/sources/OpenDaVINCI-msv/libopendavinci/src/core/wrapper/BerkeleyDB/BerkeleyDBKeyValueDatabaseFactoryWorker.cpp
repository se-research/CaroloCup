/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/wrapper/KeyValueDatabaseFactoryWorker.h"
#include "core/wrapper/KeyValueDatabaseLibraryProducts.h"

#include "core/wrapper/BerkeleyDB/BerkeleyDBKeyValueDatabaseFactoryWorker.h"
#include "core/wrapper/BerkeleyDB/BerkeleyDBKeyValueDatabaseFile.h"
#include "core/wrapper/BerkeleyDB/BerkeleyDBKeyValueDatabaseInMemory.h"

namespace core {
    namespace wrapper {

        SharedPointer<KeyValueDatabase> KeyValueDatabaseFactoryWorker<KeyValueDatabaseBerkeleyDB>::createKeyValueDatabase(const string &fileName)
        {
            SharedPointer<KeyValueDatabase> keyValueDatabase;
            if (fileName != "") {
                keyValueDatabase = SharedPointer<KeyValueDatabase>(new BerkeleyDB::BerkeleyDBKeyValueDatabaseFile(fileName));
            } else {
                keyValueDatabase = SharedPointer<KeyValueDatabase>(new BerkeleyDB::BerkeleyDBKeyValueDatabaseInMemory());
            }
            return keyValueDatabase;
        }

    }
} // core::wrapper::BerkeleyDB
