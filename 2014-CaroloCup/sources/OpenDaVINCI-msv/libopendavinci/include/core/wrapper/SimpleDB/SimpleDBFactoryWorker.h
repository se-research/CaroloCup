/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_SIMPLEDB_SIMPLEDBFACTORY_H_
#define OPENDAVINCI_CORE_WRAPPER_SIMPLEDB_SIMPLEDBFACTORY_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/KeyValueDatabaseLibraryProducts.h"
#include "core/wrapper/KeyValueDatabaseFactoryWorker.h"

#include "core/wrapper/SimpleDB/SimpleDB.h"

namespace core {
    namespace wrapper {

        template <> struct OPENDAVINCI_API KeyValueDatabaseFactoryWorker<KeyValueDatabaseSimpleDB>
        {
            static SharedPointer<KeyValueDatabase> createKeyValueDatabase(const string& /*fileName*/)
            {
                return SharedPointer<KeyValueDatabase>(new SimpleDB::SimpleDB());
            }
        };

    }
} // core::wrapper::SimpleDB

#endif /*OPENDAVINCI_CORE_WRAPPER_SIMPLEDB_SIMPLEDBFACTORY_H_*/
