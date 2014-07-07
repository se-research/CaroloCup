/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_BASE_DATASTOREMANAGER_H_
#define OPENDAVINCI_BASE_DATASTOREMANAGER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/base/AbstractDataStore.h"
#include "core/base/KeyValueDataStore.h"
#include "core/data/Container.h"

namespace core {
    namespace base {

        using namespace std;

        /**
         * This interface manages access to the data store management.
         */
        class DataStoreManager {
            public:
                virtual ~DataStoreManager();

                /**
                 * This method adds a data store for a all data types.
                 *
                 * @param dataStore Data store to be added.
                 */
                virtual void addDataStoreFor(core::base::AbstractDataStore &dataStore) = 0;

                /**
                 * This method adds a data store for a given data type.
                 *
                 * @param datatype Datatype for which a datastore should be added.
                 * @param dataStore Data store to be added.
                 */
                virtual void addDataStoreFor(const core::data::Container::DATATYPE &datatype, core::base::AbstractDataStore &dataStore) = 0;

                /**
                 * This method returns a key/value-datastore for all
                 * captured data.
                 *
                 * @return Key/Value-data store containing all received data.
                 */
                virtual core::base::KeyValueDataStore& getKeyValueDataStore() = 0;
        };

    }
} // core::base

#endif /*OPENDAVINCI_BASE_DATASTOREMANAGER_H_*/
