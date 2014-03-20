/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_BASE_DATASTOREMANAGER_H_
#define HESPERIA_BASE_DATASTOREMANAGER_H_

#include "core/base/AbstractDataStore.h"
#include "core/base/KeyValueDataStore.h"
#include "core/data/Container.h"

namespace hesperia {
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
} // hesperia::base

#endif /*HESPERIA_BASE_DATASTOREMANAGER_H_*/
