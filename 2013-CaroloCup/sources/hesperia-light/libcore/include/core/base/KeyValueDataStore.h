/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_BASE_KEYVALUEDATASTORE_H_
#define HESPERIA_CORE_BASE_KEYVALUEDATASTORE_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/SharedPointer.h"
#include "core/data/Container.h"
#include "core/exceptions/Exceptions.h"
#include "core/wrapper/KeyValueDatabase.h"

namespace core {
    namespace base {

        using namespace std;

        /**
         * This class is a key/value-based implementation for a datastore.
         * It can be used as follows:
         *
         * @code
         * KeyValueDataStore &kv = new KeyValueDataStore(wrapper::KeyValueDatabaseFactory::getInstance().createKeyValueDatabase(""));
         * int32_t key = 1;
         * TimeStamp ts;
         * Container c(TIMESTAMP, ts);
         * kv.put(key, c);
         * @endcode
         */
        class HESPERIA_API KeyValueDataStore {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 *
                 * @param obj Reference to an object of this class.
                 */
                KeyValueDataStore(const KeyValueDataStore&);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 *
                 * @param obj Reference to an object of this class.
                 * @return Reference to this instance.
                 */
                KeyValueDataStore& operator=(const KeyValueDataStore&);

            public:
                /**
                 * Constructor.
                 *
                 * @param keyValueDatabase Associated key/value database.
                 * @throws NoDatabaseAvailableException if keyValueDatabase is NULL.
                 */
                KeyValueDataStore(SharedPointer<wrapper::KeyValueDatabase> keyValueDatabase) throw (core::exceptions::NoDatabaseAvailableException);

                virtual ~KeyValueDataStore();

                /**
                 * This method puts a key/value pair into the data store.
                 *
                 * @param key The key.
                 * @param value The value.
                 */
                virtual void put(const int32_t &key, const data::Container &value);

                /**
                 * This method returns the value for a key.
                 *
                 * @param key The key for which the value has to be returned.
                 * @return The value.
                 */
                data::Container get(const int32_t &key) const;

            private:
                SharedPointer<wrapper::KeyValueDatabase> m_keyValueDatabase;
        };

    }
} // core::base

#endif /*HESPERIA_CORE_BASE_KEYVALUEDATASTORE_H_*/
