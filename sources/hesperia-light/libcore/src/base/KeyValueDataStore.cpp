/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "core/base/KeyValueDataStore.h"
#include <cstring>

namespace core {
    namespace base {

        using namespace data;
        using namespace exceptions;

        KeyValueDataStore::KeyValueDataStore(SharedPointer<wrapper::KeyValueDatabase> keyValueDatabase) throw (NoDatabaseAvailableException) :
                m_keyValueDatabase(keyValueDatabase) {
            if (!m_keyValueDatabase.isValid()) {
                HESPERIA_CORE_THROW_EXCEPTION(NoDatabaseAvailableException, "Given database is NULL.");
            }
        }

        KeyValueDataStore::~KeyValueDataStore() {}

        void KeyValueDataStore::put(const int32_t &key, const Container &value) {
            // Transform the given Container to a plain string...
            stringstream stringStreamValue;
            stringStreamValue << value;
            string stringValue = stringStreamValue.str();

            // ...and use the datastore backend for storing the content.
            m_keyValueDatabase->put(key, stringValue);
        }

        Container KeyValueDataStore::get(const int32_t &key) const {
            Container value;

            // Try to get the value from the database backend and try to parse a Container.
            string stringValue(m_keyValueDatabase->get(key));
            if (stringValue != "") {
                stringstream stringStreamValue;
                stringStreamValue.str(stringValue);
                stringStreamValue >> value;
            }

            return value;
        }

    }
} // core::base
