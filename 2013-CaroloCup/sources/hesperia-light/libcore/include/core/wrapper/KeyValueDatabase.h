/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_KEYVALUEDATABASE_H_
#define HESPERIA_CORE_WRAPPER_KEYVALUEDATABASE_H_

#include <stdint.h>
#include <string>

namespace core {
    namespace wrapper {

        using namespace std;

        /**
         * This interface encapsulates all methods necessary to
         * implement a key/value-database.
         *
         * @See KeyValueDatabaseFactory.
         */
        class KeyValueDatabase {
            public:
                virtual ~KeyValueDatabase();

                /**
                 * This method puts a key/value pair into the database. An
                 * existing entry is substituted.
                 *
                 * @param key The key.
                 * @param value The value.
                 */
                virtual void put(const int32_t &key, const string &value) = 0;

                /**
                 * This method returns the value for a key. The value for
                 * a non-existing key is "".
                 *
                 * @param key The key for which the value has to be returned.
                 * @return The value.
                 */
                virtual const string get(const int32_t &key) const = 0;
        };

    }
} // core::wrapper

#endif /*HESPERIA_CORE_WRAPPER_KEYVALUEDATABASE_H_*/
