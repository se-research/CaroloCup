/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_KEYVALUEDATABASE_H_
#define OPENDAVINCI_CORE_WRAPPER_KEYVALUEDATABASE_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

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

#endif /*OPENDAVINCI_CORE_WRAPPER_KEYVALUEDATABASE_H_*/
