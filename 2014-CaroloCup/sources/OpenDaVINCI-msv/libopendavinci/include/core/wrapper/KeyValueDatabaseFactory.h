/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_WRAPPEDKEYVALUEDATABASEFACTORY_H_
#define OPENDAVINCI_CORE_WRAPPER_WRAPPEDKEYVALUEDATABASEFACTORY_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"
#include "core/SharedPointer.h"
#include "core/wrapper/KeyValueDatabase.h"

namespace core {
    namespace wrapper {

        /**
         * Abstract factory for creating wrapped key/value databases (i.e.
         * based on BerkeleyDB):
         *
            * It can be used as follows:
            *
            * @code
            * SharedPointer<KeyValueDatabase> kvdb;
            *
            * try {
            *     kvdb = KeyValueDatabaseFactory::getInstance().createKeyValueDatabase("");
            * }
            * catch(string &s) {
            *    clog << "Failed: " << s << endl;
            * }
            *
            * if (kvdb != NULL) {
            *     const string value = "ABCD";
            *     const int32_t key = 123;
            *     kvdb->put(key, value);
            *
            *     string retrievedValue = kvdb->get(key);
            * }
            *
            * @endcode
         */
        struct OPENDAVINCI_API KeyValueDatabaseFactory
        {
            static SharedPointer<KeyValueDatabase> createKeyValueDatabase(const string &fileName);
        };
    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_KEYVALUEDATABASEFACTORY_H_*/
