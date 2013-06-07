/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_WRAPPEDKEYVALUEDATABASEFACTORY_H_
#define HESPERIA_CORE_WRAPPER_WRAPPEDKEYVALUEDATABASEFACTORY_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/SharedPointer.h"
#include "core/wrapper/Disposable.h"
#include "core/wrapper/KeyValueDatabase.h"
#include "core/wrapper/Mutex.h"

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
        class HESPERIA_API KeyValueDatabaseFactory : public Disposable {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                KeyValueDatabaseFactory(const KeyValueDatabaseFactory &);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                KeyValueDatabaseFactory& operator=(const KeyValueDatabaseFactory &);

            protected:
                KeyValueDatabaseFactory();

            public:
                virtual ~KeyValueDatabaseFactory();

                /**
                 * Singleton getter.
                 *
                 * @return Instance of the concrete factory.
                 */
                static KeyValueDatabaseFactory& getInstance();

                /**
                 * This method returns a wrapped key/value database.
                 *
                 * @param fileName Name of the underlying file for the database.
                 *                 If left blank (i.e. ""), this method tries to
                 *                 create either an in-memory-only database or
                 *                 uses a default file.
                 * @return  key/value database based on the type of instance this factory is.
                 */
                virtual SharedPointer<KeyValueDatabase> createKeyValueDatabase(const string &fileName) = 0;

            private:
                static Mutex *m_singletonMutex;
                static KeyValueDatabaseFactory *m_singleton;
        };

    }
} // core::wrapper

#endif /*HESPERIA_CORE_WRAPPER_KEYVALUEDATABASEFACTORY_H_*/
