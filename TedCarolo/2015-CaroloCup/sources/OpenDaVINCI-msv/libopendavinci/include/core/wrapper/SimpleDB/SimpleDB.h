/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_SIMPLEDB_SIMPLEDB_H_
#define OPENDAVINCI_CORE_WRAPPER_SIMPLEDB_SIMPLEDB_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/SharedPointer.h"
#include "core/wrapper/KeyValueDatabase.h"
#include "core/wrapper/Mutex.h"

#include "core/wrapper/KeyValueDatabaseFactoryWorker.h"
#include "core/wrapper/KeyValueDatabaseLibraryProducts.h"

namespace core {
    namespace wrapper {
        namespace SimpleDB {

            class SimpleDB : public KeyValueDatabase {
                friend class KeyValueDatabaseFactoryWorker<KeyValueDatabaseSimpleDB>;
                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    SimpleDB(const SimpleDB &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    SimpleDB& operator=(const SimpleDB &);

                protected:
                    SimpleDB();

                public:
                    virtual ~SimpleDB();

                    virtual void put(const int32_t &key, const string &value);

                    virtual const string get(const int32_t &key) const;

                protected:
                    Mutex* m_mutex;
                    mutable map<int, string> m_entries;
            };

        }
    }
} // core::wrapper::SimpleDB

#endif /*OPENDAVINCI_CORE_WRAPPER_SIMPLEDB_SIMPLEDB_H_*/
