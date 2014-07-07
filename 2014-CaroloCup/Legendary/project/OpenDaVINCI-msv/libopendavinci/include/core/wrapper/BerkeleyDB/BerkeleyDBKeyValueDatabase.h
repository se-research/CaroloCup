/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_BERKELEYDB_BERKELEYDBKEYVALUEDATABASE_H_
#define OPENDAVINCI_CORE_WRAPPER_BERKELEYDB_BERKELEYDBKEYVALUEDATABASE_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include <db.h>

#include "core/wrapper/Mutex.h"
#include "core/wrapper/KeyValueDatabase.h"

namespace core {
    namespace wrapper {
        namespace BerkeleyDB {

            /**
             * This class is the super class for either the in-memory only
             * database or the file based database using the BerkeleyDB.
             * This class implements generically the storing- and retrieving-methods
             * for both types.
             *
             * @See KeyValueDatabaseFactory
             */
            class BerkeleyDBKeyValueDatabase : public KeyValueDatabase {
                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    BerkeleyDBKeyValueDatabase(const BerkeleyDBKeyValueDatabase &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    BerkeleyDBKeyValueDatabase& operator=(const BerkeleyDBKeyValueDatabase &);

                protected:
                    BerkeleyDBKeyValueDatabase();

                public:
                    virtual ~BerkeleyDBKeyValueDatabase();

                    virtual void put(const int32_t &key, const string &value);

                    virtual const string get(const int32_t &key) const;

                private:
                    Mutex* m_mutex;

                protected:
                    /**
                     * This method returns the database handle. This method
                     * is implemented in subclasses return either file-based
                     * or in-memory-only databases. This method is used in
                     * put(...) and get(...) methods.
                     *
                     * @return Handle to the actual database.
                     */
                    virtual DB* getDatabase() const = 0;
            };

        }
    }
} // core::wrapper::BerkeleyDB

#endif /*OPENDAVINCI_CORE_WRAPPER_BERKELEYDB_BERKELEYDBKEYVALUEDATABASEFILE_H_*/
