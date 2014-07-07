/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_BERKELEYDB_BERKELEYDBKEYVALUEDATABASEINMEMORY_H_
#define OPENDAVINCI_CORE_WRAPPER_BERKELEYDB_BERKELEYDBKEYVALUEDATABASEINMEMORY_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include <db.h>

#include "core/wrapper/BerkeleyDB/BerkeleyDBKeyValueDatabase.h"
#include "core/wrapper/KeyValueDatabaseFactoryWorker.h"
#include "core/wrapper/KeyValueDatabaseLibraryProducts.h"

namespace core {
    namespace wrapper {
        namespace BerkeleyDB {


            /**
             * This class implements a key/value database held only in memory
             * using the BerkeleyDB.
             *
             * @See KeyValueDatabaseFactory
             */
            class BerkeleyDBKeyValueDatabaseInMemory : public BerkeleyDBKeyValueDatabase {
                private:
                   friend class KeyValueDatabaseFactoryWorker<KeyValueDatabaseBerkeleyDB>;;

                    BerkeleyDBKeyValueDatabaseInMemory();

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    BerkeleyDBKeyValueDatabaseInMemory(const BerkeleyDBKeyValueDatabaseInMemory &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    BerkeleyDBKeyValueDatabaseInMemory& operator=(const BerkeleyDBKeyValueDatabaseInMemory &);

                public:
                    virtual ~BerkeleyDBKeyValueDatabaseInMemory();

                private:
                    DB_ENV *m_databaseEnvironment;
                    DB *m_database;

                    virtual DB* getDatabase() const;

                    /**
                     * This method sets up the database environment.
                     */
                    void setupEnvironment();

                    /**
                     * This method sets up the database using the previously
                     * set up environment.
                     */
                    void setupDatabase();
            };

        }
    }
} // core::wrapper::BerkeleyDB

#endif /*OPENDAVINCI_CORE_WRAPPER_BERKELEYDB_BERKELEYDBKEYVALUEDATABASEINMEMORY_H_*/
