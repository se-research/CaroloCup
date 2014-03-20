/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_BERKELEYDB_BERKELEYDBKEYVALUEDATABASEFACTORY_H_
#define HESPERIA_CORE_WRAPPER_BERKELEYDB_BERKELEYDBKEYVALUEDATABASEFACTORY_H_

#include "core/wrapper/KeyValueDatabaseFactory.h"

namespace core {
    namespace wrapper {
        namespace BerkeleyDB {

            /**
             * This class is a concrete derivative for the abstract
             * factory KeyValueDatabaseFactory.
                *
                * @See KeyValueDatabaseFactory
             */
            class BerkeleyDBKeyValueDatabaseFactory : public KeyValueDatabaseFactory {
                protected:
                    friend class KeyValueDatabaseFactory;

                    BerkeleyDBKeyValueDatabaseFactory();

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    BerkeleyDBKeyValueDatabaseFactory(const BerkeleyDBKeyValueDatabaseFactory &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    BerkeleyDBKeyValueDatabaseFactory& operator=(const BerkeleyDBKeyValueDatabaseFactory &);

                public:
                    virtual ~BerkeleyDBKeyValueDatabaseFactory();

                    virtual SharedPointer<KeyValueDatabase> createKeyValueDatabase(const string &fileName);
            };

        }
    }
} // core::wrapper::BerkeleyDB

#endif /*HESPERIA_CORE_WRAPPER_BERKELEYDB_BERKELEYDBKEYVALUEDATABASEFACTORY_H_*/
