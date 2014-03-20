/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_SIMPLEDB_SIMPLEDBFACTORY_H_
#define HESPERIA_CORE_WRAPPER_SIMPLEDB_SIMPLEDBFACTORY_H_

#include "core/wrapper/KeyValueDatabaseFactory.h"

namespace core {
    namespace wrapper {
        namespace SimpleDB {

            class SimpleDBFactory : public KeyValueDatabaseFactory {
                protected:
                    friend class KeyValueDatabaseFactory;

                    SimpleDBFactory();

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    SimpleDBFactory(const SimpleDBFactory &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    SimpleDBFactory& operator=(const SimpleDBFactory &);

                public:
                    virtual ~SimpleDBFactory();

                    virtual SharedPointer<KeyValueDatabase> createKeyValueDatabase(const string &fileName);
            };

        }
    }
} // core::wrapper::SimpleDB

#endif /*HESPERIA_CORE_WRAPPER_SIMPLEDB_SIMPLEDBFACTORY_H_*/
