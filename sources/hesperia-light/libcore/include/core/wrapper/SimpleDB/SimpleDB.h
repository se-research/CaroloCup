/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_SIMPLEDB_SIMPLEDB_H_
#define HESPERIA_CORE_WRAPPER_SIMPLEDB_SIMPLEDB_H_

#include <map>

#include "core/SharedPointer.h"
#include "core/wrapper/KeyValueDatabase.h"
#include "core/wrapper/Mutex.h"

namespace core {
    namespace wrapper {
        namespace SimpleDB {

            class SimpleDB : public KeyValueDatabase {
                friend class SimpleDBFactory;
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

#endif /*HESPERIA_CORE_WRAPPER_SIMPLEDB_SIMPLEDB_H_*/
