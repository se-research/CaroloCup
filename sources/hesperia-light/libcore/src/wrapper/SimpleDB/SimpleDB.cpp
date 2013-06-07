/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include <cstdlib>
#include <cstring>
#include <iostream>

#include "core/wrapper/SimpleDB/SimpleDB.h"
#include "core/wrapper/MutexFactory.h"

namespace core {
    namespace wrapper {
        namespace SimpleDB {

            using namespace std;

            SimpleDB::SimpleDB()
            : m_mutex(NULL),
              m_entries()
            {
                m_mutex = MutexFactory::getInstance().createMutex();
                if (m_mutex == NULL) {
                    throw std::string("[SimpleDB] Error creating mutex");
                }
            }

            SimpleDB::~SimpleDB()
            {
                if (m_mutex) {
                    delete m_mutex;
                    m_mutex=NULL;
                }

                m_entries.clear();
            }

            void SimpleDB::put(const int32_t &key, const string &value) {
                m_mutex->lock();
                m_entries[key] = value;
                m_mutex->unlock();
            }

            const string SimpleDB::get(const int32_t &key) const {
                string retVal;
                m_mutex->lock();
                retVal = m_entries[key];
                m_mutex->unlock();

                return retVal;
            }

        }
    }
} // core::wrapper::SimpleDB
