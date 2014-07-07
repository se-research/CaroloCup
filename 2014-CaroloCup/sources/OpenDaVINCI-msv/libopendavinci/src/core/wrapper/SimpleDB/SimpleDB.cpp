/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

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
                m_mutex = MutexFactory::createMutex();
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
