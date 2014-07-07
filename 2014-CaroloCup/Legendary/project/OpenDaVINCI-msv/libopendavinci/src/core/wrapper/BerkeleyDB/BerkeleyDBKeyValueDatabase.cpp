/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/wrapper/BerkeleyDB/BerkeleyDBKeyValueDatabase.h"
#include "core/wrapper/MutexFactory.h"

namespace core {
    namespace wrapper {
        namespace BerkeleyDB {

            using namespace std;

            BerkeleyDBKeyValueDatabase::BerkeleyDBKeyValueDatabase() : m_mutex(NULL) {
                m_mutex = MutexFactory::createMutex();
                if (m_mutex == NULL) {
                    throw std::string("[BerkeleyDB] Error creating mutex");
                }
            }

            BerkeleyDBKeyValueDatabase::~BerkeleyDBKeyValueDatabase() {
                if (m_mutex) {
                    delete m_mutex;
                    m_mutex=NULL;
                }
            }

            void BerkeleyDBKeyValueDatabase::put(const int32_t &key, const string &value) {
                m_mutex->lock();
                    ::DBT databaseKey;
                    ::DBT databaseValue;

                    memset(&databaseKey, 0, sizeof(DBT));
                    memset(&databaseValue, 0, sizeof(DBT));

                    databaseKey.data = (void*) & key;
                    databaseKey.size = sizeof(int);

                    databaseValue.data = (char*)(value.c_str());
                    databaseValue.size = (uint32_t)value.length();

                    // Store key/value.
                    int32_t retVal = getDatabase()->put(getDatabase(), NULL, &databaseKey, &databaseValue, 0);
                    if (retVal != 0) {
                        clog << "Error putting key/value into the database: " << ::db_strerror(retVal) << endl;
                    }
                m_mutex->unlock();
            }

            const string BerkeleyDBKeyValueDatabase::get(const int32_t &key) const {
                m_mutex->lock();
                    ::DBT databaseKey;
                    ::DBT databaseValue;

                    memset(&databaseKey, 0, sizeof(DBT));
                    memset(&databaseValue, 0, sizeof(DBT));

                    databaseKey.data = (void*) & key;
                    databaseKey.size = sizeof(int);

                    databaseValue.flags = DB_DBT_MALLOC;

                    // Retrieve key/value.
                    int32_t retVal = getDatabase()->get(getDatabase(), NULL, &databaseKey, &databaseValue, 0);
                    if ( (retVal != 0) && (retVal != DB_NOTFOUND) ) {
                        clog << "Error getting key/value from the database: " << ::db_strerror(retVal) << endl;
                        return "";
                    }

                    // Try to create a string from the given data and size.
                    string value((char*)databaseValue.data, databaseValue.size);
                    free(databaseValue.data);
                m_mutex->unlock();
                return value;
            }

        }
    }
} // core::wrapper::BerkeleyDB
