/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include <cstdio>

#include "core/wrapper/DisposalService.h"
#include "core/wrapper/Libraries.h"
#include "core/wrapper/MutexFactory.h"
#include "core/wrapper/KeyValueDatabaseFactory.h"
#include "core/wrapper/BerkeleyDB/BerkeleyDBKeyValueDatabaseFactory.h"
#include "core/wrapper/SimpleDB/SimpleDBFactory.h"

namespace core {
    namespace wrapper {

        using namespace std;

        // Initialization of the singleton instance.
        Mutex* KeyValueDatabaseFactory::m_singletonMutex = MutexFactory::getInstance().createMutex();
        KeyValueDatabaseFactory* KeyValueDatabaseFactory::m_singleton = NULL;

        KeyValueDatabaseFactory::KeyValueDatabaseFactory() {}

        KeyValueDatabaseFactory::~KeyValueDatabaseFactory() {
            KeyValueDatabaseFactory::m_singleton = NULL;
        }

        KeyValueDatabaseFactory& KeyValueDatabaseFactory::getInstance() {
            KeyValueDatabaseFactory::m_singletonMutex->lock();
            {
                if (KeyValueDatabaseFactory::m_singleton == NULL) {
                    switch (USEKEYVALUEDATABASELIBRARY) {
                    case BERKELEYDB_LIBRARIES:
                        KeyValueDatabaseFactory::m_singleton = new BerkeleyDB::BerkeleyDBKeyValueDatabaseFactory();
                        break;

                    case SIMPLEDB_LIBRARIES:
                        KeyValueDatabaseFactory::m_singleton = new SimpleDB::SimpleDBFactory();
                        break;
                    }

                    // Add to disposal service.
                    DisposalService::getInstance().addDisposableForFinalRemoval(KeyValueDatabaseFactory::m_singleton);
                }
            }
            KeyValueDatabaseFactory::m_singletonMutex->unlock();

            return *m_singleton;
        }

    }
} // core::wrapper
