/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include <cstdio>

#include "core/wrapper/BerkeleyDB/BerkeleyDBKeyValueDatabaseFactory.h"
#include "core/wrapper/BerkeleyDB/BerkeleyDBKeyValueDatabaseFile.h"
#include "core/wrapper/BerkeleyDB/BerkeleyDBKeyValueDatabaseInMemory.h"

namespace core {
    namespace wrapper {
        namespace BerkeleyDB {

            BerkeleyDBKeyValueDatabaseFactory::BerkeleyDBKeyValueDatabaseFactory() {}

            BerkeleyDBKeyValueDatabaseFactory::~BerkeleyDBKeyValueDatabaseFactory() {}

            SharedPointer<KeyValueDatabase> BerkeleyDBKeyValueDatabaseFactory::createKeyValueDatabase(const string &fileName) {
                SharedPointer<KeyValueDatabase> keyValueDatabase;
                if (fileName != "") {
                    keyValueDatabase = SharedPointer<KeyValueDatabase>(new BerkeleyDBKeyValueDatabaseFile(fileName));
                } else {
                    keyValueDatabase = SharedPointer<KeyValueDatabase>(new BerkeleyDBKeyValueDatabaseInMemory());
                }
                return keyValueDatabase;
            }

        }
    }
} // core::wrapper::BerkeleyDB
