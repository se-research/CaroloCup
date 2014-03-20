/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include <cstdio>

#include "core/wrapper/SimpleDB/SimpleDBFactory.h"
#include "core/wrapper/SimpleDB/SimpleDB.h"

namespace core {
    namespace wrapper {
        namespace SimpleDB {

        SimpleDBFactory::SimpleDBFactory() {}

        SimpleDBFactory::~SimpleDBFactory() {}

            SharedPointer<KeyValueDatabase> SimpleDBFactory::createKeyValueDatabase(const string &/*fileName*/) {
                SharedPointer<KeyValueDatabase> keyValueDatabase;
                keyValueDatabase = SharedPointer<KeyValueDatabase>(new SimpleDB());

                return keyValueDatabase;
            }

        }
    }
} // core::wrapper::SimpleDB
