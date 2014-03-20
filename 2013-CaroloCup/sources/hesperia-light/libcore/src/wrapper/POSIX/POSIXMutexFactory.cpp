/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "core/wrapper/POSIX/POSIXMutex.h"
#include "core/wrapper/POSIX/POSIXMutexFactory.h"

namespace core {
    namespace wrapper {
        namespace POSIX {

            POSIXMutexFactory::POSIXMutexFactory() {}

            POSIXMutexFactory::~POSIXMutexFactory() {}

            Mutex* POSIXMutexFactory::createMutex() {
                return new POSIXMutex();
            }

        }
    }
} // core::wrapper::POSIX
