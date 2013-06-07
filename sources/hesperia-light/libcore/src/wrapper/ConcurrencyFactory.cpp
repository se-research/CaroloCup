/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include <cstdio>

#include "core/wrapper/Libraries.h"
#include "core/wrapper/ConcurrencyFactory.h"
#include "core/wrapper/MutexFactory.h"
#ifndef WIN32
#include "core/wrapper/POSIX/POSIXConcurrencyFactory.h"
#endif

namespace core {
    namespace wrapper {

        // Initialization of the singleton instance.
        Mutex* ConcurrencyFactory::m_singletonMutex = MutexFactory::getInstance().createMutex();
        ConcurrencyFactory* ConcurrencyFactory::m_singleton = NULL;

        ConcurrencyFactory::ConcurrencyFactory() {}

        ConcurrencyFactory::~ConcurrencyFactory() {
            ConcurrencyFactory::m_singleton = NULL;
        }

        ConcurrencyFactory& ConcurrencyFactory::getInstance() {
            ConcurrencyFactory::m_singletonMutex->lock();
            {
                if (ConcurrencyFactory::m_singleton == NULL) {
                    switch (USESYSTEMLIBRARY) {
                    case POSIX_LIBRARIES:
#ifndef WIN32
                        ConcurrencyFactory::m_singleton = new POSIX::POSIXConcurrencyFactory();
#endif
                        break;
                    }
                }
            }
            ConcurrencyFactory::m_singletonMutex->unlock();

            return *m_singleton;
        }

    }
} // core::wrapper
