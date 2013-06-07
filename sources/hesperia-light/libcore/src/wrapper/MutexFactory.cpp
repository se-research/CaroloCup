/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include <cstdio>

#include "core/wrapper/Libraries.h"
#include "core/wrapper/MutexFactory.h"
#ifndef WIN32
#include "core/wrapper/POSIX/POSIXMutexFactory.h"
#endif

namespace core {
    namespace wrapper {

        // Initialization of the singleton instance.
        Mutex* MutexFactory::m_singletonMutex = MutexFactory::getInstance().createMutex();
        MutexFactory* MutexFactory::m_singleton = NULL;

        MutexFactory::MutexFactory() {
            m_singleton = this;
        }

        MutexFactory::~MutexFactory() {
            m_singleton = NULL;
        }

        MutexFactory& MutexFactory::getInstance() {
            // This check is necessary to prevent an uninitialized mutex for the singleton.
            // MutexFactory::m_singletonMutex for the first call to getInstance() is NULL.
            if (MutexFactory::m_singletonMutex != NULL) {
                MutexFactory::m_singletonMutex->lock();
            }

            if (MutexFactory::m_singleton == NULL) {
                switch (USESYSTEMLIBRARY) {
                case POSIX_LIBRARIES:
#ifndef WIN32
                    MutexFactory::m_singleton = new POSIX::POSIXMutexFactory();
#endif
                    break;
                }
            }

            // This check is necessary to prevent an uninitialized mutex for the singleton.
            // MutexFactory::m_singletonMutex for the first call to getInstance() is NULL.
            if (MutexFactory::m_singletonMutex != NULL) {
                MutexFactory::m_singletonMutex->unlock();
            }

            return *m_singleton;
        }

    }
} // core::wrapper
