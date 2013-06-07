/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include <cstdio>

#include "core/wrapper/DisposalService.h"
#include "core/wrapper/Libraries.h"
#include "core/wrapper/SharedMemoryFactory.h"
#include "core/wrapper/MutexFactory.h"
#ifndef WIN32
#include "core/wrapper/POSIX/POSIXSharedMemoryFactory.h"
#endif

namespace core {
    namespace wrapper {

        // Initialization of the singleton instance.
        Mutex* SharedMemoryFactory::m_singletonMutex = MutexFactory::getInstance().createMutex();
        SharedMemoryFactory* SharedMemoryFactory::m_singleton = NULL;

        SharedMemoryFactory::SharedMemoryFactory() {}

        SharedMemoryFactory::~SharedMemoryFactory() {
            SharedMemoryFactory::m_singleton = NULL;
        }

        SharedMemoryFactory& SharedMemoryFactory::getInstance() {
            SharedMemoryFactory::m_singletonMutex->lock();
            {
                if (SharedMemoryFactory::m_singleton == NULL) {
                    switch (USESYSTEMLIBRARY) {
                    case POSIX_LIBRARIES:
#ifndef WIN32
                        SharedMemoryFactory::m_singleton = new POSIX::POSIXSharedMemoryFactory();
#endif
                        break;
                    }

                    // Add to disposal service.
                    DisposalService::getInstance().addDisposableForFinalRemoval(SharedMemoryFactory::m_singleton);
                }
            }
            SharedMemoryFactory::m_singletonMutex->unlock();

            return *m_singleton;
        }

    }
} // core::wrapper
