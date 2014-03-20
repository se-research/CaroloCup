/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include <cstdio>

#include "core/wrapper/DisposalService.h"
#include "core/wrapper/Libraries.h"
#include "core/wrapper/MutexFactory.h"
#include "core/wrapper/TimeFactory.h"
#ifndef WIN32
#include "core/wrapper/POSIX/POSIXTimeFactory.h"
#endif

namespace core {
    namespace wrapper {

        // Initialization of the singleton instance.
        Mutex* TimeFactory::m_singletonMutex = MutexFactory::getInstance().createMutex();
        TimeFactory* TimeFactory::m_singleton = NULL;

        TimeFactory::TimeFactory() {}

        TimeFactory::~TimeFactory() {
            setSingleton(NULL);
            TimeFactory::m_singleton = NULL;
        }

        void TimeFactory::setSingleton(TimeFactory *singleton) {
            TimeFactory::m_singleton = singleton;
        }

        TimeFactory& TimeFactory::getInstance() {
            TimeFactory::m_singletonMutex->lock();
            {
                if (TimeFactory::m_singleton == NULL) {
                    switch (USESYSTEMLIBRARY) {
                    case POSIX_LIBRARIES:
#ifndef WIN32
                        setSingleton(new POSIX::POSIXTimeFactory());
#endif
                        break;
                    }

                    // Add to disposal service.
                    DisposalService::getInstance().addDisposableForFinalRemoval(TimeFactory::m_singleton);
                }
            }
            TimeFactory::m_singletonMutex->unlock();

            return *m_singleton;
        }

    }
} // core::wrapper
