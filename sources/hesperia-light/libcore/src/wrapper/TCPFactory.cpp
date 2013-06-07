/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include <string>

#include "core/wrapper/DisposalService.h"
#include "core/wrapper/Libraries.h"
#include "core/wrapper/MutexFactory.h"
#include "core/wrapper/TCPFactory.h"
#ifndef WIN32
#include "core/wrapper/POSIX/POSIXTCPFactory.h"
#endif

namespace core {
    namespace wrapper {

        using namespace std;

        // Initialization of the singleton instance.
        Mutex* TCPFactory::m_singletonMutex = MutexFactory::getInstance().createMutex();
        TCPFactory* TCPFactory::m_singleton = NULL;

        TCPFactory::TCPFactory() {}

        TCPFactory::~TCPFactory() {
            if (TCPFactory::m_singleton != NULL ) {
                TCPFactory::m_singleton = NULL;
            }
        }

        TCPFactory& TCPFactory::getInstance() {
            TCPFactory::m_singletonMutex->lock();
            {
                if (TCPFactory::m_singleton == NULL) {
                    switch (USESYSTEMLIBRARY) {
                    case POSIX_LIBRARIES:
#ifndef WIN32
                        TCPFactory::m_singleton = new POSIX::POSIXTCPFactory();
#endif
                        break;
                    }

                    // Add to disposal service.
                    DisposalService::getInstance().addDisposableForFinalRemoval(TCPFactory::m_singleton);
                }
            }
            TCPFactory::m_singletonMutex->unlock();

            return *m_singleton;
        }

        TCPFactory* TCPFactory::createBoostFactory() {
            return NULL;
        }

        TCPFactory* TCPFactory::createPOSIXFactory() {
            TCPFactory *factory = NULL;
#ifndef WIN32
            factory = new POSIX::POSIXTCPFactory();
#endif
            return factory;
        }

    }
} // core::wrapper
