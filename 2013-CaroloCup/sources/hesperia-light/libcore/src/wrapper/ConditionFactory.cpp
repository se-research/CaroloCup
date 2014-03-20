/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include <cstdio>

#include "core/wrapper/Libraries.h"
#include "core/wrapper/ConditionFactory.h"
#include "core/wrapper/MutexFactory.h"
#ifndef WIN32
#include "core/wrapper/POSIX/POSIXConditionFactory.h"
#endif

namespace core {
    namespace wrapper {

        // Initialization of the singleton instance.
        Mutex* ConditionFactory::m_singletonMutex = MutexFactory::getInstance().createMutex();
        ConditionFactory* ConditionFactory::m_singleton = NULL;

        ConditionFactory::ConditionFactory() {}

        ConditionFactory::~ConditionFactory() {
            ConditionFactory::m_singleton = NULL;
        }

        ConditionFactory& ConditionFactory::getInstance() {
            ConditionFactory::m_singletonMutex->lock();
            {
                if (ConditionFactory::m_singleton == NULL) {
                    switch (USESYSTEMLIBRARY) {
                    case POSIX_LIBRARIES:
#ifndef WIN32
                        ConditionFactory::m_singleton = new POSIX::POSIXConditionFactory();
#endif
                        break;
                    }
                }
            }
            ConditionFactory::m_singletonMutex->unlock();

            return *m_singleton;
        }

    }
} // core::wrapper
