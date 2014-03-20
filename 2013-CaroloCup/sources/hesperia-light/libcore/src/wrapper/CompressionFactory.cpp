/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include <cstdio>

#include "core/wrapper/DisposalService.h"
#include "core/wrapper/Libraries.h"
#include "core/wrapper/CompressionFactory.h"
#include "core/wrapper/MutexFactory.h"
#include "core/wrapper/Zip/ZipCompressionFactory.h"

namespace core {
    namespace wrapper {

        // Initialization of the singleton instance.
        Mutex* CompressionFactory::m_singletonMutex = MutexFactory::getInstance().createMutex();
        CompressionFactory* CompressionFactory::m_singleton = NULL;

        CompressionFactory::CompressionFactory() {}

        CompressionFactory::~CompressionFactory() {
            CompressionFactory::m_singleton = NULL;
        }

        CompressionFactory& CompressionFactory::getInstance() {
            CompressionFactory::m_singletonMutex->lock();
            {
                if (CompressionFactory::m_singleton == NULL) {
                    switch (USECOMPRESSIONLIBRARY) {
                    case ZIP_LIBRARIES:
                        CompressionFactory::m_singleton = new Zip::ZipCompressionFactory();
                        break;
                    }

                    // Add to disposal service.
                    DisposalService::getInstance().addDisposableForFinalRemoval(CompressionFactory::m_singleton);
                }
            }
            CompressionFactory::m_singletonMutex->unlock();

            return *m_singleton;
        }

    }
} // core::wrapper
