/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include <cstdio>

#include "core/wrapper/DisposalService.h"
#include "core/wrapper/Libraries.h"
#include "core/wrapper/ImageFactory.h"
#include "core/wrapper/MutexFactory.h"
#include "core/wrapper/OpenCV/OpenCVImageFactory.h"

namespace core {
    namespace wrapper {

        // Initialization of the singleton instance.
        Mutex* ImageFactory::m_singletonMutex = MutexFactory::getInstance().createMutex();
        ImageFactory* ImageFactory::m_singleton = NULL;

        ImageFactory::ImageFactory() {}

        ImageFactory::~ImageFactory() {
            ImageFactory::m_singleton = NULL;
        }

        ImageFactory& ImageFactory::getInstance() {
            ImageFactory::m_singletonMutex->lock();
            {
                if (ImageFactory::m_singleton == NULL) {
                    switch (USEIMAGINGLIBRARY) {
                    case OPENCV_LIBRARIES:
                        ImageFactory::m_singleton = new OpenCV::OpenCVImageFactory();
                        break;
                    }

                    // Add to disposal service.
                    DisposalService::getInstance().addDisposableForFinalRemoval(ImageFactory::m_singleton);
                }
            }
            ImageFactory::m_singletonMutex->unlock();

            return *m_singleton;
        }

    }
} // core::wrapper
