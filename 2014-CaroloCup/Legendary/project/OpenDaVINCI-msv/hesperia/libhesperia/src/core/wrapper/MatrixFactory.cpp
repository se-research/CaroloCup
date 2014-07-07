/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include <cstdio>

#include "core/wrapper/DisposalService.h"
#include "core/wrapper/HesperiaLibraries.h"
#include "core/wrapper/MatrixFactory.h"
#include "core/wrapper/MutexFactory.h"
#include "core/wrapper/OpenCV/OpenCVMatrixFactory.h"

namespace core {
    namespace wrapper {

        // Initialization of the singleton instance.
        Mutex* MatrixFactory::m_singletonMutex = MutexFactory::createMutex();
        MatrixFactory* MatrixFactory::m_singleton = NULL;

        MatrixFactory::MatrixFactory() {}

        MatrixFactory::~MatrixFactory() {
            MatrixFactory::m_singleton = NULL;
        }

        MatrixFactory& MatrixFactory::getInstance() {
            MatrixFactory::m_singletonMutex->lock();
            {
                if (MatrixFactory::m_singleton == NULL) {
                    switch (USEMATRIXLIBRARY) {
                    case MATRIX_OPENCV_LIBRARIES:
                        MatrixFactory::m_singleton = new OpenCV::OpenCVMatrixFactory();
                        break;
                    }

                    // Add to disposal service.
                    DisposalService::getInstance().addDisposableForFinalRemoval((Disposable**)&MatrixFactory::m_singleton);
                }
            }
            MatrixFactory::m_singletonMutex->unlock();

            return *m_singleton;
        }

    }
} // core::wrapper
