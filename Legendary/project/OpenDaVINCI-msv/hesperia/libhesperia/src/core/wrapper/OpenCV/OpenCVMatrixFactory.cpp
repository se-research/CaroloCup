/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "core/wrapper/OpenCV/OpenCVMatrix.h"
#include "core/wrapper/OpenCV/OpenCVMatrixFactory.h"

namespace core {
    namespace wrapper {
        namespace OpenCV {

            OpenCVMatrixFactory::OpenCVMatrixFactory() {}

            OpenCVMatrixFactory::~OpenCVMatrixFactory() {}

            Matrix* OpenCVMatrixFactory::createMatrix(const uint32_t &dimensionX, const uint32_t &dimensionY) {
                return new OpenCVMatrix(dimensionX, dimensionY);
            }

        }
    }
} // core::wrapper::OpenCV
