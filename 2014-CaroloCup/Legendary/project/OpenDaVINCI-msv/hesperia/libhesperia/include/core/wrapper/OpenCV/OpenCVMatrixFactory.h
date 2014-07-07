/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_OPENCV_OPENCVMATRIXFACTORY_H_
#define HESPERIA_CORE_WRAPPER_OPENCV_OPENCVMATRIXFACTORY_H_

#include "core/wrapper/MatrixFactory.h"

namespace core {
    namespace wrapper {
        namespace OpenCV {

            /**
             * This class is a concrete derivative for the abstract
             * factory MatrixFactory.
             *
             * @See MatrixFactory
             */
            class OpenCVMatrixFactory : public MatrixFactory {
                protected:
                    friend class MatrixFactory;

                    OpenCVMatrixFactory();

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    OpenCVMatrixFactory(const OpenCVMatrixFactory &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    OpenCVMatrixFactory& operator=(const OpenCVMatrixFactory &);

                public:
                    virtual ~OpenCVMatrixFactory();

                    virtual Matrix* createMatrix(const uint32_t &dimensionX, const uint32_t &dimensionY);
            };

        }
    }
} // core::wrapper::OpenCV

#endif /*HESPERIA_CORE_WRAPPER_OPENCV_OPENCVMATRIXFACTORY_H_*/
