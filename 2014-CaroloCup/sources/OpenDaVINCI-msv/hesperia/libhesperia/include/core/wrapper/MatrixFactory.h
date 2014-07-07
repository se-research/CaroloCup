/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_MATRIXFACTORY_H_
#define HESPERIA_CORE_WRAPPER_MATRIXFACTORY_H_

#include <iostream>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/wrapper/Disposable.h"
#include "core/wrapper/Matrix.h"
#include "core/wrapper/Mutex.h"

namespace core {
    namespace wrapper {

        using namespace std;

        /**
         * Abstract factory for handling matrices using a
         * native library (i.e. OpenCV).
         *
         * It can be used as follows:
         *
         * @code
         * Matrix *m = MatrixFactory::getInstance().getMatrix(3, 4);
         * m[2][1] = 7.0;
         *
         * ...
         *
         * if (m != NULL) {
         *     delete m;
         * }
         *
         * @endcode
         */
        class OPENDAVINCI_API MatrixFactory : public Disposable {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                MatrixFactory(const MatrixFactory &);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                MatrixFactory& operator=(const MatrixFactory &);

            protected:
                MatrixFactory();

            public:
                virtual ~MatrixFactory();

                /**
                 * Singleton getter.
                 *
                 * @return Instance of the concrete factory.
                 */
                static MatrixFactory& getInstance();

                /**
                 * This method creates a new matrix.
                 *
                 * @param in The stream to be used to create the image.
                 * @return time based on the type of instance this factory is.
                 */
                virtual Matrix* createMatrix(const uint32_t &dimensionX, const uint32_t &dimensionY) = 0;

            private:
                static Mutex *m_singletonMutex;
                static MatrixFactory *m_singleton;
        };

    }
} // core::wrapper

#endif /*HESPERIA_CORE_WRAPPER_MATRIXFACTORY_H_*/
