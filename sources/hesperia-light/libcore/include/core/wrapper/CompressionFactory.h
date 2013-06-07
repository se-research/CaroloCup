/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_COMPRESSIONFACTORY_H_
#define HESPERIA_CORE_WRAPPER_COMPRESSIONFACTORY_H_

#include <iostream>
#include <string>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/wrapper/DecompressedData.h"
#include "core/wrapper/Disposable.h"
#include "core/wrapper/Mutex.h"

namespace core {
    namespace wrapper {

        using namespace std;

        /**
         * Abstract factory for reading compressed files.
         *
         * It can be used as follows:
         *
         * @code
         * DecompressedData *dd = NULL;
         * fstream fin("zip-file", ios::binary|ios::in);
         * dd = CompressionFactory::getInstance().getContents(fin);
         * fin.close();
         *
         * if (cf != NULL) {
         *     istream &s = cf->getEntryByName("file");
         * }

         * ...
         * if (cf != NULL) {
         *     delete cf;
         * }
         * @endcode
         */
        class HESPERIA_API CompressionFactory : public Disposable {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                CompressionFactory(const CompressionFactory &);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                CompressionFactory& operator=(const CompressionFactory &);

            protected:
                CompressionFactory();

            public:
                virtual ~CompressionFactory();

                /**
                 * Singleton getter.
                 *
                 * @return Instance of the concrete factory.
                 */
                static CompressionFactory& getInstance();

                /**
                 * This method returns the condition.
                 *
                 * @param in The stream from which the compressed data should be read.
                 * @return Compressed file based on the type of instance this factory is.
                 */
                virtual DecompressedData* getContents(istream &in) = 0;

            private:
                static Mutex *m_singletonMutex;
                static CompressionFactory *m_singleton;
        };

    }
} // core::wrapper

#endif /*HESPERIA_CORE_WRAPPER_COMPRESSIONFACTORY_H_*/
