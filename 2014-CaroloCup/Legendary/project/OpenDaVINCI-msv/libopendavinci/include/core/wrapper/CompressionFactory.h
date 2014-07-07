/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_COMPRESSIONFACTORY_H_
#define OPENDAVINCI_CORE_WRAPPER_COMPRESSIONFACTORY_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/DecompressedData.h"

namespace core {
    namespace wrapper {

        using namespace std;

        /**
         * A factory for reading compressed files.
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
         *
         * @See CompressionFactoryWorker
         */

        struct OPENDAVINCI_API CompressionFactory
        {
            static DecompressedData* getContents(istream &in);
        };

    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_COMPRESSIONFACTORY_H_*/
