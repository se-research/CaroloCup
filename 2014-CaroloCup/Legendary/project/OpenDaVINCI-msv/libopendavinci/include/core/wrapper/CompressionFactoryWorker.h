/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_COMPRESSIONFACTORYWORKER_H_
#define OPENDAVINCI_CORE_WRAPPER_COMPRESSIONFACTORYWORKER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/DecompressedData.h"
#include "core/wrapper/CompressionLibraryProducts.h"

namespace core {
    namespace wrapper {

        /**
         * This template class provides factory methods to the
         * ConcurrencyFactory. The factory methods' implementations
         * for different products have to be defined in specializations
         * of the ConcurrencyFactoryWorker template class.
         *
         * @See CompressionFactory, CompressionLibraryProducts,
         *      ZipCompressionFactoryWorker
         */
        template <CompressionLibraryProducts product>
        struct OPENDAVINCI_API CompressionFactoryWorker
        {
            /**
             * This method creates a DecompressedData object based on a given
             * input stream.
             *
             * @param in The stream from which the compressed data should be read.
             * @return Compressed file based on the type of instance this factory is.
             */
            static DecompressedData* getContents(istream &in);
        };

    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_COMPRESSIONFACTORYWORKER_H_*/
