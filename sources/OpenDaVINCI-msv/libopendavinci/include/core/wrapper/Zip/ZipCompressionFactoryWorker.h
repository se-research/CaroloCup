/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_ZIP_ZIPCOMPRESSIONFACTORYWORKER_H_
#define OPENDAVINCI_CORE_WRAPPER_ZIP_ZIPCOMPRESSIONFACTORYWORKER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/CompressionLibraryProducts.h"
#include "core/wrapper/CompressionFactoryWorker.h"
#include "core/wrapper/Zip/ZipDecompressedData.h"

namespace core {
    namespace wrapper {

        template <> struct OPENDAVINCI_API CompressionFactoryWorker<CompressionLibraryZIP>
        {
            static DecompressedData* getContents(istream &in)
            {
                return new Zip::ZipDecompressedData(in);
            };
        };

    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_ZIP_ZIPCOMPRESSIONFACTORYWORKER_H_*/
