/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/wrapper/CompressionFactory.h"
#include "core/wrapper/CompressionFactoryWorker.h"
#include "core/wrapper/Libraries.h"
#include "core/wrapper/CompressionLibraryProducts.h"
#include "core/wrapper/ConfigurationTraits.h"

#include "core/wrapper/Zip/ZipCompressionFactoryWorker.h"

namespace core {
    namespace wrapper {

        DecompressedData* CompressionFactory::getContents(istream &in)
        {
            typedef ConfigurationTraits<CompressionLibraryProducts>::configuration configuration;

            return CompressionFactoryWorker<configuration::value>::getContents(in);
        }
    }
} // core::wrapper
