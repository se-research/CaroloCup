/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include "core/wrapper/Zip/ZipCompressionFactory.h"
#include "core/wrapper/Zip/ZipDecompressedData.h"

namespace core {
    namespace wrapper {
        namespace Zip {

            ZipCompressionFactory::ZipCompressionFactory() {}

            ZipCompressionFactory::~ZipCompressionFactory() {}

            DecompressedData* ZipCompressionFactory::getContents(istream &in) {
                return new ZipDecompressedData(in);
            }

        }
    }
} // core::wrapper::Zip
