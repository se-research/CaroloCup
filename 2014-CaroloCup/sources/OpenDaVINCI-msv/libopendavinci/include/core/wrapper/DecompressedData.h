/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_DECOMPRESSEDDATA_H_
#define OPENDAVINCI_CORE_WRAPPER_DECOMPRESSEDDATA_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

namespace core {
    namespace wrapper {

        using namespace std;

        /**
         * This interface encapsulates all methods necessary to
         * access the contents of compressed data.
         *
         * @See CompressionFactory
         */
        class DecompressedData {
            public:
                virtual ~DecompressedData();

                /**
                 * This method returns a list of all entries.
                 *
                 * @return List of all entries.
                 */
                virtual vector<string> getListOfEntries() = 0;

                /**
                 * This method returns an input stream for
                 * one specific entry. The look up for the specified
                 * entry is done case insensitively.
                 *
                 * @return Input stream or NULL if the specified file could not be found.
                 */
                virtual istream* getInputStreamFor(const string &entry) = 0;
        };

    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_DECOMPRESSEDDATA_H_*/
