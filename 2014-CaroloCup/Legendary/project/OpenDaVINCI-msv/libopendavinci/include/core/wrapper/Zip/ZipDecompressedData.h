/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_ZIP_ZIPDECOMPRESSEDDATA_H_
#define OPENDAVINCI_CORE_WRAPPER_ZIP_ZIPDECOMPRESSEDDATA_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/DecompressedData.h"
#include "core/wrapper/StringComparator.h"

#include "core/wrapper/CompressionFactoryWorker.h"
#include "core/wrapper/CompressionLibraryProducts.h"

namespace core {
    namespace wrapper {
        namespace Zip {

            using namespace std;

            /**
             * This class implements an abstract object containing
             * the decompressed contents of a compressed archive.
             *
             * @See DecompressedData.
             */
            class ZipDecompressedData : public DecompressedData {
                private:
                    enum {
                        BUFFER_SIZE = 1024
                    };

                private:
                    friend struct CompressionFactoryWorker<CompressionLibraryZIP>;

                    /**
                     * Constructor.
                     *
                     * @param in Stream to be used for reading the contents.
                     */
                    ZipDecompressedData(istream &in);

                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    ZipDecompressedData(const ZipDecompressedData &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    ZipDecompressedData& operator=(const ZipDecompressedData &);

                public:
                    virtual ~ZipDecompressedData();

                    virtual vector<string> getListOfEntries();

                    virtual istream* getInputStreamFor(const string &entry);

                private:
                    map<string, stringstream*, StringComparator> m_mapOfDecompressedEntries;

                    /**
                     * This method tries to decompress the given archive and
                     * read the complete content into memory.
                     *
                     * @param in Stream to be used for reading the contents.
                     */
                    void decompressData(istream &in);
            };

        }
    }
} // core::wrapper::Zip

#endif /*OPENDAVINCI_CORE_WRAPPER_ZIP_ZIPDECOMPRESSEDDATA_H_*/
