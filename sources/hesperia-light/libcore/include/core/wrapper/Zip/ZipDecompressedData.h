/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_ZIP_ZIPDECOMPRESSEDDATA_H_
#define HESPERIA_CORE_WRAPPER_ZIP_ZIPDECOMPRESSEDDATA_H_

#include <map>
#include <sstream>
#include <string>

#include "core/wrapper/DecompressedData.h"
#include "core/wrapper/StringComparator.h"

namespace core {
    namespace wrapper {
        namespace Zip {

            using namespace std;

            // Forward declaration to prevent circular dependencies.
            class ZipCompressionFactory;

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
                    friend class ZipCompressionFactory;

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

#endif /*HESPERIA_CORE_WRAPPER_ZIP_ZIPDECOMPRESSEDDATA_H_*/
