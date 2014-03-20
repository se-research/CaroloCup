/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_DECOMPRESSEDDATA_H_
#define HESPERIA_CORE_WRAPPER_DECOMPRESSEDDATA_H_

#include <iostream>
#include <string>
#include <vector>

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

#endif /*HESPERIA_CORE_WRAPPER_DECOMPRESSEDDATA_H_*/
