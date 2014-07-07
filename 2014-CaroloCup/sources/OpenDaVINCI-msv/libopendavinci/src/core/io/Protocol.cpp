/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/io/Protocol.h"

namespace core {
    namespace io {

        using namespace std;

        Protocol::Protocol() {}

        Protocol::~Protocol() {}

        Protocol::PROTOCOL Protocol::getProtocol(string &connection) {
            string connectionString = connection;

            // Transform to lower case for case insensitive searches.
            transform(connectionString.begin(), connectionString.end(), connectionString.begin(), ptr_fun(::tolower));
            core::StringToolbox::trim(connectionString);

            // Trying to parse "file://" at position 0.
            if (connectionString.length() >= 7) {
                string::size_type pos = connectionString.find("file://");

                if (pos == 0) {
                    // Remove "file://" - part.
                    connection = connection.substr(7);
                    return FILEPROTOCOL;
                }
            }

            return UNKNOWNPROTOCOL;
        }

    }
} // core::io
