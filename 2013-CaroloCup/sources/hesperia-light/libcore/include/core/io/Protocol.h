/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_IO_PROTOCOL_H_
#define HESPERIA_CORE_IO_PROTOCOL_H_

#include <string>

namespace core {
    namespace io {

        using namespace std;

        /**
         * This class provides a protocol parser.
         *
         * @code
         * void foo() {
         *     Protocol::PROTOCOL p = Protocol::getProtocol("file://myFile");
         *     ...
         * }
         * @endcode
         */
        class Protocol {
            public:
                enum PROTOCOL {
                    UNKNOWNPROTOCOL,
                    FILEPROTOCOL
                };

            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                Protocol(const Protocol &);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                Protocol& operator=(const Protocol &);

            private:
                Protocol();

            public:
                virtual ~Protocol();

                /**
                 * This method tries to extract the connection type by parsing
                 * the protocol type.
                 *
                 * @param connection The connection string to be parsed. The type of protocol is removed from the string.
                 * @return Connection type.
                 */
                static PROTOCOL getProtocol(string &connection);
        };

    }
} // core::io

#endif /*HESPERIA_CORE_IO_PROTOCOL_H_*/
