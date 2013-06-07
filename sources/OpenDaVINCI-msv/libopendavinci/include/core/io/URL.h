/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_IO_URL_H_
#define OPENDAVINCI_CORE_IO_URL_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/io/Protocol.h"

namespace core {
    namespace io {

        using namespace std;

        /**
         * This class represents a URL:
         *
         * URL u("file:///tmp/file.txt");
         */
        class OPENDAVINCI_API URL {
            public:
                /**
                 * Constructor.
                 *
                 * @param connection Connection string.
                 */
                URL(const string &connection);

                /**
                 * Copy constructor.
                 *
                 * @param obj Reference to an object of this class.
                 */
                URL(const URL &obj);

                virtual ~URL();

                /**
                 * Assignment operator.
                 *
                 * @param obj Reference to an object of this class.
                 * @return Reference to this instance.
                 */
                URL& operator=(const URL &obj);

                /**
                * This method returns true if the connection string is valid.
                *
                * @return true if the connection string is valid.
                */
                bool isValid() const;

                /**
                 * This method returns the protocol type.
                 *
                 * @return Protocol type.
                 */
                Protocol::PROTOCOL getProtocol() const;

                /**
                 * This method returns the resource.
                 *
                 * @return Resource.
                 */
                const string getResource() const;

                /**
                 * This method returns a string representation of the URL.
                 *
                 * @return string representation.
                 */
                const string toString() const;

            private:
                bool m_valid;
                Protocol::PROTOCOL m_protocol;
                string m_resource;
        };

    }
} // core::io

#endif /*OPENDAVINCI_CORE_IO_URL_H_*/
