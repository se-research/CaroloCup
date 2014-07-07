/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/io/URL.h"
#include "core/io/Protocol.h"

namespace core {
    namespace io {

        URL::URL(const string &connection) :
                m_valid(false),
                m_protocol(Protocol::UNKNOWNPROTOCOL),
                m_resource(connection) {
            // Parse protocol type and remove it from connection string.
            m_protocol = Protocol::getProtocol(m_resource);
            m_valid = (m_resource != connection) && (m_resource.length() > 0) && (m_protocol != Protocol::UNKNOWNPROTOCOL);
        }

        URL::URL(const URL &obj) :
                m_valid(obj.m_valid),
                m_protocol(obj.m_protocol),
                m_resource(obj.m_resource) {}

        URL::~URL() {}

        URL& URL::operator=(const URL &obj) {
            m_valid = obj.m_valid;
            m_protocol = obj.m_protocol;
            m_resource = obj.m_resource;

            return (*this);
        }

        bool URL::isValid() const {
            return m_valid;
        }

        Protocol::PROTOCOL URL::getProtocol() const {
            return m_protocol;
        }

        const string URL::getResource() const {
            return m_resource;
        }

        const string URL::toString() const {
            if (isValid()) {
                stringstream s;
                switch (getProtocol()) {
                case Protocol::FILEPROTOCOL:
                    s << "file://" << getResource();
                    break;

                case Protocol::UNKNOWNPROTOCOL:
                    // This case is unreachable.
                    break;
                }
                return s.str();
            }
            return "";
        }

    }
} // core::io
