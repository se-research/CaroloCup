/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include <arpa/inet.h>
#include "core/base/NetstringsSerializer.h"
#include "core/base/Serializable.h"

namespace core {
    namespace base {

        using namespace std;

        NetstringsSerializer::NetstringsSerializer(ostream &out) :
                m_out(out),
                m_buffer() {}

        NetstringsSerializer::~NetstringsSerializer() {
            // Write length.
            uint16_t length = static_cast<uint16_t>(m_buffer.str().length());
            length = htons(length);
            m_out.write(reinterpret_cast<const char *>(&length), sizeof(uint16_t));

            m_out << ':';

            // Write payload.
            m_out << m_buffer.str();

            // Write End-Of-Data for checking corruptness.
            m_out << ',';
        }

        void NetstringsSerializer::write(const uint32_t /*id*/, const Serializable &s) {
            m_buffer << s;
        }

        void NetstringsSerializer::write(const uint32_t /*id*/, const bool &b) {
            m_buffer.write(reinterpret_cast<const char *>(&b), sizeof(const bool));
        }

        void NetstringsSerializer::write(const uint32_t /*id*/, const char &c) {
            m_buffer.write(&c, sizeof(const char));
        }

        void NetstringsSerializer::write(const uint32_t /*id*/, const unsigned char &uc) {
            m_buffer.write(reinterpret_cast<const char *>(&uc), sizeof(const unsigned char));
        }

        void NetstringsSerializer::write(const uint32_t /*id*/, const int32_t &i) {
            m_buffer.write(reinterpret_cast<const char *>(&i), sizeof(const int));
        }

        void NetstringsSerializer::write(const uint32_t /*id*/, const uint32_t &ui) {
            m_buffer.write(reinterpret_cast<const char *>(&ui), sizeof(const uint32_t));
        }

        void NetstringsSerializer::write(const uint32_t /*id*/, const float &f) {
            m_buffer.write(reinterpret_cast<const char *>(&f), sizeof(const float));
        }

        void NetstringsSerializer::write(const uint32_t /*id*/, const double &d) {
            m_buffer.write(reinterpret_cast<const char *>(&d), sizeof(const double));
        }

        void NetstringsSerializer::write(const uint32_t /*id*/, const string &s) {
            uint32_t stringLength = static_cast<uint32_t>(s.length());
            stringLength = htonl(stringLength);
            m_buffer.write(reinterpret_cast<const char *>(&stringLength), sizeof(uint32_t));
            m_buffer.write(reinterpret_cast<const char *>(s.c_str()), static_cast<uint32_t>(stringLength));
        }

        void NetstringsSerializer::write(const uint32_t /*id*/, const void *data, const uint32_t &size) {
            m_buffer.write(reinterpret_cast<const char*>(data), size);
        }

    }
} // core::base
