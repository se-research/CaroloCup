/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include <arpa/inet.h>
#include "core/macros.h"
#include "core/base/NetstringsDeserializer.h"
#include "core/base/Serializable.h"

namespace core {
    namespace base {

        using namespace std;

        NetstringsDeserializer::NetstringsDeserializer(istream &in) :
                m_in(in) {
            // Decoding length of the payload.
            uint16_t length = 0;
            m_in.read(reinterpret_cast<char*>(&length), sizeof(uint16_t));
            length = ntohs(length);

            // Decode ':'.
            char c = 0;
            m_in.get(c);
            if (c != ':') {
                clog << "Stream corrupt: splitting ':' missing,  found: '" << c << "'" << endl;
            }

            // Check for trailing ','.
            streampos oldPosition = m_in.tellg();

            // Seek to ','.
            m_in.seekg(length);
            m_in.get(c);
            if (c != ',') {
                clog << "Stream corrupt: trailing ',' missing,  found: '" << c << "'" << endl;
            }

            // Rewind stream.
            m_in.seekg(oldPosition);
        }

        NetstringsDeserializer::~NetstringsDeserializer() {}

        void NetstringsDeserializer::read(const uint32_t /*id*/, Serializable &s) {
            if (m_in.good()) {
                m_in >> s;
            }
        }

        void NetstringsDeserializer::read(const uint32_t /*id*/, bool &b) {
            if (m_in.good()) {
                m_in.read(reinterpret_cast<char *>(&b), sizeof(bool));
            }
        }

        void NetstringsDeserializer::read(const uint32_t /*id*/, char &c) {
            if (m_in.good()) {
                m_in.read(&c, sizeof(char));
            }
        }

        void NetstringsDeserializer::read(const uint32_t /*id*/, unsigned char &uc) {
            if (m_in.good()) {
                m_in.read(reinterpret_cast<char *>(&uc), sizeof(unsigned char));
            }
        }

        void NetstringsDeserializer::read(const uint32_t /*id*/, int32_t &i) {
            if (m_in.good()) {
                m_in.read(reinterpret_cast<char *>(&i), sizeof(int));
            }
        }

        void NetstringsDeserializer::read(const uint32_t /*id*/, uint32_t &ui) {
            if (m_in.good()) {
                m_in.read(reinterpret_cast<char *>(&ui), sizeof(uint32_t));
            }
        }

        void NetstringsDeserializer::read(const uint32_t /*id*/, float &f) {
            if (m_in.good()) {
                m_in.read(reinterpret_cast<char *>(&f), sizeof(float));
            }
        }

        void NetstringsDeserializer::read(const uint32_t /*id*/, double &d) {
            if (m_in.good()) {
                m_in.read(reinterpret_cast<char *>(&d), sizeof(double));
            }
        }

        void NetstringsDeserializer::read(const uint32_t /*id*/, string &s) {
            if (m_in.good()) {
                uint32_t stringLength = s.length();
                m_in.read(reinterpret_cast<char *>(&stringLength), sizeof(uint32_t));
                stringLength = ntohl(stringLength);
                char *str = new char[stringLength+1];
                m_in.read(reinterpret_cast<char *>(str), static_cast<uint32_t>(stringLength));
                str[stringLength] = '\0';
                // It is absolutely necessary to specify the size of the serialized string, otherwise, s contains only data until the first '\0' is read.
                s = string(str, stringLength);
                HESPERIA_CORE_DELETE_POINTER(str);
            }
        }

        void NetstringsDeserializer::read(const uint32_t /*id*/, void *data, uint32_t size) {
            if (m_in.good()) {
                m_in.read(reinterpret_cast<char*>(data), size);
            }
        }

    }
} // core::base
