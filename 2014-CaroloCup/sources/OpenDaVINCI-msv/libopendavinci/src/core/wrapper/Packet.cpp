/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/wrapper/Packet.h"

namespace core {
    namespace wrapper {

        using namespace std;

        Packet::Packet() :
            m_sender(),
            m_data() {}

        Packet::Packet(const string &s, const string &d) :
            m_sender(s),
            m_data(d) {}

        Packet::~Packet() {}

        Packet::Packet(const Packet &obj) :
            m_sender(obj.m_sender),
            m_data(obj.m_data) {}

        Packet& Packet::operator=(const Packet &obj) {
            setSender(obj.getSender());
            setData(obj.getData());

            return (*this);
        }

        const string Packet::getSender() const {
            return m_sender;
        }

        void Packet::setSender(const string &s) {
            m_sender = s;
        }

        const string Packet::getData() const {
            return m_data;
        }

        void Packet::setData(const string &d) {
            m_data = d;
        }

    }
} // core::wrapper
