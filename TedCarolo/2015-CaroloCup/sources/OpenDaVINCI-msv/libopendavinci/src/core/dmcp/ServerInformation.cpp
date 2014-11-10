#include "core/dmcp/ServerInformation.h"

#include <sstream>
#include "core/base/Hash.h"
#include "core/base/Deserializer.h"
#include "core/base/Serializer.h"
#include "core/base/SerializationFactory.h"

namespace core {
    namespace dmcp {
        using namespace core::base;
        ServerInformation::ServerInformation() :
            m_serverIP(""),
            m_serverPort(0)
        {}

        ServerInformation::ServerInformation(const string& ip, const uint32_t& port) :
            m_serverIP(ip),
            m_serverPort(port)
        {}

        ServerInformation::~ServerInformation()
        {}

        const string ServerInformation::getIP() const
        {
            return m_serverIP;
        }

        uint32_t ServerInformation::getPort() const
        {
            return m_serverPort;
        }

        ostream& ServerInformation::operator<<(ostream &out) const
        {
            SerializationFactory sf;
            Serializer &s = sf.getSerializer(out);

            s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL2('i', 'p') >::RESULT, m_serverIP);
            s.write(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('p', 'o', 'r', 't') >::RESULT, m_serverPort);

            return out;
        }

        istream& ServerInformation::operator>>(istream &in)
        {
            SerializationFactory sf;
            Deserializer &d = sf.getDeserializer(in);

            d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL2('i', 'p') >::RESULT, m_serverIP);
            d.read(CRC32 < OPENDAVINCI_CORE_STRINGLITERAL4('p', 'o', 'r', 't') >::RESULT, m_serverPort);

            return in;
        }

        const string ServerInformation::toString() const
        {
            stringstream ss;
            ss << "IP: " << m_serverIP << ", Port: " << m_serverPort;
            return ss.str();
        }

        bool ServerInformation::operator==(const ServerInformation& other) const {
            return (m_serverIP ==other.m_serverIP ) && (m_serverPort==other.m_serverPort);
        }
    }
}



