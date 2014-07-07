/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_DMCP_SERVERINFORMATION_H_
#define OPENDAVINCI_DMCP_SERVERINFORMATION_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/base/Serializable.h"

namespace core {
    namespace dmcp {

        using namespace std;

        class OPENDAVINCI_API ServerInformation : public core::base::Serializable {
            public:
                ServerInformation();
                ServerInformation(const string& ip, const uint32_t& port);

                virtual ~ServerInformation();

                const string getIP() const;
                uint32_t getPort() const;

                virtual ostream& operator<<(ostream &out) const;
                virtual istream& operator>>(istream &in);
                virtual const string toString() const;

                bool operator==(const ServerInformation& other) const;
            protected:
                string m_serverIP;
                uint32_t m_serverPort;
        };
    }
} // core::dmcp

#endif /*OPENDAVINCI_DMCP_SERVERINFORMATION_H_*/
