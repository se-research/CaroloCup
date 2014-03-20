/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_DMCP_SERVERINFORMATION_H_
#define HESPERIA_DMCP_SERVERINFORMATION_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include <string>
#include "core/base/Serializable.h"

namespace hesperia {
    namespace dmcp {

        using namespace std;

        class HESPERIA_API ServerInformation : public core::base::Serializable {
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
} // hesperia::dmcp

#endif /*HESPERIA_DMCP_SERVERINFORMATION_H_*/
