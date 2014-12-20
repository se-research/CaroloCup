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
                    enum MANAGED_LEVEL {
                        ML_NONE = 0,
                        ML_PULSE,
                        ML_PULSE_SHIFT,
                        ML_PULSE_TIME,
                    };

            public:
                ServerInformation();

                ServerInformation(const string& ip, const uint32_t& port, const MANAGED_LEVEL &managedLevel = ML_NONE);

                /**
                 * Copy constructor.
                 *
                 * @param obj Reference to an object of this class.
                 */
                ServerInformation(const ServerInformation &obj);

                /**
                 * Assignment operator.
                 *
                 * @param obj Reference to an object of this class.
                 * @return Reference to this instance.
                 */
                ServerInformation& operator=(const ServerInformation &obj);

                virtual ~ServerInformation();

                const string getIP() const;
                uint32_t getPort() const;
                MANAGED_LEVEL getManagedLevel() const;

                virtual ostream& operator<<(ostream &out) const;
                virtual istream& operator>>(istream &in);
                virtual const string toString() const;

                bool operator==(const ServerInformation& other) const;
            protected:
                string m_serverIP;
                uint32_t m_serverPort;
                MANAGED_LEVEL m_managedLevel;
        };
    }
} // core::dmcp

#endif /*OPENDAVINCI_DMCP_SERVERINFORMATION_H_*/
