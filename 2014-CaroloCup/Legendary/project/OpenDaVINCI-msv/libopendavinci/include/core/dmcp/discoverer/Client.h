/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_DMCP_DISCOVER_CLIENT_H_
#define OPENDAVINCI_DMCP_DISCOVER_CLIENT_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/SharedPointer.h"

#include "core/base/Condition.h"
#include "core/wrapper/PacketListener.h"
#include "core/wrapper/UDPSender.h"
#include "core/wrapper/UDPReceiver.h"

#include "core/dmcp/ServerInformation.h"

namespace core {
    namespace dmcp {
        namespace discoverer {

            class OPENDAVINCI_API Client : protected core::wrapper::PacketListener {
                private:
                    /**
                     * "Forbidden" copy constructor. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the copy constructor.
                     */
                    Client(const Client &);

                    /**
                     * "Forbidden" assignment operator. Goal: The compiler should warn
                     * already at compile time for unwanted bugs caused by any misuse
                     * of the assignment operator.
                     */
                    Client& operator=(const Client &);

                public:
                    Client(const std::string& group,
                           const uint32_t serverPort,
                           const uint32_t clientPort);
                    virtual ~Client();

                    bool existsServer();

                    ServerInformation getServerInformation() const;

                    virtual void onResponse();

                protected:
                    void sendDiscoverMessage();
                    void waitForResponse();
                    virtual void nextPacket(const core::wrapper::Packet &p);

                    core::SharedPointer<core::wrapper::UDPSender> m_sender;
                    core::SharedPointer<core::wrapper::UDPReceiver> m_receiver;

                    core::base::Condition m_responseCondition;
                    bool m_response;

                    ServerInformation m_serverInformation;
            };
        }
    }
}
#endif //OPENDAVINCI_DMCP_DISCOVER_CLIENT_H_
