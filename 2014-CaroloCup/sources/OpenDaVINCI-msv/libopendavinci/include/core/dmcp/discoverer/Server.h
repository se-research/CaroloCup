/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_DMCP_DISCOVER_SERVER_H_
#define OPENDAVINCI_DMCP_DISCOVER_SERVER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/UDPReceiver.h"
#include "core/wrapper/UDPSender.h"
#include "core/wrapper/StringListener.h"

#include "core/data/dmcp/DiscoverMessage.h"
#include "core/dmcp/Config.h"
#include "core/dmcp/ServerInformation.h"

namespace core {
    namespace dmcp {
        namespace discoverer {

        using namespace std;

        class OPENDAVINCI_API Server : public core::wrapper::StringListener {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                Server(const Server &);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                Server& operator=(const Server &);

            public:
                Server(const ServerInformation& serverInformation,
                       const std::string& group, const uint32_t serverPort, const uint32_t clientPort);
                virtual ~Server();

                void startResponding();
                void stopResponding();

                virtual void onRequest();

            protected:
                virtual void nextString(const string &s);

                void sendResponseMessage();

                core::wrapper::UDPSender* m_sender;
                core::wrapper::UDPReceiver* m_receiver;

                ServerInformation m_serverInformation;
        };
    }
    }
}
#endif // OPENDAVINCI_DMCP_DISCOVER_SERVER_H_
