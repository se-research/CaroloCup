/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_DMCP_DISCOVER_SERVER_H_
#define HESPERIA_DMCP_DISCOVER_SERVER_H_

#include <string>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"
#include "core/wrapper/UDPReceiver.h"
#include "core/wrapper/UDPSender.h"
#include "core/wrapper/StringListener.h"

#include "hesperia/data/dmcp/DiscoverMessage.h"
#include "hesperia/dmcp/Config.h"
#include "hesperia/dmcp/ServerInformation.h"

namespace hesperia {
    namespace dmcp {
        namespace discoverer {

        using namespace std;

        class HESPERIA_API Server : public core::wrapper::StringListener {
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
#endif // HESPERIA_DMCP_DISCOVER_SERVER_H_
