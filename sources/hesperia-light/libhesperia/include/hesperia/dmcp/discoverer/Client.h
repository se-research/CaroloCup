/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_DMCP_DISCOVER_CLIENT_H_
#define HESPERIA_DMCP_DISCOVER_CLIENT_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"
#include "core/SharedPointer.h"

#include "core/base/Condition.h"
#include "core/wrapper/StringListener.h"
#include "core/wrapper/UDPSender.h"
#include "core/wrapper/UDPReceiver.h"

#include "hesperia/dmcp/ServerInformation.h"

namespace hesperia {
    namespace dmcp {
        namespace discoverer {

            class HESPERIA_API Client : protected core::wrapper::StringListener {
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
                    virtual void nextString(const string &s);

                    core::SharedPointer<core::wrapper::UDPSender> m_sender;
                    core::SharedPointer<core::wrapper::UDPReceiver> m_receiver;

                    core::base::Condition m_responseCondition;
                    bool m_response;

                    ServerInformation m_serverInformation;
            };
        }
    }
}
#endif //HESPERIA_DMCP_DISCOVER_CLIENT_H_
