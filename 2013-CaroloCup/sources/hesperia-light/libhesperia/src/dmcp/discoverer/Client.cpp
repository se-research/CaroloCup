/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include <sstream>

#include "core/base/Lock.h"
#include "core/data/Container.h"
#include "core/exceptions/Exceptions.h"
#include "core/wrapper/UDPFactory.h"

#include "hesperia/data/dmcp/DiscoverMessage.h"
#include "hesperia/dmcp/Config.h"
#include "hesperia/dmcp/discoverer/Client.h"


namespace hesperia {
    namespace dmcp {
        namespace discoverer {

            using namespace std;
            using namespace core::base;
            using namespace core::data;
            using namespace core::exceptions;

            using namespace data::dmcp;

            Client::Client(const string& group,
                           const uint32_t serverPort,
                           const uint32_t clientPort ) :
                    m_sender(core::wrapper::UDPFactory::getInstance().createUDPSender(group, serverPort)),
                    m_receiver(core::wrapper::UDPFactory::getInstance().createUDPReceiver(group, clientPort)),
                    m_responseCondition(),
                    m_response(false),
                    m_serverInformation() {
                m_receiver->setStringListener(this);
                m_receiver->start();
            }

            Client::~Client() {
                m_receiver->setStringListener(NULL);
                m_receiver->stop();
            }

            bool Client::existsServer() {
                m_response = false;
                sendDiscoverMessage();
                waitForResponse();

                return m_response;
            }

            ServerInformation Client::getServerInformation() const {
                return m_serverInformation;
            }

            void Client::onResponse() {}

            void Client::sendDiscoverMessage() {
                Container discover = Container(Container::DMCP_DISCOVER,
                                               DiscoverMessage(DiscoverMessage::DISCOVER));
                stringstream ss;
                ss << discover;

                m_sender->send(ss.str());
            }

            void Client::nextString(const string &s) {
                Container container;
                stringstream ss(s);
                ss >> container;

                if ( container.getDataType() != Container::DMCP_DISCOVER ) {
                    clog << "(DMCP-DiscovererClient) received unknown message: " << container.toString() << endl;
                }
                else {
                    DiscoverMessage msg = container.getData<DiscoverMessage>();

                    if ( msg.getType() == DiscoverMessage::RESPONSE ) {
                        Lock l(m_responseCondition);

                        if (!m_response) {
                            m_serverInformation = msg.getServerInformation();
                            m_response = true;
                            onResponse();
                            m_responseCondition.wakeAll();
                        }
                    }
                }
            }

            void Client::waitForResponse() {
                Lock  l(m_responseCondition);
                if ( !m_response ) {
                    m_responseCondition.waitOnSignalWithTimeout(DISCOVER_TIMEOUT);
                }
            }
        }
    }
}
