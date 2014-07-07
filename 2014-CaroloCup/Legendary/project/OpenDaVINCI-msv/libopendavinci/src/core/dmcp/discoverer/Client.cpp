/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/Lock.h"
#include "core/data/Container.h"
#include "core/exceptions/Exceptions.h"
#include "core/wrapper/UDPFactory.h"

#include "core/data/dmcp/DiscoverMessage.h"
#include "core/dmcp/Config.h"
#include "core/dmcp/discoverer/Client.h"


namespace core {
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
                    m_sender(core::wrapper::UDPFactory::createUDPSender(group, serverPort)),
                    m_receiver(core::wrapper::UDPFactory::createUDPReceiver(group, clientPort)),
                    m_responseCondition(),
                    m_response(false),
                    m_serverInformation() {
                m_receiver->setPacketListener(this);
                m_receiver->start();
            }

            Client::~Client() {
                m_receiver->setPacketListener(NULL);
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

            void Client::nextPacket(const core::wrapper::Packet &p) {
                Container container;
                stringstream ss(p.getData());
                ss >> container;

                if ( container.getDataType() != Container::DMCP_DISCOVER ) {
                    clog << "(DMCP-DiscovererClient) received unknown message: " << container.toString() << endl;
                }
                else {
                    DiscoverMessage msg = container.getData<DiscoverMessage>();

                    if ( msg.getType() == DiscoverMessage::RESPONSE ) {
                        Lock l(m_responseCondition);

                        if (!m_response) {
                        	ServerInformation tmp = msg.getServerInformation();
                        	// Use the IP address from the received UDP packet.
                            m_serverInformation = ServerInformation(p.getSender(), tmp.getPort());
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
