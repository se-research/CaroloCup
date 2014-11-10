/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/Lock.h"
#include "core/data/Container.h"
#include "core/exceptions/Exceptions.h"
#include "core/wrapper/UDPFactory.h"

#include "core/data/Configuration.h"
#include "core/dmcp/discoverer/Server.h"

namespace core {
    namespace dmcp {
        namespace discoverer {

        using namespace std;

        using namespace std;
        using namespace core::base;
        using namespace core::data;

        using namespace data::dmcp;

        Server::Server(const ServerInformation& serverInformation,
                       const std::string& group,
                       const uint32_t serverPort,
                       const uint32_t clientPort ) :
                m_sender(core::wrapper::UDPFactory::createUDPSender(group, clientPort)),
                m_receiver(core::wrapper::UDPFactory::createUDPReceiver(group, serverPort)),
                m_serverInformation(serverInformation)
        {
            m_receiver->start();
        }

        Server::~Server() {
            m_receiver->setStringListener(NULL);
            m_receiver->stop();
        }

        void Server::nextString(const string &s) {
            Container container;
            stringstream ss(s);
            ss >> container;

            if ( container.getDataType() != Container::DMCP_DISCOVER ) {
                clog << "(DMCP-DiscovererServer) received unkown message: " << container.toString() << endl;
            } else {
                DiscoverMessage msg = container.getData<DiscoverMessage>();

                if ( msg.getType() == DiscoverMessage::DISCOVER ) {
                    onRequest();
                    sendResponseMessage();
                }
            }
        }

        void Server::sendResponseMessage() {
            clog << "(DMCP-DiscovererServer) responding to DMCP_DISCOVER" << endl;
            DiscoverMessage msg(DiscoverMessage::RESPONSE, m_serverInformation);
            Container c = Container(Container::DMCP_DISCOVER, msg);

            stringstream ss;
            ss << c;
            m_sender->send(ss.str());
        }

        void Server::startResponding() {
            m_receiver->setStringListener(this);
        }

        void Server::stopResponding() {
            m_receiver->setStringListener(NULL);
        }

        void Server::onRequest()
        {

        }
    }
    }
}
