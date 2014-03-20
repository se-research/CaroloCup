/*
 * Copyright (c) Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef CORE_DMCPDISCOVERERTESTSUITE_H_
#define CORE_DMCPDISCOVERERTESTSUITE_H_

#include "cxxtest/TestSuite.h"

#include <sstream>
#include <string>
#include <memory>

#include "core/mocks/FunctionCallWaiter.h"

#include "hesperia/dmcp/Config.h"
#include "hesperia/dmcp/ServerInformation.h"
#include "hesperia/dmcp/discoverer/Client.h"
#include "hesperia/dmcp/discoverer/Server.h"

using namespace std;

using namespace hesperia::dmcp;
using namespace hesperia::data;
using namespace hesperia::data::dmcp;

class TestClient : public hesperia::dmcp::discoverer::Client
{
    public:
        TestClient(const string& group, const uint32_t serverPort, const uint32_t clientPort) :
            Client(group, serverPort, clientPort),
            WAITER()
        {}

        virtual void onResponse() {
            WAITER.called();
        }

    mocks::FunctionCallWaiter WAITER;
};

class TestServer : public hesperia::dmcp::discoverer::Server
{
    public:
        TestServer(const ServerInformation& serverInformation,
                   const std::string& group, const uint32_t serverPort, const uint32_t clientPort) :
           Server(serverInformation, group, serverPort, clientPort),
           WAITER()
        {};

        virtual void onRequest() {
            WAITER.called();
        }

        mocks::FunctionCallWaiter WAITER;
};

class DMCPDiscovererTestsuite : public CxxTest::TestSuite
{
    public:
        // TODO: Client und Server einzeln testen!

        void testClientAndServer() {
            //DMCPConfig::TEST_GROUP
            clog << endl << "DMCPDiscovererTestsuite::testClient" << endl;
            
            ServerInformation myServerInfo("myServer", 12345);
            TestServer server(myServerInfo, DMCPConfig_TEST_GROUP, DMCPConfig_TEST_SERVERPORT, DMCPConfig_TEST_CLIENTPORT );

            TestClient client(DMCPConfig_TEST_GROUP, DMCPConfig_TEST_SERVERPORT, DMCPConfig_TEST_CLIENTPORT );
            TS_ASSERT( !client.existsServer() );
            TS_ASSERT( !client.WAITER.wasCalled() );
            TS_ASSERT( !server.WAITER.wasCalled() );
            TS_ASSERT( client.getServerInformation() == ServerInformation() );

            server.startResponding();

            TS_ASSERT( client.existsServer() );
            TS_ASSERT( client.WAITER.wasCalled() );
            TS_ASSERT( server.WAITER.wasCalled() );
            TS_ASSERT( client.getServerInformation() == myServerInfo );
        }
};


//            // Umgebung aufbauen
//            core::wrapper::UDPMulticastFactory& factory = core::wrapper::UDPMulticastFactory::getInstance();
//            auto_ptr<core::wrapper::UDPMulticastSender> sender(factory.
//                    createUDPMulticastSender("225.0.0.10", BROADCAST_PORT_SERVER));
//
//            // Testling
//            DispatcherServerUDP dispatcher("225.0.0.10",
//                                           BROADCAST_PORT_SERVER,
//                                           BROADCAST_PORT_CLIENT);
//
//            // Mock erstellen und erwartete Daten setzen
//            mocks::ConnectionRequestListenerMock mock;
//            dispatcher.setConnectionRequestListener(&mock);
//
//            ModuleDescriptor md("xNAMEx", "xIDENTIFIERx", "xVERSIONx", "xIPx", 666);
//            mock.setExpected(md);
//
//
//            // Testdurchführung
//            hesperia::data::dmcp::ConnectionRequest msg(md.getName(),
//                    md.getIdentifier(),
//                    md.getVersion(),
//                    md.getIP(),
//                    md.getPort() );
//
//            Container c = Container(Container::DMCP_CONNECTION_REQUEST, msg);
//
//            stringstream ss;
//            ss << c;
//            sender->send(ss.str());
//
//            // Überprüfung
//            clog << "A" << endl;
//            mock.waitForRequest();
//            clog << "B" << endl;
//            TS_ASSERT(mock.wasCalled());
//            clog << "C" << endl;
//        }
//
//        /**
//          * Testet den Versand eines ConnectionRequests durch den DispatcherClientUDP
//          */
//        void testConnectionRequestClientUDP() {
//            clog << endl << "DMCPDispatcherTestsuite::testConnectionRequestClientUDP" << endl;
//
//            // Umgebung aufbauen
//            core::wrapper::UDPMulticastFactory& factory = core::wrapper::UDPMulticastFactory::getInstance();
//            auto_ptr<core::wrapper::UDPMulticastReceiver> receiver(factory.
//                    createUDPMulticastReceiver("225.0.0.10", BROADCAST_PORT_SERVER));
//
//            // Mock erstellen und erwartete Daten setzen
//            mocks::StringListenerMock stringListenerMock;
//
//            ModuleDescriptor md("xNAMEx", "xIDENTIFIERx", "xVERSIONx", "xIPx", 666);
//            hesperia::data::dmcp::ConnectionRequest msg(md.getName(),
//                    md.getIdentifier(),
//                    md.getVersion(),
//                    md.getIP(),
//                    md.getPort() );
//
//            Container c = Container(Container::DMCP_CONNECTION_REQUEST, msg);
//            stringstream ss;
//            ss << c;
//            stringListenerMock.VALUES_nextString.addItem(ss.str());
//            stringListenerMock.VALUES_nextString.prepare();
//
//            receiver->setStringListener(&stringListenerMock);
//            receiver->start();
//
//            // Testling
//            DispatcherClientUDP client("225.0.0.10", BROADCAST_PORT_SERVER, BROADCAST_PORT_CLIENT);
//
//            // Testdurchführung
//            client.sendConnectionRequest(md);
//
//            // Überprüfung
//            TS_ASSERT( stringListenerMock.CALLWAITER_nextString.wait() );
//            TS_ASSERT( stringListenerMock.correctCalled() );
//        }
//
//        /**
//         * Testet den Emfpang einer DMCP_DISCOVER Nachricht und den Versand
//         * einer Antwort
//         */
//        void testDiscoverServer() {
//            clog << endl << "DMCPDispatcherTestsuite::testDiscoverServer" << endl;
//
//            // Umgebung aufbauen
//            core::wrapper::UDPMulticastFactory& factory = core::wrapper::UDPMulticastFactory::getInstance();
//            auto_ptr<core::wrapper::UDPMulticastSender> sender(factory.
//                    createUDPMulticastSender("225.0.0.10",   BROADCAST_PORT_SERVER));
//            auto_ptr<core::wrapper::UDPMulticastReceiver> receiver(factory.
//                    createUDPMulticastReceiver("225.0.0.10", BROADCAST_PORT_CLIENT));
//
//            // Mock erstellen und erwartete Daten setzen
//            mocks::StringListenerMock stringListenerMock;
//
//            Container response = Container(Container::DMCP_DISCOVER,
//                                           hesperia::data::dmcp::DiscoverMessage(hesperia::data::dmcp::DiscoverMessage::RESPONSE));
//            stringstream ssResponse;
//            ssResponse << response;
//
//            stringListenerMock.VALUES_nextString.addItem(ssResponse.str());
//            stringListenerMock.VALUES_nextString.prepare();
//
//            receiver->setStringListener(&stringListenerMock);
//            receiver->start();
//
//            // Testling
//            DispatcherServerUDP dispatcher("225.0.0.10",
//                                           BROADCAST_PORT_SERVER,
//                                           BROADCAST_PORT_CLIENT);
//
//            // Testdurchführung
//            Container discover = Container(Container::DMCP_DISCOVER,
//                                           hesperia::data::dmcp::DiscoverMessage(hesperia::data::dmcp::DiscoverMessage::DISCOVER));
//            stringstream ssDiscover;
//            ssDiscover << discover;
//            sender->send(ssDiscover.str());
//
//            // Überprüfung
//            TS_ASSERT( stringListenerMock.CALLWAITER_nextString.wait() );
//            TS_ASSERT( stringListenerMock.correctCalled() );
//        }
//
//
//
//        /**
//         * Testet das Zusammenspiel zwischen DispatcherClientUDP und
//         * DispatcherServerUDP beim Serversuchen.
//         */
//
//        void testDiscover() {
//            clog << endl << "DMCPDispatcherTestsuite::testDiscover" << endl;
//            DispatcherServerUDP server("225.0.0.10",
//                                       BROADCAST_PORT_SERVER,
//                                       BROADCAST_PORT_CLIENT);
//            DispatcherClientUDP client("225.0.0.10",
//                                       BROADCAST_PORT_SERVER,
//                                       BROADCAST_PORT_CLIENT);
//
//            TS_ASSERT(client.existsServer());
//        }
//
//        void testNoDiscover() {
//            clog << endl << "DMCPDispatcherTestsuite::testNoDiscover" << endl;
//            DispatcherServerUDP server("225.0.0.150",
//                                       BROADCAST_PORT_SERVER,
//                                       BROADCAST_PORT_CLIENT);
//            DispatcherClientUDP client("225.0.0.160",
//                                       BROADCAST_PORT_SERVER,
//                                       BROADCAST_PORT_CLIENT);
//
//            TS_ASSERT(!client.existsServer());
//        }
//
//        /**
//         * Testet das Zusammenspiel von DispatchClientUDP und
//         * DispatcherServerUDP bei einem ConnectionRequest.
//         */
//        void testConnectionRequest() {
//            clog << endl << "DMCPDispatcherTestsuite::testConnectionRequest" << endl;
//            DispatcherServerUDP server("225.0.0.10",
//                                       BROADCAST_PORT_SERVER,
//                                       BROADCAST_PORT_CLIENT);
//            DispatcherClientUDP client("225.0.0.10",
//                                       BROADCAST_PORT_SERVER,
//                                       BROADCAST_PORT_CLIENT);
//
//            // Mock erstellen und erwartete Daten setzen
//            mocks::ConnectionRequestListenerMock mock;
//            server.setConnectionRequestListener(&mock);
//
//            ModuleDescriptor md("xNAMEx", "xIDENTIFIERx", "xVERSIONx", "xIPx", 666);
//            mock.setExpected(md);
//
//            // Testdurchführung
//            client.sendConnectionRequest(md);
//
//            // Überprüfung
//            mock.waitForRequest();
//            TS_ASSERT(mock.wasCalled());
//        }
//};

#endif /*CORE_SERVERDISCOVERERTESTSUITE_H_*/
