/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef CORE_DMCPCONNECTIONTESTSUITE_H_
#define CORE_DMCPCONNECTIONTESTSUITE_H_

#include "cxxtest/TestSuite.h"

#include <sstream>
#include <string>
#include <memory>

#include "core/base/KeyValueConfiguration.h"
#include "core/exceptions/Exceptions.h"

#include "hesperia/mocks/ModuleConfigurationProviderMock.h"
#include "hesperia/mocks/ConnectionHandlerMock.h"

#include "hesperia/data/dmcp/ModuleDescriptor.h"
#include "hesperia/dmcp/Config.h"
#include "hesperia/dmcp/ServerInformation.h"
#include "hesperia/dmcp/connection/Client.h"
#include "hesperia/dmcp/connection/ConnectionHandler.h"
#include "hesperia/dmcp/connection/ModuleConnection.h"
#include "hesperia/dmcp/connection/Server.h"

using namespace std;

using namespace core::base;
using namespace core::exceptions;
using namespace hesperia::dmcp;
using namespace hesperia::data;
using namespace hesperia::data::dmcp;

class DMCPConnectionTestsuite : public CxxTest::TestSuite,
                                public connection::ConnectionHandler
{
    private:
        DMCPConnectionTestsuite(const DMCPConnectionTestsuite& /*obj*/);
        DMCPConnectionTestsuite& operator=(const DMCPConnectionTestsuite& /*obj*/);

    public:
        DMCPConnectionTestsuite() :
            connection(NULL) {}

        // TODO: Client und Server einzeln testen!
        connection::ModuleConnection* connection;

        void testClientAndServer()
        {
            clog << endl << "DMCPConnectionTestsuite::testClientAndServer" << endl;
            connection = NULL;
            stringstream sstr;
            sstr << "global.exampleKey = exampleValue" << endl;
            sstr << "NAME.key=Test" << endl;

            KeyValueConfiguration kv;
            sstr >> kv;

            mocks::ModuleConfigurationProviderMock configProvider;
            mocks::ConnectionHandlerMock connectionHandler;

            ServerInformation serverInfo("127.0.0.1", DMCPConfig_TEST_SERVERPORT);
            ModuleDescriptor descriptor("DMCPConnectionTestSuite", "NONE", "TestVersion");

            configProvider.addConfig(descriptor, kv);

            connection::Server server(serverInfo, configProvider);
            server.setConnectionHandler(&connectionHandler);

            connection::Client client(descriptor, serverInfo);
            TS_ASSERT(connectionHandler.WAITER.wait());
            TS_ASSERT(connectionHandler.WAITER.wasCalled());

            client.initialize();

            TS_ASSERT(client.getConfiguration().getValue<string>("NAME.Key") == "Test");
            TS_ASSERT(client.getConfiguration().getValue<string>("global.exampleKey") == "exampleValue");
        }

        virtual void onNewModule(connection::ModuleConnection* mc)
        {
            connection = mc;
        }
};

#endif


///*
// * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
// *
// * The Hesperia Framework.
// */
//
//#ifndef CORE_DMCPTESTSUITE_H_
//#define CORE_DMCPTESTSUITE_H_
//
//#include "cxxtest/TestSuite.h"
//
//#include <sstream>
//#include <string>
//#include <memory>
//
//#include "core/base/Condition.h"
//#include "core/base/Mutex.h"
//#include "core/base/Lock.h"
//#include "core/base/KeyValueConfiguration.h"
//#include "core/data/Container.h"
//#include "core/exceptions/Exceptions.h"
//#include "core/io/ConnectionAcceptor.h"
//#include "core/wrapper/UDPMulticastSender.h"
//#include "core/wrapper/UDPMulticastFactory.h"
//
//#include "hesperia/data/Configuration.h"
//#include "hesperia/data/dmcp/ConnectionRequest.h"
//#include "hesperia/data/dmcp/ConfigurationRequest.h"
//#include "hesperia/data/dmcp/ModuleDescriptor.h"
//#include "hesperia/data/dmcp/ModuleStateMessage.h"
//#include "hesperia/data/dmcp/ModuleExitCodeMessage.h"
//#include "hesperia/dmcp/ModuleConfigurationProvider.h"
//
//#include "core/mocks/StringListenerMock.h"
//#include "core/mocks/ConnectionAcceptorListenerMock.h"
//#include "core/mocks/ContainerListenerMock.h"
//
//#include "hesperia/mocks/ConnectionRequestListenerMock.h"
//#include "hesperia/mocks/ModuleConfigurationProviderMock.h"
//#include "hesperia/mocks/ModuleStateListenerMock.h"
//
//using namespace std;
//using namespace core::base;
//using namespace core::data;
//using namespace core::exceptions;
//using namespace core::io;
//
//using namespace hesperia::dmcp;
//using namespace hesperia::data;
//using namespace hesperia::data::dmcp;
//
//class ModuleConnectionTCPTestsuite : public CxxTest::TestSuite {
//
//    public:
//        /**
//         * Testet, ob sich eine ModuleConnectionTCP zu gegebener IP-Adresse
//         * verbindet.
//         */
//        void testConnection() {
//#ifndef WIN32
//            clog << endl << "ModuleConnectionTestsuite::testConnection" << endl;
//            // Setup ModuleDescriptor
//            ModuleDescriptor md("NAME", "ID", "VERSION", "127.0.0.1", 10000);
//
//            // Setup acceptor simulating a DMCP client.
//            ConnectionAcceptor acceptor(10000);
//            mocks::ConnectionAcceptorListenerMock calMock;
//            acceptor.setConnectionAcceptorListener(&calMock);
//            acceptor.start();
//
//            // Create ModuleConnection
//            ModuleConnectionTCP connection(md);
//
//            // ModuleConnection tries to connect die localhost:10000. Wait...
//            calMock.waitForConnection();
//
//            // ConnectionAcceptorListenerMock should have a connection...
//            TS_ASSERT( calMock.hasConnection() );
//#endif
//        }
//
//        /**
//         * Testet, ob eine ModuleConnectionTCP auf ein DMCP_CONFIGURATION_REQUEST
//         * mit dem Versand einer Configuration reagiert.
//         */
//        void testRequestConfiguration() {
//#ifndef WIN32
//            clog << endl << "ModuleConnectionTestsuite::testRequestConfiguration" << endl;
//            // Setup ModuleDescriptor and Configuration
//            ModuleDescriptor md("NAME", "ID", "VERSION", "127.0.0.1", 10000);
//
//            stringstream sstr;
//            sstr << "global.exampleKey = exampleValue" << endl;
//            sstr << "NAME.key=Test" << endl;
//
//            KeyValueConfiguration kv;
//            sstr >> kv;
//
//            // Setup acceptor simulating a DMCP client.
//            ConnectionAcceptor acceptor(10000);
//            mocks::ConnectionAcceptorListenerMock calMock;
//            acceptor.setConnectionAcceptorListener(&calMock);
//            acceptor.start();
//
//            // Create ModuleConnection
//            mocks::ModuleConfigurationProviderMock mcpMock;
//            mcpMock.addConfig(md, kv);
//
//            ModuleConnectionTCP connection(md);
//            connection.setModuleConfigurationProvider(&mcpMock);
//
//            // ModuleConnection tries to connect die localhost:1000. Wait...
//            calMock.waitForConnection();
//            TS_ASSERT( calMock.hasConnection() );
//
//            // ModuleConnection is now connected. Setup a ContainerListenerMock
//            // to receive the configuration.
//            mocks::ContainerListenerMock clMock;
//            hesperia::data::dmcp::ConfigurationRequest request(hesperia::data::dmcp::ConfigurationRequest::SOMETHING);
//            Container container(Container::DMCP_CONFIGURATION_REQUEST, request);
//            clMock.VALUES_nextContainer.addItem(container);
//            clMock.VALUES_nextContainer.prepare();
//
//            calMock.getConnection()->setContainerListener(&clMock);
//            calMock.getConnection()->start();
//
//            calMock.getConnection()->send(container);
//
//            // Wait until we received configuration.
//
//            TS_ASSERT( clMock.CALLWAITER_nextContainer.wait() );
//            KeyValueConfiguration config = clMock.currentValue.getData<Configuration>().getKeyValueConfiguration();
//            TS_ASSERT(config.getValue<string>("NAME.Key") == "Test");
//            TS_ASSERT(config.getValue<string>("global.exampleKey") == "exampleValue");
//#endif
//        }
//
//        void testModuleConnectionStateListenerWithStateMessage() {
//#ifndef WIN32
//            clog << endl << "ModuleConnectionTestsuite::testConnection" << endl;
//            // Setup ModuleDescriptor
//            ModuleDescriptor md("NAME", "ID", "VERSION", "127.0.0.1", 10000);
//
//            // Setup acceptor simulating a DMCP client.
//            ConnectionAcceptor acceptor(10000);
//            mocks::ConnectionAcceptorListenerMock calMock;
//            acceptor.setConnectionAcceptorListener(&calMock);
//            acceptor.start();
//
//            // Create ModuleConnection
//            ModuleConnectionTCP connection(md);
//            mocks::ModuleStateListenerMock mslMock;
//            connection.setModuleConnectionStateListener(&mslMock);
//
//            // ModuleConnection tries to connect die localhost:1000. Wait...
//            calMock.waitForConnection();
//
//            // ConnectionAcceptorListenerMock should have a connection...
//            TS_ASSERT( calMock.hasConnection() );
//            calMock.getConnection()->start();
//
//            Container containerRunning(Container::DMCP_MODULESTATEMESSAGE, hesperia::data::dmcp::ModuleStateMessage(ModuleState::RUNNING));
//            calMock.getConnection()->send(containerRunning);
//            mslMock.waitForStateCall();
//            TS_ASSERT(mslMock.wasStateCalled());
//            TS_ASSERT(mslMock.getModuleState() == ModuleState::RUNNING);
//            mslMock.reset();
//
//            Container containerNotRunning(Container::DMCP_MODULESTATEMESSAGE, hesperia::data::dmcp::ModuleStateMessage(ModuleState::NOT_RUNNING));
//            calMock.getConnection()->send(containerNotRunning);
//            mslMock.waitForStateCall();
//            TS_ASSERT(mslMock.wasStateCalled());
//            TS_ASSERT(mslMock.getModuleState() == ModuleState::NOT_RUNNING);
//            mslMock.reset();
//#endif
//        }
//
//        void testModuleConnectionStateListenerWithExitCode() {
//#ifndef WIN32
//            clog << endl << "ModuleConnectionTestsuite::testConnection" << endl;
//            // Setup ModuleDescriptor
//            ModuleDescriptor md("NAME", "ID", "VERSION", "127.0.0.1", 10000);
//
//            // Setup acceptor simulating a DMCP client.
//            ConnectionAcceptor acceptor(10000);
//            mocks::ConnectionAcceptorListenerMock calMock;
//            acceptor.setConnectionAcceptorListener(&calMock);
//            acceptor.start();
//
//            // Create ModuleConnection
//            ModuleConnectionTCP connection(md);
//            mocks::ModuleStateListenerMock mslMock;
//            connection.setModuleConnectionStateListener(&mslMock);
//
//            // ModuleConnection tries to connect die localhost:1000. Wait...
//            calMock.waitForConnection();
//
//            // ConnectionAcceptorListenerMock should have a connection...
//            TS_ASSERT( calMock.hasConnection() );
//            calMock.getConnection()->start();
//
//            Container containerOK(Container::DMCP_MODULEEXITCODEMESSAGE,
//                                  dmcp::ModuleExitCodeMessage(ModuleState::OKAY));
//            calMock.getConnection()->send(containerOK);
//            mslMock.waitForExitCodeCall();
//            TS_ASSERT(mslMock.wasExitCodeCalled());
//            TS_ASSERT(mslMock.getExitCode() == ModuleState::OKAY);
//            mslMock.reset();
//
//            Container containerError(Container::DMCP_MODULEEXITCODEMESSAGE,
//                                     dmcp::ModuleExitCodeMessage(ModuleState::SERIOUS_ERROR));
//            calMock.getConnection()->send(containerError);
//            mslMock.waitForExitCodeCall();
//            TS_ASSERT(mslMock.wasExitCodeCalled());
//            TS_ASSERT(mslMock.getExitCode() == ModuleState::SERIOUS_ERROR);
//            mslMock.reset();
//
//            Container containerException(Container::DMCP_MODULEEXITCODEMESSAGE,
//                                         dmcp::ModuleExitCodeMessage(ModuleState::EXCEPTION_CAUGHT));
//            calMock.getConnection()->send(containerException);
//            mslMock.waitForExitCodeCall();
//            TS_ASSERT(mslMock.wasExitCodeCalled());
//            TS_ASSERT(mslMock.getExitCode() == ModuleState::EXCEPTION_CAUGHT);
//            mslMock.reset();
//
//
//            // Aufruf des ModuleStateListeners im Fall eines Verbindungsabbruchs testen
//            delete calMock.getConnection();
//            calMock.dontDeleteConnection();
//            mslMock.waitForExitCodeCall();
//            TS_ASSERT(mslMock.wasExitCodeCalled());
//            TS_ASSERT(mslMock.getExitCode() == ModuleState::CONNECTION_LOST);
//            mslMock.reset();
//#endif
//        }
//};
//
//class ServerTestsuite : public CxxTest::TestSuite {
//    public:
//        void testExistsServer() {
//            clog << endl << "ServerTestsuite::testExistsServer" << endl;
//            ServerTCP server1("225.0.0.10", BROADCAST_PORT_SERVER, BROADCAST_PORT_CLIENT);
//
//            bool caught = false;
//            try {
//                ServerTCP server2("225.0.0.10", BROADCAST_PORT_SERVER, BROADCAST_PORT_CLIENT);
//            } catch (DMCPServerAlreadyStartedException &/*e*/) {
//                caught = true;
//            }
//
//            TS_ASSERT(caught);
//        }
//};
//
//class ClientTestsuite : public CxxTest::TestSuite {
//    public:
//        void testNotExistsServer() {
//            clog << endl << "ClientTestsuite::testNotExistsServer" << endl;
//
//            bool caught = false;
//            try {
//                Client client("TestClient",
//                              "0.0",
//                              "TestVersion",
//                              "225.0.0.200",
//                              BROADCAST_PORT_SERVER,
//                              BROADCAST_PORT_CLIENT);
//
//                client.initialize();
//
//            } catch (DMCPServerNotFoundException &/*e*/) {
//                caught = true;
//            }
//
//            TS_ASSERT(caught);
//        }
//
//        void testExistsServer() {
//            clog << endl << "ClientTestsuite::testExistsServer" << endl;
//            ServerTCP server("225.0.0.200", BROADCAST_PORT_SERVER, BROADCAST_PORT_CLIENT);
//            server.start();
//
//            bool caught = false;
//            try {
//                Client client("TestClient",
//                              "0.0",
//                              "TestVersion",
//                              "225.0.0.200",
//                              BROADCAST_PORT_SERVER,
//                              BROADCAST_PORT_CLIENT);
//
//                client.initialize();
//
//            } catch (exception e) {
//                caught = true;
//            }
//
//            TS_ASSERT(!caught);
//        }
//};
//
//class DMCPTestsuite : public CxxTest::TestSuite,
//            public ModuleConnectionListener {
//    public:
//        void testClientConfiguration() {
//            clog << endl << "DMCPTestsuite::testClientConfiguration" << endl;
//            stringstream sstr;
//            sstr << "global.exampleKey = exampleValue" << endl;
//            sstr << "TestClient.key=Test" << endl;
//
//            KeyValueConfiguration kv;
//            sstr >> kv;
//
//            ModuleDescriptor md("TestClient", "", "1.0", "127.0.0.1", 10000);
//
//            mocks::ModuleConfigurationProviderMock mclMock;
//            mclMock.addConfig(md, kv);
//
//            ServerTCP server("225.0.0.10", BROADCAST_PORT_SERVER, BROADCAST_PORT_CLIENT);
//            server.setConfigurationProvider(&mclMock);
//            server.setModuleConnectionListener(this);
//            server.start();
//
//            Client client("TestClient",
//                          "",
//                          "1.0",
//                          "225.0.0.10",
//                          BROADCAST_PORT_SERVER,
//                          BROADCAST_PORT_CLIENT);
//            client.initialize();
//
//            TS_ASSERT( server.hasModule("TestClient") == true);
//
//            KeyValueConfiguration config = client.getConfiguration();
//            TS_ASSERT(config.getValue<string>("TestClient.Key") == "Test");
//            server.stop();
//        }
//
//        virtual void onConnectionError(const ModuleDescriptor&) {};
//        virtual void onHeartbeat(const ModuleDescriptor&, const core::data::TimeStamp&) {};
//        virtual void onContainer(const ModuleDescriptor&, core::data::Container&) {};
//};
//
//class DMCPDataContainerTestSuite : public CxxTest::TestSuite {
//    public:
//        void testDiscoverMessage() {
//            dmcp::DiscoverMessage msgDiscover(dmcp::DiscoverMessage::DISCOVER);
//            TS_ASSERT( msgDiscover.getType() == dmcp::DiscoverMessage::DISCOVER );
//            TS_ASSERT( msgDiscover.toString() == "DiscoverMessage::DISCOVER" );
//
//            dmcp::DiscoverMessage msgResponse(dmcp::DiscoverMessage::RESPONSE);
//            TS_ASSERT( msgResponse.getType() == dmcp::DiscoverMessage::RESPONSE );
//            TS_ASSERT( msgResponse.toString() == "DiscoverMessage::RESPONSE" );
//
//            dmcp::DiscoverMessage msgUndefined;
//            TS_ASSERT( msgUndefined.getType() == dmcp::DiscoverMessage::UNDEFINED );
//            TS_ASSERT( msgUndefined.toString() == "DiscoverMessage::UNDEFINED" );
//
//            dmcp::DiscoverMessage msgDeserialized;
//            stringstream sstr;
//
//            sstr << msgDiscover;
//            sstr >> msgDeserialized;
//            TS_ASSERT( msgDeserialized.getType() == dmcp::DiscoverMessage::DISCOVER);
//
//            sstr << msgResponse;
//            sstr >> msgDeserialized;
//            TS_ASSERT( msgDeserialized.getType() == dmcp::DiscoverMessage::RESPONSE);
//        }
//        void testModuleStateMessage() {
//            dmcp::ModuleStateMessage msgRunning(ModuleState::RUNNING);
//            TS_ASSERT( msgRunning.getModuleState() == ModuleState::RUNNING);
//            TS_ASSERT( msgRunning.toString() == "MODULE_STATE::RUNNING");
//
//            dmcp::ModuleStateMessage msgNotRunning(ModuleState::NOT_RUNNING);
//            TS_ASSERT( msgNotRunning.getModuleState() == ModuleState::NOT_RUNNING);
//            TS_ASSERT( msgNotRunning.toString() == "MODULE_STATE::NOT_RUNNING");
//
//            dmcp::ModuleStateMessage msgUndefined;
//            TS_ASSERT( msgUndefined.getModuleState() == ModuleState::UNDEFINED_STATE);
//            TS_ASSERT( msgUndefined.toString() == "MODULE_STATE::UNDEFINED_STATE");
//
//            dmcp::ModuleStateMessage msgDeserialized;
//            stringstream sstr;
//
//            sstr << msgRunning;
//            sstr >> msgDeserialized;
//            TS_ASSERT( msgDeserialized.getModuleState() == ModuleState::RUNNING);
//
//            sstr << msgNotRunning;
//            sstr >> msgDeserialized;
//            TS_ASSERT( msgDeserialized.getModuleState() == ModuleState::NOT_RUNNING);
//        }
//
//        void testModuleExitCodeMessage() {
//            dmcp::ModuleExitCodeMessage msgOkay(ModuleState::OKAY);
//            TS_ASSERT(msgOkay.getModuleExitCode() == ModuleState::OKAY);
//            TS_ASSERT(msgOkay.toString() == "MODULE_EXITCODE::OKAY" );
//
//            dmcp::ModuleExitCodeMessage msgError(ModuleState::SERIOUS_ERROR);
//            TS_ASSERT(msgError.getModuleExitCode() == ModuleState::SERIOUS_ERROR);
//            TS_ASSERT(msgError.toString() == "MODULE_EXITCODE::SERIOUS_ERROR" );
//
//            dmcp::ModuleExitCodeMessage msgException(ModuleState::EXCEPTION_CAUGHT);
//            TS_ASSERT(msgException.getModuleExitCode() == ModuleState::EXCEPTION_CAUGHT);
//            TS_ASSERT(msgException.toString() == "MODULE_EXITCODE::EXCEPTION_CAUGHT" );
//
//            dmcp::ModuleExitCodeMessage msgConLost(ModuleState::CONNECTION_LOST);
//            TS_ASSERT(msgConLost.getModuleExitCode() == ModuleState::CONNECTION_LOST);
//            TS_ASSERT(msgConLost.toString() == "MODULE_EXITCODE::CONNECTION_LOST" );
//
//            dmcp::ModuleExitCodeMessage msgUndefined;
//            TS_ASSERT(msgUndefined.getModuleExitCode() == ModuleState::UNDEFINED_EXITCODE);
//            TS_ASSERT(msgUndefined.toString() == "MODULE_EXITCODE::UNDEFINED_EXITCODE" );
//
//            dmcp::ModuleExitCodeMessage msgDeserialized;
//            stringstream sstr;
//
//            sstr << msgOkay;
//            sstr >> msgDeserialized;
//            TS_ASSERT( msgDeserialized.getModuleExitCode() == ModuleState::OKAY);
//
//            sstr << msgError;
//            sstr >> msgDeserialized;
//            TS_ASSERT( msgDeserialized.getModuleExitCode() == ModuleState::SERIOUS_ERROR);
//
//            sstr << msgException;
//            sstr >> msgDeserialized;
//            TS_ASSERT( msgDeserialized.getModuleExitCode() == ModuleState::EXCEPTION_CAUGHT);
//
//            sstr << msgConLost;
//            sstr >> msgDeserialized;
//            TS_ASSERT( msgDeserialized.getModuleExitCode() == ModuleState::CONNECTION_LOST);
//
//            sstr << msgUndefined;
//            sstr >> msgDeserialized;
//            TS_ASSERT( msgDeserialized.getModuleExitCode() == ModuleState::UNDEFINED_EXITCODE);
//        }
//
//};
//
//#endif /*CORE_SERVERDISCOVERERTESTSUITE_H_*/
