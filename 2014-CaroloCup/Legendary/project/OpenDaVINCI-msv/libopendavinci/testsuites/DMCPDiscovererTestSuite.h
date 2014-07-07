/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef CORE_DMCPDISCOVERERTESTSUITE_H_
#define CORE_DMCPDISCOVERERTESTSUITE_H_

#include "cxxtest/TestSuite.h"

#include <sstream>
#include <string>
#include <memory>

#include "mocks/FunctionCallWaiter.h"

#include "core/dmcp/Config.h"
#include "core/dmcp/ServerInformation.h"
#include "core/dmcp/discoverer/Client.h"
#include "core/dmcp/discoverer/Server.h"

using namespace std;

using namespace core::dmcp;
using namespace core::data;
using namespace core::data::dmcp;

class TestClient : public core::dmcp::discoverer::Client
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

class TestServer : public core::dmcp::discoverer::Server
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

        void testClientAndServer() {
            //DMCPConfig::TEST_GROUP
            clog << endl << "DMCPDiscovererTestsuite::testClient" << endl;
            
            ServerInformation myServerInfo("0.0.0.0", 12345);
            TestServer server(myServerInfo, DMCPConfig_TEST_GROUP, DMCPConfig_TEST_SERVERPORT, DMCPConfig_TEST_CLIENTPORT );

            TestClient client(DMCPConfig_TEST_GROUP, DMCPConfig_TEST_SERVERPORT, DMCPConfig_TEST_CLIENTPORT );
            TS_ASSERT( !client.existsServer() );
            TS_ASSERT( !client.WAITER.wasCalled() );
            TS_ASSERT( !server.WAITER.wasCalled() );
            TS_ASSERT( client.getServerInformation().toString() == ServerInformation().toString() );

            server.startResponding();

            TS_ASSERT( client.existsServer() );
            TS_ASSERT( client.WAITER.wasCalled() );
            TS_ASSERT( server.WAITER.wasCalled() );
            TS_WARN(client.getServerInformation().toString());
            // The server sends a message containing 0.0.0.0 and the client uses recvfrom to get the sender's IP address. Thus, the following test fails.
            // TS_ASSERT( client.getServerInformation().toString() == myServerInfo.toString() );
        }
};


#endif /*CORE_SERVERDISCOVERERTESTSUITE_H_*/
