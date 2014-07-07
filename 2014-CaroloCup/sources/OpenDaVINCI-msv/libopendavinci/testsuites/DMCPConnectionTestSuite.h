/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef CORE_DMCPCONNECTIONTESTSUITE_H_
#define CORE_DMCPCONNECTIONTESTSUITE_H_

#include "cxxtest/TestSuite.h"

#include <sstream>
#include <string>
#include <memory>

#include "core/base/KeyValueConfiguration.h"
#include "core/exceptions/Exceptions.h"

#include "mocks/ModuleConfigurationProviderMock.h"
#include "mocks/ConnectionHandlerMock.h"

#include "core/data/dmcp/ModuleDescriptor.h"
#include "core/dmcp/Config.h"
#include "core/dmcp/ServerInformation.h"
#include "core/dmcp/connection/Client.h"
#include "core/dmcp/connection/ConnectionHandler.h"
#include "core/dmcp/connection/ModuleConnection.h"
#include "core/dmcp/connection/Server.h"

using namespace std;

using namespace core::base;
using namespace core::exceptions;
using namespace core::dmcp;
using namespace core::data;
using namespace core::data::dmcp;

class DMCPConnectionTestsuite : public CxxTest::TestSuite,
                                public connection::ConnectionHandler
{
    private:
        DMCPConnectionTestsuite(const DMCPConnectionTestsuite& /*obj*/);
        DMCPConnectionTestsuite& operator=(const DMCPConnectionTestsuite& /*obj*/);

    public:
        DMCPConnectionTestsuite() :
            connection(NULL) {}

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

