/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_DMCP_CONFIG_H_
#define HESPERIA_DMCP_CONFIG_H_

#include <string>

namespace hesperia {
    namespace dmcp {
        using namespace std;

//        class DMCPConfig
//        {
//            public:
//                static string TEST_GROUP;
//                static uint32_t TEST_SERVERPORT;
//                static uint32_t TEST_CLIENTPORT;
//        };

         static const string DMCPConfig_TEST_GROUP = "225.0.0.250";
         static const uint32_t DMCPConfig_TEST_SERVERPORT = 25000;
         static const uint32_t DMCPConfig_TEST_CLIENTPORT = 25100;

        enum DMCP {
            CONNECTIONSERVER_PORT_BASE = 19755, // This is the base TCP port for connections.
            BROADCAST_PORT_SERVER = 19750,
            BROADCAST_PORT_CLIENT = 19751,
            CONNECTION_TIMEOUT = 1000,
            CONNECTION_RETRIES = 5,
            CONFIGURATION_TIMEOUT = 1000,
            DISCOVER_TIMEOUT = 1000,
            CONTROL_PORT_BASE = 10000,
        };
    }
} // hesperia::dmcp
#endif /*HESPERIA_DMCP_CONFIG_H_*/
