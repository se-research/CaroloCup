/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_DMCP_CONFIG_H_
#define OPENDAVINCI_DMCP_CONFIG_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

namespace core {
    namespace dmcp {
        using namespace std;

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
} // core::dmcp
#endif /*OPENDAVINCI_DMCP_CONFIG_H_*/
