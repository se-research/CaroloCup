/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_DMCP_CONNECTION_CONNECTIONHANDLER_H_
#define OPENDAVINCI_DMCP_CONNECTION_CONNECTIONHANDLER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/dmcp/connection/ModuleConnection.h"

namespace core {
    namespace dmcp {
        namespace connection {

            class OPENDAVINCI_API ConnectionHandler {
                public:
                    virtual ~ConnectionHandler() {};

                    virtual void onNewModule(ModuleConnection* mc) = 0;
            };
        }
    }
} // core::dmcp

#endif /* OPENDAVINCI_DMCP_CONNECTION_CONNECTIONLISTENER_H_ */
