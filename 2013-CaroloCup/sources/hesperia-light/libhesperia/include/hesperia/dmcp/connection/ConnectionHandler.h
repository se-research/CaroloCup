/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_DMCP_CONNECTION_CONNECTIONHANDLER_H_
#define HESPERIA_DMCP_CONNECTION_CONNECTIONHANDLER_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "hesperia/dmcp/connection/ModuleConnection.h"

namespace hesperia {
    namespace dmcp {
        namespace connection {

            class HESPERIA_API ConnectionHandler {
                public:
                    virtual ~ConnectionHandler() {};

                    virtual void onNewModule(ModuleConnection* mc) = 0;
            };
        }
    }
} // hesperia::dmcp

#endif /* HESPERIA_DMCP_CONNECTION_CONNECTIONLISTENER_H_ */
