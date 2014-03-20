/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_DMCP_SUPERCOMPONENTSTATELISTENER_H_
#define OPENDAVINCI_DMCP_SUPERCOMPONENTSTATELISTENER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

namespace core {
    namespace dmcp {

        class OPENDAVINCI_API SupercomponentStateListener {
            public:
                virtual ~SupercomponentStateListener() {};

                virtual void handleConnectionLost() = 0;
        };
    }
} // core::dmcp

#endif /* OPENDAVINCI_DMCP_SUPERCOMPONENTSTATELISTENER_H_ */
