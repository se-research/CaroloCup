/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_IO_CONNECTIONERRORLISTENER
#define OPENDAVINCI_CORE_IO_CONNECTIONERRORLISTENER

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

namespace core {
    namespace io {

        class ConnectionErrorListener {
            public:
                virtual ~ConnectionErrorListener() {};

                virtual void handleConnectionError() = 0;
        };
    }
}
#endif //OPENDAVINCI_CORE_IO_CONNECTIONERRORLISTENER
