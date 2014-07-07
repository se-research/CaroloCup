/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_IO_CONNECTIONACCEPTORLISTENER_H_
#define OPENDAVINCI_CORE_IO_CONNECTIONACCEPTORLISTENER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/io/Connection.h"

namespace core {
    namespace io {

        class OPENDAVINCI_API ConnectionAcceptorListener {
            public:
                virtual ~ConnectionAcceptorListener() {};

                virtual void onNewConnection(Connection* connection) = 0;
        };
    }
}

#endif /* CONNECTIONACCEPTORLISTENER_H_ */
