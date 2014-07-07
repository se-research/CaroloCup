/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_CONNECTIONLISTENER_H_
#define OPENDAVINCI_CORE_WRAPPER_CONNECTIONLISTENER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

namespace core {
    namespace wrapper {

        using namespace std;

        /**
         * This class provides an interface to handle connection errors
         */
        class OPENDAVINCI_API ConnectionListener {
            public:
                virtual ~ConnectionListener();

                /**
                 * This method is called whenever an error occurs.
                 */
                virtual void handleConnectionError() = 0;
        };

    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_CONNECTIONLISTENER_H_*/
