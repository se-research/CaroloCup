/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_CONNECTIONOBSERVER_H_
#define OPENDAVINCI_CORE_WRAPPER_CONNECTIONOBSERVER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/ConnectionListener.h"

namespace core {
    namespace wrapper {

        using namespace std;

        /**
         * This class provides an interface for registering
         * an ConnectionListener to handle connection error
         */
        class OPENDAVINCI_API ConnectionObserver {
            public:
                virtual ~ConnectionObserver();

                /**
                 * This method sets or unsets a connection listener.
                 *
                 * @param sl Connection listener to be set. If set to NULL, error handling is suspended.
                 */
                virtual void setConnectionListener(ConnectionListener *cl) = 0;
        };

    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_CONNECTIONOBSERVER_H_*/
