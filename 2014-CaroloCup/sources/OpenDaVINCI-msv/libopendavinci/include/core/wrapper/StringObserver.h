/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_STRINGOBSERVER_H_
#define OPENDAVINCI_CORE_WRAPPER_STRINGOBSERVER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/StringListener.h"

namespace core {
    namespace wrapper {

        using namespace std;

        /**
         * This class provides an interface for registering
         * as a string listener at a string observer.
         */
        class OPENDAVINCI_API StringObserver {
            public:
                virtual ~StringObserver();

                /**
                 * This method sets or unsets a string listener.
                 *
                 * @param sl StringListener to be set. If set to NULL, observing is suspended.
                 */
                virtual void setStringListener(StringListener *sl) = 0;
        };

    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_STRINGOBSERVER_H_*/
