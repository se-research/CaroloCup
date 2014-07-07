/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_IO_CONTAINEROBSERVER_H_
#define OPENDAVINCI_CORE_IO_CONTAINEROBSERVER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/io/ContainerListener.h"

namespace core {
    namespace io {

        /**
         * This class provides an interface for registering
         * as a container listener at an observer.
         */
        class OPENDAVINCI_API ContainerObserver {
            public:
                virtual ~ContainerObserver();

                /**
                 * This method sets or unsets a container listener.
                 *
                 * @param cl ContainerListener to be set. If set to NULL, observing is suspended.
                 */
                virtual void setContainerListener(ContainerListener *cl) = 0;
        };

    }
} // core::io

#endif /*OPENDAVINCI_CORE_IO_CONTAINEROBSERVER_H_*/
