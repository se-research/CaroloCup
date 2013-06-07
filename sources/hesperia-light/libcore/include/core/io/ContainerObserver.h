/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_IO_CONTAINEROBSERVER_H_
#define HESPERIA_CORE_IO_CONTAINEROBSERVER_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/io/ContainerListener.h"

namespace core {
    namespace io {

        /**
         * This class provides an interface for registering
         * as a container listener at an observer.
         */
        class HESPERIA_API ContainerObserver {
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

#endif /*HESPERIA_CORE_IO_CONTAINEROBSERVER_H_*/
