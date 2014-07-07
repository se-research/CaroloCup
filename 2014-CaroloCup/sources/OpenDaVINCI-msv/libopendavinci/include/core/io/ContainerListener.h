/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_IO_CONTAINERLISTENER_H_
#define OPENDAVINCI_CORE_IO_CONTAINERLISTENER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/data/Container.h"

namespace core {
    namespace io {

        /**
         * This class provides an interface for getting informed
         * about new containers by an observer.
         */
        class OPENDAVINCI_API ContainerListener {
            public:
                virtual ~ContainerListener();

                /**
                 * This method is called whenever a new container occurs.
                 *
                 * @param c Container that has been occured.
                 */
                virtual void nextContainer(data::Container &c) = 0;
        };

    }
} // core::io

#endif /*OPENDAVINCI_CORE_IO_CONTAINERLISTENER_H_*/
