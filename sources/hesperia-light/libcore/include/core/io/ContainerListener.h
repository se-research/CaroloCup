/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_IO_CONTAINERLISTENER_H_
#define HESPERIA_CORE_IO_CONTAINERLISTENER_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/data/Container.h"

namespace core {
    namespace io {

        /**
         * This class provides an interface for getting informed
         * about new containers by an observer.
         */
        class HESPERIA_API ContainerListener {
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

#endif /*HESPERIA_CORE_IO_CONTAINERLISTENER_H_*/
