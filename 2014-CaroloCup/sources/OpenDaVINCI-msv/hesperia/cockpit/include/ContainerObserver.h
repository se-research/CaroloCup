/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef CONTAINEROBSERVER_H_
#define CONTAINEROBSERVER_H_

#ifdef PANDABOARD
#include <stdc-predef.h>
#endif

#include "core/io/ContainerListener.h"

namespace cockpit {

    using namespace std;

    /**
     * This interface manages multiple ContainerListeners.
     */
    class ContainerObserver {
        public:
            virtual ~ContainerObserver();

            /**
             * This method adds a container listener.
             *
             * @param containerListener ContainerListener to be added.
             */
            virtual void addContainerListener(core::io::ContainerListener *containerListener) = 0;

            /**
             * This method removes a container listener.
             *
             * @param containerListener ContainerListener to be removed.
             */
            virtual void removeContainerListener(core::io::ContainerListener *containerListener) = 0;
    };

} // cockpit

#endif /*CONTAINEROBSERVER_H_*/
