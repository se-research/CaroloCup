/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef CONTAINEROBSERVER_H_
#define CONTAINEROBSERVER_H_

#include "core/io/ContainerListener.h"

namespace monitor {

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

} // monitor

#endif /*CONTAINEROBSERVER_H_*/
