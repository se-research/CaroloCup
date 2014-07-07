/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_PACKETOBSERVER_H_
#define OPENDAVINCI_CORE_WRAPPER_PACKETOBSERVER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/PacketListener.h"

namespace core {
    namespace wrapper {

        using namespace std;

        /**
         * This class provides an interface for registering
         * as a packet listener at a packet observer.
         */
        class OPENDAVINCI_API PacketObserver {
            public:
                virtual ~PacketObserver();

                /**
                 * This method sets or sets a packet listener.
                 *
                 * @param pl PacketListener to be set. If set to NULL, observing is suspended.
                 */
                virtual void setPacketListener(PacketListener *pl) = 0;
        };

    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_PACKETOBSERVER_H_*/
