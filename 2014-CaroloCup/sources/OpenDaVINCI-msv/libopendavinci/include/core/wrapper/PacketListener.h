/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_PACKETLISTENER_H_
#define OPENDAVINCI_CORE_WRAPPER_PACKETLISTENER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/wrapper/Packet.h"

namespace core {
    namespace wrapper {

        using namespace std;

        /**
         * This class provides an interface for getting informed
         * about new packets by a packet observer.
         */
        class OPENDAVINCI_API PacketListener {
            public:
                virtual ~PacketListener();

                /**
                 * This method is called whenever a new packet occurs.
                 *
                 * @param p Packet that has been occurred.
                 */
                virtual void nextPacket(const Packet &p) = 0;
        };

    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_PACKETLISTENER_H_*/
