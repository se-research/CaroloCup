/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_PACKETLISTENER_H_
#define HESPERIA_CORE_WRAPPER_PACKETLISTENER_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/wrapper/Packet.h"

namespace core {
    namespace wrapper {

        using namespace std;

        /**
         * This class provides an interface for getting informed
         * about new packets by a packet observer.
         */
        class HESPERIA_API PacketListener {
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

#endif /*HESPERIA_CORE_WRAPPER_PACKETLISTENER_H_*/
