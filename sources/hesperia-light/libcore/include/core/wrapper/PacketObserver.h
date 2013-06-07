/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_PACKETOBSERVER_H_
#define HESPERIA_CORE_WRAPPER_PACKETOBSERVER_H_

#include "core/native.h"

#include "core/wrapper/PacketListener.h"

namespace core {
    namespace wrapper {

        using namespace std;

        /**
         * This class provides an interface for registering
         * as a packet listener at a packet observer.
         */
        class HESPERIA_API PacketObserver {
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

#endif /*HESPERIA_CORE_WRAPPER_PACKETOBSERVER_H_*/
