/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_CONNECTIONOBSERVER_H_
#define HESPERIA_CORE_WRAPPER_CONNECTIONOBSERVER_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/wrapper/ConnectionListener.h"

namespace core {
    namespace wrapper {

        using namespace std;

        /**
         * This class provides an interface for registering
         * an ConnectionListener to handle connection error
         */
        class HESPERIA_API ConnectionObserver {
            public:
                virtual ~ConnectionObserver();

                /**
                 * This method sets or unsets a connection listener.
                 *
                 * @param sl Connection listener to be set. If set to NULL, error handling is suspended.
                 */
                virtual void setConnectionListener(ConnectionListener *cl) = 0;
        };

    }
} // core::wrapper

#endif /*HESPERIA_CORE_WRAPPER_CONNECTIONOBSERVER_H_*/
