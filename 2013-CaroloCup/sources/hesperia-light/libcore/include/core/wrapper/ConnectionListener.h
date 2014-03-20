/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_CONNECTIONLISTENER_H_
#define HESPERIA_CORE_WRAPPER_CONNECTIONLISTENER_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

namespace core {
    namespace wrapper {

        using namespace std;

        /**
         * This class provides an interface to handle connection errors
         */
        class HESPERIA_API ConnectionListener {
            public:
                virtual ~ConnectionListener();

                /**
                 * This method is called whenever a new string occurs.
                 */
                virtual void handleConnectionError() = 0;
        };

    }
} // core::wrapper

#endif /*HESPERIA_CORE_WRAPPER_CONNECTIONLISTENER_H_*/
