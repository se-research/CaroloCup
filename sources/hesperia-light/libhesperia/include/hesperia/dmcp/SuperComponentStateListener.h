/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_DMCP_SUPERCOMPONENTSTATELISTENER_H_
#define HESPERIA_DMCP_SUPERCOMPONENTSTATELISTENER_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

namespace hesperia {
    namespace dmcp {

        class HESPERIA_API SupercomponentStateListener {
            public:
                virtual ~SupercomponentStateListener() {};

                virtual void handleConnectionLost() = 0;
        };
    }
} // hesperia::dmcp

#endif /* HESPERIA_DMCP_SUPERCOMPONENTSTATELISTENER_H_ */
