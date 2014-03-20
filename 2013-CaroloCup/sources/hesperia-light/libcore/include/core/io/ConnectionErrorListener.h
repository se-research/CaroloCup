/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_IO_CONNECTIONERRORLISTENER
#define HESPERIA_CORE_IO_CONNECTIONERRORLISTENER

#include "core/native.h"

namespace core {
    namespace io {

        class ConnectionErrorListener {
            public:
                virtual ~ConnectionErrorListener() {};

                virtual void handleConnectionError() = 0;
        };
    }
}
#endif //HESPERIA_CORE_IO_CONNECTIONERRORLISTENER
