/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_IO_CONNECTIONACCEPTORLISTENER_H_
#define HESPERIA_CORE_IO_CONNECTIONACCEPTORLISTENER_H_

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/io/Connection.h"

namespace core {
    namespace io {

        class HESPERIA_API ConnectionAcceptorListener {
            public:
                virtual ~ConnectionAcceptorListener() {};

                virtual void onNewConnection(Connection* connection) = 0;
        };
    }
}

#endif /* CONNECTIONACCEPTORLISTENER_H_ */
