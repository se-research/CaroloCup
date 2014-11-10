/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_STRINGSENDER_H_
#define OPENDAVINCI_CORE_WRAPPER_STRINGSENDER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

namespace core {
    namespace wrapper {

        using namespace std;

        class OPENDAVINCI_API StringSender {
            public:
                virtual ~StringSender();

                /**
                 * This method has to be implemented in subclasses
                 * to send data.
                 *
                 * param data Data to be sent.
                 */
                virtual void send(const string& data) = 0;
        };
    }
}

#endif /* OPENDAVINCI_CORE_WRAPPER_STRINGSENDER_H_ */
