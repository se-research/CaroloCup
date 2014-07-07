/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_PARTIALSTRINGRECEIVER_H_
#define OPENDAVINCI_CORE_WRAPPER_PARTIALSTRINGRECEIVER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

namespace core {
    namespace wrapper {

        using namespace std;

        class OPENDAVINCI_API PartialStringReceiver {
            public:
                virtual ~PartialStringReceiver();

                /**
                 * This method handles data which was received by a connection.
                 *
                 * param partialData Data that has been received and should be handled by a protocol.
                 */
                virtual void receivedPartialString(const string &partialData) = 0;
        };
    }
}

#endif /* OPENDAVINCI_CORE_WRAPPER_PARTIALSTRINGRECEIVER_H_ */
