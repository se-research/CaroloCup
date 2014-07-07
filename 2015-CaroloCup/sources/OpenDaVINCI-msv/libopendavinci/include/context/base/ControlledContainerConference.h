/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef CONTEXT_BASE_CONTROLLEDCONTAINERCONFERENCE_H_
#define CONTEXT_BASE_CONTROLLEDCONTAINERCONFERENCE_H_

// native.h must be included right before boost/asio.hpp because of the definition of _WIN32_WINNT.
#include "core/native.h"
#include "core/data/Container.h"
#include "core/io/ContainerListener.h"
#include "context/base/SendContainerToSystemsUnderTest.h"

namespace context {
    namespace base {

        using namespace std;

        /**
         * This interface provides methods to get data sent to
         * ControlledContainerConference and to send data to a
         * ControlledContainerConference.
         */
        class OPENDAVINCI_API ControlledContainerConference : public SendContainerToSystemsUnderTest {
            public:
                virtual ~ControlledContainerConference();

                /**
                 * This method adds a ContainerListener to receive data from
                 * a system under test.
                 *
                 * @param cl ContainerListener to add.
                 */
                virtual void add(core::io::ContainerListener *cl) = 0;
        };

    }
} // context::base

#endif /*CONTEXT_BASE_CONTROLLEDCONTAINERCONFERENCE_H_*/
