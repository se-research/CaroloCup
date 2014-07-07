/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef CONTEXT_BASE_SENDCONTAINERTOSYSTEMSUNDERTEST_H_
#define CONTEXT_BASE_SENDCONTAINERTOSYSTEMSUNDERTEST_H_

// native.h must be included right before boost/asio.hpp because of the definition of _WIN32_WINNT.
#include "core/native.h"
#include "core/data/Container.h"
#include "core/io/ContainerListener.h"

namespace context {
    namespace base {

        using namespace std;

        /**
         * This interface provides methods to send data to a
         * ControlledContainerConferenceForSystemsUnderTest.
         */
        class OPENDAVINCI_API SendContainerToSystemsUnderTest {
            public:
                virtual ~SendContainerToSystemsUnderTest();

                /**
                 * This method sends a Container to all created
                 * ControlledContainerConferences and thus to all
                 * running systems under test.
                 *
                 * @param c Container to send.
                 */
                virtual void sendToSystemsUnderTest(core::data::Container &c) = 0;
        };

    }
} // context::base

#endif /*CONTEXT_BASE_SENDCONTAINERTOSYSTEMSUNDERTEST_H_*/
