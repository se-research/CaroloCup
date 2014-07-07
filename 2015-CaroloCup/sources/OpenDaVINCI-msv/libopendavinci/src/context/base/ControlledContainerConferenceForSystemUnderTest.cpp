/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include <iostream>

#include "core/data/Container.h"
#include "core/data/TimeStamp.h"

#include "context/base/ControlledContainerConferenceForSystemUnderTest.h"

namespace context {
    namespace base {

        using namespace std;
        using namespace core::base;
        using namespace core::data;
        using namespace core::io;

        ControlledContainerConferenceForSystemUnderTest::ControlledContainerConferenceForSystemUnderTest(const string &address, const uint32_t &port, BlockableContainerReceiver &bcl, ContainerObserver &receiveFromObserver) :
            m_sendToListener(bcl),
            m_receiveFromObserver(receiveFromObserver) {
            clog << "ControlledContainerConferenceForSystemUnderTest: " << address << ":" << port << endl;

            // Register ourselves as ContainerListener.
            m_receiveFromObserver.setContainerListener(this);
        }

        ControlledContainerConferenceForSystemUnderTest::~ControlledContainerConferenceForSystemUnderTest() {
            // Deregister ourselves as ContainerListener.
            m_receiveFromObserver.setContainerListener(NULL);
        }

        void ControlledContainerConferenceForSystemUnderTest::send(Container &container) const {
            // Set sending time stamp.
            container.setSentTimeStamp(TimeStamp());

            clog << "Sending '" << container.toString() << "' in ControlledContainerConferenceForSystemUnderTest." << endl;

            m_sendToListener.nextContainer(container);
        }

        BlockableContainerReceiver& ControlledContainerConferenceForSystemUnderTest::getBlockableContainerReceiver() {
            return m_sendToListener;
        }

        void ControlledContainerConferenceForSystemUnderTest::nextContainer(Container &c) {
            c.setReceivedTimeStamp(TimeStamp());

            // Actually receive Container.
            receive(c);
        }

    }
} // context::base
