/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/Thread.h"
#include "core/data/TimeStamp.h"
#include "context/base/BlockableContainerReceiver.h"

namespace context {
    namespace base {

        using namespace std;
        using namespace core::base;
        using namespace core::data;

        BlockableContainerReceiver::BlockableContainerReceiver(core::io::ContainerListener &cl) :
            m_dispatcherForContainersSentFromSystemUnderTest(cl) {}

        BlockableContainerReceiver::~BlockableContainerReceiver() {
            // Break blocking.
            setNextContainerAllowed(true);
        }

        void BlockableContainerReceiver::nextContainer(Container &c) {
            while (!isNextContainerAllowed()) {
                Thread::usleep(BlockableContainerReceiver::YIELDING_TIME);
            }

            // Set received TimeStamp.
            c.setReceivedTimeStamp(TimeStamp());

            // Delegate Containter to dispatcher.
            m_dispatcherForContainersSentFromSystemUnderTest.nextContainer(c);
        }

    }
} // context::base
