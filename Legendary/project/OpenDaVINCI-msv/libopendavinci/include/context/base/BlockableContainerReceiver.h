/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef CONTEXT_BASE_BLOCKABLECONTAINERRECEIVER_H_
#define CONTEXT_BASE_BLOCKABLECONTAINERRECEIVER_H_

#include <vector>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"
#include "core/base/BufferedFIFOQueue.h"
#include "core/io/ContainerListener.h"
#include "context/base/BlockableContainerListener.h"

namespace context {
    namespace base {

        using namespace std;

        /**
         * This class realizes a blockable container receiver which can
         * blocked from sending. This class is used for every registered
         * system under test.
         */
        class OPENDAVINCI_API BlockableContainerReceiver : public BlockableContainerListener {
            private:
                enum THREAD_YIELDING {
                    YIELDING_TIME = 100,
                };

            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                BlockableContainerReceiver(const BlockableContainerReceiver&);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                BlockableContainerReceiver& operator=(const BlockableContainerReceiver&);

            public:
                /**
                 * Constructor.
                 *
                 * @param cl ContainerListener for finally receiving Containers from the System Under Test.
                 */
                BlockableContainerReceiver(core::io::ContainerListener &cl);

                virtual ~BlockableContainerReceiver();

                // This method is called by ControlledContainerConference to send c from an app to all SystemParts.
                virtual void nextContainer(core::data::Container &c);

            private:
                // This ContainerListener receives the containers sent from the System Under Test to which this BlockableContainerReceiver belongs to all SystemParts and all other Systems Under Test.
                core::io::ContainerListener &m_dispatcherForContainersSentFromSystemUnderTest;
        };

    }
} // context::base

#endif /*CONTEXT_BASE_BLOCKABLECONTAINERRECEIVER_H_*/
