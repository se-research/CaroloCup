/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef CONTEXT_BASE_CONTROLLEDCONTAINERCONFERENCEFORSYSTEMUNDERTEST_H_
#define CONTEXT_BASE_CONTROLLEDCONTAINERCONFERENCEFORSYSTEMUNDERTEST_H_

// native.h must be included right before boost/asio.hpp because of the definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/data/Container.h"
#include "core/io/ContainerConference.h"
#include "core/io/ContainerListener.h"
#include "core/io/ContainerObserver.h"
#include "context/base/BlockableContainerReceiver.h"

namespace context {
    namespace base {

        using namespace std;

        class ControlledContainerConferenceFactory;

        /**
         * This class provides a controlled container conference.
         */
        class OPENDAVINCI_API ControlledContainerConferenceForSystemUnderTest : public core::io::ContainerConference, public core::io::ContainerListener {
            private:
                friend class ControlledContainerConferenceFactory;

            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                ControlledContainerConferenceForSystemUnderTest(const ControlledContainerConferenceForSystemUnderTest &);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                ControlledContainerConferenceForSystemUnderTest& operator=(const ControlledContainerConferenceForSystemUnderTest &);

            protected:
                /**
                 * Constructor.
                 *
                 * @param address Create controlled container conference for this address.
                 * @param port Create controlled container conference for this port.
                 * @param bcl BlockableContainerListener to which we send our Containers.
                 * @param receiveFromObserver ContainerObserver which delivers incoming Containers to us.
                 */
                ControlledContainerConferenceForSystemUnderTest(const string &address, const uint32_t &port, BlockableContainerReceiver &bcl, core::io::ContainerObserver &receiveFromObserver);

            public:
                virtual ~ControlledContainerConferenceForSystemUnderTest();

                virtual void send(core::data::Container &container) const;

                virtual void nextContainer(core::data::Container &c);

                /**
                 * This method returns the BlockableContainerReceiver.
                 *
                 * @return BlockableContainerReceiver to which we send our data.
                 */
                BlockableContainerReceiver& getBlockableContainerReceiver();

            private:
                BlockableContainerReceiver &m_sendToListener;
                core::io::ContainerObserver &m_receiveFromObserver;
        };

    }
} // context::base

#endif /*CONTEXT_BASE_CONTROLLEDCONTAINERCONFERENCEFORSYSTEMUNDERTEST_H_*/
