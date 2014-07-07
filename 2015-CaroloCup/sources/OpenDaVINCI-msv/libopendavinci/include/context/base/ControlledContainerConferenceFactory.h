/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef CONTEXT_BASE_CONTROLLEDCONTAINERCONFERENCEFACTORY_H_
#define CONTEXT_BASE_CONTROLLEDCONTAINERCONFERENCEFACTORY_H_

#include <string>
#include <vector>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/base/Mutex.h"
#include "core/data/Container.h"
#include "core/io/ContainerConference.h"
#include "core/io/ContainerConferenceFactory.h"
#include "core/io/ContainerListener.h"
#include "context/base/BlockableContainerReceiver.h"
#include "context/base/ContainerDeliverer.h"
#include "context/base/ControlledContainerConference.h"

namespace context {
    namespace base {

        using namespace std;

        /**
         * This class provides controlled ContainerConferences.
         */
        class OPENDAVINCI_API ControlledContainerConferenceFactory : public core::io::ContainerConferenceFactory, public ControlledContainerConference, public core::io::ContainerListener {
            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                ControlledContainerConferenceFactory(const ControlledContainerConferenceFactory &);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                ControlledContainerConferenceFactory& operator=(const ControlledContainerConferenceFactory &);

            public:
                ControlledContainerConferenceFactory();

                virtual ~ControlledContainerConferenceFactory();

                /**
                 * This method returns a new ContainerConference.
                 *
                 * @param address Use address for joining.
                 * @param port Use port for joining.  If omitted, MULTICAST_PORT will be used.
                 * @return ContainerConference or NULL.
                 */
                virtual core::io::ContainerConference* getContainerConference(const string &address, const uint32_t &port = core::io::ContainerConferenceFactory::MULTICAST_PORT);

                virtual void sendToSystemsUnderTest(core::data::Container &c);

                virtual void add(core::io::ContainerListener *cl);

                // This method is called by BlockableContainerReceiver to send data FROM a specific System Under Test to all SystemParts.
                // Furthermore, every container send from a System Under Test is also dispatched to all Systems Under Test using sendToSystemsUnderTest
                virtual void nextContainer(core::data::Container &c);

            private:
                /**
                 * This method sends the given container to all systems under test
                 * without modifying the sent timestamp.
                 *
                 * @param c Container to send.
                 */
                void sendToSUD(core::data::Container &c);

                /**
                 * This method sends the given container to all SystemContextComponents
                 * without modifying the sent timestamp.
                 *
                 * @param c Container to send.
                 */
                void sendToSCC(core::data::Container &c);

            private:
                core::base::Mutex m_listOfContainerListenersToReceiveContainersFromSystemsUnderTestMutex;
                vector<core::io::ContainerListener*> m_listOfContainerListenersToReceiveContainersFromSystemsUnderTest;

                // Using ContainerDeliverers, synchronous communication is enforced.
                core::base::Mutex m_listOfContainerDelivererToSystemUnderTestMutex;
                vector<ContainerDeliverer*> m_listOfContainerDelivererToSystemUnderTest;

                core::base::Mutex m_listOfContainerDelivererFromSystemUnderTestMutex;
                vector<BlockableContainerReceiver*> m_listOfContainerDelivererFromSystemUnderTest;
        };

    }
} // context::base

#endif /*CONTEXT_BASE_CONTROLLEDCONTAINERCONFERENCEFACTORY_H_*/
