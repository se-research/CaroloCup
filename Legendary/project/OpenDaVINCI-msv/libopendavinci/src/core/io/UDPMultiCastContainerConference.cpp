/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/Lock.h"
#include "core/data/Container.h"
#include "core/data/TimeStamp.h"
#include "core/io/UDPMultiCastContainerConference.h"
#include "core/wrapper/UDPFactory.h"

namespace core {
    namespace io {

        using namespace std;
        using namespace base;
        using namespace data;
        using namespace exceptions;

        UDPMultiCastContainerConference::UDPMultiCastContainerConference(const string &address, const uint32_t &port) throw (ConferenceException) :
                m_sender(NULL),
                m_receiver(NULL) {
            try {
                m_sender = wrapper::UDPFactory::createUDPSender(address, port);
            } catch (string &s) {
                OPENDAVINCI_CORE_THROW_EXCEPTION(ConferenceException, s);
            }

            try {
                m_receiver = wrapper::UDPFactory::createUDPReceiver(address, port);
            } catch (string &s) {
                OPENDAVINCI_CORE_THROW_EXCEPTION(ConferenceException, s);
            }

            // Register ourselves as string listeners.
            m_receiver->setStringListener(this);

            // Start receiving.
            m_receiver->start();
        }

        UDPMultiCastContainerConference::~UDPMultiCastContainerConference() {
            // Stop receiving.
            m_receiver->stop();

            // Unregister ourselves.
            m_receiver->setStringListener(NULL);

            OPENDAVINCI_CORE_DELETE_POINTER(m_sender);
            OPENDAVINCI_CORE_DELETE_POINTER(m_receiver);
        }

        void UDPMultiCastContainerConference::nextString(const string &s) {
            if (hasContainerListener()) {
                stringstream stringstreamData(s);
                Container container;
                stringstreamData >> container;
                container.setReceivedTimeStamp(TimeStamp());

                // Use superclass to distribute any received containers.
                receive(container);
            }
        }

        void UDPMultiCastContainerConference::send(Container &container) const {
            // Set sending time stamp.
            container.setSentTimeStamp(TimeStamp());

            stringstream stringstreamValue;
            stringstreamValue << container;
            string stringValue = stringstreamValue.str();

            // Send data.
            m_sender->send(stringValue);
        }

    }
} // core::io
