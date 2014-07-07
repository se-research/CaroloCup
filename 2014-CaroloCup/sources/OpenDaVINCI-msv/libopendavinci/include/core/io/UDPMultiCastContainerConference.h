/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_IO_UDPMULTICASTCONTAINERCONFERENCE_H_
#define OPENDAVINCI_CORE_IO_UDPMULTICASTCONTAINERCONFERENCE_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/exceptions/Exceptions.h"
#include "core/io/ContainerConference.h"
#include "core/wrapper/StringListener.h"
#include "core/wrapper/UDPSender.h"
#include "core/wrapper/UDPReceiver.h"

namespace core {
    namespace io {

        using namespace std;

        /**
         * This class encapsulates a conference about containers.
         * Therefore, it uses UDP multicast sending and receiving for
         * sending and receiving containers. Therefore, it implements
         * a StringListener for getting informed about new strings from
         * the UDPReceiver and informs any connected ContainerListener.
         */
        class OPENDAVINCI_API UDPMultiCastContainerConference : public ContainerConference, public wrapper::StringListener {
            private:
                friend class ContainerConferenceFactory;

            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                UDPMultiCastContainerConference(const UDPMultiCastContainerConference &);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                UDPMultiCastContainerConference& operator=(const UDPMultiCastContainerConference &);

            protected:
                /**
                 * Constructor.
                 *
                 * @param address Use address for joining.
                 * @param port Use port for joining.
                 * @throws ConferenceException if the conference could not be created.
                 */
                UDPMultiCastContainerConference(const string &address, const uint32_t &port) throw (exceptions::ConferenceException);

            public:
                virtual ~UDPMultiCastContainerConference();

                virtual void nextString(const string &s);

                virtual void send(core::data::Container &container) const;

            private:
                wrapper::UDPSender *m_sender;
                wrapper::UDPReceiver *m_receiver;
        };

    }
} // core::io

#endif /*OPENDAVINCI_CORE_IO_UDPMULTICASTCONTAINERCONFERENCE_H_*/
