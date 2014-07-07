/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_WRAPPER_PACKET_H_
#define OPENDAVINCI_CORE_WRAPPER_PACKET_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

namespace core {
    namespace wrapper {

        using namespace std;

        /**
         * This class provides information about a newly received packet.
         */
        class OPENDAVINCI_API Packet {
            public:
                Packet();

                /**
                 * Constructor.
                 *
                 * @param s Sender.
                 * @param d Data.
                 */
                Packet(const string &s, const string &d);

                virtual ~Packet();

                /**
                 * Copy constructor.
                 *
                 * @param obj Reference to an object of this class.
                 */
                Packet(const Packet &obj);

                /**
                 * Assignment operator.
                 *
                 * @param obj Reference to an object of this class.
                 * @return Reference to this instance.
                 */
                Packet& operator=(const Packet &obj);

                /**
                 * This method returns the sender.
                 *
                 * @return Sender.
                 */
                const string getSender() const;

                /**
                 * This method sets the sender.
                 *
                 * @param s Sender.
                 */
                void setSender(const string &s);

                /**
                 * This method returns the data.
                 *
                 * @return Data.
                 */
                const string getData() const;

                /**
                 * This method sets the data.
                 *
                 * @param d Data.
                 */
                void setData(const string &d);

            private:
                string m_sender;
                string m_data;
        };

    }
} // core::wrapper

#endif /*OPENDAVINCI_CORE_WRAPPER_PACKET_H_*/
