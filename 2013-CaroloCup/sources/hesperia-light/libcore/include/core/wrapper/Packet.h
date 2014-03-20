/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_WRAPPER_PACKET_H_
#define HESPERIA_CORE_WRAPPER_PACKET_H_

#include <string>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

namespace core {
    namespace wrapper {

        using namespace std;

        /**
         * This class provides information about a newly received packet.
         */
        class HESPERIA_API Packet {
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

#endif /*HESPERIA_CORE_WRAPPER_PACKET_H_*/
