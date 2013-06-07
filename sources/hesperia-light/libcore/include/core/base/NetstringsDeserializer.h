/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_BASE_NETSTRINGSDESERIALIZER_H_
#define HESPERIA_CORE_BASE_NETSTRINGSDESERIALIZER_H_

#include <iostream>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/base/Deserializer.h"

namespace core {
    namespace base {

        using namespace std;

        class SerializationFactory;

        /**
         * This class implements the interface Deserializer for
         * Netstrings. The original version (found at:
         * http://cr.yp.to/proto/netstrings.txt ) has been modified:
         *
         * 'binary length (as uint32_t)' ':' 'PAYLOAD' ','
         *
         * @See Serializable
         */
        class HESPERIA_API NetstringsDeserializer : public Deserializer {
            protected:
                // Only the SerializationFactory or its subclasses are allowed to create instances of this Deserializer.
                friend class SerializationFactory;

                /**
                 * Constructor.
                 *
                 * @param in Input stream for the data.
                 */
                NetstringsDeserializer(istream &in);

            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                NetstringsDeserializer(const NetstringsDeserializer &);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 *
                 * @param obj Reference to an object of this class.
                 * @return Reference to this instance.
                 */
                NetstringsDeserializer& operator=(const NetstringsDeserializer &);

            public:
                virtual ~NetstringsDeserializer();

                virtual void read(const uint32_t id, Serializable &s);

                virtual void read(const uint32_t id, bool &b);

                virtual void read(const uint32_t id, char &c);

                virtual void read(const uint32_t id, unsigned char &uc);

                virtual void read(const uint32_t id, int32_t &i);

                virtual void read(const uint32_t id, uint32_t &ui);

                virtual void read(const uint32_t id, float &f);

                virtual void read(const uint32_t id, double &d);

                virtual void read(const uint32_t id, string &s);

                virtual void read(const uint32_t id, void *data, uint32_t size);

            private:
                istream &m_in;
        };

    }
} // core::base

#endif /*HESPERIA_CORE_BASE_NETSTRINGSDESERIALIZER_H_*/
