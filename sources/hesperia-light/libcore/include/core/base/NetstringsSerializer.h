/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#ifndef HESPERIA_CORE_BASE_NETSTRINGSSERIALIZER_H_
#define HESPERIA_CORE_BASE_NETSTRINGSSERIALIZER_H_

#include <iostream>
#include <sstream>
#include <string>

// native.h must be included as first header file for definition of _WIN32_WINNT.
#include "core/native.h"

#include "core/base/Serializer.h"

namespace core {
    namespace base {

        using namespace std;

        class SerializationFactory;

        /**
         * This class implements the interface Serializer for
         * Netstrings. The original version (found at:
         * http://cr.yp.to/proto/netstrings.txt ) has been modified:
         *
         * 'binary length (as uint32_t)' ':' 'PAYLOAD' ','
         *
         * @See Serializable
         */
        class HESPERIA_API NetstringsSerializer : public Serializer {
            protected:
                // Only the SerializationFactory or its subclasses are allowed to create instances of this Serializer.
                friend class SerializationFactory;

                /**
                 * Constructor.
                 *
                 * @param out Output stream for the data.
                 */
                NetstringsSerializer(ostream &out);

            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                NetstringsSerializer(const NetstringsSerializer &);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                NetstringsSerializer& operator=(const NetstringsSerializer &);

            public:
                virtual ~NetstringsSerializer();

                virtual void write(const uint32_t id, const Serializable &s);

                virtual void write(const uint32_t id, const bool &b);

                virtual void write(const uint32_t id, const char &c);

                virtual void write(const uint32_t id, const unsigned char &uc);

                virtual void write(const uint32_t id, const int32_t &i);

                virtual void write(const uint32_t id, const uint32_t &ui);

                virtual void write(const uint32_t id, const float &f);

                virtual void write(const uint32_t id, const double &d);

                virtual void write(const uint32_t id, const string &s);

                virtual void write(const uint32_t id, const void *data, const uint32_t &size);

            private:
                ostream &m_out;
                stringstream m_buffer;
        };

    }
} // core::base

#endif /*HESPERIA_CORE_BASE_NETSTRINGSSERIALIZER_H_*/
