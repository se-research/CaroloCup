/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_BASE_QUERYABLENETSTRINGSSERIALIZER_H_
#define OPENDAVINCI_CORE_BASE_QUERYABLENETSTRINGSSERIALIZER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/base/Serializer.h"

namespace core {
    namespace base {

        using namespace std;

        class SerializationFactory;

        /**
         * This class implements the interface Serializer for queryable
         * Netstrings. The original version (found at:
         * http://cr.yp.to/proto/netstrings.txt ) has been modified:
         *
         * '0xAA' '0xCF' 'binary length (as uint32_t)' 'PAYLOAD' ','
         *
         * @See Serializable
         */
        class QueryableNetstringsSerializer : public Serializer {
            private:
                // Only the SerializationFactory or its subclasses are allowed to create instances of this Deserializer.
                friend class SerializationFactory;

                /**
                 * Constructor.
                 *
                 * @param out Output stream for the data.
                 */
                QueryableNetstringsSerializer(ostream &out);

            private:
                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                QueryableNetstringsSerializer(const QueryableNetstringsSerializer &);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                QueryableNetstringsSerializer& operator=(const QueryableNetstringsSerializer &);

            public:
                virtual ~QueryableNetstringsSerializer();

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

#endif /*OPENDAVINCI_CORE_BASE_QUERYABLENETSTRINGSSERIALIZER_H_*/
