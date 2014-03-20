/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_BASE_QUERYABLENETSTRINGSDESERIALIZER_H_
#define OPENDAVINCI_CORE_BASE_QUERYABLENETSTRINGSDESERIALIZER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/base/Deserializer.h"

namespace core {
    namespace base {

        using namespace std;

        class SerializationFactory;

        /**
         * This class implements the interface Deserializer for queryable
         * Netstrings. The original version (found at:
         * http://cr.yp.to/proto/netstrings.txt ) has been modified:
         *
         * '0xAA' '0xCF' 'binary length (as uint32_t)' 'PAYLOAD' ','
         *
         * @See Serializable
         */
        class OPENDAVINCI_API QueryableNetstringsDeserializer : public Deserializer {
            private:
                // Only the SerializationFactory or its subclasses are allowed to create instances of this Deserializer.
                friend class SerializationFactory;

                /**
                 * Constructor.
                 *
                 * @param in Input stream for the data.
                 */
                QueryableNetstringsDeserializer(istream &in);

            private:
                /**
                 * Forbidden default constructor.
                 */
                QueryableNetstringsDeserializer();

                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                QueryableNetstringsDeserializer(const QueryableNetstringsDeserializer &);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                QueryableNetstringsDeserializer& operator=(const QueryableNetstringsDeserializer &);


            public:
                virtual ~QueryableNetstringsDeserializer();

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
                stringstream m_buffer;
                map<uint32_t, streampos> m_values;
        };

    }
} // core::base

#endif /*OPENDAVINCI_CORE_BASE_QUERYABLENETSTRINGSDESERIALIZER_H_*/
