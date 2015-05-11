/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_BASE_LCMDESERIALIZER_H_
#define OPENDAVINCI_CORE_BASE_LCMDESERIALIZER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/base/Deserializer.h"
#include "core/data/Container.h"

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
        class OPENDAVINCI_API LCMDeserializer : public Deserializer {
            private:
                // Only the SerializationFactory or its subclasses are allowed to create instances of this Deserializer.
                friend class SerializationFactory;

                /**
                 * Constructor.
                 *
                 * @param in Input stream for the data.
                 */
                LCMDeserializer(istream &in);

            private:
                /**
                 * Forbidden default constructor.
                 */
                LCMDeserializer();

                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                LCMDeserializer(const LCMDeserializer &);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                LCMDeserializer& operator=(const LCMDeserializer &);


            public:
                virtual ~LCMDeserializer();
                
                /*
                 * The read functions below are called to decode and get the variables from the payload.
                 * The variables must be read in the order they were written.
                 * 
                 * For single byte variables, they are just read without any decoding.
                 * For others, the variable is read into a uint8_t buffer and then decoded.
                 * The decoding procedure is the encoding procedure backwards.
                 */
                
                virtual void read(const uint32_t id, Serializable &s);

                virtual void read(const uint32_t id, bool &b);

                virtual void read(const uint32_t id, char &c);

                virtual void read(const uint32_t id, unsigned char &uc);

                virtual void read(const uint32_t id, int32_t &i);

                virtual void read(const uint32_t id, uint32_t &ui);
                
                void read(const uint32_t id, int64_t &i);

                virtual void read(const uint32_t id, float &f);

                virtual void read(const uint32_t id, double &d);

                virtual void read(const uint32_t id, string &s);

                virtual void read(const uint32_t id, void *data, uint32_t size);

                void read(istream &in, core::data::Container &container );

            private:
                stringstream m_buffer; // Buffer where the payload is stored to then get decoded
        };

    }
} // core::base

#endif /*OPENDAVINCI_CORE_BASE_LCMDESERIALIZER_H_*/
