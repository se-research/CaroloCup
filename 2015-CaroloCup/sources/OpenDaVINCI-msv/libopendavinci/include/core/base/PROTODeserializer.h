/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#ifndef OPENDAVINCI_CORE_BASE_PROTODESERIALIZER_H_
#define OPENDAVINCI_CORE_BASE_PROTODESERIALIZER_H_

// core/platform.h must be included to setup platform-dependent header files and configurations.
#include "core/platform.h"

#include "core/base/Deserializer.h"
#include "core/data/Container.h"


namespace core {
    namespace base {

        using namespace std;

        class SerializationFactory;


        class OPENDAVINCI_API PROTODeserializer : public Deserializer {
            private:
                // Only the SerializationFactory or its subclasses are allowed to create instances of this Deserializer.
                friend class SerializationFactory;

                /**
                 * Constructor.
                 *
                 * @param in Input stream for the data.
                 */
                PROTODeserializer(istream &in);

            private:
                /**
                 * Forbidden default constructor.
                 */
                PROTODeserializer();

                /**
                 * "Forbidden" copy constructor. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the copy constructor.
                 */
                PROTODeserializer(const PROTODeserializer &);

                /**
                 * "Forbidden" assignment operator. Goal: The compiler should warn
                 * already at compile time for unwanted bugs caused by any misuse
                 * of the assignment operator.
                 */
                PROTODeserializer& operator=(const PROTODeserializer &);


            public:
                virtual ~PROTODeserializer();

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

                void read(istream &in, core::data::Container &container);

            private:
                enum WIRE_TYPE { VARINT = 0, BIT_64 = 1, LENGTH_DELIMITED = 2, BIT_32 = 5, OTHER = 255 };
                enum PROTO_TYPE { DOUBLE = 5, FLOAT = 4, INT32 = 0, INT64 = 1, UINT32 = 2, UINT64 = 3, BOOL = 6, BYTES = 7, STRING = 8, UNKNOWN = 255 };
    
                static WIRE_TYPE getWireType ( PROTO_TYPE type );

                static  WIRE_TYPE getWireType(uint32_t key) { return (WIRE_TYPE) (key & 0x7); }
                static  uint32_t getFieldNumber(uint32_t key) { return (key >> 3); }
                static  uint32_t getKey(uint32_t fieldNumber, uint8_t wireType) { return (fieldNumber << 3) | wireType; }
                uint32_t decodeVar(istream &in, uint64_t &value);
                void encode(ostream &out, uint64_t value);
                stringstream m_buffer;
                uint32_t m_size;
                uint32_t position;
                uint32_t decode(istream &in, uint64_t &value);
                istream &m_in;
        };

    }
} // core::base

#endif /*OPENDAVINCI_CORE_BASE_PROTODESERIALIZER_H_*/
