/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information
 * Header file for PROTOSerializer
 */

#ifndef OPENDAVINCI_CORE_BASE_PROTOSERIALIZER_H_
#define OPENDAVINCI_CORE_BASE_PROTOSERIALIZER_H_

#include "core/platform.h"

#include "core/base/Serializer.h"
#include "core/data/Container.h"

namespace core{
    namespace base{
        
        using namespace std;

        class SerializationFactory;

        class PROTOSerializer : public Serializer{
            private:
                //Only Serialization factory or its subclasses are allowed to create instances of this Serializer
                friend class SerializationFactory;

                /**
                *  Constructor.
                *
                *  @param out Output stream for the data
                */
                PROTOSerializer(ostream &out);

            private:
                /**
                *  "Forbidden" copy constructor. Goal: The compiler should warn
                *  already at complie time for unwanted bugs cause by any misuse
                *  of the copy constructor.
                */
                PROTOSerializer(const PROTOSerializer &);

                /**
                *  Forbidden" copy constructor. Goal: The compiler should warn
                *  already at complie time for unwanted bugs cause by any misuse
                *  of the copy constructor.
                */
                PROTOSerializer& operator=(const PROTOSerializer &);

            public:
                virtual ~PROTOSerializer();

                virtual void write( const uint32_t id, const Serializable &s );

                virtual void write( const uint32_t id, const bool &b );

                virtual void write( const uint32_t id, const char &c );

                virtual void write( const uint32_t id, const unsigned char &uc );

                virtual void write( const uint32_t id, const int32_t &i );

                virtual void write( const uint32_t id, const uint32_t &ui );

                virtual void write( const uint32_t id, const float &f );

                virtual void write( const uint32_t id, const double &d );

                virtual void write( const uint32_t id, const string &s );

                virtual void write( const uint32_t id, const void *data, const uint32_t &size );

                void write(core::data::Container &container);

            private:
                enum WIRE_TYPE { VARINT = 0, BIT_64 = 1, LENGTH_DELIMITED = 2, BIT_32 = 5, OTHER = 255 };
                enum PROTO_TYPE { DOUBLE = 5, FLOAT = 4, INT32 = 0, INT64 = 1, UINT32 = 2, UINT64 = 3, BOOL = 6, BYTES = 7, STRING = 8, UNKNOWN = 255 };
    
                static WIRE_TYPE getWireType ( PROTO_TYPE type );
                static  WIRE_TYPE getWireType(uint32_t key) { return (WIRE_TYPE) (key & 0x7); }
                static  uint32_t getFieldNumber(uint32_t key) { return (key >> 3); }
                static  uint32_t getKey(uint32_t fieldNumber, uint8_t wireType) { return (fieldNumber << 3) | wireType; }
                uint8_t encode(ostream &out, uint64_t value);
                uint32_t decodeVar(istream &in, uint64_t &value);
                ostream &m_out;
                stringstream m_buffer;
                uint32_t m_size;
        };
    }
} // core::base

#endif /*OPENDAVINCI_CORE_BASE_PROTOSERIALIZER_H_*/


