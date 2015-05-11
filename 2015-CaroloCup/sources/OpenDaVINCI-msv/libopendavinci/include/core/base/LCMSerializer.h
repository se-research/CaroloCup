/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information
 * Header file for LCMSerializer
 */

#ifndef OPENDAVINCI_CORE_BASE_LCMSERIALIZER_H_
#define OPENDAVINCI_CORE_BASE_LCMSERIALIZER_H_

#include "core/platform.h"
#include "core/data/Container.h"


#include "core/base/Serializer.h"

namespace core{
    namespace base{
        
        using namespace std;
     //   using namespace core::data;

        class SerializationFactory;

        class LCMSerializer : public Serializer{
            private:
                //Only Serialization factory or its subclasses are allowed to create instances of this Serializer
                friend class SerializationFactory;

                /**
                *  Constructor.
                *
                *  @param out Output stream for the data
                */
                LCMSerializer(ostream &out);

            private:
                /**
                *  "Forbidden" copy constructor. Goal: The compiler should warn
                *  already at complie time for unwanted bugs cause by any misuse
                *  of the copy constructor.
                */
                LCMSerializer(const LCMSerializer &);

                /**
                *  Forbidden" copy constructor. Goal: The compiler should warn
                *  already at complie time for unwanted bugs cause by any misuse
                *  of the copy constructor.
                */
                LCMSerializer& operator=(const LCMSerializer &);

            public:
                virtual ~LCMSerializer();
                
                /*
                 * The write functions below are called to encode and write variables to a stringstream buffer.
                 * The variables will be written to the buffer in the order the write functions are called.
                 * 
                 * A hash number is also generated based on the id and the type of variable that is being written.
                 * This hash is used to check that the right container is reading from the data later.
                 * 
                 * For single byte variables, they are just written to the buffer without any encoding.
                 * For others, the bytes of the variable are stored into a uint8_t buffer and the buffer is then written to the stream.
                 */
                
                virtual void write( const uint32_t id, const Serializable &s );

                virtual void write( const uint32_t id, const bool &b );

                virtual void write( const uint32_t id, const char &c );

                virtual void write( const uint32_t id, const unsigned char &uc );

                virtual void write( const uint32_t id, const int32_t &i );

                virtual void write( const uint32_t id, const uint32_t &ui );
                
                void write( const uint32_t id, const int64_t &i );

                virtual void write( const uint32_t id, const float &f );

                virtual void write( const uint32_t id, const double &d );

                virtual void write( const uint32_t id, const string &s );

                virtual void write( const uint32_t id, const void *data, const uint32_t &size );

                void write(core::data::Container &container );

                string getSerializerType(){return "LCM";}
                int64_t getHash() const;



            private:
                ostream &m_out; // Buffer that will be sent
                stringstream m_buffer; // Buffer where all encoded variables will be stored to later be written to m_out
                int64_t m_hash; // The variable where the hash will be stored
        };
        // Functions taken from LCM for calculating hash
        int64_t calculate_hash(int64_t v, char c);
        
        int64_t hash_string(int64_t v, const char *s);
    }
} // core::base


#endif /*OPENDAVINCI_CORE_BASE_LCMSERIALIZER_H_*/


