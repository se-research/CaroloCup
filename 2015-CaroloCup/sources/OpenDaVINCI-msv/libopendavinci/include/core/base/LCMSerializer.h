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

                void write(core::data::Container &container );

                void setHash(int64_t hash);

                int64_t getHash();

                string getSerialyzerType(){return "LCM";}



            private:
                ostream &m_out;
                stringstream m_buffer;
                uint64_t m_hash;
            //    uint64_t payload_hash;

        };
        int64_t calculate_hash(int64_t v, char c);
        int64_t hash_string(int64_t v, const char *s);




    }
} // core::base


#endif /*OPENDAVINCI_CORE_BASE_LCMSERIALIZER_H_*/


