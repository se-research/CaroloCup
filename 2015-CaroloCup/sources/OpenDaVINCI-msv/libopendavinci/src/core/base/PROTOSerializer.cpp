/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/PROTOSerializer.h"
#include "core/base/Serializable.h"
#include "endian.h"
#include "core/data/Container.h"
#include <typeinfo>






namespace core {
    namespace base {
        
        using namespace std;
        
        PROTOSerializer::PROTOSerializer(ostream& out) :
                m_out(out),
                m_buffer(),
                m_size(0){
                }
        
        PROTOSerializer::~PROTOSerializer(){
            // Here suppose to be finalized message based on PROTO approach.
            // It is : 
            // - > size of message
            // - > payload

            
          // Keeping payload size infront of payload.
          // if m_size == 0, then nothing was written to buffer.
            if(m_size !=0 ){
              stringstream ss ;
              encode(ss,m_size);
              m_out << ss.str();
              m_out << m_buffer.str();
            }  
       
        } // end of ~constructor

        void PROTOSerializer::write ( const uint32_t id, const Serializable& s ) {
            (void)id;
            
            stringstream buffer;
            buffer << s;
            uint64_t size = 0;
            decodeVar(buffer,size);
            m_size += static_cast<uint32_t>(size);
            char c = 0;
            buffer.get(c);
            uint32_t counter = 0;
            while(buffer.good() ){
                m_buffer.put(c);
                buffer.get(c);
                counter++;
            }            
        }
    
    
        void PROTOSerializer::write ( const uint32_t id, const bool& b ) {
            (void)id;    
            uint32_t size;
                
            PROTO_TYPE protoType =  BOOL;
            WIRE_TYPE wireType = getWireType( protoType) ;
                
            uint32_t key = getKey ( id, wireType );              
  
            size = encode(m_buffer,key);
            m_size += size;
            size = encode(m_buffer, b );
            m_size += size;
        }
        
        void PROTOSerializer::write ( const uint32_t id, const char& c ) {
            //Doesnt support this type, expressing as INT32 to write as varint.
            (void)id;                
            uint32_t size; 
            
            PROTO_TYPE protoType = INT32;              
            WIRE_TYPE wireType = getWireType ( protoType) ;  
            
            uint32_t key = getKey ( id, wireType );                                                   
           
            size = encode(m_buffer,key);        
            m_size += size;        
            size = encode(m_buffer,c );      
            m_size += size;
        }
        
        void PROTOSerializer::write ( const uint32_t id, const unsigned char& uc ) {
           //Doesnt support this type, expressing expressing as INT32 to write as varint.
            (void)id;    
            uint32_t size;
                 
            PROTO_TYPE protoType = INT32;
            WIRE_TYPE wireType = getWireType ( protoType) ;
                
            uint32_t key = getKey ( id, wireType );
            
            size = encode(m_buffer,key);
            m_size += size;
            size = encode(m_buffer, uc );
            m_size += size;
        }

        void PROTOSerializer::write ( const uint32_t id, const int32_t& i ) {
            (void)id;
            uint32_t size;
                
            PROTO_TYPE protoType = INT32;
            WIRE_TYPE wireType = getWireType ( protoType) ;
               
            uint32_t key = getKey ( id, wireType );
                
            size = encode(m_buffer,key);
            m_size += size;
            size = encode(m_buffer, i );
            m_size += size;
        }
        
        void PROTOSerializer::write ( const uint32_t id, const uint32_t& ui ) {
            (void)id;
            uint32_t size;

            PROTO_TYPE protoType = UINT32;
            WIRE_TYPE wireType = getWireType ( protoType) ;
               
            uint32_t key = getKey ( id, wireType );
            size = encode(m_buffer,key);
            m_size += size;
            size = encode(m_buffer, ui );
            m_size += size;

        }
        
        void PROTOSerializer::write ( const uint32_t id, const float& f ) {
            (void)id;
            uint32_t size;
            float _f = f;
            m_size += 4; 
            
            PROTO_TYPE protoType = FLOAT;
            WIRE_TYPE wireType = getWireType ( protoType) ;
            
            uint32_t key = getKey ( id, wireType );            
              
            size = encode(m_buffer,key);
            m_size += size;
            m_buffer.write(reinterpret_cast<const char *>(&_f), 4);
       }

        void PROTOSerializer::write ( const uint32_t id, const double& d ) {
            (void)id;
            uint32_t size;
            double _d = d; 
            m_size += 8;
            
            PROTO_TYPE protoType = DOUBLE;
            WIRE_TYPE wireType = getWireType ( protoType) ;
            
            uint32_t key = getKey ( id, wireType );               
            size = encode(m_buffer,key);
            m_size += size;
            m_buffer.write(reinterpret_cast<const char *>(&_d), 8);

        }
        void PROTOSerializer::write ( const uint32_t id, const string& s ) {
            (void)id;
            uint32_t size;
            
            PROTO_TYPE protoType = STRING; 
            WIRE_TYPE wireType = getWireType ( protoType) ;
            
            uint32_t key = getKey ( id, wireType );

            size = encode(m_buffer,key);
            m_size += size;
              
            uint32_t stringSize = 0;
            stringSize = s.length() ;
            m_size += stringSize;
            size = encode( m_buffer,stringSize );
            m_size += size;
            m_buffer.write ( reinterpret_cast<const char *>(s.c_str()), stringSize );  
        }

        void PROTOSerializer::write ( const uint32_t id, const void* data, const uint32_t& size ) {
            (void) id;
            (void) data;
            (void) size;
            // express data as byte sequence and then write same as string.
        }
    

        void PROTOSerializer::write (core::data::Container &container){
            uint16_t magicNumber = 0xAABB;
            encode(m_out, magicNumber);
            m_buffer << container;
            m_out << m_buffer.str();
            m_size = 0;
        }
    


    // Source Mike Achtelik
        uint8_t PROTOSerializer::encode( ostream &out, uint64_t value){ 
                    uint8_t size = 0;
                    value = htole64( value);                
                        do {
                            char byte = value & (uint8_t) 0x7F;
                            value >>= 7;
                            if ( value) {
                                byte |= ( uint8_t ) 0x80;
                            }
                            out.put( byte );
                            ++size;
                    } while (value);
                    return size;
        } 
      

          // Source Mike Achtelik
        uint32_t PROTOSerializer::decodeVar(std::istream& in, uint64_t& value){
                uint32_t size = 0;
                int shift = 0;
                uint8_t c;
                value = 0;
                do {
                    c = in.get();
                    value |= ( uint64_t ) ( c & 0x7F ) << shift;
                    shift += 7;
                    ++size;
                } while ( in.good() && ( c & 0x80 ) != 0 );
                value = le64toh ( value );
                return size;              
        }

            // Source Mike Achtelik
        PROTOSerializer::WIRE_TYPE PROTOSerializer::getWireType ( PROTOSerializer::PROTO_TYPE type ) {
        switch ( type ) {
            case INT32:
                return VARINT;
            case INT64:
                return VARINT;
            case UINT32:
                return VARINT;
            case UINT64:
                return VARINT;
            case FLOAT:
                return BIT_32;
            case DOUBLE:
                return BIT_64;
            case BOOL:
                return VARINT;
            case STRING:
            case BYTES:
                return LENGTH_DELIMITED;
            default:
                return OTHER;
            } // end of switch case
        } // end of 
    }
} // core:base

  
