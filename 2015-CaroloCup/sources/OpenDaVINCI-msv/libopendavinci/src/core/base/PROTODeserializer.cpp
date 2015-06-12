/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/PROTODeserializer.h"
#include "core/base/Serializable.h"

namespace core {
    namespace base {

        using namespace std;
        PROTODeserializer::PROTODeserializer(istream &in) :
                m_buffer(),
                m_size(0),
                position(0),
                m_in(in){
                    
            // Initialize the stringstream for getting valid positions when calling tellp().
            // This MUST be a blank (Win32 has a *special* implementation...)!
 
                m_buffer.str("");
                position = in.tellg();
                // Reading first value and checking if it is magic number.
                uint64_t value = 0;
                decodeVar(in,value);
                uint16_t magicNumber = static_cast<uint16_t>(value);
                in.clear();
                in.seekg(0,ios_base::beg);
                
                // If magic number found, that means it is full message and read as container.
                
                if(magicNumber == 0xAABB){
                    return;                
                }
                
                // Reads message size from the payload and then puts rest of payload into m_buffer.

                                
                value = 0;
                decodeVar(in,value);
                uint16_t msgSize =static_cast<uint16_t>(value);
                char c = 0;
                in.get(c);

                // moving rest of the payload into m_buffer.
                while(in.good() ){
                    m_buffer.put(c);            
                    in.get(c);
                }
                m_size = msgSize;
           

                in.clear();
                in.seekg(position,ios_base::beg);

        }

        PROTODeserializer::~PROTODeserializer() {
                int pos = m_buffer.tellg();
                m_in.clear();
                m_in.seekg(pos, ios_base::beg);
        }
       

        void PROTODeserializer::read(const uint32_t id, Serializable &s) {
                (void) id; //to be removed in the future
                
                stringstream ss;
                // Payload size has to be infront of payload
                uint64_t tempSize = static_cast<uint64_t>(m_size);
                encode(ss,tempSize);
                int pos = m_buffer.tellg();
                char c = 0;
                m_buffer.get(c);
                while (m_buffer.good()) {
                ss.put(c);
                m_buffer.get(c);
                }

                ss >> s;
                pos += ss.tellg();
                m_buffer.clear();
                m_buffer.seekg(pos, ios_base::beg);

            }

        

        
        void PROTODeserializer::read(const uint32_t id, bool &b) {
                (void)id;
                uint32_t size ;
                uint64_t key;
                size =decodeVar(m_buffer,key);
                m_size -= size ;

        
                WIRE_TYPE wireType = getWireType(key);
                uint32_t fieldId = getFieldNumber(key);
                
                (void) wireType;
                (void) fieldId;
                uint64_t v = 0;
                size  = decodeVar(m_buffer,v);
                m_size -= size;
                b = v;
               
        }

        void PROTODeserializer::read(const uint32_t id, char &c) {
                (void)id;
                uint64_t key;
                decodeVar(m_buffer,key);                   
                
                WIRE_TYPE wireType = getWireType(key);           
                uint32_t fieldId = getFieldNumber(key);                   
                
                (void) wireType;                  
                (void) fieldId;                  
                uint64_t v;                   
                uint32_t size ;
                size  = decodeVar(m_buffer,v);
                m_size -= size;                  
                c =  v;

      }

        void PROTODeserializer::read(const uint32_t id, unsigned char &uc) {
                (void)id;
                uint64_t key;
                decodeVar(m_buffer,key);
                
                WIRE_TYPE wireType = getWireType(key);
                uint32_t fieldId = getFieldNumber(key);
                
                (void) wireType;
                (void) fieldId;
                uint64_t v = 0;
                 uint32_t size ;
                size  = decodeVar(m_buffer,v);
                m_size -= size;
                uc =  v;

        }

        void PROTODeserializer::read(const uint32_t id, int32_t &i) {
                (void)id;

                uint32_t size ;
                uint64_t key;
                size = decodeVar(m_buffer,key);
                m_size -=size;
                
                WIRE_TYPE wireType = getWireType(key);
                uint32_t fieldId = getFieldNumber(key);
                
                (void) wireType;
                (void) fieldId;
                uint64_t v = 0;
                 
                size  = decodeVar(m_buffer,v);
                
                m_size -= size;
                i = static_cast<int32_t>(v);


        }

        void PROTODeserializer::read(const uint32_t id, uint32_t &ui) {
                (void)id;
                uint64_t key;
                

                decodeVar(m_buffer,key);
                
                WIRE_TYPE wireType = getWireType(key);
                uint32_t fieldId = getFieldNumber(key);
                
                (void) wireType;
                (void) fieldId;
                
                uint64_t v = 0;
                uint32_t size;
                size  = decodeVar(m_buffer,v);
                m_size -= size;
                ui = static_cast<uint32_t>(v);

        }

        void PROTODeserializer::read(const uint32_t id, float &f) {
                (void) id;  
                uint64_t key;
                decodeVar(m_buffer,key);
                
                WIRE_TYPE wireType = getWireType(key);
                uint32_t fieldId = getFieldNumber(key);
                
                (void) wireType;
                (void) fieldId;
                m_size -= 4;
                float _f =0;
                m_buffer.read(reinterpret_cast<char *>(&_f), 4);
                f= _f;

              
        }

        void PROTODeserializer::read(const uint32_t id, double &d) {
                (void) id;
                uint64_t key;
                uint32_t size ;   
                size = decodeVar(m_buffer,key);
                m_size -= size;
                
                WIRE_TYPE wireType = getWireType(key);
                uint32_t fieldId = getFieldNumber(key);
                
                (void) wireType;
                (void) fieldId;                
                double _d =0;    
                m_size -=8;
                m_buffer.read(reinterpret_cast<char *>(&_d),8); 

                d = _d;
        }

        void PROTODeserializer::read(const uint32_t id, string &s) {
                (void) id;
                uint64_t key;
                decodeVar(m_buffer,key);
                
                WIRE_TYPE wireType = getWireType(key);
                uint32_t fieldId = getFieldNumber(key);
                
                (void) wireType;
                (void) fieldId;
                uint64_t size;
                uint32_t siz =  decodeVar(m_buffer, size );
                uint32_t length = (uint32_t) size ;

                char *str = new char[length+1];
                m_size -= length;
                m_size -= siz;
                m_buffer.read(reinterpret_cast<char *>(str), length);
                str[length] = '\0';
                s = string(str, length);

        }

        void PROTODeserializer::read(const uint32_t id, void *data, uint32_t size) {
              (void) id;
              (void) data;
              (void) size;
              // Implement read user data from byte sequence

        }
        
        void PROTODeserializer::read(istream &in, core::data::Container &container) {
                // Getting message size
                // Decoding magic number
                uint64_t value = 0;
                decodeVar(in,value);
                uint16_t magicNumber = (uint16_t) value;
                if (magicNumber != 0xAABB){
                   if (in.good()) {
                    clog << "Stream corrupt: magic number not found. Protobuf" << endl;
                    }
                return;                   
                }
                stringstream buffer;
                char c = 0;
                in.get(c);
                while(in.good()){
                  buffer.put(c);
                  in.get(c);
                }
                buffer >> container;
        }
     
        uint32_t PROTODeserializer::decodeVar ( istream &in, uint64_t &value ){
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
          
            void PROTODeserializer::encode( ostream &out, uint64_t value){
                value = htole64( value);              
                    do {
                        char byte = value & (uint8_t) 0x7F;
                        value >>= 7;
                        if ( value) {
                            byte |= ( uint8_t ) 0x80;
                        }
                        out.put( byte );
                } while (value);
            }

    }
} // core::base
