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
uint32_t decodeVar ( istream &in, uint64_t &value );
        PROTODeserializer::PROTODeserializer(istream &in) :
                m_buffer(),
                m_values(),
                m_size(0),
                position(0){
            // Initialize the stringstream for getting valid positions when calling tellp().
            // This MUST be a blank (Win32 has a *special* implementation...)!
 
                  //Decoding magic number
            uint64_t value = 0;
            decodeVar(in,value);
            //casting value to uint16
            uint16_t magicNumber = (uint16_t) value;
        
            in.clear();
            in.seekg(0, ios::beg);
            if(magicNumber == 0xAABB){
                return;
            }
        
        
        
            char c = 0;
            in.get(c);
        
            while(in.good() ){
                m_buffer.put(c);
                in.get(c);
                
            }
            
            in.clear();
            in.seekg(0,ios_base::beg);

        }

        PROTODeserializer::~PROTODeserializer() { 
        }
       

        void PROTODeserializer::read(const uint32_t id, Serializable &s) {
          (void) id; //to be removed in the future
                m_buffer >> s;
            }

        

        
        void PROTODeserializer::read(const uint32_t id, bool &b) {
                (void)id;
                uint64_t key;
                decodeVar(m_buffer,key);
           
                WIRE_TYPE wireType = getWireType(key);
                uint32_t fieldId = getFieldNumber(key);
                (void) wireType;
                (void) fieldId;
                uint64_t v;
                decodeVar(m_buffer,v);

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
                decodeVar(m_buffer,v);                   
                c =  v;
      }

        void PROTODeserializer::read(const uint32_t id, unsigned char &uc) {          (void)id;

                uint64_t key;
                decodeVar(m_buffer,key);
                WIRE_TYPE wireType = getWireType(key);
                uint32_t fieldId = getFieldNumber(key);
                (void) wireType;
                (void) fieldId;
                uint64_t v;
                decodeVar(m_buffer,v);
                uc =  v;

        }

        void PROTODeserializer::read(const uint32_t id, int32_t &i) {
                (void)id;

                uint64_t key;
                decodeVar(m_buffer,key);
                WIRE_TYPE wireType = getWireType(key);
                uint32_t fieldId = getFieldNumber(key);
                (void) wireType;
                (void) fieldId;
                uint64_t v;
                decodeVar(m_buffer,v);
                i = (int32_t) v;

        }

        void PROTODeserializer::read(const uint32_t id, uint32_t &ui) {
                (void)id;
                uint64_t key;
                decodeVar(m_buffer,key);
                WIRE_TYPE wireType = getWireType(key);
                uint32_t fieldId = getFieldNumber(key);
                (void) wireType;
                (void) fieldId;
                
                uint64_t v;
                decodeVar(m_buffer,v);
                ui = (uint32_t) v;
        }

        void PROTODeserializer::read(const uint32_t id, float &f) {
                (void) id;  
                uint64_t key;
                decodeVar(m_buffer,key);
                WIRE_TYPE wireType = getWireType(key);
                uint32_t fieldId = getFieldNumber(key);
                (void) wireType;
                (void) fieldId;
  
                float _f =0;
                m_buffer.read(reinterpret_cast<char *>(&_f), 4);
                f= _f;
              
        }

        void PROTODeserializer::read(const uint32_t id, double &d) {
                (void) id;
                uint64_t key;
                decodeVar(m_buffer,key);
                WIRE_TYPE wireType = getWireType(key);
                uint32_t fieldId = getFieldNumber(key);
                (void) wireType;
                (void) fieldId;                
          
                double _d =0;        
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
                decodeVar (m_buffer, size );
                uint32_t length = (uint32_t) size;
                char *str = new char[length];
                m_buffer.read(str, length);
                s = string(str, length);
        }

        void PROTODeserializer::read(const uint32_t id, void *data, uint32_t size) {
              cout << "Read data Deserializer" << "- ID value: " << id <<" |  data  value: " << data << " | size of data : "<< size <<endl;

        }
        void PROTODeserializer::read(istream &in, core::data::Container &container) {
                // Getting message size
                // Decoding magic number
                uint64_t value = 0;
                decodeVar(in,value);
                
                // casting value to uint16
                uint16_t magicNumber = (uint16_t) value;
                if (magicNumber != 0xAABB){
                   if (in.good()) {
                    // Stream is good but still no magic number?
                    clog << "Stream corrupt: magic number not found." << endl;
                    }
                return;                   
                }
                
                value  = 0 ;    
                decodeVar(in,value);
                
                uint16_t dataType = (uint16_t) value;
                container.setDataType(static_cast<core::data::Container::DATATYPE>(dataType));
                uint64_t size;
                
                decodeVar(in, size);
                uint64_t readSize = 0;
                
                char c = 0;
                in.get(c);
                
                while(in.good() && readSize < size){
                    m_buffer.put(c);
                    in.get(c);
                    readSize++;
                }
                
                container.setSerializedData(m_buffer.str());

    }
     
        uint32_t decodeVar ( istream &in, uint64_t &value )
          {
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

    }
} // core::base
