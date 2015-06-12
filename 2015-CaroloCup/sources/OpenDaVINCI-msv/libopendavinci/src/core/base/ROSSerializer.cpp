/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/ROSSerializer.h"
#include "core/base/Serializable.h"
#include "endian.h"
#include "core/data/Container.h"
#include <typeinfo>



namespace core {
    namespace base {
        
        using namespace std;
        
        ROSSerializer::ROSSerializer(ostream& out) :
                m_out(out),
                m_buffer(),
                m_size(0){}
        
        ROSSerializer::~ROSSerializer(){
            // Here suppose to be finalized message based on ROS approach, as far i know it is
            // It is : 
            // - > size of message
            // - > payload
            // Writing size of message
            
            // Keeping payload size infront of the payload. 
            // if m_size = 0, then it is container or nothing was writen to m_buffer.
            if(m_size !=0){
            stringstream ss;
            ss.write(reinterpret_cast<const char *>(&m_size), sizeof(uint32_t));
            m_out << ss.str();
            m_out << m_buffer.str();
            
            
            // All write functions are simple, just writes values into m_buffer.
            }
       
        } // end of ~constructor

        void ROSSerializer::write ( const uint32_t id, const Serializable& s ) {
             // writing serializable
            (void)id;
            stringstream buffer;
            buffer << s;
           // After serializable was serialized we read size of payload, an add it to current m_size.
            uint32_t size = 0;
            
            buffer.read(reinterpret_cast<char *>(&size), sizeof(uint32_t));
            m_size += size;
            char c = 0;
            buffer.get(c);
            while(buffer.good()){
                m_buffer.put(c);
                buffer.get(c);  
            }
        }
    
    
        void ROSSerializer::write ( const uint32_t id, const bool& b ) {
            (void) id;
            m_size += static_cast<uint32_t>(sizeof(b));
            
            m_buffer.write(reinterpret_cast<const char *>(&b), sizeof(const bool));
        }
        
        void ROSSerializer::write ( const uint32_t id, const char& c ) {
               
            (void) id;
            m_size += static_cast<uint32_t>(sizeof(c));
            
            m_buffer.write(&c, sizeof(const char));
        }
        

        void ROSSerializer::write ( const uint32_t id, const int32_t& i ) {
            
            (void) id;
            m_size += static_cast<uint32_t>(sizeof(i));
            
            int32_t _i = i;
            m_buffer.write(reinterpret_cast<const char *>(&_i), sizeof(const uint32_t));
        }
        
        void ROSSerializer::write ( const uint32_t id, const uint32_t& ui ) {

            (void) id;
            m_size += static_cast<uint32_t>(sizeof(ui));
            
            uint32_t _ui = ui;
            m_buffer.write(reinterpret_cast<const char *>(&_ui), sizeof(const uint32_t));

        }
        
        void ROSSerializer::write ( const uint32_t id, const float& f ) {
            
            (void) id;
            m_size += static_cast<uint32_t>(sizeof(f));
            
            float _f = f;
            m_buffer.write(reinterpret_cast<const char *>(&_f), sizeof(const float));

       }
        void ROSSerializer::write(const uint32_t id, const unsigned char& uc){
        (void) id;
        (void)uc;
        }


        void ROSSerializer::write ( const uint32_t id, const double& d ) {

            (void) id;
            m_size += static_cast<uint32_t>(sizeof(d));
            
            double _d = d;
            m_buffer.write(reinterpret_cast<const char *>(&_d), sizeof(const double));


        }
        void ROSSerializer::write ( const uint32_t id, const string& s ) {
               
            (void) id;
            m_size += static_cast<uint32_t>(sizeof(s));
            
            uint32_t stringLength = s.length();

            uint32_t _stringLength = stringLength;
            m_buffer.write(reinterpret_cast<const char *>(&_stringLength), sizeof(uint32_t));
            m_buffer.write(reinterpret_cast<const char *>(s.c_str()), stringLength);


        }

        void ROSSerializer::write ( const uint32_t id, const void* data, const uint32_t& size ) {
                    (void) id;
                    (void) data;
                    (void) size;
            
        }
    

    void ROSSerializer::write (core::data::Container &container){
   
                uint32_t connectionID = container.getDataType();
                uint8_t opcode = 0;
                uint8_t messageID = 0;
                uint16_t blockNr = 1;


                m_out.write(reinterpret_cast<const char *>(&connectionID), sizeof(const uint32_t));
                m_out.write(reinterpret_cast<const char *>(&opcode), sizeof(const uint8_t));
                m_out.write(reinterpret_cast<const char *>(&messageID), sizeof(const uint8_t));
                m_out.write(reinterpret_cast<const char *>(&blockNr), sizeof(const uint16_t));
                

                m_buffer << container;
                m_out << m_buffer.str();
                m_size = 0;

        }
    

    } 
        
} // core:base

  
