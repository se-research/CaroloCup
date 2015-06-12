/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/ROSDeserializer.h"
#include "core/base/Serializable.h"

namespace core {
    namespace base {

        using namespace std;

        ROSDeserializer::ROSDeserializer(istream &in) :
                m_buffer(),
                m_size(0),
                position(0),
                m_in(in){
            // Initialize the stringstream for getting valid positions when calling tellp().
            // This MUST be a blank (Win32 has a *special* implementation...)!
                    
            // Get currect possition and after putting everything into m_buffer we reset read pointer.
            // Used for nested data to keep track of istream in read pointer possition.
            position = in.tellg();
            uint32_t size ;
            in.read(reinterpret_cast<char *>(&size), sizeof(uint32_t));
            m_size = size;
            char c = 0;
            in.get(c);
            while(in.good() ){
                m_buffer.put(c);
                in.get(c);
                
            }
            
            in.clear();
            in.seekg(position,ios_base::beg);

        }

        ROSDeserializer::~ROSDeserializer() { 
            uint32_t pos = m_buffer.tellg();
            m_in.clear();
            m_in.seekg(pos,ios_base::beg);
        }
       

        void ROSDeserializer::read(const uint32_t id, Serializable &s) {
                (void) id;
               // As size of payload, we put m_size infront of payload, for nested data.
                stringstream ss;
                ss.write(reinterpret_cast<const char *>(&m_size), sizeof(uint32_t));
                uint32_t pos = m_buffer.tellg();
                
                char c = 0;
                m_buffer.get(c);
                while (m_buffer.good()) {
                ss.put(c);
                m_buffer.get(c);
                }
                
                ss >> s;
                
                //moving read pointer, based on how much it moved by reading nested data.
                
                pos += ss.tellg();
                m_buffer.clear();
                m_buffer.seekg(pos, ios_base::beg);
            }

        

        
        void ROSDeserializer::read(const uint32_t id, bool &b) {
               
                (void)id;           
                m_buffer.read(reinterpret_cast<char *>(&b), sizeof(bool));
        }

        void ROSDeserializer::read(const uint32_t id, char &c) {
            
                (void)id;           
                m_buffer.read(reinterpret_cast<char *>(&c), sizeof(char));
        }

        void ROSDeserializer::read(const uint32_t id, unsigned char& uc){
                (void) id;
                (void)uc;
        }

        void ROSDeserializer::read(const uint32_t id, int32_t &i) {
                (void)id;           
                m_buffer.read(reinterpret_cast<char *>(&i), sizeof(int32_t));
        }

        void ROSDeserializer::read(const uint32_t id, uint32_t &ui) {
                (void)id;           
                uint32_t _ui = 0;
                m_buffer.read(reinterpret_cast<char *>(&_ui), sizeof(uint32_t));
                ui = _ui;
        }

        void ROSDeserializer::read(const uint32_t id, float &f) {
                (void)id;           
                float _f = 0;
                m_buffer.read(reinterpret_cast<char *>(&_f), sizeof(float));
                f = _f;
        }

        void ROSDeserializer::read(const uint32_t id, double &d) {
                (void)id;         
                double _d = 0;
                m_buffer.read(reinterpret_cast<char *>(&_d), sizeof(double));
                d = _d;
        }

        void ROSDeserializer::read(const uint32_t id, string &s) {
                (void)id;           
                uint32_t stringLength = 0;
                m_buffer.read(reinterpret_cast<char *>(&stringLength), sizeof(uint32_t));
                char *str = new char[stringLength+1];
                m_buffer.read(reinterpret_cast<char *>(str), static_cast<uint32_t>(stringLength));
                str[stringLength] = '\0';
                s = string(str, stringLength);
                OPENDAVINCI_CORE_DELETE_ARRAY(str);
        }

        void ROSDeserializer::read(const uint32_t id, void *data, uint32_t size) {
                (void) id;
                (void) data;
                (void) size;
        }
        void ROSDeserializer::read(istream &in, core::data::Container &container) {
                
                uint32_t connectionID;
                uint8_t opcode;
                uint8_t messageID;
                uint16_t blockNr;
                m_buffer.str("");
                m_size = 0;
               // Read Header values
                in.read(reinterpret_cast<char *>(&connectionID), sizeof(const uint32_t));
                in.read(reinterpret_cast<char *>(&opcode), sizeof(const uint8_t));
                in.read(reinterpret_cast<char *>(&messageID), sizeof(const uint8_t));
                in.read(reinterpret_cast<char *>(&blockNr), sizeof(const uint16_t));
                container.setDataType(static_cast<core::data::Container::DATATYPE>(connectionID));
                
                // rest of the buffer moved into new buffer, and deserialized as container.
               stringstream buffer;
                char c = 0;
                in.get(c);
                while(in.good()){
                  buffer.put(c);
                  in.get(c);
                }
                buffer >> container;
                

    }
     


    }
} // core::base
