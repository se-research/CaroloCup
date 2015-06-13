/*
* OpenDaVINCI.
*
* This software is open source. Please see COPYING and AUTHORS for further information.
*/

#include "core/base/LCMDeserializer.h"
#include "core/base/Serializable.h"
#include <inttypes.h>

namespace core {
    namespace base {
        
        using namespace std;
        
        LCMDeserializer::LCMDeserializer(istream &in):
            m_buffer(),
            m_in(in) {
                /* 
                 * Checks whether the instream has a magic number or not.
                 * If it has, the read(istream, container) will later be called to get the payload.
                 * If not, it means we got the payload. The payload is put into the stringstream m_buffer.
                 */
                
                int32_t _magicNumber;
                in.read(reinterpret_cast<char*>(&_magicNumber), sizeof(int32_t));
                int32_t magicNumber = ntohl(_magicNumber);
                
                // Sets the read position to the beginning
                in.clear();
                in.seekg(0, ios_base::beg);
                
                if (magicNumber == 0x4c433032) {
                    return;
                }
                
                // Since the hash is a part of the payload and the hash is not used, just read it and ignore it.
                int64_t hash;
                in.read(reinterpret_cast<char*>(&hash), sizeof(int64_t));
                
                
                char c = 0;
                in.get(c);
                while (in.good()) {
                    m_buffer.put(c);
                    in.get(c);
                }
                
                in.clear();
                in.seekg(0, ios_base::beg);
            }

        LCMDeserializer::~LCMDeserializer() {
            // This is used when reading nested data to tell how much has been read.
            int pos = m_buffer.tellg();
            m_in.clear();
            m_in.seekg(pos, ios_base::beg);
        }
        
        /*
         * The read functions below are called to decode and get the variables from the payload.
         * The variables must be read in the order they were written.
         * 
         * For single byte variables, they are just read without any decoding.
         * For others, the variable is read into a uint8_t buffer and then decoded.
         * The decoding procedure is the encoding procedure backwards.
         */
        
        // This is for nested data
        void LCMDeserializer::read(const uint32_t id, Serializable &s) {
            (void) id;
            stringstream ss;
            
            // Add a dummy hash to the beginning of the stream that are going to be read, because the deserializer always removes a hash when created.
            int64_t hash = 1234;
            ss.write(reinterpret_cast<const char *>(&hash), sizeof(const int64_t));
            
            // Get the read position and put all the data from that point and forward inside ss.
            int pos = m_buffer.tellg();
            char c = 0;
            m_buffer.get(c);
            while (m_buffer.good()) {
                ss.put(c);
                m_buffer.get(c);
            }
            
            // Read from ss into the nested data
            ss >> s;
            
            // Once we have read the data, move the read position of m_buffer forward by how much was read.
            pos += ss.tellg();
            m_buffer.clear();
            m_buffer.seekg(pos, ios_base::beg);
        }
        
        // Bool
        void LCMDeserializer::read(const uint32_t id, bool &b) {
            (void) id;
            m_buffer.read(reinterpret_cast<char *>(&b), sizeof(bool));
        }
        
        // Char
        void LCMDeserializer::read(const uint32_t id, char &c) {
            (void) id;
            m_buffer.read(&c, sizeof(char));
        }
        
        // Unsigned Char
        void LCMDeserializer::read(const uint32_t id, unsigned char &uc) {
            (void) id;
            m_buffer.read(reinterpret_cast<char *>(&uc), sizeof(unsigned char));
        }
        
        // int32_t
        void LCMDeserializer::read(const uint32_t id, int32_t &i) {
            (void) id;
            int32_t _i;
            m_buffer.read(reinterpret_cast<char *>(&_i), sizeof(int32_t));
            i = ntohl(_i);
        }
        
        // uint32_t
        void LCMDeserializer::read(const uint32_t id, uint32_t &ui) {
            (void) id;
            uint32_t _ui;
            m_buffer.read(reinterpret_cast<char *>(&_ui), sizeof(uint32_t));
            ui = ntohl(_ui);
        }
        
        // int64_t
        void LCMDeserializer::read(const uint32_t id, int64_t &i) {
            (void) id;
            uint8_t buf[8];
            m_buffer.read(reinterpret_cast<char *>(&buf), sizeof(uint64_t));
            
            // A way of converting a 64 bit variable from big endian to little endian, since there are no functions for it.
            int64_t a = (((int32_t)buf[0])<<24) + (((int32_t)buf[1])<<16) + ((int32_t)buf[2]<<8) + (int32_t)buf[3];
            int64_t b = (((int32_t)buf[4])<<24) + (((int32_t)buf[5])<<16) + ((int32_t)buf[6]<<8) + (int32_t)buf[7];
            i = (a<<32) + (b&0xffffffff);
            
        }
        
        // Float
        void LCMDeserializer::read(const uint32_t id, float &f) {
            (void) id;
            float _f;
            m_buffer.read(reinterpret_cast<char *>(&_f), sizeof(float));
            f = Deserializer::ntohf(_f);
        }
        
        // Double
        void LCMDeserializer::read(const uint32_t id, double &d) {
            (void) id;
            double _d;
            m_buffer.read(reinterpret_cast<char *>(&_d), sizeof(double));
            d = Deserializer::ntohd(_d);
        }
        
        // String
        void LCMDeserializer::read(const uint32_t id, string &s) {
            (void) id;
            
            int32_t _length;
            m_buffer.read(reinterpret_cast<char *>(&_length), sizeof(const int32_t));
            int32_t length = ntohl(_length);
            
            char *str = new char[length];
            m_buffer.read(reinterpret_cast<char *>(str), length);
            s = string(str, length - 1);
            OPENDAVINCI_CORE_DELETE_ARRAY(str);
        }
        
        // This is for data types with no appropriate read function
        void LCMDeserializer::read(const uint32_t id, void *data, uint32_t size) {
            (void) id;
            m_buffer.read(reinterpret_cast<char*>(data), size);
        }

        void LCMDeserializer::read(istream &in, core::data::Container &container){
            /*
             * This function decodes the message in the order they were written to the buffer.
             * This function will be called to decode the message and get the payload from it.
             */
            
            
            // Decoding the Magic number
            int32_t _magicNumber;
            in.read(reinterpret_cast<char*>(&_magicNumber), sizeof(int32_t));
            int32_t magicNumber = ntohl(_magicNumber);
            if (magicNumber != 0x4c433032) {
                if (in.good()) {
                    // Stream is good but still no magic number?
                    clog << "Stream corrupt: magic number not found." << endl;
                }
                return;
            }
            
            
            // Decoding the seq_number.
            int32_t _sequence;
            in.read(reinterpret_cast<char*>(&_sequence), sizeof(int32_t));
            int32_t sequence = ntohl(_sequence);
            // Sequence Number is not used so void it to avoid unused variable warning.
            (void) sequence; 
            
            
            // Decoding Channel Name
            char channel[256];
            int channel_len = 0;
            char ch = 0;
            in.get(ch);
            while (ch != 0) {
                channel[channel_len++] = ch;
                in.get(ch);
            }
            stringstream ss;
            ss << channel;
            uint32_t containerDataType = 0;
            ss >> containerDataType;
            container.setDataType(static_cast<core::data::Container::DATATYPE>(containerDataType));
            
            
            // Writing the payload to the m_buffer which will then be read and decoded.
            // This payload also includes the hash.
            stringstream ss2;
            char c = 0;
            in.get(c);
            while (in.good()) {
                ss2.put(c);
                in.get(c);
            }
            
            container.setSerializedData(ss2.str());
        }
    }
} // core::base
