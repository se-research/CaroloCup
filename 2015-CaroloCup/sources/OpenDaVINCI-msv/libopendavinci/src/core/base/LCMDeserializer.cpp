/*
* OpenDaVINCI.
*
* This software is open source. Please see COPYING and AUTHORS for further information.
*/

#include "core/base/LCMDeserializer.h"
#include "core/base/Serializable.h"

namespace core {
    namespace base {
        
        using namespace std;
        
        LCMDeserializer::LCMDeserializer(istream &in):
            m_buffer() {
                /* 
                 * Checks whether the instream has a magic number or not.
                 * If it has, the read(istream, container) will later be called to get the payload.
                 * If not, it means we got the payload. The payload is put into the stringstream m_buffer.
                 */
                
                uint8_t magicBuf[4];
                in.read(reinterpret_cast<char*>(&magicBuf), sizeof(uint32_t));
                int32_t magicNumber = (((int32_t)magicBuf[0])<<24) + (((int32_t)magicBuf[1])<<16) + (((int32_t)magicBuf[2])<<8) + ((int32_t)magicBuf[3]);
                
                // Sets the instream position to the beginning
                in.clear();
                in.seekg(0, ios_base::beg);
                
                if (magicNumber == 0x4c433032) {
                    return;
                }
                
                char c = 0;
                while (in.good()) {
                    in.get(c);
                    m_buffer.put(c);
                }
                
                in.clear();
                in.seekg(0, ios_base::beg);
            }

        LCMDeserializer::~LCMDeserializer() {}
        
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
            m_buffer >> s;
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
            uint8_t buf[4];
            m_buffer.read(reinterpret_cast<char *>(&buf), sizeof(int32_t));
            i = (((int32_t)buf[0])<<24) + (((int32_t)buf[1])<<16) + (((int32_t)buf[2])<<8) + ((int32_t)buf[3]);
        }
        
        // uint32_t
        void LCMDeserializer::read(const uint32_t id, uint32_t &ui) {
            (void) id;
            uint8_t buf[4];
            m_buffer.read(reinterpret_cast<char *>(&buf), sizeof(uint32_t));
            ui = (((int32_t)buf[0])<<24) + (((int32_t)buf[1])<<16) + (((int32_t)buf[2])<<8) + ((int32_t)buf[3]);
        }
        
        // Float
        void LCMDeserializer::read(const uint32_t id, float &f) {
            (void) id;
            uint8_t buf[4];
            m_buffer.read(reinterpret_cast<char *>(&buf), sizeof(float));
            
            int64_t *p = (int64_t*) &f;
            *p = (((int32_t)buf[0])<<24) + (((int32_t)buf[1])<<16) + (((int32_t)buf[2])<<8) + ((int32_t)buf[3]);
        }
        
        // Double
        void LCMDeserializer::read(const uint32_t id, double &d) {
            (void) id;
            uint8_t buf[8];
            m_buffer.read(reinterpret_cast<char *>(&buf), sizeof(double));
            
            int64_t *p = (int64_t*) &d;
            int64_t a = (((int32_t)buf[0])<<24) + (((int32_t)buf[1])<<16) + ((int32_t)buf[2]<<8) + (int32_t)buf[3];
            int64_t b = (((int32_t)buf[4])<<24) + (((int32_t)buf[5])<<16) + ((int32_t)buf[6]<<8) + (int32_t)buf[7];
            *p = (a<<32) + (b&0xffffffff);
        }
        
        // String
        void LCMDeserializer::read(const uint32_t id, string &s) {
            (void) id;
            uint8_t lengthBuf[4];
            m_buffer.read(reinterpret_cast<char *>(&lengthBuf), sizeof(const int32_t));
            int32_t length = (((int32_t)lengthBuf[0])<<24) + (((int32_t)lengthBuf[1])<<16) + (((int32_t)lengthBuf[2])<<8) + ((int32_t)lengthBuf[3]);
            
            char *str = new char[length];
            m_buffer.read(reinterpret_cast<char *>(str), length);
            s = string(str);
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
            uint8_t magicBuf[4];
            in.read(reinterpret_cast<char*>(&magicBuf), sizeof(uint32_t));
            int32_t magicNumber = (((int32_t)magicBuf[0])<<24) + (((int32_t)magicBuf[1])<<16) + (((int32_t)magicBuf[2])<<8) + ((int32_t)magicBuf[3]);
            if (magicNumber != 0x4c433032) {
                if (in.good()) {
                    // Stream is good but still no magic number?
                    clog << "Stream corrupt: magic number not found." << endl;
                }
                return;
            }
            
            // Decoding the seq_number.
            uint8_t seqBuf[4];
            in.read(reinterpret_cast<char*>(&seqBuf), sizeof(uint32_t));
            int32_t msg_seq = (((int32_t)seqBuf[0])<<24) + (((int32_t)seqBuf[1])<<16) + (((int32_t)seqBuf[2])<<8) + ((int32_t)seqBuf[3]);
            // Sequence Number is not used so void it to avoid unused variable warning
            (void) msg_seq; 
            
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
            
            // Decoding Hash
            uint8_t hashBuf[8];
            in.read(reinterpret_cast<char*>(&hashBuf), sizeof(uint64_t));
            uint64_t hash = (((uint64_t)hashBuf[0])<<56) + (((uint64_t)hashBuf[1])<<48) + (((uint64_t)hashBuf[2])<<40) + (((uint64_t)hashBuf[3])<<32) + (((uint64_t)hashBuf[4])<<24) + (((uint64_t)hashBuf[5])<<16) + (((uint64_t)hashBuf[6])<<8) + ((uint64_t)hashBuf[7]);
            container.setHash(hash);
            
            /* We are not using the hash for now
            if (hash != 0x0e65ec258fc2e665LL) {
                return;
            }
            */
            
            // Writing the payload to the m_buffer which will then be read and decoded
            char c = 0;
            in.get(c);
            while (in.good()) {
                m_buffer.put(c);
                in.get(c);
            }
            
            container.setSerializedData(m_buffer.str());
        }
    }
} // core::base
