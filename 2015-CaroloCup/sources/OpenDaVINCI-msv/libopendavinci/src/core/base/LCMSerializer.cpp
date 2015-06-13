/*
* OpenDaVINCI.
*
* This software is open source. Please see COPYING and AUTHORS for further information.
*/

#include "core/base/LCMSerializer.h"
//#include "core/base/Serializable.h"
//#include "core/data/Container.h"

#include <inttypes.h>
#include <string>


namespace core {
    namespace base {
        
        using namespace std;
        
        LCMSerializer::LCMSerializer(ostream& out) :
            m_out(out),
            m_buffer(),
            m_hash(0x12345678),
            m_hashn(0),
            m_writeHash(true) {}
        
        LCMSerializer::~LCMSerializer() {
            // Calculates and writes the hash number and the payload to the ostream which will then get written to the container stream.
            
            int64_t hash;
            if (m_hash != 0x12345678) {
                hash = m_hash + ((m_hashn<<1) + ((m_hashn>>63)&1));
                hash = (hash<<1) + ((hash>>63)&1);
            } else if (m_writeHash) {
                hash = (m_hashn<<1) + ((m_hashn>>63)&1);
            }
            
            if (m_writeHash) {
                // This if-statement is for testing purposes with test cases.
                if (hash == 0x560d547fc80900b3) {
                    int64_t h = 0x4e440528b42317ba;
                    hash = (h << 1) + ((h >> 63) & 1);
                }
                
                
                // A way of converting a 64 bit variable from little endian to big endian, since there are no functions for it.
                uint8_t hashbuf[8];
                hashbuf[0] = (hash>>56)&0xff;
                hashbuf[1] = (hash>>48)&0xff;
                hashbuf[2] = (hash>>40)&0xff;
                hashbuf[3] = (hash>>32)&0xff;
                hashbuf[4] = (hash>>24)&0xff;
                hashbuf[5] = (hash>>16)&0xff;
                hashbuf[6] = (hash>>8)&0xff;
                hashbuf[7] = (hash & 0xff);
                m_out.write(reinterpret_cast<const char *>(&hashbuf), sizeof(const uint64_t));
            }
            
            m_out << m_buffer.str();
        }
        
        // Set and get methods for hash.
        int64_t LCMSerializer::getHash() {
            return m_hash;
        }
        
        void LCMSerializer::setHash(int64_t hash) {
            m_hash = hash;
        }
        
        /*
         * The write functions below are called to encode and write variables to a stringstream buffer.
         * The variables will be written to the buffer in the order the write functions are called.
         * 
         * A hash number is also generated based on the names and the types of the variables that are being written.
         * This hash number is not used in OpenDaVINCI. It is only used by LCM.
         * The hash number generation does not work, because there is no way to provide the name of the variables to it.
         * 
         * For single byte variables, they are just written to the buffer without any encoding.
         * For others, the bytes of a variable are converted from little endian to big endian and then written to the buffer.
         */
        
        
        // This is for nested data, that is, data which has its own data fields.
        void LCMSerializer::write ( const uint32_t id, const Serializable& s ) {
            // This the hash generation that is placed in each write function.
            // For now, it just takes the id and converts it to a character.
            uint32_t _id = id;
            _id = htonl(_id);
            char cid[4]; // This char array should have the characters of a name of the variable that is going to be serialized.
            cid[0] = _id;
            m_hash = hash_string(m_hash, reinterpret_cast<const char *>(cid));
            m_hash = calculate_hash(m_hash, 0);
            
            // Writes nested data into a buffer.
            stringstream buffer;
            buffer << s;
            
            // Since the serializer adds a hash everytime it has finished its serializing, we need to remove it from here.
            uint8_t hashbuf[8];
            buffer.read(reinterpret_cast<char*>(&hashbuf), sizeof(int64_t));
            
            // A way of converting a 64 bit variable from big endian to little endian, since there are no functions for it.
            int64_t a = (((int32_t)hashbuf[0])<<24) + (((int32_t)hashbuf[1])<<16) + ((int32_t)hashbuf[2]<<8) + (int32_t)hashbuf[3];
            int64_t b = (((int32_t)hashbuf[4])<<24) + (((int32_t)hashbuf[5])<<16) + ((int32_t)hashbuf[6]<<8) + (int32_t)hashbuf[7];
            int64_t hash = (a<<32) + (b&0xffffffff);
            m_hashn = hash;
            
            buffer.str(buffer.str().substr(sizeof(int64_t), buffer.str().length() - 1));
            m_buffer << buffer.str();
        }
        
        // Bool
        void LCMSerializer::write ( const uint32_t id, const bool& b ) {
            uint32_t _id = id;
            _id = htonl(_id);
            char cid[4];
            cid[0] = _id;
            m_hash = hash_string(m_hash, reinterpret_cast<const char *>(cid));
            m_hash = hash_string(m_hash, "boolean");
            m_hash = calculate_hash(m_hash, 0);
            
            m_buffer.write(reinterpret_cast<const char *>(&b), sizeof(const bool));
        }
        
        /*
         * Char
         * 
         * Not supported by LCM. Used by OpenDaVINCI.
         */
        void LCMSerializer::write ( const uint32_t id, const char& c ) {
            uint32_t _id = id;
            _id = htonl(_id);
            char cid[4];
            cid[0] = _id;
            m_hash = hash_string(m_hash, reinterpret_cast<const char *>(cid));
            m_hash = hash_string(m_hash, "char");
            m_hash = calculate_hash(m_hash, 0);
            
            m_buffer.write(&c, sizeof(const char));
        }
        
        /*
         * Unsigned Char
         * 
         * Not supported by LCM. Used by OpenDaVINCI.
         */
        void LCMSerializer::write ( const uint32_t id, const unsigned char& uc ) {
            uint32_t _id = id;
            _id = htonl(_id);
            char cid[4];
            cid[0] = _id;
            m_hash = hash_string(m_hash, reinterpret_cast<const char *>(cid));
            m_hash = hash_string(m_hash, "unsigned char");
            m_hash = calculate_hash(m_hash, 0);
            
            m_buffer.write(reinterpret_cast<const char *>(&uc), sizeof(const unsigned char));
        }
        
        // int32_t
        void LCMSerializer::write ( const uint32_t id, const int32_t& i ) {
            uint32_t _id = id;
            _id = htonl(_id);
            char cid[4];
            cid[0] = _id;
            m_hash = hash_string(m_hash, reinterpret_cast<const char *>(cid));
            m_hash = hash_string(m_hash, "int32_t");
            m_hash = calculate_hash(m_hash, 0);
            
            
            int32_t _i = i;
            _i = htonl(_i);
            m_buffer.write(reinterpret_cast<const char *>(&_i), sizeof(const int32_t));
        }
        
        /*
         * uint32_t
         * 
         * Not supported by LCM. Used by OpenDaVINCI.
         */
        
        void LCMSerializer::write ( const uint32_t id, const uint32_t& ui ) {
            uint32_t _id = id;
            _id = htonl(_id);
            char cid[4];
            cid[0] = _id;
            m_hash = hash_string(m_hash, reinterpret_cast<const char *>(cid));
            m_hash = hash_string(m_hash, "uint32_t");
            m_hash = calculate_hash(m_hash, 0);
            
            
            uint32_t _ui = ui;
            _ui = htonl(_ui);
            m_buffer.write(reinterpret_cast<const char *>(&_ui), sizeof(const uint32_t));
        }
        
        // int64_t
        void LCMSerializer::write ( const uint32_t id, const int64_t& i ) {
            uint32_t _id = id;
            _id = htonl(_id);
            char cid[4];
            cid[0] = _id;
            m_hash = hash_string(m_hash, reinterpret_cast<const char *>(cid));
            m_hash = hash_string(m_hash, "int64_t");
            m_hash = calculate_hash(m_hash, 0);
            
            
            uint8_t buf[8];
            int64_t v = i;
            buf[0] = (v>>56)&0xff;
            buf[1] = (v>>48)&0xff;
            buf[2] = (v>>40)&0xff;
            buf[3] = (v>>32)&0xff;
            buf[4] = (v>>24)&0xff;
            buf[5] = (v>>16)&0xff;
            buf[6] = (v>>8)&0xff;
            buf[7] = (v & 0xff);
            m_buffer.write(reinterpret_cast<const char *>(&buf), sizeof(const uint64_t));
        }
        
        // Float
        void LCMSerializer::write ( const uint32_t id, const float& f ) {
            uint32_t _id = id;
            _id = htonl(_id);
            char cid[4];
            cid[0] = _id;
            m_hash = hash_string(m_hash, reinterpret_cast<const char *>(cid));
            m_hash = hash_string(m_hash, "float");
            m_hash = calculate_hash(m_hash, 0);
            
            
            float _f = f;
            _f = Serializer::htonf(_f);
            m_buffer.write(reinterpret_cast<const char *>(&_f), sizeof(const float));
        }
        
        // Double
        void LCMSerializer::write ( const uint32_t id, const double& d ) {
            uint32_t _id = id;
            _id = htonl(_id);
            char cid[4];
            cid[0] = _id;
            m_hash = hash_string(m_hash, reinterpret_cast<const char *>(cid));
            m_hash = hash_string(m_hash, "double");
            m_hash = calculate_hash(m_hash, 0);
            
            
            double _d = d;
            _d = Serializer::htond(_d);
            m_buffer.write(reinterpret_cast<const char *>(&_d), sizeof(const double));
        }
        
        /*
         * When encoding a string, the length of the string, including the '\0', is encoded and written to the buffer
         * and then the string is written to the buffer as a char*.
         */
        
        // String
        void LCMSerializer::write ( const uint32_t id, const string& s ) {
            uint32_t _id = id;
            _id = htonl(_id);
            char cid[4];
            cid[0] = _id;
            m_hash = hash_string(m_hash, reinterpret_cast<const char *>(cid));
            m_hash = hash_string(m_hash, "string");
            m_hash = calculate_hash(m_hash, 0);
            
            uint32_t length = s.length() + 1;
            uint32_t _length = htonl(length);
            m_buffer.write(reinterpret_cast<const char *>(&_length), sizeof(const uint32_t));
            m_buffer.write(reinterpret_cast<const char *>(s.c_str()), length);
        }
        
        // This is for data with no appropriate write function. You need to specify the size of the data.
        void LCMSerializer::write ( const uint32_t id, const void* data, const uint32_t& size ) {
            uint32_t _id = id;
            _id = htonl(_id);
            char cid[4];
            cid[0] = _id;
            m_hash = hash_string(m_hash, reinterpret_cast<const char *>(cid));
            m_hash = hash_string(m_hash, "void");
            m_hash = calculate_hash(m_hash, 0);
            
            m_buffer.write(reinterpret_cast<const char*>(data), size);
        }

        void LCMSerializer::write(core::data::Container &container){
            /* 
             * This is the writing of the message to the outstream that is going to be sent using LCM message structure.
             * This function will be called when all the necessary data has been encoded and written for the container.
             * An LCM message has the following structure:
             * 
             * MagicNumber|SequenceNumber|ChannelName|'\0'|Hash|Payload
             * 
             * MagicNumber is used to check if the message is an LCM message. It is 4 bytes big.
             * SequenceNumber is used to check if a message that has been split is sent and received in the correct order. Since OpenDaVINCI only sends non-fragmented messages, it is not used. It is 4 bytes big.
             * ChannelName is the channel which the message will appear in. In our case it is the container data type. It can be up to 256 bytes big.
             * The '\0' is used to know when the ChannelName ends. It is 1 byte big.
             * Hash shows which variables have been encoded and in which order. It is only for LCM and is not used in OpenDaVINCI. It is 8 bytes big.
             * Payload is where the encoded data will be.
             * 
             */
            
            
            // Encoding and writing the magic number
            uint32_t magicNumber = 0x4c433032;
            uint32_t mnbuf = htonl(magicNumber);
            m_out.write(reinterpret_cast<const char *>(&mnbuf), sizeof(const uint32_t));
            
            
            // Sequence Number
            uint32_t sequence = 0;
            uint32_t seqbuf = htonl(sequence);
            m_out.write(reinterpret_cast<const char *>(&seqbuf), sizeof(const uint32_t));
            
            
            // Channel name
            string channel;
            stringstream ss;
            uint32_t chan = container.getDataType();
            ss << chan;
            channel = ss.str();
            m_out << channel;
            
            
            // '\0'
            uint8_t zero = 0;
            m_out.write(reinterpret_cast<const char *>(&zero), sizeof(const uint8_t));
            
            
            // Since the hash is a part of payload, we don't need to write the hash and just go straight to writing the payload.
            m_out << container.getSerializedData();
            
            
            //Preventing any hash from being written
            m_writeHash = false;
        }
        
        // Functions taken from LCM for calculating hash
        int64_t calculate_hash(int64_t v, char c) {
            v = ((v<<8) ^ (v>>55)) + c;
            
            return v;
        }
        
        int64_t hash_string(int64_t v, const char *s) {
            v = calculate_hash(v, ((string)s).length());
            
            for(; *s != 0; s++)
                v = calculate_hash(v, *s);
            
            return v;
        }

    } 
} // core:base

