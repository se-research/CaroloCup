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
			m_buffer(),
			m_values() {
				
				uint8_t magicBuf[4];
				in.read(reinterpret_cast<char*>(&magicBuf), sizeof(uint32_t));
				int32_t magicNumber = (((int32_t)magicBuf[0])<<24) + (((int32_t)magicBuf[1])<<16) + (((int32_t)magicBuf[2])<<8) + ((int32_t)magicBuf[3]);
				
				in.clear();
				in.seekg(0, ios::beg);
				
				if (magicNumber == 0x4c433032) {
					cout << "HAH" << endl;
					return;
				}
				
				cout << "NOT" << endl;
				
				char c = 0;
				while (in.good()) {
					in.get(c);
					//cout << c << "-";
					m_buffer.put(c);
				}
				in.clear();
				in.seekg(0, ios_base::beg);
			}

        LCMDeserializer::~LCMDeserializer() {}

        void LCMDeserializer::read(const uint32_t id, Serializable &s) {
			(void) id;
			cout << "DESERRRR" << endl;
			m_buffer >> s;
        }

        void LCMDeserializer::read(const uint32_t id, bool &b) {
			(void) id;
			m_buffer.read(reinterpret_cast<char *>(&b), sizeof(bool));
        }

        void LCMDeserializer::read(const uint32_t id, char &c) {
			(void) id;
			m_buffer.read(&c, sizeof(char));
        }

        void LCMDeserializer::read(const uint32_t id, unsigned char &uc) {
			(void) id;
			m_buffer.read(reinterpret_cast<char *>(&uc), sizeof(unsigned char));
        }

        void LCMDeserializer::read(const uint32_t id, int32_t &i) {
			(void) id;
			uint8_t buf[4];
			m_buffer.read(reinterpret_cast<char *>(&buf), sizeof(int32_t));
			i = (((int32_t)buf[0])<<24) + (((int32_t)buf[1])<<16) + (((int32_t)buf[2])<<8) + ((int32_t)buf[3]);
        }

        void LCMDeserializer::read(const uint32_t id, uint32_t &ui) {
			(void) id;
			uint8_t buf[4];
			m_buffer.read(reinterpret_cast<char *>(&buf), sizeof(uint32_t));
			ui = (((int32_t)buf[0])<<24) + (((int32_t)buf[1])<<16) + (((int32_t)buf[2])<<8) + ((int32_t)buf[3]);
        }

        void LCMDeserializer::read(const uint32_t id, float &f) {
			(void) id;
			uint8_t buf[4];
			m_buffer.read(reinterpret_cast<char *>(&buf), sizeof(float));
			f = (((int32_t)buf[0])<<24) + (((int32_t)buf[1])<<16) + (((int32_t)buf[2])<<8) + ((int32_t)buf[3]);
        }

        void LCMDeserializer::read(const uint32_t id, double &d) {
			(void) id;
			uint8_t buf[4];
			m_buffer.read(reinterpret_cast<char *>(&buf), sizeof(double));
			d = (((int64_t)buf[0])<<56) + (((int64_t)buf[1])<<48) + (((int64_t)buf[2])<<40) + (((int64_t)buf[3])<<32) + (((int64_t)buf[4])<<24) + (((int64_t)buf[5])<<16) + (((int64_t)buf[6])<<8) + ((int64_t)buf[7]);
        }

        void LCMDeserializer::read(const uint32_t id, string &s) {
			(void) id;
			uint8_t lengthBuf[4];
			m_buffer.read(reinterpret_cast<char *>(&lengthBuf), sizeof(const int32_t));
			int32_t length = (((int32_t)lengthBuf[0])<<24) + (((int32_t)lengthBuf[1])<<16) + (((int32_t)lengthBuf[2])<<8) + ((int32_t)lengthBuf[3]);
			
			cout << "STRING LENGTH: " << length << endl;
			char *str = new char[length];
			m_buffer.read(str, length);
			s = string(str, length);
			cout << "STRING: " << str << endl;
        }

        void LCMDeserializer::read(const uint32_t id, void *data, uint32_t size) {
			(void) id;
			m_buffer.read(reinterpret_cast<char*>(data), size);
        }

        void LCMDeserializer::read(istream &in, core::data::Container &container){
        	m_buffer.flush();
        	// Magic number
        	//cout << "Starting with magic number " << endl;
        	uint8_t magicBuf[4];
        	in.read(reinterpret_cast<char*>(&magicBuf), sizeof(uint32_t));
        	int32_t magicNumber = (((int32_t)magicBuf[0])<<24) + (((int32_t)magicBuf[1])<<16) + (((int32_t)magicBuf[2])<<8) + ((int32_t)magicBuf[3]);
        	if (magicNumber != 0x4c433032) {
        		if (in.good()) {
        			// Stream is good but still no magic number?
        			clog << "Stream corrupt: magic number not found." << endl;
        		}
        		cout << "returning from magic number" <<endl;
        		return;
        	}
        	cout<< "rec magic number: " << magicNumber << endl;
        	// Decoding the seq_number.
        	uint8_t seqBuf[4];
        	in.read(reinterpret_cast<char*>(&seqBuf), sizeof(uint32_t));
        	int32_t msg_seq = (((int32_t)seqBuf[0])<<24) + (((int32_t)seqBuf[1])<<16) + (((int32_t)seqBuf[2])<<8) + ((int32_t)seqBuf[3]);
        	cout << "rec msg sequence: "<<  msg_seq <<endl;
			(void) msg_seq;


        	// Decoding channel
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
        	cout << "rec channel name: " << channel << endl;
		container.m_dataType = static_cast<core::data::Container::DATATYPE>(containerDataType);
		
		// Decoding Hash
        	uint8_t hashBuf[8];
        	in.read(reinterpret_cast<char*>(&hashBuf), sizeof(uint64_t));
        	uint64_t hash = (((uint64_t)hashBuf[0])<<56) + (((uint64_t)hashBuf[1])<<48) + (((uint64_t)hashBuf[2])<<40) + (((uint64_t)hashBuf[3])<<32) + (((uint64_t)hashBuf[4])<<24) + (((uint64_t)hashBuf[5])<<16) + (((uint64_t)hashBuf[6])<<8) + ((uint64_t)hashBuf[7]);
			cout << "rec hash: " << hash << endl;
		container.setHash(hash);
  
        	/*
        	if (hash != 0x0e65ec258fc2e665LL) {
        		return;
        	}
        	*/
	
        	char c = 0;
			cout << "!!!!" << endl;
			in.get(c);
        	while (in.good()) {
				m_buffer.put(c);
        		in.get(c);
				//cout << c << "-";
        	}
        	container.m_serializedData.str(m_buffer.str());
		//cout << "done " <<endl;
	

        }
    }
} // core::base
