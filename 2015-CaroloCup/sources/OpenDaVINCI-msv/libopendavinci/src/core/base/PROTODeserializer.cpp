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
                m_values(),
                m_size(0),
        		position(0){
            // Initialize the stringstream for getting valid positions when calling tellp().
            // This MUST be a blank (Win32 has a *special* implementation...)!
        		m_buffer.str(std::string());
                cout << "I AM INSIDE PROTO DESERIALIZER " << endl;
              //  cout << " I got this istream data : "  << in << endl;
                uint16_t magicNumber = 0;
                // Looking for magic number
                     int shift = 0;
                     uint8_t input;
                     do {
                          input = in.get();
                          magicNumber |= ( uint64_t ) ( input & 0x7F ) << shift;
                          shift += 7;
                     } while ( in.good() && ( input & 0x80 ) != 0 );

                     //checking if magic number correct
                     magicNumber = le64toh ( magicNumber );
                     if (magicNumber != 0xAACF) {
						if (in.good()) {
							// Stream is good but still no magic number?
							clog << "Stream corrupt: magic number not found." << endl;
						}
						return;
					} // end of checking magic number

                     cout << "found magic number : " << magicNumber << endl;

                     //extracting message size
                      shift = 0;
					  input = 0;
					  do {
						   input = in.get();
						   m_size |= ( uint64_t ) ( input & 0x7F ) << shift;
						   shift += 7;
					  } while ( in.good() && ( input & 0x80 ) != 0 );
					  //checking if magic number correct
					  m_size = le64toh ( m_size );
					  uint64_t msg_size = m_size;
                     cout << "message size : " << msg_size<< endl;

                     //Mapping payload (ID PAYLOAD)
                     while (in.good() && position < msg_size){

                    	 uint32_t size = 0;
						  shift = 0;
						  uint8_t c;
						  uint64_t decoded_id = 0;

						  do {
							   c = in.get();
							   position++;
							   decoded_id |= ( uint64_t ) ( c & 0x7F ) << shift;

							   shift += 7;
							   ++size;
			                     cout << "position " << (int)position << endl;

						  } while ( in.good() && ( c & 0x80 ) != 0 );

						  decoded_id = le64toh ( decoded_id );
						  cout << "decoded ID  " << decoded_id <<endl;;
		                     cout << "position " << (int)position << endl;

						 m_values.insert(make_pair(decoded_id, position));

                     }
                     cout << "position " << (int)position << endl;

                     // Reading payload;
                     uint64_t readbuf = 0;
                     char c = 0;
                     while ( in.good() && readbuf < msg_size ) {
                    	 in.get(c);
                    	 m_buffer.put(c);
                    	 readbuf++;
                     }

                     // Check for trailing ','
					 in.get(c);
					 if (c != ',') {
						 clog << "Stream corrupt: trailing ',' missing,  found: '" << c << "'" << endl;
					 }

            
        }

        PROTODeserializer::~PROTODeserializer() {}

        void PROTODeserializer::read(const uint32_t id, Serializable &s) {
			map<uint32_t, streampos>::iterator it = m_values.find(id);

			if(it != m_values.end()) {
				m_buffer.seekg(it->second);
				m_buffer >> s;
			}

        }

        void PROTODeserializer::read(const uint32_t id, bool &b) {
              cout << "Read Bool Deserializer" << "- ID value: " << id <<" | Bool value" << b <<endl ;
        }

        void PROTODeserializer::read(const uint32_t id, char &c) {
              cout << "Read char Deserializer" << "- ID value: " << id <<" | char  value: " << c <<endl;
        }

        void PROTODeserializer::read(const uint32_t id, unsigned char &uc) {
             cout << "Read unsgined char Deserializer" << "- ID value: " << id <<"  | unsigned char  value: " << uc <<endl;

        }

        void PROTODeserializer::read(const uint32_t id, int32_t &i) {
             cout << "Read int32_t Deserializer" << "- ID value: " << id <<" | int32_t  value: " << i <<endl;
 			map<uint32_t, streampos>::iterator it = m_values.find(id);

 			if( it != m_values.end()){
 				m_buffer.seekg(it->second);
				 // Decoding int32 value
				 uint32_t size = 0;
					  int shift = 0;
					  uint8_t c;
					  i = 0;

					  do {
						   c = m_buffer.get();
						   i |= ( uint64_t ) ( c & 0x7F ) << shift;
						   shift += 7;
						   ++size;
					  } while ( m_buffer.good() && ( c & 0x80 ) != 0 );

					  i = le64toh ( i );
					  cout << "decoded value  "<< i << endl;
 			}

        }

        void PROTODeserializer::read(const uint32_t id, uint32_t &ui) {
            cout << "Read uint32_t Deserializer" << "- ID value: " << id <<" | uint32_t  value: " << ui <<endl;

        }

        void PROTODeserializer::read(const uint32_t id, float &f) {
             cout << "Read float Deserializer" << "- ID value: " << id <<" < | float   value: " << f <<endl;

        }

        void PROTODeserializer::read(const uint32_t id, double &d) {
              cout << "Read double Deserializer" << "- ID value: " << id <<" | double  value: " << d <<endl;

        }

        void PROTODeserializer::read(const uint32_t id, string &s) {
              cout << "Read char Deserializer" << "- ID value: " << id <<" | string  value: " << s <<endl;
        }

        void PROTODeserializer::read(const uint32_t id, void *data, uint32_t size) {
              cout << "Read data Deserializer" << "- ID value: " << id <<" |  data  value: " << data << " | size of data : "<< size <<endl;

        }

    }
} // core::base
