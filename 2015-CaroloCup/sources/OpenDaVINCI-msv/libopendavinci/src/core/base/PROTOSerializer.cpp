/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/base/PROTOSerializer.h"
#include "core/base/Serializable.h"
#include "endian.h"


namespace core {
    namespace base {
        
        using namespace std;
        
        PROTOSerializer::PROTOSerializer(ostream& out) :
                m_out(out),
                m_buffer(),
                m_size(0){}
        
        PROTOSerializer::~PROTOSerializer(){
            // Here suppose to be finalized message based on PROTO approach, as far i know it is
            // It is : 
        	// - > Magic number (to know where to start)?
        	// - > size of message
        	// - > payload
        	//cout<< "Encoding magic Number" << endl;
            uint16_t magicNumber = 0xAACF;
            magicNumber = htole16( magicNumber );
				  do {
					   char nextByte = magicNumber & ( uint8_t ) 0x7F;
					   magicNumber >>= 7;

					   if ( magicNumber ) { nextByte |= ( uint8_t ) 0x80;}

					   m_out << nextByte ;
				  } while (magicNumber);
			// Writing size of message
		        	//cout<< "Encoding size of message: " <<  (int)m_size <<endl;

				  m_size = htole16(m_size);
				  do {
				  		char nextByte = m_size & ( uint8_t ) 0x7F;
				  	   m_size >>= 7;

				  	   if ( m_size ) { nextByte |= ( uint8_t ) 0x80;}

				  	   m_out << nextByte ;
				  } while (m_size);


		  // Writing payload;
		        	//cout<< "Writing payload" << endl;

				  m_out << m_buffer.str();

				  //no real need for this , but for now keeping it.
				  m_out << ",";


            //cout<< "created PROTOserializer" << endl;


        } // end of ~constructor

        void PROTOSerializer::write ( const uint32_t id, const Serializable& s ) {
             //cout<< " Serializing of SERIALIZABLE " << endl;
             // Writing id
                   uint64_t _id = htole64( id );
                   uint8_t size = 0;
                        do {
                             char nextByte = _id & ( uint8_t ) 0x7F;
                             _id >>= 7;

                             if ( _id ) { nextByte |= ( uint8_t ) 0x80;}

                             m_buffer << nextByte ;
                             ++size;

                        } while ( _id );
                   m_size += size;
             // writing serializable
             stringstream buffer;
             buffer << s;

             uint32_t buffer_size = buffer.str().length();
             buffer_size = htole64( buffer_size );
			 size = 0;
			  do {
				char nextByte = buffer_size & ( uint8_t ) 0x7F;
				buffer_size >>= 7;

				if ( buffer_size ) { nextByte |= ( uint8_t ) 0x80;}

				m_buffer << nextByte ;
				++size;

			   } while ( buffer_size );
			  m_size += size;
			  m_buffer << buffer.str();
			  //cout<< "serializable" <<endl;

        }
    
    
        void PROTOSerializer::write ( const uint32_t id, const bool& b ) {
          //cout<< " Serialization of BOOLEAN. "<< endl;
          // Writing id
                uint64_t _id = htole64( id );
                uint8_t size = 0;
                     do {
                          char nextByte = _id & ( uint8_t ) 0x7F;
                          _id >>= 7;

                          if ( _id ) { nextByte |= ( uint8_t ) 0x80;}

                          m_buffer << nextByte ;
                          ++size;

                     } while ( _id );
                m_size += size;
          // writing boolean (bool is being encoded as Varint)
                uint64_t _b = htole64( b );
				size = 0;
					 do {
						  char nextByte = _b & ( uint8_t ) 0x7F;
						  _id >>= 7;

						  if ( _b ) { nextByte |= ( uint8_t ) 0x80;}

						  m_buffer << nextByte ;
						  ++size;

					 } while ( _b );
				m_size += size;
				  //cout<< "boolean" <<endl;

        }
        
        void PROTOSerializer::write ( const uint32_t id, const char& c ) {
                  //cout<< " Serialization  of char. " <<endl;
                  // Writing id
                      uint64_t _id = htole64( id );
                      uint8_t size = 0;
                           do {
                                char nextByte = _id & ( uint8_t ) 0x7F;
                                _id >>= 7;

                                if ( _id ) { nextByte |= ( uint8_t ) 0x80;}

                                m_buffer << nextByte ;
                                ++size;

                           } while ( _id );
                           m_size += size;
		// Writing char / same as bool because bool and char same size ?
                           uint64_t _c = htole64( c );
                           size = 0;
							do {
								 char nextByte = _c & ( uint8_t ) 0x7F;
								 _c >>= 7;

								 if ( _c ) { nextByte |= ( uint8_t ) 0x80;}

								 m_buffer << nextByte ;
								 ++size;

							} while ( _c );
							m_size += size;
							  //cout<< "char" <<endl;

		}
        
        void PROTOSerializer::write ( const uint32_t id, const unsigned char& uc ) {
            //cout<< " Serialization of  unsigned char : "  << endl;
            // Writing id
                  uint64_t _id = htole64( id );
                  uint8_t size = 0;
                       do {
                            char nextByte = _id & ( uint8_t ) 0x7F;
                            _id >>= 7;

                            if ( _id ) { nextByte |= ( uint8_t ) 0x80;}

                            m_buffer << nextByte ;
                            ++size;

                       } while ( _id );
                  m_size += size;
             // writing unsigned char
                  uint64_t _uc = htole64( uc );
				  size = 0;
				  	  	  do {
							  char nextByte = _uc & ( uint8_t ) 0x7F;
							  _uc >>= 7;

							  if ( _uc ) { nextByte |= ( uint8_t ) 0x80;}

							  m_buffer << nextByte ;
							  ++size;

				  	  	  } while ( _uc );
					m_size += size;
					  //cout<< " unsigned char" <<endl;

        }

        void PROTOSerializer::write ( const uint32_t id, const int32_t& i ) {
          //cout<< "Serialization of int32 " <<endl;
          // Writing id
                uint64_t _id = htole64( id );
                uint8_t size = 0;
                     do {
                          char nextByte = _id & ( uint8_t ) 0x7F;
                          _id >>= 7;

                          if ( _id ) { nextByte |= ( uint8_t ) 0x80;}

                          m_buffer << nextByte ;
                          ++size;

                     } while ( _id );
                m_size += size;

          // Writing value
                int64_t _i = htole64( i );
                size = 0;
                     do {
                          char nextByte = _i & ( uint8_t ) 0x7F;
                          _i >>= 7;

                          if ( _i ) { nextByte |= ( uint8_t ) 0x80;}

                          m_buffer << nextByte ;
                          ++size;

                     } while ( _i );
                  m_size += size;
    			  //cout<< " int " <<endl;

        }
        
        void PROTOSerializer::write ( const uint32_t id, const uint32_t& ui ) {
                        //cout<< " Serialization of uint32_t : "  << endl;
                        // Writing id
                              uint64_t _id = htole64( id );
                              uint8_t size = 0;
                                   do {
                                        char nextByte = _id & ( uint8_t ) 0x7F;
                                        _id >>= 7;

                                        if ( _id ) { nextByte |= ( uint8_t ) 0x80;}

					m_buffer << nextByte ;
					++size;

				   } while ( _id );
			      m_size += size;

			// Writing value
			      int64_t _ui = htole64( ui );
			      size = 0;
				   do {
					char nextByte = _ui & ( uint8_t ) 0x7F;
					_ui >>= 7;

					if ( _ui ) { nextByte |= ( uint8_t ) 0x80;}

					m_buffer << nextByte ;
					++size;

				   } while ( _ui );
				m_size += size;
				  //cout<< " unsigned int" <<endl;

		}
        
        void PROTOSerializer::write ( const uint32_t id, const float& f ) {
			//cout<< "Writing id: " << id <<  " of float "  <<endl;
			// Writing id
			      uint64_t _id = htole64( id );
			      uint8_t size = 0;
				   do {
					char nextByte = _id & ( uint8_t ) 0x7F;
					_id >>= 7;

					if ( _id ) { nextByte |= ( uint8_t ) 0x80;}

					m_buffer << nextByte ;
					++size;

				   } while ( _id );
			      m_size += size;
			//Writing float
			      float _f = f;
		          m_buffer.write(reinterpret_cast<const char *>(&_f), 4);
		          m_size += 4;
				  //cout<< "float" <<endl;


	   }

        void PROTOSerializer::write ( const uint32_t id, const double& d ) {
			//cout<< " Writing id: " << id << " of double " <<endl;
			// Writing id
			      uint32_t _id = htole64( id );
			      uint8_t size = 0;
				   do {
					char nextByte = _id & ( uint8_t ) 0x7F;
					_id >>= 7;

					if ( _id ) { nextByte |= ( uint8_t ) 0x80;}

					m_buffer << nextByte ;
					++size;

				   } while ( _id );
			      m_size += size;
		  // Writing double
			     double _d = d;
			     m_buffer.write(reinterpret_cast<const char *>(&_d), 8);
			     m_size += 8;
				  //cout<< "double" <<endl;

        }
        void PROTOSerializer::write ( const uint32_t id, const string& s ) {
			//cout<< "Writing id: " << id << "of string "  << endl;
			// Writing id
			      uint32_t _id = htole64( id );
			      uint8_t size = 0;
				   do {
					char nextByte = _id & ( uint8_t ) 0x7F;
					_id >>= 7;

					if ( _id ) { nextByte |= ( uint8_t ) 0x80;}

					m_buffer << nextByte ;
					++size;

				   } while ( _id );
			      m_size += size;
			      //cout<< "done with string id" <<endl;
            //Writing string
			// Writing strings length
			      //cout<< "writing strings size" <<endl;
			 uint32_t string_size = s.size();
			 string_size = htole64( string_size );
			 size = 0;//cout
			 m_size += string_size;
			 do {
				  char nextByte = string_size & ( uint8_t ) 0x7F;
				  string_size >>= 7;

				  if ( string_size ) { nextByte |= ( uint8_t ) 0x80;}

				  m_buffer << nextByte ;
				  ++size;

			 	 } while ( string_size );
			  m_size += size;

			  // writing string

	          m_buffer.write(reinterpret_cast<const char *>(s.c_str()), string_size);
			  //cout<< "string" <<endl;



		}

        void PROTOSerializer::write ( const uint32_t id, const void* data, const uint32_t& size ) {
			cout<< "Writing id: " << id << "of user data " << data << " size of it " << size << endl;
		}

        } 
} // core:base

  
