/*
 * sensorProtocol.cpp
 *
 *  Created on: Nov 18, 2014
 *      Author: ashfaq
 */
#include "sensorProtocol.h"
#include "core/wrapper/MutexFactory.h"


namespace msv{
using namespace std;
using namespace core::wrapper;
	sensorProtocol::sensorProtocol() :
            m_stringListenerMutex(),
            m_stringListener(NULL),
            m_partialData()
        {
            m_stringListenerMutex = auto_ptr<Mutex>(MutexFactory::createMutex());
            if (m_stringListenerMutex.get() == NULL) {
                throw std::string("(SerialPort) Error creating mutex for string listener.");
            }
        }

	sensorProtocol::~sensorProtocol() {
            setStringListener(NULL);
        }

        void sensorProtocol::setStringListener(StringListener* listener) {
            m_stringListenerMutex->lock();
                m_stringListener = listener;
            m_stringListenerMutex->unlock();
        }


        void sensorProtocol::receivedPartialString(const string &s) {
                  m_partialData.write(s.c_str(), s.length());
                  if(hasCompleteData()){
                    m_partialData.seekg(0, ios_base::beg);
                     // cout<<"Recived New:"<<m_partialData.str()<<endl;
                    int posFirst=m_partialData.str().find('.')+1;
                	  int posSecond=m_partialData.str().find('.',posFirst);
                    int length=posSecond-posFirst;
                	 // cout<<"pos:"<<pos<<endl;
               	   // cout<<"length:"<<length<<endl;
                  
                  invokeStringListener(m_partialData.str().substr(posFirst,length));
                  // cout<<"m_par invoked:"<<m_partialData.str()<<endl;
                  m_partialData.str(m_partialData.str().substr(posSecond));
                
                  // cout<<"remaining data:"<<m_partialData.str()<<endl;
                 
                	  // After using str() to set the remaining string, the write pointer
                	  // points to the beginning of the stream and further receivedString() calls
                	  // would override existing data. So the write pointer has to point to the
                	  // end of the stream.
                	 // m_partialData.seekp(0, ios_base::end);
			    m_partialData.str("");
                  }
               }

               bool sensorProtocol::hasCompleteData() {
                  //TODO:VerifyProtocol if required
            	   m_partialData.seekg(0, ios_base::end);
            	   const uint8_t streamSize = m_partialData.tellg();
            	   m_partialData.seekg(0, ios_base::beg);
            	   if(streamSize>60){ //Currently 100 is used as a safety value to receive 2 lines from the sensor data, going ahead we need to make this more robust with some inherent logic
            		   return true;
            	   }
            	   return false;
               }

               void sensorProtocol::invokeStringListener(const string& data) {
                   m_stringListenerMutex->lock();
                       if (m_stringListener != NULL) {
                           m_stringListener->nextString(data);
                       }
                   m_stringListenerMutex->unlock();
               }

}



