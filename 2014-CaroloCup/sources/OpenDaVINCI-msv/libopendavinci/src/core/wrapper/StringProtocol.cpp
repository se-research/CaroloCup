/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/wrapper/MutexFactory.h"
#include "core/wrapper/StringProtocol.h"

namespace core {
    namespace wrapper {

        StringProtocol::StringProtocol() :
            AbstractProtocol(),
            m_stringListenerMutex(),
            m_stringListener(NULL),
            m_partialData()
        {
            m_stringListenerMutex = auto_ptr<Mutex>(MutexFactory::createMutex());
            if (m_stringListenerMutex.get() == NULL) {
                throw std::string("(SerialPort) Error creating mutex for string listener.");
            }
        }

        StringProtocol::~StringProtocol() {
            setStringListener(NULL);
        }

        void StringProtocol::setStringListener(StringListener* listener) {
            m_stringListenerMutex->lock();
                m_stringListener = listener;
            m_stringListenerMutex->unlock();
        }

        void StringProtocol::send(const string& data) {
            if (data.length() > 0) {
                const uint32_t dataSize = htonl(data.length());
                stringstream dataStream;
                dataStream.write(reinterpret_cast<const char*>(&dataSize), sizeof(uint32_t));
                dataStream << data;

                sendByStringSender(dataStream.str());
            }
        }

        void StringProtocol::receivedPartialString(const string &s) {
           m_partialData.write(s.c_str(), s.length());

            if ( hasCompleteData() ) {
                m_partialData.seekg(0, ios_base::beg);

                uint32_t dataSize = 0;
                m_partialData.read(reinterpret_cast<char*>(&dataSize), sizeof(uint32_t));
                dataSize = ntohl(dataSize);
                const uint32_t bytesToIgnore = m_partialData.gcount();

                // Split the stringstreams's string into to pieces. The first
                // piece contains the data for the StringListener, the second
                // piece will remain in the stringstream.
                invokeStringListener(m_partialData.str().substr(bytesToIgnore, dataSize));
                m_partialData.str(m_partialData.str().substr(bytesToIgnore+dataSize));

                // After using str() to set the remaining string, the write pointer
                // points to the beginning of the stream and further receivedString() calls
                // would override existing data. So the write pointer has to point to the
                // end of the stream.
                m_partialData.seekp(0, ios_base::end);
            }
        }

        bool StringProtocol::hasCompleteData() {
            // Get size of stringstream
            m_partialData.seekg(0, ios_base::end);
            const uint32_t streamSize = m_partialData.tellg();
            m_partialData.seekg(0, ios_base::beg);

            // Read size of transfered data
            uint32_t dataSize = 0;
            m_partialData.read(reinterpret_cast<char*>(&dataSize), sizeof(uint32_t));
            dataSize = ntohl(dataSize);
            const uint32_t bytesToIgnore = m_partialData.gcount();

            if ( streamSize >= dataSize + bytesToIgnore) {
                return true;
            }

            return false;
        }

        void StringProtocol::invokeStringListener(const string& data) {
            m_stringListenerMutex->lock();
                if (m_stringListener != NULL) {
                    m_stringListener->nextString(data);
                }
            m_stringListenerMutex->unlock();
        }

    }
}

