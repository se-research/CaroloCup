/*
 * Copyright (c) Christian Berger and Bernhard Rumpe, Rheinisch-Westfaelische Technische Hochschule Aachen, Germany.
 *
 * The Hesperia Framework.
 */

#include <arpa/inet.h>

#include <iostream>

#include "core/wrapper/MutexFactory.h"
#include "core/wrapper/TCPConnection.h"

namespace core {
    namespace wrapper {

        TCPConnection::TCPConnection() :
            m_connectionListenerMutex(),
            m_connectionListener(NULL),
            m_stringListenerMutex(),
            m_stringListener(NULL),
            m_buffer(),
            m_disableDefragmentationMutex(),
            m_disableDefragmentation(false) {

            m_connectionListenerMutex = auto_ptr<Mutex>(MutexFactory::getInstance().createMutex());
            if (m_connectionListenerMutex.get() == NULL) {
                throw std::string("(TCPConnection) Error creating mutex for connection listener.");
            }

            m_stringListenerMutex = auto_ptr<Mutex>(MutexFactory::getInstance().createMutex());
            if (m_stringListenerMutex.get() == NULL) {
                throw std::string("(TCPConnection) Error creating mutex for string listener.");
            }

            m_disableDefragmentationMutex = auto_ptr<Mutex>(MutexFactory::getInstance().createMutex());
            if (m_disableDefragmentationMutex.get() == NULL) {
                throw std::string("(TCPConnection) Error creating mutex for disabling defragmentation.");
            }
        }

        TCPConnection::~TCPConnection() {
            setStringListener(NULL);
            setConnectionListener(NULL);
        }

        void TCPConnection::setConnectionListener(ConnectionListener* listener) {
            m_connectionListenerMutex->lock();
                m_connectionListener = listener;
            m_connectionListenerMutex->unlock();
        }

        void TCPConnection::invokeConnectionListener() {
            m_connectionListenerMutex->lock();
                if (m_connectionListener != NULL) {
                    m_connectionListener->handleConnectionError();
                }
            m_connectionListenerMutex->unlock();
        }

        void TCPConnection::setStringListener(StringListener *listener) {
            m_stringListenerMutex->lock();
                m_stringListener = listener;
            m_stringListenerMutex->unlock();
        }

        void TCPConnection::invokeStringListener(const string& data) {
            m_stringListenerMutex->lock();
                if (m_stringListener != NULL) {
                    m_stringListener->nextString(data);
                }
            m_stringListenerMutex->unlock();
        }

        void TCPConnection::disableAutomaticDataDefragmentation() {
        	m_disableDefragmentationMutex->lock();
				m_disableDefragmentation = true;
        	m_disableDefragmentationMutex->unlock();
        }

        void TCPConnection::send(const string& data) {
        	bool bypassDefragmentation = false;

        	m_disableDefragmentationMutex->lock();
				bypassDefragmentation = m_disableDefragmentation;
        	m_disableDefragmentationMutex->unlock();

        	if (bypassDefragmentation) {
        		sendImplementation(data);
        	}
        	else {
            	const uint32_t dataSize = htonl(data.length());

                stringstream dataStream;
                dataStream.write(reinterpret_cast<const char*>(&dataSize), sizeof(uint32_t));
                dataStream << data;

                sendImplementation(dataStream.str());
        	}
        }

        void TCPConnection::receivedString(const string &s) {
        	bool bypassDefragmentation = false;

        	m_disableDefragmentationMutex->lock();
				bypassDefragmentation = m_disableDefragmentation;
        	m_disableDefragmentationMutex->unlock();

        	if (bypassDefragmentation) {
        		invokeStringListener(s);
        	}
        	else {
            	// Add newly received data to buffer.
            	m_buffer << s;

                // Decode a packet and return remaining bytes.
                const string remainder = decodePacket(m_buffer.str());

                m_buffer.str("");
                m_buffer << remainder;
        	}
        }

        const string TCPConnection::decodePacket(const string &packet) {
            // If nothing could be found simply return the received string as remainder.
        	string remainder = packet;

        	const uint32_t lengthSize = sizeof(uint32_t);
        	// If the received string might contain data try to decode the data.
            while (remainder.length() > lengthSize) {
            	// Read the encoded size of the sent data.
            	stringstream packetStream(remainder.substr(0, lengthSize));

            	uint32_t sentPacketSize = 0;
            	packetStream.read(reinterpret_cast<char*>(&sentPacketSize), sizeof(uint32_t));
            	sentPacketSize = ntohl(sentPacketSize);

                // Check if the sent data might be decoded as well.
            	if (sentPacketSize <= (remainder.length() - lengthSize) ) {
                	const string payload = remainder.substr(lengthSize, sentPacketSize);
            		invokeStringListener(payload);

            		remainder = remainder.substr(lengthSize + sentPacketSize, string::npos);
            	}
            	else {
            		// No more parseable data.
            		break;
            	}
            }

            return remainder;
        }
    }
}
