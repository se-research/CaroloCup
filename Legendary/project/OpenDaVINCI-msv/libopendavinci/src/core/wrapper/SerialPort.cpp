/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/wrapper/MutexFactory.h"
#include "core/wrapper/SerialPort.h"

namespace core {
    namespace wrapper {

        SerialPortSettings::SerialPortSettings() :
            m_parity(PARITY_NONE),
            m_dataBits(DATA_8),
            m_stopBits(STOP_1) {}

        SerialPort::SerialPort() :
            m_settings(),
            m_connectionListenerMutex(),
            m_connectionListener(NULL),
            m_partialStringReceiverMutex(),
            m_partialStringReceiver(NULL)
        {
            m_connectionListenerMutex = auto_ptr<Mutex>(MutexFactory::createMutex());
            if (m_connectionListenerMutex.get() == NULL) {
                throw std::string("(SerialPort) Error creating mutex for connection listener.");
            }

            m_partialStringReceiverMutex = auto_ptr<Mutex>(MutexFactory::createMutex());
            if (m_partialStringReceiverMutex.get() == NULL) {
                throw std::string("(SerialPort) Error creating mutex for partial string receiver.");
            }
        }

        SerialPort::~SerialPort() {
            setConnectionListener(NULL);
            setPartialStringReceiver(NULL);
        }

        void SerialPort::setSettings(const SerialPortSettings &settings) {
            m_settings = settings;
        }

        const SerialPortSettings SerialPort::getSettings() const {
            return m_settings;
        }

        void SerialPort::setConnectionListener(ConnectionListener* listener) {
            m_connectionListenerMutex->lock();
                m_connectionListener = listener;
            m_connectionListenerMutex->unlock();
        }

        void SerialPort::setPartialStringReceiver(PartialStringReceiver* psr) {
            m_partialStringReceiverMutex->lock();
                m_partialStringReceiver = psr;
            m_partialStringReceiverMutex->unlock();
        }

        void SerialPort::send(const string& data) {
            sendImplementation(data);
        }

        void SerialPort::receivedString(const string &s)
        {
            m_partialStringReceiverMutex->lock();
                if (m_partialStringReceiver != NULL) {
                    m_partialStringReceiver->receivedPartialString(s);
                }
            m_partialStringReceiverMutex->unlock();
        }

         void SerialPort::invokeConnectionListener() {
            m_connectionListenerMutex->lock();
                if (m_connectionListener != NULL) {
                    m_connectionListener->handleConnectionError();
                }
            m_connectionListenerMutex->unlock();
        }

   }
}
