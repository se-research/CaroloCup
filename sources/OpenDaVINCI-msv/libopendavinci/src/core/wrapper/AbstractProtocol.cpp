/*
 * OpenDaVINCI.
 *
 * This software is open source. Please see COPYING and AUTHORS for further information.
 */

#include "core/wrapper/MutexFactory.h"
#include "core/wrapper/AbstractProtocol.h"

namespace core {
    namespace wrapper {

        AbstractProtocol::AbstractProtocol() :
            m_stringSenderMutex(),
            m_stringSender(NULL)
        {
            m_stringSenderMutex = auto_ptr<Mutex>(MutexFactory::createMutex());
            if (m_stringSenderMutex.get() == NULL) {
                throw std::string("(SerialPort) Error creating mutex for string sender.");
            }
        }

        AbstractProtocol::~AbstractProtocol() {
            setStringSender(NULL);
        }

        void AbstractProtocol::setStringSender(StringSender* sender) {
            m_stringSenderMutex->lock();
                m_stringSender = sender;
            m_stringSenderMutex->unlock();
        }

        void AbstractProtocol::sendByStringSender(const string &data) {
            if (data.length() > 0) {
                m_stringSenderMutex->lock();
                    if (m_stringSender != NULL) {
                        m_stringSender->send(data);
                    }
                m_stringSenderMutex->unlock();
            }
        }

    }
}

